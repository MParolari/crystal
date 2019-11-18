#!/usr/bin/env Rscript

# minimum duration (in minutes) for tests (aka shorter tests will be ignored)
MIN_DUR <- 0

# Input filenames
TA_LOG_FILENAME <- "ta.log"
SUMMARY_T_FILENAME <- "summary_T.txt"
ENV_FILENAME <- "../crystal-test.sky.env"
PARAMS_FILENAME <- "params_tbl.txt"

# Output pdf filename (an empty string disables plot printing)
PLOT_FILENAME <- "etest_summary.pdf"

# get current dir
orig_dir <- getwd()

# initialize data frame
st <- data.frame(sink=numeric(), jitter=numeric(), senders=numeric(), rate=numeric())
sj <- data.frame(sink=numeric(), jitter=numeric(), senders=numeric(),
    node=numeric(), total=numeric(), delayed=numeric(), effective=numeric())

# cur_idx and delayed_idx formulas, from crystal-test.c
cur_idx <- function(epoch) {
    ((epoch - params$start_epoch) %% params$active_epochs) * params$senders
}
delayed_idx <- function(epoch) {
    ((epoch - params$start_epoch) %% params$active_epochs) %% params$senders
}

# given an epoch, this returns the jitter/delayed node
get_delayed <- function(epoch) {
    sndtbl[1 + cur_idx(epoch) + delayed_idx(epoch)]
}
# given an epoch, this returns the (array of) senders in that epoch
get_senders <- function(epoch) {
    idx <- cur_idx(epoch)
    v <- c(idx:(idx + params$senders -1)) + 1
    return(sndtbl[v])
}

# get test directories
dirs <- grep(pattern=file.path("^\\.",".*","data-[0-9]+"), list.dirs(), value=T)
dirs <- file.path(orig_dir, dirs) # absolute path

message("Loading test:")
for (dir in dirs) {
    message("  ",basename(dir), appendLF=F)
    setwd(dir)
    # read params file
    params <- read.table(PARAMS_FILENAME, header=T)[c("sink","period","start_epoch","active_epochs","senders","jitter")]
    # get success rate for the sink
    if (!file.exists(SUMMARY_T_FILENAME)) { message(" Skip (no ",SUMMARY_T_FILENAME,")"); next }
    T_sink_success_rate <- read.table(SUMMARY_T_FILENAME, header=T)$T_sink_success_rate * 100
    # read TA log
    if (!file.exists(TA_LOG_FILENAME)) { message(" Skip (no ",TA_LOG_FILENAME,")"); next }
    ta_log <- read.table(TA_LOG_FILENAME, header=T)[c("epoch","dst","status")]
    # clean
    ta_log <- ta_log[ta_log$dst==params$sink,]
    ta_log$dst <- NULL
    # check minimum duration
    if (max(ta_log$epoch) * params$period / 60 < MIN_DUR) { message(" Skip (too short test)"); next}
    # read sndtbl
    fs <- file(ENV_FILENAME)
    # read and parse file lines
    raw_data <- readLines(fs, warn=F)
    raw_data <- grep("uint8_t sndtbl\\[\\]", raw_data, value=T)
    # clean
    raw_data <- sub("^.*\\{([0-9,]+)\\}.*$", "\\1", raw_data)
    raw_data <- strsplit(raw_data,",")[[1]]
    # get data and close connections
    sndtbl <- as.integer(raw_data)
    closeAllConnections()

    # get the jitter node for each epoch
    ta_log$jitter_node <- get_delayed(ta_log$epoch)
    # get the "sender list"
    total_senders <- c()
    for(e in ta_log$epoch) { # note: we should consider the first epoch of each node
        total_senders <- c(total_senders, get_senders(e))
    }
    # calculate the frequency, i.e. how many times a node was a sender
    tb <- table(total_senders)
    tb <- data.frame(node = as.integer(names(tb)), total = as.vector(tb))
    jd <- tb # simply save the result
    # calculate how many times a node was delayed (i.e. has a jitter)
    tb <- table(ta_log$jitter_node)
    tb <- data.frame(node = as.integer(names(tb)), delayed = as.vector(tb))
    jd <- merge(x = jd, y = tb, all.x=T) # left join
    # calculate how many times a node was an effective jitter/delayed node
    tb <- table(ta_log[ta_log$status!=0,]$jitter_node)
    tb <- data.frame(node = as.integer(names(tb)), effective = as.vector(tb))
    jd <- merge(x = jd, y = tb, all.x=T) # left join
    # remove NA values (to 0)
    jd$delayed[is.na(jd$delayed)] <- 0
    jd$effective[is.na(jd$effective)] <- 0
    # add params
    jd$sink <- params$sink
    jd$jitter <- params$jitter
    jd$senders <- params$senders

    # save data
    st[nrow(st)+1,] <- c(params$sink,params$jitter,params$senders,T_sink_success_rate)
    sj <- rbind(sj,jd)
    message()
}

# return to the previous directory
setwd(orig_dir)

# check if data exist
if ((nrow(st) == 0) || (nrow(sj) == 0)) { message("No data loaded"); quit() } 

# compute mean if multiple test are available for the same configuration
st <- aggregate(rate ~ sink + jitter + senders, data=st, FUN=mean)
sj <- aggregate(cbind(total,delayed,effective) ~ sink + jitter + senders + node, data=sj, FUN=sum)
# sorting
st <- st[order(st$sink,st$jitter,st$sender),]
sj <- sj[order(sj$sink,sj$jitter,sj$sender,sj$node),]

print(st)
#print(sj)

################################################################################
# Plots
################################################################################

# if filename for plots isn't defined, simply quit
if ((!exists("PLOT_FILENAME")) || (!is.character(PLOT_FILENAME))
	|| (PLOT_FILENAME == "")) quit()

# load library
library(ggplot2)

# open pdf
pdf(PLOT_FILENAME)

message("Plot success rate")
st_plot <- ggplot(data=st, aes(x=jitter, y=rate, color=factor(sink), shape=factor(senders))) +
	geom_line() + geom_point() + ylim(0, 100) +
    scale_x_continuous(breaks=unique(st$jitter)) +
	theme(legend.position="top") + labs(color="Sinks/Subnets:",shape="Number of senders:") +
	ggtitle("T Sink success rate") + 
    xlab("jitter (us)") + ylab("success rate %")
print(st_plot)

# plots for each test (sink/jitter/senders)
message("Plot jitter node distribution: ", appendLF=F)
tests <- unique(sj[,c("sink","jitter","senders")])
for (test in seq(1,nrow(tests))) {
    test <- tests[test,] # get row
    # get the title for the plot
    notes <- sprintf("Sink %d, Jitter %d, Senders %d", test$sink, test$jitter, test$senders)
    # get pertest data (intersection)
    pertest <- merge(sj, test)
    pertest[,c("sink","jitter","senders")] <- NULL
    # prepare a data frame for the barplot
    # (only one column "value" with the values, and the column "type" attached)
    tb <- pertest[,c("node","total")]
    colnames(tb)[colnames(tb)=="total"] <- "value"
    tb$type <- "1 - Sender node"
    jd <- tb
    tb <- pertest[,c("node","delayed")]
    colnames(tb)[colnames(tb)=="delayed"] <- "value"
    tb$type <- "2 - Jitter node"
    jd <- rbind(jd, tb)
    tb <- pertest[,c("node","effective")]
    colnames(tb)[colnames(tb)=="effective"] <- "value"
    tb$type <- "3 - Effective jitter node"
    jd <- rbind(jd, tb)
    # plot
    message(notes, "; ", appendLF=F)
    jd_plot <- ggplot(data=jd, aes(x=factor(node), y=value, fill=factor(type))) +
	    geom_bar(stat="identity", position=position_dodge()) +
        geom_text(aes(label=value), size=3, vjust=-0.1, position = position_dodge(1) ) +
	    theme(legend.position="top") + labs(fill="") +
	    ggtitle(paste("Jitter nodes distribution (",notes,")")) +
        xlab("Node") + ylab("Count")
    print(jd_plot)
}
message()
