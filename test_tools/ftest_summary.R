#!/usr/bin/env Rscript

# minimum duration (in minutes) for tests (aka shorter tests will be ignored)
MIN_DUR <- 0

# Input filenames
TA_LOG_FILENAME <- "ta.log"
SUMMARY_T_FILENAME <- "summary_T.txt"
LOG_CLEANED_FILENAME <- "log.cleaned"
PARAMS_FILENAME <- "params_tbl.txt"

# Output pdf filename (an empty string disables plot printing)
PLOT_FILENAME <- "ftest_summary.pdf"

# get current dir
orig_dir <- getwd()

# initialize data frame
st <- data.frame(jitter=numeric(), rate=numeric())
sc <- data.frame(jitter=numeric(), node=numeric(), rate=numeric())

# get test directories
dirs <- grep(pattern=file.path("^\\.",".*","data-[0-9]+"), list.dirs(), value=T)
dirs <- file.path(orig_dir, dirs) # absolute path

message("Loading test:")
for (dir in dirs) {
    message("  ",basename(dir), appendLF=F)
    setwd(dir)
    # read params file
    params <- read.table(PARAMS_FILENAME, header=T)[c("sink","period","jitter","jitter_node","glossy_relay_max")]
    # get success rate for the sink
    if (!file.exists(SUMMARY_T_FILENAME)) { message(" Skip (no ",SUMMARY_T_FILENAME,")"); next }
    T_sink_success_rate <- read.table(SUMMARY_T_FILENAME, header=T)$T_sink_success_rate * 100
    # read TA log
    if (!file.exists(TA_LOG_FILENAME)) { message(" Skip (no ",TA_LOG_FILENAME,")"); next }
    ta_log <- read.table(TA_LOG_FILENAME, header=T)[c("epoch","src","dst","status")]
    #senders <- sort(unique(ta_log[ta_log$status==5,]$src))
    # clean
    ta_log <- ta_log[ta_log$dst==params$sink,]
    ta_log$dst <- NULL
    # check minimum duration
    if (max(ta_log$epoch) * params$period / 60 < MIN_DUR) { message(" Skip (too short test)"); next}
    
    # read raw H log
    fs <- file(LOG_CLEANED_FILENAME)
    # read and parse file lines
    raw_data <- readLines(fs)
    raw_data <- grep("^H ", raw_data, value=T)
    raw_data <- sub("^H ([0-9]+) ([0-9]+)\\t.*$", "\\1 \\2", raw_data)
    # get data and close connections
    raw_data <- read.table(textConnection(raw_data), col.names=c("epoch","hop"))
    ta_log <- merge(ta_log, raw_data)
    closeAllConnections()
    
    # check packets received with the wrong hop number
    recv_out_hop <- ta_log[(ta_log$status==0)&(ta_log$hop!=params$glossy_relay_max),]
    if (nrow(recv_out_hop)) { message(" Packets with wrong hops (",nrow(recv_out_hop),")", appendLF=F) }
    recv <- ta_log[(ta_log$status==0)&(ta_log$hop==params$glossy_relay_max),]
    
    # calculate the frequency, i.e. how many times senders were received
    tb <- table(recv$src)
    tb <- data.frame(node = as.integer(names(tb)), count = as.vector(tb))
    tb$rate <- tb$count / sum(tb$count) * 100
    tb$rate <- round(tb$rate, 2)
    tb$count <- NULL
    # add params
    tb$jitter <- params$jitter
    
    # save data
    st[nrow(st)+1,] <- c(params$jitter,T_sink_success_rate)
    sc <- rbind(sc,tb)
    message()
}

# return to the previous directory
setwd(orig_dir)

# check if data exist
if ((nrow(st) == 0) || (nrow(sc) == 0)) { message("No data loaded"); quit() } 

# compute mean if multiple test are available for the same configuration
st <- aggregate(rate ~ jitter, data=st, FUN=mean)
sc <- aggregate(rate ~ jitter + node, data=sc, FUN=mean)
# sorting
st <- st[order(st$jitter),]
sc <- sc[order(sc$jitter,sc$node),]

# add some missing entries
sc <- merge(x = sc, all.y=T,
    y = setNames(merge(unique(sc$node), unique(sc$jitter)), c("node","jitter")))
sc$rate[is.na(sc$rate)] <- 0 # remove NA values (to 0)

print(st)
print(sc)

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

# plot
message("Plot success rate")
st_plot <- ggplot(data=st, aes(x=jitter, y=rate)) +
	geom_line() + geom_point() + ylim(0, 100) +
    scale_x_continuous(breaks=unique(st$jitter)) +
	ggtitle("T Sink success rate") + 
    xlab("Jitter (us)") + ylab("Success rate %")
print(st_plot)

message("Plot packet rate received")
sc_plot <- ggplot(data=sc, aes(x=factor(jitter), y=rate, fill=factor(node))) +
	geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=rate), size=3, vjust=-0.1, position = position_dodge(1) ) +
	theme(legend.position="top") + labs(fill="") +
	ggtitle(paste("Packet rate received (jitter node ",params$jitter_node,")")) +
    xlab("Jitter (us)") + ylab("Recv rate %")
print(sc_plot)
