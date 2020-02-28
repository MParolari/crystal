#!/usr/bin/env Rscript

# minimum duration (in minutes) for tests (aka shorter tests will be ignored)
MIN_DUR <- 5
# only epochs after this are considered
FIRST_EPOCH <- 60*2

# Input filenames
ENERGY_LOG_FILENAME <- "energy.log"
PARAMS_FILENAME <- "params_tbl.txt"

# Output pdf filename (an empty string disables plot printing)
PLOT_FILENAME <- "energy_coll.pdf"

# Parameters and settings used
# TODO get this values from logs and check they are the same for all tests
DUR_S <- 10
DUR_T <- 5
DUR_A <- 7
EMPTY_R <- 2

# get current dir
orig_dir <- getwd()

# initialize data frame
data <- data.frame()

# get test directories
dirs <- grep(pattern=file.path("^\\.",".*","data-[0-9]+"), list.dirs(), value=T)
dirs <- file.path(orig_dir, dirs) # absolute path

message("Loading test:")
for (dir in dirs) {
    message("  ",basename(dir), appendLF=F)
    setwd(dir)
    # read params file
    params <- read.table(PARAMS_FILENAME, header=T)
    if (!("no_S" %in% colnames(params))) { params$no_S <- 0 }
    params <- params[c("sink","senders","period","no_S")]
    # get energy consumption
    if (!file.exists(ENERGY_LOG_FILENAME)) {
        message(" Skip (no ",ENERGY_LOG_FILENAME,")"); next }
    energy <- read.table(ENERGY_LOG_FILENAME, header=T, colClasses=c("numeric"))
    # check minimum duration
    if (max(energy$epoch) * params$period / 60 < MIN_DUR) {
        message(" Skip (too short test)"); next}
    # clean data
    energy <- energy[(energy$epoch >= FIRST_EPOCH), c("node","ontime")]
    energy$sink <- params$sink
    energy$senders <- params$senders
    energy$no_S <- as.logical(params$no_S)
    
    # save data
    data <- rbind(data,energy)
    message()
}; rm(dir, params, energy)

# return to the previous directory
setwd(orig_dir)

# check if data exist
if (nrow(data) == 0) { message("No data loaded"); quit() }

# compute mean if multiple test are available for the same configuration
data <- aggregate(ontime ~ sink + senders + no_S + node, data=data, FUN=mean)
# compute mean between nodes
tmp_data <- data[data$sink!=data$node,]
davg <- aggregate(ontime ~ sink + senders + no_S, data=tmp_data, FUN=mean)
# compute other stats
tmp <- aggregate(ontime ~ sink + senders + no_S, data=tmp_data, FUN=sd)
colnames(tmp)[which(names(tmp) == "ontime")] <- "sd"
davg <- merge(davg, tmp, by=c("sink","senders","no_S"))
tmp <- aggregate(ontime ~ sink + senders + no_S, data=tmp_data, FUN=max)
colnames(tmp)[which(names(tmp) == "ontime")] <- "max"
davg <- merge(davg, tmp, by=c("sink","senders","no_S"))
rm(tmp,tmp_data)
# compute pernode diff
pernode <- merge(
    data[!data$no_S,], data[data$no_S,], by=c("sink","senders","node"))
pernode$saved <- pernode$ontime.x - pernode$ontime.y
pernode$error <- pernode$ontime.x * (1 -
    (DUR_S / (DUR_S + (DUR_T + DUR_A)*(EMPTY_R + pernode$senders)) )) -
    pernode$ontime.y
pernode$error <- abs(pernode$error)
pernode$rel_error <- pernode$error * 100 / pernode$ontime.x
pernode$no_S.x <- NULL; pernode$no_S.y <- NULL;
pernode$ontime.x <- NULL; pernode$ontime.y <- NULL;
# compute average errors
davg <- merge(davg, aggregate(cbind(error,rel_error) ~ sink + senders,
            data=pernode[pernode$sink!=pernode$node,], FUN=mean),
        by=c("sink","senders"))
davg[!davg$no_S,c("error","rel_error")] <- NA

# sorting
data <- data[order(data$sink,data$senders,data$no_S,data$node),]
davg <- davg[order(davg$sink,davg$senders,davg$no_S),]
pernode <- pernode[order(pernode$sink,pernode$senders,pernode$node),]

print(davg)

################################################################################
# Plots
################################################################################

# if filename for plots isn't defined, simply quit
if ((!exists("PLOT_FILENAME")) || (!is.character(PLOT_FILENAME))
	|| (PLOT_FILENAME == "")) quit()

# load library
library(ggplot2)

# human readable labels
data$S <- NA
data[ as.logical(data$no_S),]$S <- "Disable"
data[!as.logical(data$no_S),]$S <- "Enable"
davg$S <- NA
davg[ as.logical(davg$no_S),]$S <- "Disable"
davg[!as.logical(davg$no_S),]$S <- "Enable"

# open pdf
pdf(PLOT_FILENAME)

# plot for each sink
message("Plot ontime mean and max value for sink:", appendLF=F)
for (sink in unique(data$sink)) {
    message(" ", sink, appendLF=F)
    avg_plot <- ggplot(data=davg[davg$sink==sink,],
            aes(x=factor(senders), y=ontime, fill=factor(S))) +
        geom_bar(stat="identity", position=position_dodge2(reverse=T)) +
        geom_errorbar(width=0.25, position=position_dodge2(reverse=T),
            aes(ymin=ontime-sd, ymax=ontime+sd)) +
        geom_text(aes(label=round(ontime, 2)), size=3, vjust=-0.1,
            position = position_dodge2(1, reverse=T) ) +
        theme(legend.position="top") + labs(fill="S phase:") +
        ggtitle(paste("Mean per-epoch Radio-ON time with sink", sink)) +
        xlab("Number of senders") + ylab("ms/s")
    print(avg_plot)

    message("x", appendLF=F)
    max_plot <- ggplot(data=davg[davg$sink==sink,],
            aes(x=factor(senders), y=max, fill=factor(S))) +
        geom_bar(stat="identity", position=position_dodge2(reverse=T)) +
        geom_text(aes(label=round(max, 2)), size=3, vjust=-0.1,
            position = position_dodge2(1, reverse=T) ) +
        theme(legend.position="top") + labs(fill="S phase:") +
        ggtitle(paste("Max per-epoch Radio-ON time with sink", sink)) +
        xlab("Number of senders") + ylab("ms/s")
    print(max_plot)

    message("e", appendLF=F)
    err_plot <- ggplot(data=davg[(davg$sink==sink)&(davg$no_S),],
            aes(x=factor(senders), y=error, fill=factor(senders))) +
        geom_bar(stat="identity") +
        geom_text(aes(label=round(error, 2)), size=3, vjust=-0.1) +
        geom_text(aes(label=round(rel_error, 2)), size=3, vjust=+1.1) +
        theme(legend.position="none") +
        ggtitle(paste("Error per-epoch Radio-ON time with sink", sink)) +
        xlab("Number of senders") + ylab("ms/s")
    print(err_plot)

    message("s", appendLF=F)
    for (sender in unique(data$senders)) {
        pernode_plot <- ggplot(
            data=pernode[(pernode$sink==sink)&(pernode$senders==sender),],
            aes(x=factor(node), y=saved, fill=factor(node))) +
        geom_bar(stat="identity") +
        geom_text(aes(label=round(saved, 2)), size=3, vjust=-0.1) +
        theme(legend.position="none") +
        ggtitle(paste(
            "Saved per-epoch Radio-ON time with sink", sink,
            "and", sender, "senders")) +
        xlab("Node") + ylab("ms/s")
        print(pernode_plot)
    }
}; rm(sink,sender)
message()
