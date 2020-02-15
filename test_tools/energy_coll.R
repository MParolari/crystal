#!/usr/bin/env Rscript

# minimum duration (in minutes) for tests (aka shorter tests will be ignored)
MIN_DUR <- 5

# Input filenames
TA_LOG_FILENAME <- "ta.log"
ENERGY_LOG_FILENAME <- "energy.log"
PARAMS_FILENAME <- "params_tbl.txt"

# Output pdf filename (an empty string disables plot printing)
PLOT_FILENAME <- "energy_coll.pdf"

# get current dir
orig_dir <- getwd()

# initialize data frame
data <- data.frame(sink=numeric(), senders=numeric(), no_S=logical(),
    ontime_mean=numeric(), ontime_sd=numeric(), ontime_max=numeric())

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
    # read TA log
    if (!file.exists(TA_LOG_FILENAME)) {
        message(" Skip (no ",TA_LOG_FILENAME,")"); next }
    max_epoch <- max( read.table(TA_LOG_FILENAME, header=T)$epoch )
    # check minimum duration
    if (max_epoch * params$period / 60 < MIN_DUR) {
        message(" Skip (too short test)"); next}
    # get energy consumption
    if (!file.exists(ENERGY_LOG_FILENAME)) {
        message(" Skip (no ",ENERGY_LOG_FILENAME,")"); next }
    energy <- read.table(ENERGY_LOG_FILENAME, header=T, colClasses=c("numeric"))
    energy <- energy[energy$node!=params$sink,]
    energy <- aggregate(ontime ~ node, data=energy, FUN=mean)
    
    # save data
    data[nrow(data)+1,] <- c(params$sink, params$senders, params$no_S,
        mean(energy$ontime), ontime_sd=sd(energy$ontime), max(energy$ontime))
    message()
}; rm(dir, params, energy, max_epoch)

# return to the previous directory
setwd(orig_dir)

# check if data exist
if (nrow(data) == 0) { message("No data loaded"); quit() }

# compute mean if multiple test are available for the same configuration
data <- aggregate(
    cbind(ontime_mean,ontime_sd,ontime_max) ~ sink + senders + no_S, data=data, FUN=mean)
# sorting
data <- data[order(data$sink,data$senders,data$no_S),]

print(data)

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

# open pdf
pdf(PLOT_FILENAME)

# plot for each sink
message("Plot ontime mean and max value for sink:", appendLF=F)
for (sink in unique(data$sink)) {
    message(" ", sink, appendLF=F)
    mean_plot <- ggplot(data=data[data$sink==sink,],
            aes(x=factor(senders), y=ontime_mean, fill=factor(S))) +
        geom_bar(stat="identity", position=position_dodge2(reverse=T)) +
        geom_errorbar(width=0.25, position=position_dodge2(reverse=T),
            aes(ymin=ontime_mean-ontime_sd,ymax=ontime_mean+ontime_sd)) +
        geom_text(aes(label=round(ontime_mean,2)), size=3, vjust=-0.1,
            position = position_dodge2(1, reverse=T) ) +
        theme(legend.position="top") + labs(fill="S phase:") +
        ggtitle(paste("Mean per-epoch Radio-ON time with sink", sink)) +
        xlab("Number of senders") + ylab("ms/s")
    print(mean_plot)

    message("x", appendLF=F)
    max_plot <- ggplot(data=data[data$sink==sink,],
            aes(x=factor(senders), y=ontime_max, fill=factor(S))) +
        geom_bar(stat="identity", position=position_dodge2(reverse=T)) +
        geom_text(aes(label=round(ontime_max,2)), size=3, vjust=-0.1,
            position = position_dodge2(1, reverse=T) ) +
        theme(legend.position="top") + labs(fill="S phase:") +
        ggtitle(paste("Max per-epoch Radio-ON time with sink", sink)) +
        xlab("Number of senders") + ylab("ms/s")
    print(max_plot)
}; rm(sink)
message()
