#!/usr/bin/env Rscript

# This script searches for "clock_stats.txt" and "skew_stats.txt"
# files in local dir and subdirs, and reads them as tables
# (aka #-commented tsv files).
# After loading these data (static skew and estimated skew),
# it computes the difference between the measured skew and the estimated one.

# RTimer (32kHz clock) ticks every second
RTIMER_SECONDS <- 32768
# DCO ticks every RTimer ticks
CLOCK_PHI <- 128
# If true, outputs will be in microseconds
MICROSECONDS_CONV <- T

# Input filenames
CLOCK_STATS_FILENAME <- "clock_stats.txt"
SKEW_STATS_FILENAME <- "skew_stats.txt"

# Output pdf filename (an empty string disables plot printing)
PLOT_FILENAME <- "est_ver.pdf"

# search for files in current directory (and sub dirs)
clock_filenames <- list.files(pattern=CLOCK_STATS_FILENAME, recursive=T)
skew_filenames <- list.files(pattern=SKEW_STATS_FILENAME, recursive=T)

# stop if not files are found
if (length(clock_filenames) == 0) { stop("No clock files found")
} else message(length(clock_filenames)," clock files found")
if (length(skew_filenames) == 0) { stop("No skew files found")
} else message(length(skew_filenames)," skew files found")

# main data structures
clock_data <- data.frame()
skew_data <- data.frame()
# sink list declaration
clock_sinks <- c()

for (f in clock_filenames) {
    # read the file, ignoring lines commented with '#'
    d <- read.table(f, header=T, comment.char="#")[c("sink","node","skew")]
    # get the (hopefully new) sink number
    ns <- unique(d$sink)
    # search the new sink among the sinks already collected
    collected <- match(ns, clock_sinks, nomatch = 0)
    # check is the input data contains some row
    if (nrow(d) == 0) { warning("No data found"); next }
    # check that input data contains only one sink
    if (length(ns) > 1) { warning("Not unique sink!"); next }
    # if the new sink was found (aka it's already collected)
    if (collected != 0) {
        warning("Sink ",ns," already collected in file: ",clock_filenames[collected]);
        next
    }
    # add this new sink to the collected data
    clock_sinks <- c(clock_sinks, ns)
    clock_data <- rbind(clock_data, d)
}; rm(f, d, ns, collected)

# check that some data is found, otherwise stop
if (nrow(clock_data) == 0) stop("No clock data found")

# check sinks (just in order to avoid errors/bugs)
clock_sinks <- sort(clock_sinks)
if (! setequal(clock_sinks, unique(clock_data$sink))) stop("Wrong sink set!")

# rename column
names(clock_data)[names(clock_data) == "skew"] <- "static_skew"


for (f in skew_filenames) {
    # read the file, ignoring lines commented with '#'
    d <- read.table(f, header=T, comment.char="#")[c("sink","node","skew","std_dev","abs_error")]
    # check is the input data contains some row
    if (nrow(d) == 0) { warning("No data found"); next }
    # add data
    skew_data <- rbind(skew_data, d)
}; rm(f, d)

# check that some data is found, otherwise stop
if (nrow(skew_data) == 0) stop("No skew data found")

# rename column
names(skew_data)[names(skew_data) == "skew"] <- "est_skew"

# compute mean if multiple test are available for the same configuration
skew_data <- aggregate(cbind(est_skew,std_dev,abs_error) ~ sink + node, data=skew_data, FUN=mean)

# merge dataset
data <- merge(clock_data, skew_data)
# sorting
data <- data[order(data$sink,data$node),]
# compute estimation error
data$est_error <- abs(data$static_skew - data$est_skew)

# time conversion in 10^-6 seconds
if (MICROSECONDS_CONV) {
	message("Conversion in microseconds")
	# from ticks to microseconds
	data$est_error <- data$est_error / CLOCK_PHI / RTIMER_SECONDS * 10^6
    data$std_dev <- data$std_dev / CLOCK_PHI / RTIMER_SECONDS * 10^6
    data$abs_error <- data$abs_error / CLOCK_PHI / RTIMER_SECONDS * 10^6
	# time/skew unit for output label/log
	TIME_UNIT <- "(micro-seconds)"
} else {
	# default time/skew unit for output label/log
	TIME_UNIT <- "(VHT ticks)"
}

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

message("Plot estimation error for sink: ", appendLF=F)
for (sink in unique(data$sink)) {
    message(sink, appendLF=F)
    est_error_plot <- ggplot(data=data[data$sink==sink,],
            aes(x=factor(node), y=est_error, fill=factor(node))) +
        geom_bar(stat="identity", position=position_dodge()) +
        geom_text(aes(label=round(est_error, digits=1)), size=3, vjust=-0.1 ) +
        theme(legend.position="none") +
        ggtitle(paste("Error of the estimated skew with sink", sink)) +
        xlab("Nodes") + ylab(paste("Error",TIME_UNIT))
    print(est_error_plot)

    message("e", appendLF=F)
    abs_error_plot <- ggplot(data=data[data$sink==sink,],
            aes(x=factor(node), y=abs(abs_error), fill=factor(node))) +
        geom_bar(stat="identity", position=position_dodge()) +
        geom_text(aes(label=round(abs_error, digits=1)), size=3, vjust=-0.1 ) +
        theme(legend.position="none") +
        ggtitle(paste("Synchronization average error (abs()) with sink", sink)) +
        xlab("Nodes") + ylab(paste("Error",TIME_UNIT))
    print(abs_error_plot)

    message("d ", appendLF=F)
    std_dev_plot <- ggplot(data=data[data$sink==sink,],
            aes(x=factor(node), y=std_dev, fill=factor(node))) +
        geom_bar(stat="identity", position=position_dodge()) +
        geom_text(aes(label=round(std_dev, digits=1)), size=3, vjust=-0.1 ) +
        theme(legend.position="none") +
        ggtitle(paste("Standard deviation for the estimated skew with sink", sink)) +
        xlab("Nodes") + ylab(paste("Standard deviation",TIME_UNIT))
    print(std_dev_plot)
}; rm(sink)
message()
