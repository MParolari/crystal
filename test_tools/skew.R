#!/usr/bin/env Rscript

# This script takes logged skew errors (in VHT resolution) and computes
# some statistical properties (like mean and standard deviation).
# 'ggplot2' library is required for plots
# 'moments' library is required for extra stats (like skewness and kurtosis)

# RTimer (32kHz clock) ticks every second
RTIMER_SECONDS <- 32768
# DCO ticks every RTimer ticks
CLOCK_PHI <- 128
# System should be synced after this epoch (only epochs after this are plotted)
FIRST_EPOCH <- 60
# If true, outputs will be in microseconds
MICROSECONDS_CONV <- T
# Input filename
LOG_FILENAME <- "skew.log"
# Output pdf filename (an empty string disables plot printing)
PLOT_FILENAME <- "skew.pdf"

# try to load functions for extra stats
EXTRA_STATS <- require(moments)

# read parsed log file
message("Read input: ", LOG_FILENAME)
fields <- c("epoch", "time", "src", "skew_error", "flag")
data <- read.table(LOG_FILENAME, header=T)[fields]

# time conversion in 10^-6 seconds
if (MICROSECONDS_CONV) {
	message("Conversion in microseconds")
	# from ticks to microseconds
	data$skew_error <- data$skew_error / CLOCK_PHI / RTIMER_SECONDS * 10^6
	# binwidth for histograms
	HIST_BINWIDTH <- 1 / CLOCK_PHI / RTIMER_SECONDS * 10^6
	# time/skew unit for output label/log
	TIME_UNIT <- "(micro-seconds)"
} else {
	# default binwidth for histograms
	HIST_BINWIDTH <- 1
	# default time/skew unit for output label/log
	TIME_UNIT <- "(VHT ticks)"
}

# allocate some new columns in main data structure
data$boxplot_outlier <- NA

# allocate data.frame for stats
stats <- data.frame(src=sort(unique(data$src)), max=NA, mean=NA, sd=NA)
# allocate for extra stats (if they can be computed)
if (EXTRA_STATS) {
	stats$sk <- NA
	stats$ku <- NA
}

# stats computing (for each node)
message("Computing skew stats for node: ", appendLF=F)
for (node in stats$src) {
	message(node, " ", appendLF=F)
	# get entries of this node in "realiable" epochs
	x <- data[(data$src==node)&(data$epoch>=FIRST_EPOCH),]
	# compute and save stats
	stats[stats$src==node,]$max <- max(abs(x$skew_error))
	stats[stats$src==node,]$mean <- mean(x$skew_error)
	stats[stats$src==node,]$sd <- sd(x$skew_error)
	if (EXTRA_STATS) {
		stats[stats$src==node,]$sk <- skewness(x$skew_error)
		stats[stats$src==node,]$ku <- kurtosis(x$skew_error)
	}
	# get outliers for the boxplot
	q25 <- quantile(x$skew_error, 1/4)
	q75 <- quantile(x$skew_error, 3/4)
	data[(data$src==node)&(data$epoch>=FIRST_EPOCH),]$boxplot_outlier <-
		(x$flag) & (
			(x$skew_error > q75 + (q75-q25) * 1.5) |
			(x$skew_error < q25 - (q75-q25) * 1.5) )
}; rm(node, x, q25, q75)
message()

# stats are printed on stdout (commented, for user)
cat("#\n# Skew error stats",TIME_UNIT,"\n#\n")
writeLines(paste("#", capture.output(print(stats))))


################################################################################
# Plots
################################################################################

# if filename for plots isn't defined, simply quit
if ((!exists("PLOT_FILENAME")) || (!is.character(PLOT_FILENAME))
	|| (PLOT_FILENAME == "")) quit()

# load library
library(ggplot2)

# delete initial epochs
data <- data[data$epoch>=FIRST_EPOCH,]

# open pdf
pdf(PLOT_FILENAME)

message("Plot skew error")
skew_error_plot <- ggplot() + theme(legend.position="none") +
	geom_boxplot(data=data,
		aes(x=factor(src), y=skew_error, color=factor(src), group=src)) +
	geom_point(data=data[data$boxplot_outlier,], color="black", shape=4, size=1,
		aes(x=factor(src), y=skew_error)) +
	geom_errorbar(data=stats, color="black",
		aes(x=factor(src), ymin=mean-sd, ymax=mean+sd)) +
	geom_point(data=stats, color="black", shape=23, size=3,
		aes(x=factor(src), y=mean)) +
	ggtitle("skew error") + ylab(paste("skew",TIME_UNIT))
print(skew_error_plot)

message("Plot filtered skew error")
filtered_skew_error_plot <- ggplot(data=data[!data$flag,],
		aes(x=factor(src), y=skew_error, color=factor(src), group=src)) +
	theme(legend.position="none") +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", color="black", shape=23, size=3) +
	stat_summary(geom="errorbar", color="black",
		fun.ymin = function(x) mean(x) - sd(x),
		fun.ymax = function(x) mean(x) + sd(x)) +
	ggtitle("filtered skew error") + ylab(paste("skew",TIME_UNIT))
print(filtered_skew_error_plot)

# plots for each node
message("Plot skew error for node: ", appendLF=F)
for (node in stats$src) {
	message(node, appendLF=F)
	skew_error_node_plot <-
		ggplot(data=data[(data$src==node),], aes(x=epoch, y=skew_error)) +
		geom_line() +
		geom_point(data=data[(data$src==node)&(data$flag),]) +
		ggtitle(paste("skew error node",node)) + ylab(paste("skew",TIME_UNIT))
	print(skew_error_node_plot)

	message("h ", appendLF=F)
	hist_skew_error_node_plot <-
		ggplot(data=data[(data$src==node),], aes(x=skew_error)) +
		geom_histogram(binwidth=HIST_BINWIDTH) +
		ggtitle(paste("histogram skew error node",node)) + ylab("frequency")
	print(hist_skew_error_node_plot)
}; rm(node)
message()

