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
FIRST_EPOCH <- 5
# If true, outputs will be in microseconds
MICROSECONDS_CONV <- T
# Input filename
LOG_FILENAME <- "skew.log"
PARAMS_FILENAME <- "params_tbl.txt"
# Output pdf filename (an empty string disables plot printing)
PLOT_FILENAME <- "skew.pdf"

# try to load functions for extra stats
EXTRA_STATS <- require(moments)

# read params file
params <- read.table(PARAMS_FILENAME, header=T)[c("sink")]
# sink node id
sink_id = params$sink[1]

# read parsed log file
message("Read input: ", LOG_FILENAME)
fields <- c("epoch", "n_ta", "src", "skew_error", "flag", "period_skew", "samples", "time")
data <- read.table(LOG_FILENAME, header=T)[fields]
# delete unused columns
data$time <- NULL

# time conversion in 10^-6 seconds
if (MICROSECONDS_CONV) {
	message("Conversion in microseconds")
	# from ticks to microseconds
	data$skew_error <- data$skew_error / CLOCK_PHI / RTIMER_SECONDS * 10^6
	data$period_skew <- data$period_skew / CLOCK_PHI / RTIMER_SECONDS * 10^6
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
data$centered_skew <- NA
data$unreliable <- (data$samples < 100)

# allocate data.frame for stats
err_stats <- data.frame(src=sort(unique(data$src)), max=NA, mean=NA, sd=NA, abs_mean=NA)
est_stats <- data.frame(src=sort(unique(data$src)), max=NA, mean=NA, sd=NA, clean=NA)
# allocate for extra stats (if they can be computed)
if (EXTRA_STATS) {
	err_stats$sk <- NA
	err_stats$ku <- NA
	est_stats$sk <- NA
	est_stats$ku <- NA
}

# stats computing (for each node)
message("Computing skew stats for node: ", appendLF=F)
for (node in err_stats$src) {
	message(node, " ", appendLF=F)
	# get entries of this node in "realiable" epochs
	x <- data[(data$src==node)&(data$epoch>=FIRST_EPOCH),]
	# compute the mean and the centered estimated skew period
	mx <- mean(x$period_skew)
	data[data$src==node,]$centered_skew <- data[data$src==node,]$period_skew - mx
	# compute and save stats
	err_stats[err_stats$src==node,]$max <- max(abs(x$skew_error))
	err_stats[err_stats$src==node,]$mean <- mean(x$skew_error)
	err_stats[err_stats$src==node,]$sd <- sd(x$skew_error)
	err_stats[err_stats$src==node,]$abs_mean <- mean(abs(x$skew_error))
	est_stats[est_stats$src==node,]$max <- max(abs(x$period_skew))
	est_stats[est_stats$src==node,]$mean <- mx
	est_stats[est_stats$src==node,]$sd <- sd(x$period_skew)
	est_stats[est_stats$src==node,]$clean <- all(!x$unreliable)
	if (EXTRA_STATS) {
		err_stats[err_stats$src==node,]$sk <- skewness(x$skew_error)
		err_stats[err_stats$src==node,]$ku <- kurtosis(x$skew_error)
		est_stats[est_stats$src==node,]$sk <- skewness(x$period_skew)
		est_stats[est_stats$src==node,]$ku <- kurtosis(x$period_skew)
	}
	# get outliers for the boxplot
	q25 <- quantile(x$skew_error, 1/4)
	q75 <- quantile(x$skew_error, 3/4)
	data[(data$src==node)&(data$epoch>=FIRST_EPOCH),]$boxplot_outlier <-
		(!(x$flag)) & (
			(x$skew_error > q75 + (q75-q25) * 1.5) |
			(x$skew_error < q25 - (q75-q25) * 1.5) )
}; rm(node, x, q25, q75, mx)
message()

# stats are printed on stdout (commented, for user)
cat("#\n# Skew error stats",TIME_UNIT,"\n#\n")
writeLines(paste("#", capture.output(print(err_stats))))


# if the unit is still in VHT-ticks, output data for later analysis
if (!MICROSECONDS_CONV) {
	# merge by node ids
	m <- merge(est_stats[,c("src","mean","sd","clean")], err_stats[,c("src","abs_mean")], by="src")
	# add a column with the sink id reference
	m$sink <- sink_id
	# write sink, nodes and estimated skew mean
	cat("#\n# Table for later analysis\n#\n")
	cat("sink\tnode\tskew\tstd_dev\tabs_error\n")
	write.table(m[m$clean,c("sink","src","mean","sd","abs_mean")], col.names=F, row.names=F)
	cat("#\n")
} else {
	# stats are printed on stdout (commented, for user)
	cat("#\n# Estimated skew stats",TIME_UNIT,"\n#\n")
	writeLines(paste("#", capture.output(print(est_stats))))
}
cat("# Nodes that lost synchronization\n")
writeLines(paste("#", capture.output(print(est_stats[(!est_stats$clean),]$src))))
cat("#\n")


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

# x axis values
data$X <- data$n_ta
data[data$X == (2^16 -1),]$X <- (-1)
data$X <- data$epoch * 10 + data$X + 1

# open pdf
pdf(PLOT_FILENAME)

message("Plot skew error")
skew_error_plot <- ggplot() + theme(legend.position="none") +
	geom_boxplot(data=data,
		aes(x=factor(src), y=skew_error, color=factor(src), group=src)) +
	geom_point(data=data[data$boxplot_outlier,], color="black", shape=4, size=1,
		aes(x=factor(src), y=skew_error)) +
	geom_errorbar(data=err_stats, color="black",
		aes(x=factor(src), ymin=mean-sd, ymax=mean+sd)) +
	geom_point(data=err_stats, color="black", shape=23, size=3,
		aes(x=factor(src), y=mean)) +
	ggtitle("Skew error") + ylab(paste("skew",TIME_UNIT))
print(skew_error_plot)

message("Plot filtered skew error")
filtered_skew_error_plot <- ggplot(data=data[as.logical(data$flag),],
		aes(x=factor(src), y=skew_error, color=factor(src), group=src)) +
	theme(legend.position="none") +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", color="black", shape=23, size=3) +
	stat_summary(geom="errorbar", color="black",
		fun.ymin = function(x) mean(x) - sd(x),
		fun.ymax = function(x) mean(x) + sd(x)) +
	ggtitle("Filtered skew error") + ylab(paste("skew",TIME_UNIT))
print(filtered_skew_error_plot)

message("Plot estimated skew")
estimated_skew_plot <- ggplot(data=data,
		aes(x=X, y=period_skew, color=factor(src))) +
	geom_line() +
	geom_text(data=data[data$epoch==data$epoch[1],], check_overlap=F,
		aes(x=max(data$X)*(1+1/30), y=period_skew, label=src), show.legend=F) +
	theme(legend.position="none") +
	ggtitle("Estimated period skew") + ylab(paste("skew",TIME_UNIT))
print(estimated_skew_plot)

message("Plot centered estimated skew")
centered_estimated_skew_plot <- ggplot() + theme(legend.position="none") +
	geom_boxplot(data=data,
		aes(x=factor(src), y=centered_skew, color=factor(src), group=src)) +
	geom_errorbar(data=est_stats, color="black",
		aes(x=factor(src), ymin=-sd, ymax=sd)) +
	ggtitle("Centered estimated period skew") + ylab(paste("skew",TIME_UNIT))
print(centered_estimated_skew_plot)

message("Plot reliable estimated skew")
reliable_estimated_skew_plot <- ggplot(data=data[!data$unreliable,],
		aes(x=factor(src), y=centered_skew, color=factor(src), group=src)) +
	theme(legend.position="none") +
	geom_boxplot() +
	stat_summary(fun.y=mean, geom="point", color="black", shape=23, size=3) +
	stat_summary(geom="errorbar", color="black",
		fun.ymin = function(x) mean(x) - sd(x),
		fun.ymax = function(x) mean(x) + sd(x)) +
	ggtitle("Reliable estimated period skew") + ylab(paste("skew",TIME_UNIT))
print(reliable_estimated_skew_plot)

# plots for each node
message("Plot skew error for node:", appendLF=F)
for (node in err_stats$src) {
	message(" ", node, appendLF=F)
	skew_error_node_plot <-
		ggplot(data=data[(data$src==node),], aes(x=X, y=skew_error)) +
		geom_line() +
		geom_point(data=data[(data$src==node)&(!(data$flag)),]) +
		geom_point(data=data[(data$src==node)&(data$flag>1),], aes(color=factor(flag))) +
		ggtitle(paste("skew error node",node)) + ylab(paste("skew",TIME_UNIT))
	print(skew_error_node_plot)

	message("h", appendLF=F)
	hist_skew_error_node_plot <-
		ggplot(data=data[(data$src==node),], aes(x=skew_error)) +
		geom_histogram(binwidth=HIST_BINWIDTH) +
		ggtitle(paste("Histogram skew error node",node)) +
		xlab(paste("skew",TIME_UNIT)) + ylab("frequency")
	print(hist_skew_error_node_plot)

	message("e", appendLF=F)
	estimated_skew_node_plot <-
		ggplot(data=data[(data$src==node),], aes(x=X, y=period_skew)) +
		geom_line() +
		geom_point(data=data[(data$src==node)&(data$unreliable),]) +
		ggtitle(paste("Estimated skew node",node)) + ylab(paste("skew",TIME_UNIT))
	print(estimated_skew_node_plot)

	message("h", appendLF=F)
	hist_estimated_skew_node_plot <-
		ggplot(data=data[(data$src==node),], aes(x=period_skew)) +
		geom_histogram(binwidth=HIST_BINWIDTH) +
		ggtitle(paste("Histogram estimated skew node",node)) +
		xlab(paste("skew",TIME_UNIT)) + ylab("frequency")
	print(hist_estimated_skew_node_plot)
}; rm(node)
message()

