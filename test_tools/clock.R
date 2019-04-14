#!/usr/bin/env Rscript

# This script takes logged timestamps of VHT and computes the skew along with
# some statistical properties (like mean and standard deviation).
# 'ggplot2' library is required for plots
# 'moments' library is required for extra stats (like skewness and kurtosis)

# RTimer (32kHz clock) ticks every second
RTIMER_SECONDS <- 32768
# DCO ticks every RTimer ticks
CLOCK_PHI <- 128
# Epoch duration in seconds
EPOCH_DUR <- 1
# System should be synced after this epoch (only epochs after this are plotted)
FIRST_EPOCH <- 60
# If true, outputs will be in microseconds
MICROSECONDS_CONV <- T
# Input filename
LOG_FILENAME <- "clock.log"
PARAMS_FILENAME <- "params_tbl.txt"
# Output pdf filename (an empty string disables plot printing)
PLOT_FILENAME <- "clock.pdf"

# try to load functions for extra stats
EXTRA_STATS <- require(moments)

# read parsed log file
message("Read input: ", LOG_FILENAME)
fields <- c("epoch", "time", "src", "t_ref_h")
data <- read.table(LOG_FILENAME, header=T)[fields]

# time conversion in 10^-6 seconds
if (MICROSECONDS_CONV) {
	message("Conversion in microseconds")
	# from ticks to microseconds
	data$t_ref_h <- data$t_ref_h / CLOCK_PHI / RTIMER_SECONDS * 10^6
	# from seconds to microseconds
	EPOCH_DUR <- EPOCH_DUR * 10^6
	# binwidth for histograms
	HIST_BINWIDTH <- 1 / CLOCK_PHI / RTIMER_SECONDS * 10^6
	# time/skew unit for output label/log
	TIME_UNIT <- "(micro-seconds)"
} else {
	# with output in number of number of ticks, convert from seconds to ticks
	EPOCH_DUR <- EPOCH_DUR * CLOCK_PHI * RTIMER_SECONDS
	# default binwidth for histograms
	HIST_BINWIDTH <- 1
	# default time/skew unit for output label/log
	TIME_UNIT <- "(VHT ticks)"
}

# allocate some new columns in main data structure
data$out_of_sync <- NA
data$diff_t_ref_h <- NA
data$skew <- NA
data$centered_skew <- NA

# allocate data.frame for stats
stats <- data.frame(src=sort(unique(data$src)), max=NA, mean=NA, sd=NA)
# allocate for extra stats (if they can be computed)
if (EXTRA_STATS) {
	stats$sk <- NA
	stats$ku <- NA
}

# clock skew computing (for each node)
for (node in stats$src) {
	message("Computing skew for node ", node)
	cleaning <- TRUE
	# clean the dataset from wrong samples (ie ref_time (i) is < ref_time (i-1))
  	while (cleaning) {
		cleaning <- FALSE
		# get valid entries (the ones that have a timestamp != 0)
		# and use diff on timestamps, negative values are the wrong samples,
		# so get a boolean array and delete them
		b <- c(FALSE, diff(data[(data$src==node)&(data$t_ref_h!=0),]$t_ref_h)<0)
		# if there are some wrong samples
		if (!all(!b)) {
			# wrong values are overwritten with 0 (and so not considered later)
			data[(data$src==node)&(data$t_ref_h!=0),]$t_ref_h[b] <- 0
			cleaning <- TRUE # clean again....
		}
	}; rm(cleaning, b)
	# get valid entries (the ones that have a timestamp != 0)
	x <- data[(data$src==node)&(data$t_ref_h!=0),]
	# difference between epochs
	x$ediff <- c(1,diff(x$epoch))
	# difference between timestamp
	x$tdiff <- c(0,diff(x$t_ref_h))
	# compute the skew as the difference between timestamps over
	# the difference between epochs ("local clock" / "real global clock")
	# minus the epoch duration
	x$skew <- x$tdiff / x$ediff - EPOCH_DUR
	# get only "realiable" epochs
	y <- x[x$epoch>=FIRST_EPOCH,]
	# compute and save stats
	my <- mean(y$skew)
	stats[stats$src==node,]$max <- max(abs(y$skew - my))
	stats[stats$src==node,]$mean <- my
	stats[stats$src==node,]$sd <- sd(y$skew)
	if (EXTRA_STATS) {
		stats[stats$src==node,]$sk <- skewness(y$skew)
		stats[stats$src==node,]$ku <- kurtosis(y$skew)
	}
	# save results (missing entries should be NA)
	data[(data$src==node)&(data$t_ref_h!=0),]$out_of_sync <- x$ediff
	data[(data$src==node)&(data$t_ref_h!=0),]$diff_t_ref_h <- x$tdiff
	data[(data$src==node)&(data$t_ref_h!=0),]$skew <- x$skew
	data[(data$src==node)&(data$t_ref_h!=0),]$centered_skew <- x$skew - my
}; rm(node, x, y, my)

# stats are printed on stdout (commented, for user)
cat("#\n# Clock skew stats",TIME_UNIT,"\n#\n")
writeLines(paste("#", capture.output(print(stats))))

# if the unit is still in VHT-ticks, output data for later analysis
if (!MICROSECONDS_CONV) {
	# read the sink id from parameters
	sink_id = read.table(PARAMS_FILENAME, header=T)$sink[1]
	# add a column with the sink id reference
	stats$sink <- sink_id
	# write sink, nodes and estimated skew mean
	cat("#\n# Table for later analysis\n#\n")
	cat("sink\tnode\tskew\n")
	write.table(stats[,c("sink","src","mean")], col.names=F, row.names=F)
	cat("#\n")
	# delete the sink id column
	stats$sink <- NULL
} else { cat("#\n# Table for later analysis not printed\n#\n") }


################################################################################
# Plots
################################################################################

# if filename for plots isn't defined, simply quit
if ((!exists("PLOT_FILENAME")) || (!is.character(PLOT_FILENAME))
	|| (PLOT_FILENAME == "")) quit()

# load library
library(ggplot2)

# delete NA and initial epochs
data <- data[!is.na(data$skew)&(data$epoch>=FIRST_EPOCH),]

# open pdf
pdf(PLOT_FILENAME)

message("Plot clock skew")
skew_plot <- ggplot(data=data, aes(x=epoch, y=skew, color=factor(src))) +
	geom_line() +
	geom_text(data=data[data$epoch==data$epoch[1],], check_overlap=F,
		aes(x=max(data$epoch)*(1+1/30), y=skew, label=src), show.legend=F) +
	theme(legend.position="none") +
	ggtitle("clock skew") + ylab(paste("skew",TIME_UNIT))
print(skew_plot)

message("Plot centered clock skew")
centered_skew_plot <- ggplot() + theme(legend.position="none") +
	geom_boxplot(data=data,
		aes(x=factor(src), y=centered_skew, color=factor(src), group=src)) +
	geom_errorbar(data=stats, color="black",
		aes(x=factor(src), ymin=-sd, ymax=sd)) +
	ggtitle("centered clock skew") + ylab(paste("skew",TIME_UNIT))
print(centered_skew_plot)

# plots for each node
for (node in stats$src) {
	message("Plot clock skew for node ", node)
	skew_node_plot <-
		ggplot(data=data[(data$src==node),], aes(x=epoch, y=skew)) +
		geom_line() +
		ggtitle(paste("clock skew node",node)) + ylab(paste("skew",TIME_UNIT))
	print(skew_node_plot)

	message("Plot histogram skew for node ", node)
	hist_skew_node_plot <-
		ggplot(data=data[(data$src==node),], aes(x=skew)) +
		geom_histogram(binwidth=HIST_BINWIDTH) +
		ggtitle(paste("histogram skew node",node)) + ylab("frequency")
	print(hist_skew_node_plot)
}; rm(node)


