
#TODO proper conversion in seconds or microseconds
#TODO general stats (max abs skew, mean, std deviation, ecc.)
#TODO general efficiency

library(ggplot2)


# RTimer (32kHz clock) ticks every second
RTIMER_SECONDS <- 32768
# DCO ticks every RTimer ticks
CLOCK_PHI <- 128
# epoch duration in seconds
EPOCH_DUR <- 1
# system should be synced after this epoch (only epochs after this are plotted)
FIRST_EPOCH <- 5

# read parsed log file
fields <- c("epoch", "time", "src", "t_ref_h")
data = read.table("clock.log", h=T)[fields]
# time conversion in 10^-6 seconds
#data$t_ref_h = data$t_ref_h / CLOCK_PHI / RTIMER_SECONDS * 10^6

# allocate some new columns
data$out_of_sync <- array(dim=length(data$time))
data$diff_t_ref_h <- array(dim=length(data$time))
data$skew <- array(dim=length(data$time))
data$centered_skew <- array(dim=length(data$time))
# clock skew computing (for each node)
for (node in sort(unique(data$src))) {
	cat("computing skew for node", node, "\n")
	cleaning <- TRUE
	# clean the dataset from wrong samples (ie ref_time (i) is < ref_time (i-1))
  	while (cleaning) {
		cleaning <- FALSE
		# get valid entries (the ones that have a timestamp != 0)
		# and use diff on timestamps, negative values are the wrong samples,
		# so get a boolean array and delete them
		b <- c(FALSE, (diff(data[(data$src==node)&(data$t_ref_h!=0),]$t_ref_h) < 0))
		# if there are some wrong samples
		if (!all(!b)) {
			# wrong values are overwritten with 0 (and so not considered later)
			data[(data$src==node)&(data$t_ref_h!=0),]$t_ref_h[b] <- 0
			cleaning <- TRUE # clean again....
		}
	}; rm(cleaning, b)
	# get valid entries (the ones that have a timestamp != 0) (for the last time)
	x <- data[(data$src==node)&(data$t_ref_h!=0),]
	# difference between epochs
	x$ediff <- c(1,diff(x$epoch))
	# difference between timestamp
	x$tdiff <- c(0,diff(x$t_ref_h))
	# compute the skew as the difference between timestamps over
	# the difference between epochs ("local clock" / "real global clock")
	x$skew <- x$tdiff / (x$ediff * EPOCH_DUR * CLOCK_PHI * RTIMER_SECONDS)
	# compute the average skew (not considering the first unrealible values)
	m <- mean(x[x$epoch>=FIRST_EPOCH,]$skew)
	# save results (missing entries should be NA)
	data[(data$src==node)&(data$t_ref_h!=0),]$out_of_sync <- x$ediff
	data[(data$src==node)&(data$t_ref_h!=0),]$diff_t_ref_h <- x$tdiff
	data[(data$src==node)&(data$t_ref_h!=0),]$skew <- x$skew
	data[(data$src==node)&(data$t_ref_h!=0),]$centered_skew <- x$skew - m
}; rm(node, x, m)

################################################################################
# Plots
################################################################################

# delete NA and initial epochs
data <- data[!is.na(data$skew)&(data$epoch>=FIRST_EPOCH),]

pdf("clock.pdf")

print("clock skew")
skew_plot <- ggplot(data=data, aes(x=epoch, y=skew, color=factor(src))) + #theme_bw() +
	geom_line() +
	geom_point() +
	ggtitle("clock skew")
print(skew_plot)

print("centered clock skew")
centered_skew_plot <- ggplot(data=data, aes(x=factor(src), y=centered_skew, color=factor(src))) + #theme_bw() +
	geom_boxplot(aes(group=src)) + theme (legend.position="none") +
	stat_summary(fun.y=mean, geom="point", color="black", shape=23) +
	stat_summary(geom="errorbar", color="black",
		fun.ymin = function(x) mean(x) - sd(x),
		fun.ymax = function(x) mean(x) + sd(x)) +
	ggtitle("centered clock skew")
print(centered_skew_plot)

# plots for each node
for (node in sort(unique(data$src))) {
	print(paste("centered clock skew node",node))
	centered_skew_node_plot <- ggplot(data=data[(data$src==node),], aes(x=epoch, y=centered_skew)) + #theme_bw() +
		geom_line() +
		#geom_point() +
		ggtitle(paste("centered clock skew node",node))
	print(centered_skew_node_plot)
}

