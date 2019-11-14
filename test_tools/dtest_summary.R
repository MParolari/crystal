#!/usr/bin/env Rscript

# Output pdf filename (an empty string disables plot printing)
PLOT_FILENAME <- "dtest_summary.pdf"

# get current dir
orig_dir <- getwd()

# initialize data frame
st <- data.frame(sink=numeric(),jitter_node=numeric(),sender=numeric(),jitter=numeric(),
    rate=numeric())
sr <- data.frame(sink=numeric(),jitter_node=numeric(),sender=numeric(),jitter=numeric(),
    node=numeric(),rate=numeric())

# get test directories
dirs <- grep(pattern=file.path("^\\.",".*","data-[0-9]+"), list.dirs(), value=T)
dirs <- file.path(orig_dir, dirs) # absolute path

message("Loading test:")
for (dir in dirs) {
    message("  ",basename(dir), appendLF=F)
    setwd(dir)
    # read params file
    params <- read.table("params_tbl.txt", header=T)[c("sink","jitter","jitter_node")]
    # get success rate for the sink
    if (!file.exists("summary_T.txt")) { message(" Skip (no summary_T.txt)"); next }
    T_sink_success_rate <- read.table("summary_T.txt", header=T)$T_sink_success_rate * 100
    # get pernode packets stats (sent and received)
    if (!file.exists("pernode.txt")) { message(" Skip (no pernode.txt)"); next }
    pkt <- read.table("pernode.txt", header=T)[c("node","n_sent","n_recv")]
    # get the sender (i.e. remove from nodelist the sink and the jitter node)
    sender <- setdiff(pkt$node, c(params$sink, params$jitter_node))
    if (length(sender) != 1) { message(" Skip - Error: sender not valid"); next }
    # computes the packets rate
    pkt <- pkt[pkt$node!=params$sink,] # remove sink stats
    pkt$rate <- pkt$n_recv / pkt$n_sent * 100
    pkt$rate[is.na(pkt$rate)] <- 0 # remove NA values (to 0)
    # save data
    st[nrow(st)+1,] <- c(params$sink,params$jitter_node,sender,params$jitter,T_sink_success_rate)
    for (i in c(1:nrow(pkt))) {
        sr[nrow(sr)+1,] <- c(params$sink,params$jitter_node,sender,params$jitter,pkt$node[i],pkt$rate[i])
    }
    message()
}

# return to the previous directory
setwd(orig_dir)

# compute mean if multiple test are available for the same configuration
st <- aggregate(rate ~ sink + jitter_node + sender + jitter, data=st, FUN=mean)
sr <- aggregate(rate ~ sink + jitter_node + sender + jitter + node, data=sr, FUN=mean)
# sorting
st <- st[order(st$sink,st$jitter_node,st$sender,st$jitter),]
sr <- sr[order(sr$sink,sr$jitter_node,sr$sender,sr$jitter,sr$node),]

print(st)
print(sr)

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
st_plot <- ggplot(data=st, aes(x=jitter, y=rate, color=interaction(sink,jitter_node,sender,sep='-'))) +
	geom_line() + geom_point() + ylim(0, 100) +
    scale_x_continuous(breaks=unique(st$jitter)) +
	theme(legend.position="top") + labs(color="Subnets:") +
	ggtitle("T Sink success rate") + 
    xlab("jitter (us)") + ylab("success rate %")
print(st_plot)

# plots for each node
message("Plot packets success rate: ", appendLF=F)
nodes <- unique(sr[,c("sink","jitter_node","sender","node")])
for (i in seq(1,nrow(nodes))) {
    # get pernode data (intersection)
    pernode <- merge(sr, nodes[i,])
    # get the title for the plot
    notes <- sprintf("%d-%d-%d",pernode$sink[1],pernode$jitter_node[1],pernode$sender[1])
    notes <- paste("node",pernode$node[1],"in",notes)
    # plot
    message(notes, ", ", appendLF=F)
    sr_plot <- ggplot(data=pernode, aes(x=jitter, y=rate)) +
	    geom_line() + geom_point() +
        scale_x_continuous(breaks=unique(pernode$jitter)) +
	    theme(legend.position="none") +
	    ggtitle(paste("Packets received by the sink (",notes,")")) +
        xlab("jitter (us)") + ylab("recv/sent %")
    print(sr_plot)
}
message()
