#!/usr/bin/env Rscript

# This script searches for "clock_stats.txt" files in local dir and subdirs,
# and reads them as tables (aka #-commented tsv files).
# After loading these data (average skew for nodes with given sinks)
# it prints a C header file on standard output, that can loaded in Crystal.

# search for files in current directory (and sub dirs)
filenames <- list.files(pattern="clock_stats.txt", recursive=T)

# stop if not files are found
if (length(filenames) == 0)
    stop("No files found") else message(length(filenames)," files found")

# sink list declaration
sinks <- c()
# main data structure (table sink/node/skew)
data <- data.frame()

for (f in filenames) {
    # read the file, ignoring lines commented with '#'
    d <- read.table(f, header=T, comment.char="#")[c("sink","node","skew")]
    # get number of row and the (hopefully new) sink number
    nr <- nrow(d)
    ns <- unique(d$sink)
    # search the new sink among the sinks already collected
    collected <- match(ns, sinks, nomatch = 0)
    # check is the input data contains some row
    if (nr == 0) { warning("No data found"); next }
    # check that input data contains only one sink
    if (length(ns) > 1) { warning("Not unique sink!"); next }
    # if the new sink was found (aka it's already collected)
    if (collected != 0) {
        warning("Sink ",ns," already collected in file: ",filenames[collected]);
        next
    }
    # add this new sink to the collected data
    sinks <- c(sinks, ns)
    data <- rbind(data, d)
}; rm(f, d, nr, ns, collected)

# check that some data is found, otherwise stop
if (nrow(data) == 0) stop("No data found")

# get nodes (as sorted set)
nodes <- sort(unique(data$node))
# check sinks (just in order to avoid errors/bugs)
sinks <- sort(sinks)
if (! setequal(sinks, unique(data$sink))) stop("Wrong sink set!")

# sink interval is [1,max_id]
max_id <- max(sinks, nodes)
# get node ids that never appear and ids never used as sink
missing <- setdiff(c(1:max_id), union(sinks,nodes))
missing_sinks <- setdiff(c(1:max_id), sinks)

# output some comment and info
cat("//",date(),"\n")
cat("// Table generated automatically by skew_ctable.R\n\n")

cat("// SINK_ID macro and skew_t type must be defined\n\n")

cat("// IDs interval: [1,", max_id, "]\n", sep="")
cat("// Missing nodes:", missing, "\n")
cat("// Sinks not available:", missing_sinks, "\n")

# check, foreach sink, that the skew is estimated for all the nodes we know
for (sink in sinks) {
    # get the nodes for which we have the estimated skew
    est_nodes <- data[(data$sink==sink),]$node
    # subtract these nodes (and the current sink) from the entire set of nodes
    missing_nodes <- setdiff(nodes, c(sink, est_nodes))
    # if it's not empty, the set contains the missing nodes for this sink
    if (length(missing_nodes) > 0) {
        # warn and print about this
        warning("Sink ", sink, " has no skew for nodes: ", missing_nodes)
        cat("// Sink", sink, "nodes not available:", missing_nodes, "\n")
    }
}; rm(sink, est_nodes, missing_nodes)

# round the skew to integer
data$skew <- round(data$skew)

# print the skew table
cat("\nstatic const skew_t skew_ctable[", max_id, "] = {\n")
for (sink in sinks) {
    # print #if or #elif and the condition
    if (sink == sinks[1]) cat("#if ") else cat("#elif ")
    cat("SINK_ID ==", sink, "\n")
    # print the skew for each node in [1,max_id]
    cat("  ")
    for (node in c(1:max_id)) {
        # get the skew value
        skew <- data[(data$sink==sink)&(data$node==node),]$skew[1]
        # if the skew is not found, use value 0
        if (is.na(skew)) skew <- 0
        # print the skew value (trailing comma is ignored by C-compiler)
        cat(skew, ",", sep="")
    }
    cat("\n")
}; rm(sink, node, skew)
cat("#elif SINK_ID\n")
cat("  #error SINK_ID defined but not found\n")
cat("#else\n")
cat("  #error SINK_ID not defined\n")
cat("#endif\n")
cat("};\n")
