#!/usr/bin/env Rscript

d = read.table("ta.log",h=T)

# read params file
sink_id <- read.table("params_tbl.txt", header=T)[c("sink")]$sink

# remove for print better
d$time <- NULL

d <- d[d$dst==sink_id,]

print(d)

