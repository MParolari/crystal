#!/usr/bin/env python2.7

#import sys
#import os
import argparse

# get arguments
ap = argparse.ArgumentParser(description='Generate a map')
ap.add_argument('--input', required=True, type=str, help='Input file')
ap.add_argument('--output', required=True, type=str, help='Output file')
ap.add_argument('--nodelist', required=False, type=str, default="nodelist.txt", help='Nodelist file')
args = ap.parse_args()

nodelist_txt = args.nodelist
input_txt = args.input
output_txt = args.output

# get all the nodes
nodes = set()
with open(nodelist_txt, "r") as f:
    for l in f:
        l = l.strip()
        if l:
            nodes.add(int(l.strip()))

# remove neighbors
with open(input_txt, "r") as f:
    # get the sink in the first line
    l = f.readline().strip()
    if l:
        sink = l
        nodes.remove(int(l))
    else: raise Exception("No sink")
    # get all the neighbors
    for l in f:
        l = l.strip()
        if l:
            nodes.remove(int(l))

# all the left nodes aren't neighbors
with open(output_txt, "w") as f:
    for n in nodes:
        f.write("%d none\n"%n)

