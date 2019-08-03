#!/bin/bash

# toolpath
declare -r TOOLPATH=$(dirname $(realpath $0))

# at least a argument must be provided
[[ $# == 0 ]] && echo "No args" && exit 1

# check that the filenames exist
for FILENAME in "$@"; do
    [[ ! -r "$FILENAME" ]] && echo "File $FILENAME not found" && exit 1
done

# directory for results; it's a CRC32 of the given filenames (thx stackoverflow)
declare -r RESULT_DIR=$(echo \
    $(echo -n "$*" | gzip -c | tail -c8 | od -t x4 -N 4 -A n))
mkdir -p "$RESULT_DIR"
echo "Result directory: $RESULT_DIR"

# declare constant regex
declare -r starting_line_regex="Contiki [0-9\.]+ started"
declare -r node_id_regex="Crystal config. Node ID ([0-9]+)"

# array of filenames
declare -a CLEANED=()

# foreach file
for FILENAME in "$@"; do
    # declare some vars
    declare -i STARTING_LINE=0
    declare -i NODE_ID=0
    declare -i I=0

    # get some info and parameters
    while read line; do
        I=$I+1
        [[ "$line" =~ $starting_line_regex ]] && STARTING_LINE=$I
        [[ "$line" =~ $node_id_regex ]] && NODE_ID=${BASH_REMATCH[1]} && break
    done < "$FILENAME"

    # check parameters just obtained
    [[ $STARTING_LINE == 0 ]] \
        && echo "Starting line not found - $FILENAME" && continue
    [[ $NODE_ID == 0 ]] && \
        echo "Node ID not found - $FILENAME" && continue

    # cut the first starting line, add the node id and save the output
    tail -n +$STARTING_LINE "$FILENAME" | \
        sed -e "s/|/ $(printf %0*d 2 $NODE_ID)|/g" \
        > "$RESULT_DIR/$FILENAME.cleaned"

    # add the filename to the array, they will be merged later
    CLEANED+=( "$FILENAME.cleaned" )
done

# copy params file
[[ -f params_tbl.txt ]] && cp -v params_tbl.txt "$RESULT_DIR/"

# change dir and merge files
cd "$RESULT_DIR"
sort -s -m -k1n -o log.cleaned "${CLEANED[@]}"
echo "Log files merged"

# finally run the scripts
$TOOLPATH/parser_ta.py --format local
Rscript $TOOLPATH/stats_ta.R > stats_out.txt
Rscript $TOOLPATH/clock.R > clock_stats.txt
Rscript $TOOLPATH/skew.R > skew_stats.txt
