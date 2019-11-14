#!/bin/bash

# toolpath
declare -r TOOLPATH=$(dirname $(realpath $0))

# at least a device must be provided
[[ $# < 2 ]] && echo "Not enough args" && exit 1

# check timeout (the first arg)
declare -r number_regex="^[0-9]+$"
! [[ "$1" =~ $number_regex ]] || [[ $1 == 0 ]] && \
    echo "Timeout value (in minutes) must be the first argument" && exit 1

# save the timeout aka test duration in minutes and shift args
declare -ir TIMEOUT="$1"
shift

# check devices
for DEV in "$@"; do
    [[ ! -c "$DEV" || ! -r "$DEV" ]] && echo "$DEV is not a device" && exit 1
done

# this function kill every jobs
kill_jobs() {
    local j="$(jobs -p)"
    [[ ! -z "$j" ]] && kill $j
}
# be sure to kill jobs on SIGINT or SIGTERM
trap kill_jobs SIGINT SIGTERM

# listen in backgrounds
for DEV in "$@"; do
    declare OUT="$(basename "$DEV").log"
    echo "$DEV --> $OUT"
    # reset the device
    $TOOLPATH/tos-bsl.py -r --tmote -c "$DEV"
    # dump in background
    $TOOLPATH/../tools/sky/serialdump-linux -T%s -b115200 "$DEV" > "$OUT" &
    # just wait a bit...
    sleep 2
done

# sleep waiting $TIMEOUT minutes
echo "Wait $TIMEOUT minutes..."
# print a progress bar of $n_spaces
progress_bar() {
    [[ -z "$TIMEOUT" ]] && return

    declare -ir n_spaces=30
    declare -ir interval=$(expr $TIMEOUT \* 60 / ${n_spaces} )

    printf "[%${n_spaces}s]\r[" ' '
    for (( I=0; I<${n_spaces}; I++ )); do
        sleep $interval
        echo -n "="
    done
    echo "]"
}
# call the progress bar / waiting function
progress_bar

# kill jobs before exit
kill_jobs

exit 0

