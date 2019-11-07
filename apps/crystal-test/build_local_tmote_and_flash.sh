#!/bin/bash

# set to 1 for motelab testbeds
#export CFLAGS="$CFLAGS -DTINYOS_SERIAL_FRAMES=1"

# set COOJA to 1 for simulating Glossy in Cooja
#CFLAGS+=" -DCOOJA=1"


CFLAGS+=" -DSINK_ID=1" # the sink node id
CFLAGS+=" -DTX_POWER=31 -DCRYSTAL_CONF_DEF_CHANNEL=26 -DCONCURRENT_TXS=2 -DNUM_ACTIVE_EPOCHS=1"
CFLAGS+=" -DPAYLOAD_LENGTH=20"
CFLAGS+=" -DCRYSTAL_CONF_LOGLEVEL=CRYSTAL_LOGS_ALL"
#CFLAGS+=" -DCRYSTAL_CONF_LOGLEVEL=CRYSTAL_LOGS_EPOCH_STATS"

CFLAGS+=" -DSTART_DELAY_SINK=20"
CFLAGS+=" -DSTART_DELAY_NONSINK=10"

CFLAGS+=" -DCRYSTAL_CONF_NTX_T=1"

CFLAGS+=" -DSTART_EPOCH=1"

if [ $# -lt 1 ]; 
    then echo "Specify the node ID"
    exit
fi

NODE_ID=$1
CFLAGS+=" -DNODE_ID=$NODE_ID"

# default values (no jitter)
JITTER=0
JITTER_NODE=0
# 120 us ~=  504 DCOticks
# 160 us ~=  672 DCOticks
# 220 us ~=  923 DCOticks
# 260 us ~= 1091 DCOticks
JITTER=923
JITTER_NODE=2

CFLAGS+=" -DJITTER=$JITTER"
CFLAGS+=" -DJITTER_NODE=$JITTER_NODE"


export CFLAGS

echo "static uint8_t sndtbl[] = {2,3};" > sndtbl.c
make clean
rm -f crystal-test.sky crystal-test.ihex

make crystal-test.ihex && mv crystal-test.sky crystal-test-$NODE_ID.sky && mv crystal-test.ihex crystal-test-$NODE_ID.ihex 
rm -f sndtbl.c symbols.h symbols.c

echo "testbed active_epochs start_epoch period senders sink n_tx_s n_tx_t n_empty payload jitter    jitter_node" > params_tbl.txt
echo "local   1             1           1      2       1    3      1      2       2       $JITTER   $JITTER_NODE" >> params_tbl.txt


DEV=/dev/ttyUSB0
[[ -c "$DEV" && -w "$DEV" ]] && \
    ../../test_tools/tos-bsl.py -e -p -r -I --tmote -c "$DEV" crystal-test-$NODE_ID.ihex

make clean
