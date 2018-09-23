#ifndef CRYSTAL_PRIVATE_H
#define CRYSTAL_PRIVATE_H

#include "crystal.h"

//#define CRYSTAL_INTER_PHASE_GAP (RTIMER_SECOND / 250) // 4 ms
#define CRYSTAL_INTER_PHASE_GAP (RTIMER_SECOND / 500) // 2 ms
//#define CRYSTAL_INTER_PHASE_GAP (RTIMER_SECOND / 625) // 1.6 ms

//#define CRYSTAL_SHORT_GUARD           5
#define CRYSTAL_SHORT_GUARD           5
#define CRYSTAL_SHORT_GUARD_NOSYNC    5

#define CRYSTAL_SINK_END_GUARD  8  // end guard for T slot at sink (give a bit more time to receive packets)

//#define CRYSTAL_LONG_GUARD       (16*(CRYSTAL_LONGSKIP+1))
//#define CRYSTAL_LONG_GUARD       (16+CRYSTAL_LONGSKIP)
//#define CRYSTAL_LONG_GUARD       16   // ~488 us
#define CRYSTAL_LONG_GUARD        5    // ~150 us


// The receivers capture the reference at SFD while the sender uses STXON as
// the reference (which happens 22 symbols earlier).
// We need to compensate it on the non-sink devices.
//
// In fact the delay consists of the following:
// 1. 12 symbols of the "turnaround time" used for calibration
// 2. preamble 8 symbols
// 3. SFD 2 symbols
// 22 symbols in total
//
// TODO 1: check that the parameters are left default in Glossy. DONE: yes, they are (MDMCTRL0, TXCTRL in cc2420.c)
// TODO 2: check that Cooja models them correctly. DONE: no, Cooja sets the calibration to 12+2=14 symbols

// Therefore, for Cooja we have 14+8+2 = 24 symbols, or 24*16 = 384 us, or 384/1000000*32768 = 12.6 ticks
// for real cc2420, 12+8+2 = 22 symbols, or 22*16 = 352 us, or 352/1000000*32768 = 11.5 ticks
// TODO: there is also a program delay between firing the timer and sending the STXON. How long is it?

#if COOJA
#define CRYSTAL_REF_SHIFT 13   // 397us
#else
#define CRYSTAL_REF_SHIFT 10   // 305us IPSN'18 submission
//#define CRYSTAL_REF_SHIFT 12   // 366us // this seems to be the correct setting but 10 works slightly better
#endif

/**
 * Guard-time when clock skew is not yet estimated
 */
//#define CRYSTAL_INIT_GUARD  (RTIMER_SECOND / 50)     //  20 ms, IPSN'18
#define CRYSTAL_INIT_GUARD  (RTIMER_SECOND / 100)    //  10 ms

/**
 * Duration during bootstrapping at receivers.
 */
#define CRYSTAL_SCAN_SLOT_DURATION    (RTIMER_SECOND / 20) //  50 ms

// Time for the radio crystal oscillator to stabilize
#define OSC_STAB_TIME (RTIMER_SECOND/500) // 2 ms

#define N_MISSED_FOR_INIT_GUARD 3

#define N_SILENT_EPOCHS_TO_RESET 100
#define N_SILENT_EPOCHS_TO_STOP_SENDING 3

#define APP_PING_INTERVAL (RTIMER_SECOND / 31) // 32 ms


#define CRYSTAL_BAD_DATA   1
#define CRYSTAL_BAD_CRC    2
#define CRYSTAL_HIGH_NOISE 3
#define CRYSTAL_SILENCE    4


typedef struct {
  crystal_addr_t src;
  crystal_epoch_t epoch;
} 
__attribute__((packed))
crystal_sync_hdr_t;

typedef struct {
} 
__attribute__((packed))
crystal_data_hdr_t;

typedef struct {
  crystal_epoch_t epoch;
  uint8_t n_ta;
  uint8_t cmd;
}
__attribute__((packed))
crystal_ack_hdr_t;

#define CRYSTAL_TYPE_SYNC 0x01
#define CRYSTAL_TYPE_DATA 0x02
#define CRYSTAL_TYPE_ACK  0x03

#define CRYSTAL_ACK_AWAKE(ack) ((ack).cmd == 0x11)
#define CRYSTAL_ACK_SLEEP(ack) ((ack).cmd == 0x22)

#define CRYSTAL_SET_ACK_AWAKE(ack) ((ack).cmd = 0x11)
#define CRYSTAL_SET_ACK_SLEEP(ack) ((ack).cmd = 0x22)

#define CRYSTAL_ACK_CMD_CORRECT(ack) (CRYSTAL_ACK_AWAKE(ack) || CRYSTAL_ACK_SLEEP(ack))

#endif //CRYSTAL_PRIVATE_H
