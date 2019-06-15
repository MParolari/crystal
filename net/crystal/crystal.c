/*
 * Copyright (c) 2018, University of Trento, Italy and 
 * Fondazione Bruno Kessler, Trento, Italy.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its 
 *    contributors may be used to endorse or promote products derived from this 
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE INSTITUTE AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE INSTITUTE OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * Authors: Timofei Istomin <tim.ist@gmail.com>
 *          Matteo Trobinger <matteo.trobinger@unitn.it>
 *
 */

#include "crystal.h"
#include "crystal-conf.h"
#include "crystal-private.h"


#if CRYSTAL_2538
#include <stdio.h>
#include "cc2538-rf.h"
#include "dev/leds.h"
//#include "node-id.h"
#include "unimplemented.h"
#endif

#if CRYSTAL_2420
#include "cc2420.h"
#include "node-id.h"
#endif

#if CRYSTAL_2420
static union {
  uint8_t raw[CRYSTAL_PKTBUF_LEN];
  crystal_sync_hdr_t sync_hdr;
  crystal_data_hdr_t data_hdr;
  crystal_ack_hdr_t ack_hdr;
} buf;
#define CRYSTAL_S_HDR_LEN (sizeof(crystal_sync_hdr_t))
#define CRYSTAL_T_HDR_LEN (sizeof(crystal_data_hdr_t))
#define CRYSTAL_A_HDR_LEN (sizeof(crystal_ack_hdr_t))
#endif

#if CRYSTAL_2538
static union {
  uint8_t raw[CRYSTAL_PKTBUF_LEN];
  struct  __attribute__((packed, aligned(1))) {
    uint8_t type;
    union  __attribute__((packed, aligned(1))){
      crystal_sync_hdr_t sync_hdr;
      crystal_data_hdr_t data_hdr;
      crystal_ack_hdr_t ack_hdr;
    };
  };
} buf;
#define CRYSTAL_S_HDR_LEN (sizeof(crystal_sync_hdr_t) + sizeof(buf.type))
#define CRYSTAL_T_HDR_LEN (sizeof(crystal_data_hdr_t) + sizeof(buf.type))
#define CRYSTAL_A_HDR_LEN (sizeof(crystal_ack_hdr_t)  + sizeof(buf.type))
#endif
#define BZERO_BUF() bzero(buf.raw, CRYSTAL_PKTBUF_LEN)


static crystal_config_t conf = {
  .period  = CRYSTAL_CONF_PERIOD,
  .is_sink = CRYSTAL_CONF_IS_SINK,
  .ntx_S   = CRYSTAL_CONF_NTX_S,
  .w_S     = CRYSTAL_CONF_DUR_S,
  .plds_S  = 0,
  .ntx_T   = CRYSTAL_CONF_NTX_T,
  .w_T     = CRYSTAL_CONF_DUR_T,
  .plds_T  = 0,
  .ntx_A   = CRYSTAL_CONF_NTX_A,
  .w_A     = CRYSTAL_CONF_DUR_A,
  .plds_A  = 0,
  .r       = CRYSTAL_CONF_SINK_MAX_EMPTY_TS,
  .y       = CRYSTAL_CONF_MAX_SILENT_TAS,
  .z       = CRYSTAL_CONF_MAX_MISSING_ACKS,
  .x       = CRYSTAL_CONF_SINK_MAX_NOISY_TS,
  .xa      = CRYSTAL_CONF_MAX_NOISY_AS,
  
  .ch_whitelist  = 0xFFFF,
  .enc_enable    = 0,
  .scan_duration = CRYSTAL_MAX_SCAN_EPOCHS,
};

crystal_info_t crystal_info;        // public read-only status information about Crystal
crystal_app_log_t crystal_app_log;  // public writeable structure for app info in Crystal logs

static uint8_t* payload;            // application payload pointer for the current slot

#if CRYSTAL_2420
static struct glossy glossy_S, glossy_T, glossy_A;  // Glossy objects for different phases
#endif

static struct rtimer rt;                // Rtimer used to schedule Crystal
static rtimer_callback_t timer_handler; // Pointer to the main thread function (either root or node)

static struct pt pt;              // Main protothread of Crystal
static struct pt pt_s_root;       // Protothread for S phase (root)
static struct pt pt_ta_root;      // Protothread for TA pair (root)
static struct pt pt_scan;         // Protothread for scanning the channel
static struct pt pt_s_node;       // Protothread for S phase (non-root)
static struct pt pt_ta_node;      // Protothread for TA pair (non-root)

static crystal_epoch_t epoch;     // epoch seqn received from the sink (or extrapolated)
static uint8_t channel;           // current channel

static uint16_t synced_with_ack;  // Synchronized with an acknowledgement (A phase)
static uint16_t n_noack_epochs;   // Number of consecutive epochs the node did not synchronize with any acknowledgement
static uint16_t sync_missed = 0;  // Current number of consecutive S phases without resynchronization

static uint16_t sink_id = GLOSSY_UNKNOWN_INITIATOR; // the node ID of the sink (only CC2538)

static uint16_t skew_estimated;   // Whether the clock skew over CRYSTAL_PERIOD has already been estimated
static int      period_skew;      // Current estimation of clock skew over a period of length CRYSTAL_PERIOD

static uint8_t successful_scan;   // Whether the node managed to join the network (synchronise with the sink)

static rtimer_clock_t t_ref_root;        // epoch reference time (only for root)
static rtimer_clock_t t_ref_estimated;   // estimated reference time for the current epoch
static rtimer_clock_t t_ref_corrected_s; // reference time acquired during the S slot of the current epoch
static rtimer_clock_t t_ref_corrected;   // reference time acquired during the S or an A slot of the current epoch
static rtimer_clock_t t_ref_skewed;      // reference time in the local time frame
static rtimer_clock_t t_wakeup;          // Time to wake up to prepare for the next epoch
static rtimer_clock_t t_s_start, t_s_stop;        // Start/stop times for S slots
static rtimer_clock_t t_slot_start, t_slot_stop;  // Start/stop times for T and A slots
static rtimer_clock_t t_slot_start_offset;        // Start time offset (high frequency DCO ticks)

static uint16_t correct_packet; // whether the received packet is correct
static uint16_t sleep_order;    // sink sent the sleep command

static uint16_t n_ta;         // the current TA index in the epoch
static uint16_t n_ta_tx;      // how many times node tried to send data in the epoch
static uint16_t n_empty_ts;   // number of consecutive "T" phases without data
static uint16_t n_high_noise; // number of consecutive "T" phases with high noise
static uint16_t n_noacks;     // num. of consecutive "A" phases without any acks
static uint16_t n_bad_acks;   // num. of consecutive "A" phases with bad acks
static uint16_t n_all_acks;   // num. of negative and positive acks
static uint16_t n_badtype_A;  // num. of packets of wrong type received in A phase
static uint16_t n_badlen_A;   // num. of packets of wrong length received in A phase
static uint16_t n_badcrc_A;   // num. of packets with wrong CRC received in A phase
static uint16_t recvtype_S;   // type of a packet received in S phase
static uint16_t recvlen_S;    // length of a packet received in S phase
static uint16_t recvsrc_S;    // source address of a packet received in S phase
static uint16_t cca_busy_cnt; // noise detector value

static uint16_t hopcount;
static uint16_t rx_count_S, tx_count_S;  // tx and rx counters for S phase as reported by Glossy
static uint16_t rx_count_T;
static uint16_t rx_count_A;
static uint16_t ton_S, ton_T, ton_A;     // total duration of the phases in the current epoch
static uint16_t tf_S, tf_T, tf_A;        // total duration of the phases when all N packets are received
static uint16_t n_short_S, n_short_T, n_short_A; // number of "complete" S/T/A phases (those counted in tf_S/tf_T/tf_A)
static uint16_t recv_pkt_type;           // holds the received packet type after a Glossy session

// info about current TA (for logging)
static uint16_t log_recv_type;
static uint16_t log_recv_length;
static uint16_t log_ta_status;
static int      log_noise;
static uint16_t log_ack_skew_err;  // ACK skew estimation outlier
static uint16_t noise_scan_channel;
static inline void measure_noise();

// for VHT timestamp resolution and skew correction
typedef uint64_t time_h_t; // high-resolution timestamps type (unsigned)
typedef int32_t skew_t; // high-resolution time difference type (signed)
static time_h_t t_ref_epoch_h;       // crystal epoch reference time
static time_h_t last_t_ref_h;        // last (glossy! not crystal!) reference time acquired
static typeof(epoch) last_epoch;     // epoch of the last (glossy) synchronization
static typeof(n_ta) last_n_ta;       // number of TA phase of the last (glossy) synchronization

// for clock logging and measurement
static time_h_t log_t_ref_h_s;  // for clock skew measurement

// "null" value for last_n_ta (assuming it will never reach its maximum value)
#define NULL_N_TA ( (typeof(last_n_ta))( ~((typeof(last_n_ta))0) ) )
// note: last_epoch use 0 as null value, since epochs start from 1

// since RTimer is 32kHz and DCO 4MHz: (4MHz / 32kHz == 128 == CLOCK_PHI), so
// 7bit are enough for the high reference offset; in practice we have:
// 7bit for the DCO offset, 16bit for the RTimer, the rest for the "overflow".
#define TIME_H_T(_extra_ref, _low_ref, _high_offset) \
  ( (time_h_t)( (time_h_t)(_extra_ref)  * 8388608lu \
              + (time_h_t)(_low_ref)    * 128lu \
              + (time_h_t)(_high_offset)  ) )

// shortcut
#define LOW_TO_TIME_H(_low_ref) ( TIME_H_T(0,_low_ref,0) )
// from a time_h_t reference, get the low-resolution value (RTimer - 32kHz)
#define GET_LOW_REF(_ref_h) ((rtimer_clock_t)((_ref_h) / 128lu))
// from a time_h_t reference, get the high reference offset (DCO offset)
#define GET_HIGH_OFFSET(_ref_h) ((rtimer_clock_t)((_ref_h) % 128lu))

// it's important to wait the maximum possible S phase duration before starting the TAs!
#define PHASE_S_END_OFFS (CRYSTAL_INIT_GUARD*2 + conf.w_S + CRYSTAL_INTER_PHASE_GAP)
#define TAS_START_OFFS   (PHASE_S_END_OFFS + CRYSTAL_INTER_PHASE_GAP)
#define TA_DURATION      (conf.w_T+conf.w_A+2*CRYSTAL_INTER_PHASE_GAP)
#define PHASE_T_OFFS(n)  (TAS_START_OFFS + (n)*TA_DURATION)
#define PHASE_A_OFFS(n)  (PHASE_T_OFFS(n) + (conf.w_T + CRYSTAL_INTER_PHASE_GAP))

#define CRYSTAL_MAX_ACTIVE_TIME (conf.period - \
    CRYSTAL_TIME_FOR_APP - CRYSTAL_APP_PRE_EPOCH_CB_TIME - CRYSTAL_INIT_GUARD - CRYSTAL_INTER_PHASE_GAP - 100)
#define CRYSTAL_MAX_TAS (((unsigned int)(CRYSTAL_MAX_ACTIVE_TIME - TAS_START_OFFS))/(TA_DURATION))



// True if the current time offset is before the first TA and there is time to schedule TA 0
#define IS_WELL_BEFORE_TAS(offs) ((offs) + CRYSTAL_INTER_PHASE_GAP < PHASE_T_OFFS(0))
// True if the current time offset is before the first TA (number 0)
#define IS_BEFORE_TAS(offs) ((offs) < TAS_START_OFFS)
// gives the current TA number from a time offset from the epoch reference time
// valid only when IS_BEFORE_TAS() holds
#define N_TA_FROM_OFFS(offs) ((offs - TAS_START_OFFS)/TA_DURATION)

#define N_TA_TO_REF(tref, n) (tref-PHASE_A_OFFS(n))


// info about a data packet received during T phase
// TODO: add T_on here, remove the related statistics from the code
// TODO: add tx_count
struct ta_info {
  uint8_t n_ta;
  uint8_t src;
  uint16_t seqn;
  uint8_t type;
  uint8_t t_rx_count;
  uint8_t a_rx_count;
  uint8_t length;
  uint8_t status;
  uint8_t acked;
};

#if CRYSTAL_LOGLEVEL == CRYSTAL_LOGS_ALL
#define MAX_LOG_TAS 50
static struct ta_info ta_log[MAX_LOG_TAS];
static int n_rec_ta; // number of receive records in the array
#endif //CRYSTAL_LOGLEVEL

static inline void init_ta_log_vars() {
#if CRYSTAL_LOGLEVEL == CRYSTAL_LOGS_ALL
  log_recv_type = 0; log_recv_length = 0; log_ta_status = 0;

  // the following are set by the application code
  crystal_app_log.send_seqn = 0;
  crystal_app_log.recv_seqn = 0;
  crystal_app_log.recv_src  = 0;
  crystal_app_log.acked     = 0;
#endif //CRYSTAL_LOGLEVEL
}

static inline void log_ta(int tx) {
#if CRYSTAL_LOGLEVEL == CRYSTAL_LOGS_ALL
  if (n_rec_ta < MAX_LOG_TAS) {
    ta_log[n_rec_ta].n_ta = n_ta;
    ta_log[n_rec_ta].status = tx?CRYSTAL_TX:log_ta_status;
    ta_log[n_rec_ta].src = tx?node_id:crystal_app_log.recv_src;
    ta_log[n_rec_ta].seqn = tx?crystal_app_log.send_seqn:crystal_app_log.recv_seqn;
    ta_log[n_rec_ta].type = log_recv_type;
    ta_log[n_rec_ta].t_rx_count = rx_count_T;
    ta_log[n_rec_ta].a_rx_count = rx_count_A;
    ta_log[n_rec_ta].length = log_recv_length;
    ta_log[n_rec_ta].acked = crystal_app_log.acked;
    n_rec_ta ++;
  }
#endif //CRYSTAL_LOGLEVEL
}

#define IS_SYNCED()          (glossy_is_t_ref_updated())

#if CRYSTAL_USE_DYNAMIC_NEMPTY
#define CRYSTAL_SINK_MAX_EMPTY_TS_DYNAMIC(n_ta_) (((n_ta_)>1)?(conf.r):1)
#warning ------------- !!! USING DYNAMIC N_EMPTY !!! -------------
#else
#define CRYSTAL_SINK_MAX_EMPTY_TS_DYNAMIC(n_ta_) conf.r
#endif

#define UPDATE_SLOT_STATS(phase, transmitting) do { \
  int rtx_on = get_rtx_on(); \
  ton_##phase += rtx_on; \
  if (glossy_get_n_tx() >= ((transmitting)?((conf.ntx_##phase)-1):(conf.ntx_##phase))) { \
    tf_##phase += rtx_on; \
    n_short_##phase ++; \
  } \
} while(0)



#include "crystal-chseq.c"

#define CRYSTAL_S_TOTAL_LEN (CRYSTAL_S_HDR_LEN + conf.plds_S)
#define CRYSTAL_T_TOTAL_LEN (CRYSTAL_T_HDR_LEN + conf.plds_T)
#define CRYSTAL_A_TOTAL_LEN (CRYSTAL_A_HDR_LEN + conf.plds_A)

#define WAIT_UNTIL(time, cl_pt) \
{\
  rtimer_set(t, (time), 0, timer_handler, ptr); \
  PT_YIELD(cl_pt); \
}


#if CRYSTAL_2420
#define GLOSSY(glossy_obj, init_id, length, type_, ntx, channel, is_sync, stop_on_sync, pt) \
glossy_start(glossy_obj, buf.raw, length, init_id==node_id?GLOSSY_INITIATOR:GLOSSY_RECEIVER, channel, is_sync, ntx, stop_on_sync, type_, t_slot_start, t_slot_start_offset, t_slot_stop, timer_handler, &rt, ptr)

#define GLOSSY_WAIT(pt) PT_YIELD(pt); recv_pkt_type = get_app_header(); glossy_stop();
#endif

#if CRYSTAL_2538
#define GLOSSY(glossy_obj, init_id, length, type_, ntx, channel, is_sync, stop_on_sync, pt) \
buf.type = type_;\
WAIT_UNTIL(t_slot_start, pt);\
glossy_start(init_id, buf.raw, length, ntx, is_sync);

#define GLOSSY_WAIT(pt) WAIT_UNTIL(t_slot_stop, pt); recv_pkt_type = buf.type; glossy_stop();
#endif


// workarounds for wrong ref time reported by glossy (which happens VERY rarely)
// sometimes it happens due to a wrong hopcount (CRC collision?)
#define MAX_CORRECT_HOPS 30

static inline int correct_hops() {
#if (MAX_CORRECT_HOPS>0)
  return (glossy_get_relay_cnt_first_rx()<=MAX_CORRECT_HOPS);
#else
  return 1;
#endif
}

#define CRYSTAL_ACK_SKEW_ERROR_DETECTION 1 
static inline int correct_ack_skew(rtimer_clock_t new_ref) {
#if (CRYSTAL_ACK_SKEW_ERROR_DETECTION)
  static int new_skew;
#if (MAX_CORRECT_HOPS>0)
  if (glossy_get_relay_cnt_first_rx()>MAX_CORRECT_HOPS)
    return 0;
#endif
  new_skew = new_ref - t_ref_corrected;
  //if (new_skew < 20 && new_skew > -20)  // IPSN'18
  if (new_skew < 60 && new_skew > -60)
    return 1;  // the skew looks correct
  else if (sync_missed && !synced_with_ack) {
    return 1;  // the skew is big but we did not synchronise during the current epoch, so probably it is fine
  }
  else {
    log_ack_skew_err = new_skew;
    // signal error (0) only if not synchronised with S or another A in the current epoch.
    return 0;
  }
#else
  return 1;
#endif
}

static inline void init_epoch_state() { // zero out epoch-related variables
  tf_S = 0; tf_T = 0; tf_A = 0;
  n_short_S = 0; n_short_T = 0; n_short_A = 0;
  ton_S = 0; ton_T = 0; ton_A = 0;
  n_badlen_A = 0; n_badtype_A = 0; n_badcrc_A = 0;
  log_ack_skew_err = 0;
  cca_busy_cnt = 0;

  n_empty_ts = 0;
  n_noacks = 0;
  n_high_noise = 0;
  n_bad_acks = 0;
  n_ta = 0;
  n_ta_tx = 0;
  n_all_acks = 0;
  sleep_order = 0;
  synced_with_ack = 0;
}

// ------------------------------------------------------------- S thread (root) ---------------------------------------
PT_THREAD(s_root_thread(struct rtimer *t, void* ptr)) 
{
  PT_BEGIN(&pt_s_root);
  buf.sync_hdr.epoch = epoch;
  buf.sync_hdr.src   = node_id;

  if (payload) {
    memcpy(buf.raw + CRYSTAL_S_HDR_LEN,
        payload, conf.plds_S);
  }

  channel = get_channel_epoch(epoch);

  t_slot_start = t_s_start;
  t_slot_stop  = t_s_stop;

  GLOSSY(&glossy_S,
      node_id, CRYSTAL_S_TOTAL_LEN, 
      CRYSTAL_TYPE_SYNC, 
      conf.ntx_S,
      channel, 
      GLOSSY_WITH_SYNC,
      0, // don't stop on sync
      &pt_s_root);
  GLOSSY_WAIT(&pt_s_root);

  UPDATE_SLOT_STATS(S, 1);
  tx_count_S = glossy_get_n_tx();
  rx_count_S = glossy_get_n_rx();

  app_post_S(0, NULL);
  BZERO_BUF();
  PT_END(&pt_s_root);
}

// ------------------------------------------------------------ TA thread (root) ---------------------------------------
PT_THREAD(ta_root_thread(struct rtimer *t, void* ptr))
{
  PT_BEGIN(&pt_ta_root);
    while (!sleep_order && n_ta < CRYSTAL_MAX_TAS) { /* TA loop */
      init_ta_log_vars();
      crystal_info.n_ta = n_ta;

      // -- Phase T (root)
      t_slot_start = t_ref_root - CRYSTAL_SHORT_GUARD + PHASE_T_OFFS(n_ta);
      t_slot_stop = t_slot_start + conf.w_T + CRYSTAL_SHORT_GUARD + CRYSTAL_SINK_END_GUARD;

      channel = get_channel_epoch_ta(epoch, n_ta);

      app_pre_T();

      GLOSSY(&glossy_T, GLOSSY_UNKNOWN_INITIATOR, 
          CRYSTAL_T_TOTAL_LEN, 
          CRYSTAL_TYPE_DATA,
          conf.ntx_T, 
          channel, 
          GLOSSY_WITHOUT_SYNC,
          0, // don't stop on sync
          &pt_ta_root);
      GLOSSY_WAIT(&pt_ta_root);


      UPDATE_SLOT_STATS(T, 0);

      correct_packet = 0;
      cca_busy_cnt = get_cca_busy_cnt();
      rx_count_T = glossy_get_n_rx();
      if (rx_count_T) { // received data
        n_empty_ts = 0;
        n_high_noise = 0;
        log_recv_type = recv_pkt_type;
        // TBD: get_app_header() is not implemented in the current version of glossy for this platform, should we unify it? 
        //log_recv_type = get_app_header();
        log_recv_length = glossy_get_payload_len();
        correct_packet = (log_recv_length == CRYSTAL_T_TOTAL_LEN && log_recv_type == CRYSTAL_TYPE_DATA);
        log_ta_status = correct_packet?CRYSTAL_RECV_OK:CRYSTAL_BAD_DATA;
      }
      else if (is_corrupted()) {
        n_empty_ts = 0;
        n_high_noise = 0;
        log_ta_status = CRYSTAL_BAD_CRC;
      }
      else if (conf.x > 0 && cca_busy_cnt > CRYSTAL_CCA_COUNTER_THRESHOLD) {
        //n_empty_ts = 0; // should we reset it, it's a good question
        n_high_noise ++;
        log_recv_length = cca_busy_cnt;
        log_ta_status = CRYSTAL_HIGH_NOISE;
      }
      else {
        // just silence
        n_high_noise = 0;
        n_empty_ts ++;
        // logging for debugging
        log_recv_length = cca_busy_cnt;
        log_ta_status = CRYSTAL_SILENCE;
      }

      payload = app_between_TA(correct_packet, buf.raw + CRYSTAL_T_HDR_LEN);
      log_ta(0);
      BZERO_BUF();
      // -- Phase T end (root)
      sleep_order = 
        epoch >= CRYSTAL_N_FULL_EPOCHS && (
          (n_ta         >= CRYSTAL_MAX_TAS-1) || 
          (n_empty_ts   >= CRYSTAL_SINK_MAX_EMPTY_TS_DYNAMIC(n_ta)) || 
          (conf.x && n_high_noise >= conf.x)// && (n_high_noise >= CRYSTAL_SINK_MAX_EMPTY_TS_DYNAMIC(n_ta))
          );
      // -- Phase A (root)

      if (sleep_order)
        CRYSTAL_SET_ACK_SLEEP(buf.ack_hdr);
      else
        CRYSTAL_SET_ACK_AWAKE(buf.ack_hdr);

      buf.ack_hdr.n_ta = n_ta;
      buf.ack_hdr.epoch = epoch;
      memcpy(buf.raw + CRYSTAL_A_HDR_LEN,
          payload, conf.plds_A);

      t_slot_start = t_ref_root + PHASE_A_OFFS(n_ta);
      t_slot_stop = t_slot_start + conf.w_A;

      GLOSSY(&glossy_A, 
          node_id, 
          CRYSTAL_A_TOTAL_LEN,
          CRYSTAL_TYPE_ACK,
          conf.ntx_A, 
          channel,
          CRYSTAL_SYNC_ACKS?GLOSSY_WITH_SYNC:GLOSSY_WITHOUT_SYNC,
          0, // don't stop on sync 
          &pt_ta_root);
      GLOSSY_WAIT(&pt_ta_root);


      UPDATE_SLOT_STATS(A, 1);
      app_post_A(0, buf.raw + CRYSTAL_A_HDR_LEN);
      BZERO_BUF();
      // -- Phase A end (root)

      n_ta ++;
    } /* End of TA loop */
  PT_END(&pt_ta_root);
}

// ---------------------------------------------------------- Main thread (root) ---------------------------------------
static char root_main_thread(struct rtimer *t, void *ptr) {
  PT_BEGIN(&pt);

  app_crystal_start_done(true);

  //leds_off(LEDS_RED);
  t_ref_root = RTIMER_NOW() + OSC_STAB_TIME + GLOSSY_PRE_TIME + 16; // + 16 just to be sure
  while (1) {
    init_epoch_state();

    RADIO_OSC_ON();

    epoch ++;
    crystal_info.epoch = epoch;
    crystal_info.n_ta = 0;

    payload = app_pre_S();
    t_s_start = t_ref_root;
    t_s_stop = t_s_start + conf.w_S;

    // wait for the oscillator to stabilize
    WAIT_UNTIL(t_s_start - (GLOSSY_PRE_TIME + 16), &pt);

    PT_SPAWN(&pt, &pt_s_root, s_root_thread(t, ptr));

    PT_SPAWN(&pt, &pt_ta_root, ta_root_thread(t, ptr));

    RADIO_OSC_OFF(); // put radio to deep sleep

    t_ref_root += conf.period;

    // time to wake up to prepare for the next epoch
    t_wakeup = t_ref_root - (OSC_STAB_TIME + GLOSSY_PRE_TIME + CRYSTAL_INTER_PHASE_GAP);

    app_epoch_end();
    WAIT_UNTIL(t_wakeup - CRYSTAL_APP_PRE_EPOCH_CB_TIME, &pt);

    app_pre_epoch();
    WAIT_UNTIL(t_wakeup, &pt);
  }
  PT_END(&pt);
}

// ---------------------------------------------------------- Scan thread (node) ---------------------------------------
PT_THREAD(scan_thread(struct rtimer *t, void* ptr))
{
  static uint32_t max_scan_duration, scan_duration;
  PT_BEGIN(&pt_scan);
  channel = get_channel_node_bootstrap(SCAN_RX_NOTHING);

  max_scan_duration = conf.period * conf.scan_duration; // the maximums don't permit overflow
  scan_duration = 0;

  // Scanning loop
  while (1) {
#if CRYSTAL_2420
    bzero(&glossy_S, sizeof(glossy_S)); // reset the Glossy timing info
#endif
    t_slot_start = RTIMER_NOW() + (GLOSSY_PRE_TIME + 6); // + 6 is just to be sure
    t_slot_stop = t_slot_start + CRYSTAL_SCAN_SLOT_DURATION;

    GLOSSY(&glossy_S, GLOSSY_UNKNOWN_INITIATOR, GLOSSY_UNKNOWN_PAYLOAD_LEN,
        GLOSSY_IGNORE_TYPE,
        CRYSTAL_2420? 5 : GLOSSY_UNKNOWN_N_TX_MAX, // N
        channel,
        GLOSSY_WITH_SYNC,
        //1, // stop immediately on sync
        0, // don't stop on sync
        &pt_scan);
    GLOSSY_WAIT(&pt_scan);



    if (glossy_get_n_rx() > 0) {
      recvtype_S = recv_pkt_type;
      recvlen_S = glossy_get_payload_len();

      // Sync packet received
      if (recvtype_S == CRYSTAL_TYPE_SYNC && 
          recvlen_S  == CRYSTAL_S_TOTAL_LEN  //&&
          /*buf.sync_hdr.src == conf.sink_id*/) {
#if CRYSTAL_2538
        sink_id = glossy_get_initiator_id();
#endif
        epoch = buf.sync_hdr.epoch;
        crystal_info.epoch = epoch;
        n_ta = 0;
        if (IS_SYNCED()) {
          t_ref_corrected = glossy_get_t_ref();
          t_ref_epoch_h = TIME_H_T(get_timer_overflow(), t_ref_corrected, glossy_get_T_offset_h());
          last_epoch = epoch;
          last_n_ta = NULL_N_TA;
          successful_scan = 1;
          break; // exit the scanning
        }
        channel = get_channel_node_bootstrap(SCAN_RX_S);
        continue;
      }
      // Ack packet received
      else if (recvtype_S == CRYSTAL_TYPE_ACK && 
               recvlen_S  == CRYSTAL_A_TOTAL_LEN) {
        epoch = buf.ack_hdr.epoch;
        crystal_info.epoch = epoch;

        n_ta = buf.ack_hdr.n_ta;
        
        if (IS_SYNCED()) {
          t_ref_corrected = glossy_get_t_ref() - PHASE_A_OFFS(n_ta);
          t_ref_epoch_h = TIME_H_T(get_timer_overflow(), glossy_get_t_ref(), glossy_get_T_offset_h())
            - LOW_TO_TIME_H(PHASE_A_OFFS(n_ta));
          last_epoch = epoch;
          last_n_ta = n_ta;
          successful_scan = 1;
          break; // exit the scanning
        }
        channel = get_channel_node_bootstrap(SCAN_RX_A);
        continue;
      }
      // Data packet received
      else if (recvtype_S == CRYSTAL_TYPE_DATA
               /* && recvlen_s  == CRYSTAL_DATA_LEN*/
          // not checking the length because Glossy currently does not
          // copy the packet to the application buffer in this situation
          ) {
        continue; // scan again on the same channel waiting for an ACK
        // it is safe because Glossy exits immediately if it receives a non-syncronizing packet
      }
    }
    channel = get_channel_node_bootstrap(SCAN_RX_NOTHING);

    scan_duration += CRYSTAL_SCAN_SLOT_DURATION;
    if (scan_duration > max_scan_duration) {
      //leds_off(LEDS_RED);
      successful_scan = 0;
      break; // exit the scanning
    }
  }
#if CRYSTAL_2420
  if (recvtype_S != CRYSTAL_TYPE_SYNC)
    bzero(&glossy_S, sizeof(glossy_S)); // reset the timing info if a non-S packet was received
#endif

  PT_END(&pt_scan);
}

// ------------------------------------------------------------- S thread (node) ---------------------------------------
PT_THREAD(s_node_thread(struct rtimer *t, void* ptr))
{
  static uint16_t ever_synced_with_s;   // Synchronized with an S at least once
  PT_BEGIN(&pt_s_node);
  
  channel = get_channel_epoch(epoch);

  t_slot_start = t_s_start;
  t_slot_stop  = t_s_stop;
  GLOSSY(&glossy_S, sink_id, 
      CRYSTAL_S_TOTAL_LEN,
      CRYSTAL_TYPE_SYNC, 
      conf.ntx_S, channel,
      GLOSSY_WITH_SYNC,
      0, // don't stop on sync
      &pt_s_node);
  GLOSSY_WAIT(&pt_s_node);

  UPDATE_SLOT_STATS(S, 0);

  recvlen_S = glossy_get_payload_len();
  recvtype_S = recv_pkt_type;
  recvsrc_S = buf.sync_hdr.src;
  rx_count_S = glossy_get_n_rx();
  tx_count_S = glossy_get_n_tx();

  correct_packet = (recvtype_S == CRYSTAL_TYPE_SYNC 
      /*&& recvsrc_S  == conf.sink_id */
      && recvlen_S  == CRYSTAL_S_TOTAL_LEN);

  if (rx_count_S > 0 && correct_packet) {
    epoch = buf.sync_hdr.epoch;
    hopcount = glossy_get_relay_cnt_first_rx();
  }
  if (IS_SYNCED() && rx_count_S > 0
      && correct_packet
      && correct_hops()) {
    t_ref_corrected_s = glossy_get_t_ref();
    t_ref_corrected = t_ref_corrected_s; // use this corrected ref time in the current epoch

    if (ever_synced_with_s) {
      // can estimate skew
      period_skew = (int16_t)(t_ref_corrected_s - (t_ref_skewed + conf.period)) / ((int)sync_missed + 1); // cast to signed is required
      skew_estimated = 1;
    }
    t_ref_skewed = t_ref_corrected_s;
    ever_synced_with_s = 1;
    sync_missed = 0;

    // get the corrected reference time in high-resolution
    time_h_t tmp_h = TIME_H_T(get_timer_overflow(), t_ref_corrected, glossy_get_T_offset_h());
    // check if the new ref is valid
    if (tmp_h > last_t_ref_h) {
      // update the epoch reference time
      t_ref_epoch_h = tmp_h;
      // update last_* variables
      last_t_ref_h = tmp_h;
      last_epoch = epoch;
      last_n_ta = NULL_N_TA;
    }
    log_t_ref_h_s = tmp_h;
  }
  else {
    sync_missed++;
    t_ref_skewed += conf.period;
    t_ref_corrected = t_ref_estimated; // use the estimate if didn't update
    t_ref_corrected_s = t_ref_estimated;
  }

  app_post_S(correct_packet, buf.raw + CRYSTAL_S_HDR_LEN);
  BZERO_BUF();

  crystal_info.hops = hopcount;
  crystal_info.n_missed_s = sync_missed;
  PT_END(&pt_s_node);
}

// ------------------------------------------------------------ TA thread (node) ---------------------------------------
PT_THREAD(ta_node_thread(struct rtimer *t, void* ptr))
{
  PT_BEGIN(&pt_ta_node);

  while (1) { /* TA loop */
    // -- Phase T (non-root)
    static int guard;
    static uint16_t have_packet;
    static int i_tx;
    static time_h_t tmp_h;

    init_ta_log_vars();
    crystal_info.n_ta = n_ta;
    correct_packet = 0;
    payload = app_pre_T();
    have_packet = payload != NULL;

    i_tx = (have_packet && 
        (sync_missed < N_SILENT_EPOCHS_TO_STOP_SENDING || n_noack_epochs < N_SILENT_EPOCHS_TO_STOP_SENDING));
    // TODO: instead of just suppressing tx when out of sync it's better to scan for ACKs or Sync beacons...

    if (i_tx) {
      n_ta_tx ++;
      // no guards if I transmit
      guard = 0;
      memcpy(buf.raw + CRYSTAL_T_HDR_LEN, payload, conf.plds_T);
    }
    else {
      // guards for receiving
      guard = (sync_missed && !synced_with_ack)?CRYSTAL_SHORT_GUARD_NOSYNC:CRYSTAL_SHORT_GUARD;
    }
    tmp_h = t_ref_epoch_h + LOW_TO_TIME_H(PHASE_T_OFFS(n_ta))
      - LOW_TO_TIME_H(CRYSTAL_REF_SHIFT) - LOW_TO_TIME_H(guard);
    t_slot_start = GET_LOW_REF(tmp_h);
    t_slot_start_offset = GET_HIGH_OFFSET(tmp_h);
    t_slot_stop = t_slot_start + conf.w_T + guard;

    //choice of the channel for each T-A slot
    channel = get_channel_epoch_ta(epoch, n_ta);

    GLOSSY(&glossy_T,
        i_tx ? node_id : GLOSSY_UNKNOWN_INITIATOR, 
        CRYSTAL_T_TOTAL_LEN, 
        CRYSTAL_TYPE_DATA, 
        conf.ntx_T,
        channel,
        GLOSSY_WITHOUT_SYNC,
        0, // don't stop on sync
        &pt_ta_node);
    GLOSSY_WAIT(&pt_ta_node);

    UPDATE_SLOT_STATS(T, i_tx);

    rx_count_T = glossy_get_n_rx();
    if (!i_tx) {
      if (rx_count_T) { // received data
        log_recv_type = recv_pkt_type;
        log_recv_length = glossy_get_payload_len();
        correct_packet = (log_recv_length == CRYSTAL_T_TOTAL_LEN && log_recv_type == CRYSTAL_TYPE_DATA);
        log_ta_status = correct_packet?CRYSTAL_RECV_OK:CRYSTAL_BAD_DATA;
        n_empty_ts = 0;
      }
      else if (is_corrupted()) {
        log_ta_status = CRYSTAL_BAD_CRC;
        //n_empty_ts = 0; // keep it as it is to give another chance but not too many chances
      } 
      else { // TODO: should we check for the high noise also here?
        log_ta_status = CRYSTAL_SILENCE;
        n_empty_ts ++;
      }
      cca_busy_cnt = get_cca_busy_cnt();
    }

    app_between_TA(correct_packet, buf.raw + CRYSTAL_T_HDR_LEN);

    BZERO_BUF();

    // -- Phase A (non-root)

    correct_packet = 0;
    guard = (sync_missed && !synced_with_ack)?CRYSTAL_SHORT_GUARD_NOSYNC:CRYSTAL_SHORT_GUARD;
    tmp_h = t_ref_epoch_h + LOW_TO_TIME_H(PHASE_A_OFFS(n_ta))
      - LOW_TO_TIME_H(CRYSTAL_REF_SHIFT) - LOW_TO_TIME_H(guard);
    t_slot_start = GET_LOW_REF(tmp_h);
    t_slot_start_offset = GET_HIGH_OFFSET(tmp_h);
    t_slot_stop = t_slot_start + conf.w_A + guard;

    GLOSSY(&glossy_A, 
        sink_id, 
        CRYSTAL_A_TOTAL_LEN,
        CRYSTAL_TYPE_ACK, 
        conf.ntx_A,
        channel,
        CRYSTAL_SYNC_ACKS?GLOSSY_WITH_SYNC:GLOSSY_WITHOUT_SYNC,
        0, // don't stop on sync
        &pt_ta_node);
    GLOSSY_WAIT(&pt_ta_node);
    UPDATE_SLOT_STATS(A, 0);

    rx_count_A = glossy_get_n_rx();
    if (rx_count_A) {
      if (glossy_get_payload_len() == CRYSTAL_A_TOTAL_LEN 
          && recv_pkt_type == CRYSTAL_TYPE_ACK 
          && CRYSTAL_ACK_CMD_CORRECT(buf.ack_hdr)) {
        correct_packet = 1;
        n_noacks = 0;
        n_bad_acks = 0;
        n_all_acks ++;
        // Updating the epoch in case we "skipped" some epochs but got an ACK
        // We can "skip" epochs if we are too late for the next TA and set the timer to the past
        epoch = buf.ack_hdr.epoch; 
        crystal_info.epoch = epoch;

#if (CRYSTAL_SYNC_ACKS)
        // sometimes we get a packet with a corrupted n_ta
        // (e.g. 234) that's why checking the value
        // sometimes also the ref time is reported incorrectly, so have to check
        if (IS_SYNCED() && buf.ack_hdr.n_ta == n_ta
            && correct_ack_skew(N_TA_TO_REF(glossy_get_t_ref(), buf.ack_hdr.n_ta))
           ) {

          t_ref_corrected = N_TA_TO_REF(glossy_get_t_ref(), buf.ack_hdr.n_ta);
          tmp_h = TIME_H_T(get_timer_overflow(), glossy_get_t_ref(), glossy_get_T_offset_h());
          // check if ref is valid
          if (tmp_h > last_t_ref_h) {
            // update the epoch reference time
            t_ref_epoch_h = tmp_h - LOW_TO_TIME_H(PHASE_A_OFFS(buf.ack_hdr.n_ta));
            // update last_* variables
            last_t_ref_h = tmp_h;
            last_epoch = epoch;
            last_n_ta = n_ta;
          }
          synced_with_ack ++;
          n_noack_epochs = 0; // it's important to reset it here to reenable TX right away (if it was suppressed)
        }
#endif

        if (CRYSTAL_ACK_SLEEP(buf.ack_hdr)) {
          sleep_order = 1;
        }
      }
      else {
        // received something but not an ack (we might be out of sync)
        // n_noacks ++; // keep it as it is to give another chance but not too many chances
        n_bad_acks ++;
      }

      // logging info about bad packets
      if (recv_pkt_type != CRYSTAL_TYPE_ACK)
        n_badtype_A ++;
      if (glossy_get_payload_len() != CRYSTAL_A_TOTAL_LEN)
        n_badlen_A ++;

      n_high_noise = 0;
    }
    else if (is_corrupted()) { // bad CRC
      // n_noacks ++; // keep it as it is to give another chance but not too many chances
      n_bad_acks ++;
      n_badcrc_A ++;
      n_high_noise = 0;
    }
    else { // not received anything
      if (conf.xa == 0)
        n_noacks ++; // no "noise detection"
      else if (get_cca_busy_cnt() > CRYSTAL_CCA_COUNTER_THRESHOLD) {
        n_high_noise ++;
        if (n_high_noise > conf.xa)
          n_noacks ++;
      }
      else {
        n_noacks ++;
        n_high_noise = 0;
      }
    }

    app_post_A(correct_packet, buf.raw + CRYSTAL_A_HDR_LEN);

    log_ta(i_tx);

    BZERO_BUF();
    // -- Phase A end
    n_ta ++;
    // shall we stop?
    if (sleep_order || (n_ta >= CRYSTAL_MAX_TAS) || // always stop when ordered or max is reached
        (epoch >= CRYSTAL_N_FULL_EPOCHS && 
                      (
                         (  have_packet  && (n_noacks >= conf.z)) ||
                         ((!have_packet) && (n_noacks >= conf.y) && n_empty_ts >= conf.y)
                      )
        )
       ) {

      break; // Stop the TA chain
    }
  } /* End of TA loop */
  PT_END(&pt_ta_node);
}

// ---------------------------------------------------------- Main thread (node) ---------------------------------------
static char node_main_thread(struct rtimer *t, void *ptr) {
  static rtimer_clock_t s_guard;
  static rtimer_clock_t now;
  static rtimer_clock_t offs;
  static uint16_t skip_S;        // skip the S phase (if joining in the middle of TA chain)
  static uint16_t starting_n_ta; // the first TA index in an epoch (if joining in the middle of TA chain)
  static time_h_t tmp_h;
  PT_BEGIN(&pt);

  successful_scan = 0;
  PT_SPAWN(&pt, &pt_scan, scan_thread(t, ptr));

  app_crystal_start_done(successful_scan);
  if (!successful_scan)
    PT_EXIT(&pt);

  BZERO_BUF();
  //leds_off(LEDS_RED);

  // useful for debugging in Cooja
  //rtimer_set(t, RTIMER_NOW() + 15670, timer_handler, ptr);
  //PT_YIELD(&pt);

  now = RTIMER_NOW();
  offs = now - (t_ref_corrected - CRYSTAL_REF_SHIFT) + 20; // 20 just to be sure

  if (offs + CRYSTAL_INIT_GUARD + OSC_STAB_TIME + GLOSSY_PRE_TIME > conf.period) {
    // We are that late so the next epoch started
    // (for sure this will not work with period of 2s)
    epoch ++;
    crystal_info.epoch = epoch;
    t_ref_corrected += conf.period;
    if (offs > conf.period) // safe to subtract 
      offs -= conf.period;
    else // avoid wrapping around 0
      offs = 0;
  }
  
  // here we are either well before the next epoch's S
  // or right after (or inside) the current epoch's S

  if (IS_BEFORE_TAS(offs)) { // before TA chain but after S
    skip_S = 1;
    if (IS_WELL_BEFORE_TAS(offs)) {
      starting_n_ta = 0;
    }
    else {
      starting_n_ta = 1;
    }
  }
  else { // within or after TA chain
    starting_n_ta = N_TA_FROM_OFFS(offs + CRYSTAL_INTER_PHASE_GAP) + 1;
    if (starting_n_ta < CRYSTAL_MAX_TAS) { // within TA chain
      skip_S = 1;
    }
    else { // outside of the TA chain, capture the next S
      starting_n_ta = 0;
    }
  }

  // here we have the ref time pointing at the previous epoch
  t_ref_corrected_s = t_ref_corrected;

  /* For S if we are not skipping it */
  if (!skip_S) {
    t_ref_estimated = t_ref_corrected + conf.period;
    t_ref_epoch_h = t_ref_epoch_h + LOW_TO_TIME_H(conf.period);
    t_ref_skewed = t_ref_estimated;
    tmp_h = t_ref_epoch_h
      - LOW_TO_TIME_H(CRYSTAL_REF_SHIFT) - LOW_TO_TIME_H(CRYSTAL_INIT_GUARD);
    t_s_start = GET_LOW_REF(tmp_h);
    t_slot_start_offset = GET_HIGH_OFFSET(tmp_h);
    t_s_stop = t_s_start + conf.w_S + 2*CRYSTAL_INIT_GUARD;
  }

  while (1) {
    init_epoch_state();
    crystal_info.n_ta = 0;
    log_t_ref_h_s = 0;

    if (!skip_S) {
      RADIO_OSC_ON();
      epoch ++;
      crystal_info.epoch = epoch;
      starting_n_ta = 0;

      app_pre_S();

      // wait for the oscillator to stabilize
      WAIT_UNTIL(t_s_start - (GLOSSY_PRE_TIME + 16), &pt);

      PT_SPAWN(&pt, &pt_s_node, s_node_thread(t, ptr));
    }
    skip_S = 0;
    n_ta = starting_n_ta;

    PT_SPAWN(&pt, &pt_ta_node, ta_node_thread(t, ptr));

    if (!synced_with_ack) {
      n_noack_epochs ++;
    }
    RADIO_OSC_OFF(); // deep sleep

    s_guard = (!skew_estimated || sync_missed >= N_MISSED_FOR_INIT_GUARD)?CRYSTAL_INIT_GUARD:CRYSTAL_LONG_GUARD;

    // Schedule the next epoch times
    skew_t period_skew_h = (skew_t)(LOW_TO_TIME_H(period_skew));
    // if we synced during current epoch but not in S phase, take a partial skew
    if (last_epoch == epoch && last_n_ta != NULL_N_TA) {
      period_skew_h = (float)(period_skew_h) * (float)(
        1.0f - (float)((float)(PHASE_A_OFFS(last_n_ta)) / (float)(conf.period))
      );
    }
    // (period + skew) is a skew_t sum, >=0, so after that a time_h_t sum is feasible
    // and this requires ( |skew| < |epoch| )
    t_ref_epoch_h = t_ref_epoch_h
      + (time_h_t)((skew_t)LOW_TO_TIME_H(conf.period) + period_skew_h);
    t_ref_estimated = t_ref_corrected_s + conf.period + period_skew;
    tmp_h = t_ref_epoch_h
      - LOW_TO_TIME_H(CRYSTAL_REF_SHIFT) - LOW_TO_TIME_H(s_guard);
    t_s_start = GET_LOW_REF(tmp_h);
    t_slot_start_offset = GET_HIGH_OFFSET(tmp_h);
    t_s_stop = t_s_start + conf.w_S + 2*s_guard;

    // time to wake up to prepare for the next epoch
    t_wakeup = t_s_start - (OSC_STAB_TIME + GLOSSY_PRE_TIME + CRYSTAL_INTER_PHASE_GAP);

    app_epoch_end();
    WAIT_UNTIL(t_wakeup - CRYSTAL_APP_PRE_EPOCH_CB_TIME, &pt);

    app_pre_epoch();
    WAIT_UNTIL(t_wakeup, &pt);

    if (sync_missed > N_SILENT_EPOCHS_TO_RESET && n_noack_epochs > N_SILENT_EPOCHS_TO_RESET) {
      SYSTEM_RESET();
    }
  }
  PT_END(&pt);
}

// ---------------------------------------------------------------------------------------------------------------------
void crystal_init() {
#if CRYSTAL_2538
  glossy_init();
  cc2538_rf_set_channel(CRYSTAL_DEF_CHANNEL);
#endif
#if CRYSTAL_2420
  cc2420_set_channel(CRYSTAL_DEF_CHANNEL);
#endif
}

static inline void measure_noise() {
#if CRYSTAL_LOGLEVEL == CRYSTAL_LOGS_ALL
#if CRYSTAL_2420
  noise_scan_channel = channel_array[epoch % get_num_channels()];
  cc2420_set_channel(noise_scan_channel);
  RADIO_OSC_ON();
  log_noise = cc2420_rssi();
  RADIO_OSC_OFF();
#endif
#endif
}


bool crystal_start(crystal_config_t* conf_)
{
  // check the config
  if (CRYSTAL_S_HDR_LEN + conf_->plds_S > CRYSTAL_PKTBUF_LEN ||
      CRYSTAL_T_HDR_LEN + conf_->plds_T > CRYSTAL_PKTBUF_LEN ||
      CRYSTAL_A_HDR_LEN + conf_->plds_A > CRYSTAL_PKTBUF_LEN ||
      conf_->period == 0        || // TODO: put something reasonable here
      conf_->period > CRYSTAL_MAX_PERIOD ||
      conf_->scan_duration == 0 ||
      conf_->scan_duration > CRYSTAL_MAX_SCAN_EPOCHS
     )
    return false;

  // TODO: check the rest of the config
  
  conf = *conf_;
  //PRINT_CRYSTAL_CONFIG(conf);

  /* In case we start after being stopped, zero-out Crystal state */
  //TBC: Is it needed?
  bzero(&crystal_info, sizeof(crystal_info));
  epoch = 0;
  skew_estimated = 0;
  synced_with_ack = 0;
  n_noack_epochs = 0;
  sync_missed = 0;
  period_skew = 0;
  last_t_ref_h = 0;
  last_epoch = 0;
  last_n_ta = NULL_N_TA;

  /* reset the protothread */
  //TBC: Is it needed?
  //bzero(&pt, sizeof(pt));

  if (conf.is_sink)
    timer_handler = root_main_thread;
  else
    timer_handler = node_main_thread;

  //leds_on(LEDS_RED);

  channel = CRYSTAL_DEF_CHANNEL;
#if CRYSTAL_2420
  cc2420_set_txpower(TX_POWER);
  cc2420_set_cca_threshold(CRYSTAL_CCA_THRESHOLD);
#endif

#if CRYSTAL_2420
  // Start Glossy busy-waiting process.
  process_start(&glossy_process, NULL);
#endif
  // Start Crystal
  rtimer_set(&rt, RTIMER_NOW() + 10, 0, timer_handler, NULL);
  return true;
}

static void stop_timer_handler(struct rtimer *t, void *ptr) {}

void crystal_stop() {
  rtimer_set(&rt, RTIMER_NOW() + 2, 0, stop_timer_handler, NULL);
}




// ------------------------------------------------------------------ Log output ---------------------------------------
void crystal_print_epoch_logs() {
  static int first_time = 1;
  unsigned long avg_radio_on;

#if CRYSTAL_LOGLEVEL


  if (!conf.is_sink) {
    printf("S %u:%u %u %u:%u %d %u\n", epoch, n_ta_tx, n_all_acks, synced_with_ack, sync_missed, period_skew, hopcount);
    printf("P %u:%u %u %u:%u %u %u %d:%d\n", 
        epoch, recvsrc_S, recvtype_S, recvlen_S, n_badtype_A, n_badlen_A, n_badcrc_A, log_ack_skew_err, 0);
    printf("L %u %llu %ld\n", epoch, log_t_ref_h_s, 0);
  }

#if CRYSTAL_LOGLEVEL == CRYSTAL_LOGS_ALL
  static int i;
  printf("R %u:%u %u:%d %u:%u %u %u\n", 
      epoch, n_ta, n_rec_ta, log_noise, noise_scan_channel, tx_count_S, rx_count_S, cca_busy_cnt);
  for (i=0; i<n_rec_ta; i++) {
    printf("T %u:%u %u:%u %u %u %u:%u %u %u\n",
        epoch,
        ta_log[i].n_ta,
        ta_log[i].status,
        
        ta_log[i].src,
        ta_log[i].seqn,
        ta_log[i].type,
        ta_log[i].length,

        ta_log[i].t_rx_count,
        ta_log[i].a_rx_count,
        ta_log[i].acked
        );
  }
  n_rec_ta = 0;
#endif
#endif
  /*printf("D %u:%u %lu %u:%u %lu %u\n", epoch,
    glossy_S.T_slot_h, glossy_S.T_slot_h_sum, glossy_S.win_cnt,
    glossy_A.T_slot_h, glossy_A.T_slot_h_sum, glossy_A.win_cnt
    );*/

#if CRYSTAL_2538
  glossy_debug_print();
#endif

#if ENERGEST_CONF_ON && CRYSTAL_LOGLEVEL
  if (!first_time) {
    // Compute average radio-on time.
    avg_radio_on = (energest_type_time(ENERGEST_TYPE_LISTEN) + energest_type_time(ENERGEST_TYPE_TRANSMIT))
      * 1e6 /
      (energest_type_time(ENERGEST_TYPE_CPU) + energest_type_time(ENERGEST_TYPE_LPM));
    // Print information about average radio-on time per second.
    printf("E %u:%lu.%03lu:%u %u %u\n", epoch,
        avg_radio_on / 1000, avg_radio_on % 1000, ton_S, ton_T, ton_A);
    printf("F %u:%u %u %u:%u %u %u\n", epoch,
        tf_S, tf_T, tf_A, n_short_S, n_short_T, n_short_A);
  }
  // Initialize Energest values.
  energest_init();
  first_time = 0;
#endif /* ENERGEST_CONF_ON */
}


crystal_config_t crystal_get_config() {
  return conf;
}
