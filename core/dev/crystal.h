#ifndef CRYSTAL_H_
#define CRYSTAL_H_

#include "glossy.h"
#include "stdbool.h"

typedef uint8_t crystal_addr_t; // IPSN'18
//typedef uint16_t crystal_addr_t;

//typedef uint8_t crystal_epoch_t; // NOT SUPPORTED !
typedef uint16_t crystal_epoch_t; // IPSN'18

typedef struct {
  uint32_t period;   // Crystal period in rtimer clock ticks
  uint8_t is_sink;   // Is this node the sink
  uint8_t ntx_S;     // Number of Glossy TX in S slots
  uint16_t w_S;      // Max duration of S slots in rtimer ticks
  uint8_t plds_S;    // App. payload size for S slots
  uint8_t ntx_T;     // Number of Glossy TX in T slots
  uint16_t w_T;      // Max duration of T slots in rtimer ticks
  uint8_t plds_T;    // App. payload size for T slots
  uint8_t ntx_A;     // Number of Glossy TX in A slots
  uint16_t w_A;      // Max duration of A slots in rtimer ticks
  uint8_t plds_A;    // App. payload size for A slots
  uint8_t r;         // Number of empty T slots triggering epoch termination at the sink
  uint8_t y;         // Number of empty TA pairs triggering epoch termination at a non-transmitting node
  uint8_t z;         // Number of empty A slots triggering epoch termination at a transmitting node
  uint8_t x;         // Max. number of TA pairs added when high noise is detected at the sink
  uint8_t xa;        // Max. number of TA pairs added when high noise is detected at a non-sink node
  uint16_t ch_whitelist; // Channel whitelist (TBD)
  uint8_t enc_enable;    // Glossy-level encryption enabled (not supported)
  uint8_t scan_duration; // Scan duration in number of epochs (TBD)
} crystal_config_t;

/* == Crystal application interface (callbacks) ==============================*/

/* An interrupt-context callback called by Crystal when it starts or joins a net
 */
void app_crystal_start_done(bool success);

/* An interrupt-context callback called by Crystal before each S slot.
 *
 * returned value: pointer to the application payload for S slot */
uint8_t* app_pre_S();

/* An interrupt-context callback called by Crystal after S slot.
 * - received: whether a correct packet was received in the slot
 * - payload: pointer to the application payload in T slot packets
 */
void app_post_S(int received, uint8_t* payload);

/* An interrupt-context callback called by Crystal before each T slot.
 * 
 * returned value: pointer to the application payload for T slot */
uint8_t* app_pre_T();

/* An interrupt-context callback called by Crystal after each T slot.
 * - received: whether a correct packet was received in the slot
 * - payload: pointer to the application payload in T slot packets
 * 
 * returned value: pointer to the application payload for A slot */
uint8_t* app_between_TA(int received, uint8_t* payload);

/* An interrupt-context callback called by Crystal after each A slot.
 * - received: whether a correct packet was received in the slot
 * - payload: pointer to the application payload in A slot packets */
void app_post_A(int received, uint8_t* payload);

/* An interrupt-context callback that signals the end of the active
 * part of the epoch */
void app_epoch_end();

/* An interrupt-context callback that pings the app 
 * CRYSTAL_CONF_APP_PRE_EPOCH_CB_TIME before a new epoch starts */
void app_pre_epoch();

/* Print logs for the current epoch.
 * If needed, the application should call this function from its process
 * every epoch.*/
void crystal_print_epoch_logs();


/* == Crystal application interface (requests) ===============================*/

/* Init Crystal (to be called once at boot) */
void crystal_init();

/* Read the current configuration */
crystal_config_t crystal_get_config();

/* Start Crystal with the given configuration */
bool crystal_start(crystal_config_t* conf);


typedef struct {
  crystal_epoch_t epoch;
  uint16_t n_ta;
  uint16_t n_missed_s;
  uint8_t hops;
} crystal_info_t;

typedef struct {
  uint16_t send_seqn;
  uint16_t recv_seqn;
  uint16_t recv_src;
  uint8_t  acked;
  
} crystal_app_log_t;

/* A variable holding the current state of Crystal */
extern crystal_info_t crystal_info;

/* App-level information in Crystal logs */
extern crystal_app_log_t crystal_app_log;


#define PRINT_CRYSTAL_CONFIG(conf) do {\
 printf("Crystal config. Node ID %x\n", node_id);\
 printf("Period: %lu\n", (conf).period);\
 printf("Sink: %u\n", (conf).is_sink);\
 printf("S: %u %u %u\n", (conf).ntx_S, (conf).w_S, (conf).plds_S);\
 printf("T: %u %u %u\n", (conf).ntx_T, (conf).w_T, (conf).plds_T);\
 printf("A: %u %u %u\n", (conf).ntx_A, (conf).w_A, (conf).plds_A);\
 printf("Term: %u %u %u %u %u\n", (conf).r, (conf).y, (conf).z, (conf).x, (conf).xa);\
 printf("Ch: %x, Enc: %u, Scan: %u\n", (conf).ch_whitelist, (conf).enc_enable, (conf).scan_duration);\
} while (0)

#endif /* CRYSTAL_H_ */
