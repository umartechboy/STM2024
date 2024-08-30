#pragma once
#include <Arduino.h>
// Default scan settings:
#define SCAN_SIZE 100000 // ~160 nm        // Scan size in LSBs
#define IMAGE_PIXELS 512                   // Scan size in pixels
#define LINE_RATE 1                        // Number of scan lines/second
#define SETPOINT 328 // 1 nA               // Tunneling current setpoint in LSBs
#define BIAS 328 // 100 mV                 // Sample bias in LSBs
#define KP 0                               // Proportional gain
#define KI 300000                            // Integral gain


// Constants:
#define INVERT_Z true                      // Inverts the Z output signal from the DAC
#define ENGAGE_SCANNER_STEP_SIZE 50        // Number of LSBs to step the scanner by during engage
#define ENGAGE_MOTOR_STEP_SIZE 1           // Number of steps to move the motor by during engage
#define dt 40                              // Time step for scanning and PI control in microseconds
#define DATA_BUFFER_LENGTH 16386           // Number of bytes in each ping-pong buffer for data storage. Need 2 bytes for line number + 16 bytes/pixel.
#define SCAN_COUNTER_LIMIT 0x40000000      // Scan counter counts from -SCAN_COUNTER_LIMIT to SCAN_COUNTER_LIMIT-1




/**************************************************************************/
/*
    Variable declarations
*/
/**************************************************************************/


// Scan parameters:
extern float lineRate; // Scan lines per second
extern unsigned int pixelsPerLine;
extern unsigned int samplesPerPixel;
extern int scanSize; // Size of the scan in LSBs
extern int bias; // Sample bias in LSBs
extern bool scanningEnabled;
extern bool engaged;

// Sample, pixel and line counters:
extern volatile unsigned int sampleCounter, pixelCounter, lineCounter;
extern volatile int zAvg, eAvg; // Accumulates Z and error samples for later averaging


// PI variables:
extern bool pidEnabled; // Setting this to false desiables PI control
extern int setpoint, setpointLog; // setpointLog = log(|setpoint|)
extern int Kp, Ki; // Proportional and integral gains
extern volatile int16_t input; // ADC input data
extern volatile int error; // PID error signal
extern volatile int64_t iTerm; // Integral term
extern const int64_t MAX_ITERM; // Maximum integral term. Used to prevent windup.



// Position variables:
extern const int MAX_Z; // Maximum Z value
extern int xo, yo; // Scan offsets
extern volatile int x, y, z; // Scanner coordinates in LSBs


int sigmaDelta(int in, int *sigma, unsigned int shift);
int saturate(int val, int max, int min);
void updateStepSizes();
void moveTip(int xf, int yf);
void waitTimeStep();
bool engage();
void retract();
void resetScan();
void incrementScan(void);
