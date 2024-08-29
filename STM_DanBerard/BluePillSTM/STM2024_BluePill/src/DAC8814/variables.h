#pragma once


// Default scan settings:
#define SCAN_SIZE 100000 // ~160 nm        // Scan size in LSBs
#define IMAGE_PIXELS 512                   // Scan size in pixels
#define LINE_RATE 1                        // Number of scan lines/second
#define SETPOINT 328 // 1 nA               // Tunneling current setpoint in LSBs
#define BIAS 328 // 100 mV                 // Sample bias in LSBs
#define KP 0                               // Proportional gain
#define KI 300000                          // Integral gain


// Constants:
#define INVERT_Z true                      // Inverts the Z output signal from the DAC
#define ENGAGE_SCANNER_STEP_SIZE 50        // Number of LSBs to step the scanner by during engage
#define ENGAGE_MOTOR_STEP_SIZE 1           // Number of steps to move the motor by during engage
#define dt 40                              // Time step for scanning and PI control in microseconds
#define DATA_BUFFER_LENGTH 16386           // Number of bytes in each ping-pong buffer for data storage. Need 2 bytes for line number + 16 bytes/pixel.
#define SCAN_COUNTER_LIMIT 0x40000000      // Scan counter counts from -SCAN_COUNTER_LIMIT to SCAN_COUNTER_LIMIT-1


