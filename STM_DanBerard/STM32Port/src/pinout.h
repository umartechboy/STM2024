#pragma once

// Pin definitions:
#define CS_DAC PinName::PA_0                            // DAC chip select pin
#define LDAC PinName::PA_0                              // Load DAC pin, not currently used
#define CS_ADC PinName::PA_0                            // ADC chip select pin
#define CNV PinName::PA_0                               // ADC CNV pin - initiates a conversion
#define BUSY PinName::PA_0                              // ADC BUSY pin
#define SERIAL_LED PinName::PA_0                        // Indicates serial data transmission
#define TUNNEL_LED PinName::PA_0                        // Indicates tunneling


// DAC channel addresses:
#define DAC_CH_X 2
#define DAC_CH_Y 1
#define DAC_CH_Z 0
#define DAC_CH_BIAS 3


// DAC and ADC resolution:
#define DAC_BITS 16                        // Actual DAC resolution
#define POSITION_BITS 20                   // Sigma-delta resolution
#define ADC_BITS 16

