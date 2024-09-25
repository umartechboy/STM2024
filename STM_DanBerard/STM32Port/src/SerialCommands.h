#pragma once
#include <Arduino.h>
#include "DAC8814/DAC8814.h"

void serialCommand(String str);
void checkSerial();
extern bool serialEnabled; // Enables serial transfer of data
extern DAC8814 dac; // 16-bit quad DAC
