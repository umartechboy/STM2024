#pragma once
#include <Arduino.h>

void serialCommand(String str);
void checkSerial();
extern bool serialEnabled; // Enables serial transfer of data
