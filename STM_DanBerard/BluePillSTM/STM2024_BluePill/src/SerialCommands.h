#pragma once
#include <Arduino.h>

void serialCommand(String str);
void checkSerial();
extern boolean serialEnabled; // Enables serial transfer of data
