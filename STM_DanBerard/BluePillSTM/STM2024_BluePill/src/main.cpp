#include <Arduino.h>

void setup() {
  Serial.begin(115200);
  pinMode(PC13, OUTPUT);
}

int i = 0;
void loop() {
  // put your main code here, to run repeatedly:

  i++;
  Serial.println(i);

  digitalWrite(PC13, i % 2) ;
  delay(1000);
}