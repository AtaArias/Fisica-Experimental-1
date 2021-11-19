const byte Pino = 2;
byte flag = 0;
volatile unsigned long t = 0;

int n =1;
void setup() {
Serial.begin(9600); // setup serial
pinMode(Pino, INPUT_PULLUP);
attachInterrupt(digitalPinToInterrupt(Pino),inputISR1, CHANGE);
Serial.print("Interrupt");
Serial.print(";");
Serial.println("t");
}
void loop() {
//int rawTemp;
if(flag == 1){
Serial.print(n);
Serial.print(";");
Serial.println(t);
flag = 0;
n = n+1;
delay(70);
}
}
/*INTERRUPCIonN*/
void inputISR1() {
t = micros();
flag = 1;
}
