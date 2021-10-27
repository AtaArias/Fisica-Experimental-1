/* Carga de librerías  */
#include <TimeLib.h>
#include <OneWire.h>
#include <DallasTemperature.h>

/* Config de OneWire.h */
#define ONE_WIRE_BUS 2
OneWire oneWire(ONE_WIRE_BUS);

/* Ubicación de DS18B20 */
DallasTemperature sensors(&oneWire);

/* Def. variable temp */
float temp;

void setup() {
  Serial.print(" hello world ");
  // Inicializa el perto serial
  Serial.begin(9600);

  /* 1 - Setup para reloj */
  setTime(0, 0, 0, 01, 01, 0001);

  /* 2 - Setup para TERMOMETRO */
  sensors.begin();

}
void loop() {
  /* Leer sensor DS18B20 */
  sensors.requestTemperatures();

  /* Variable de temp en celcius */
  temp = sensors.getTempCByIndex(0);

  /* LOPP Imprimir */
  Serial.print(" hello world");
  Serial.print(hour(), DEC);
  Serial.print(" ; ");
  Serial.print(minute(), DEC);
  Serial.print(" ; ");
  Serial.print(second(), DEC);
  Serial.print(" ; ");
  Serial.println(temp);

  /* Demora hasta la siguiente medida */
  delay(10000);
  
}
