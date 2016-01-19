#ifndef _EB_TEMPERATURE_H_
#define _EB_TEMPERATURE_H_
#include "temperature.h"
class EBTemperatureSensor: public TemperatureSensor
{
public:
	EBTemperatureSensor(int port){
		_port = port;
	}
	virtual double readTemperature();
private:
		int _port;
};
#endif

double EBTemperatureSensor::readTemperature() {
	int a =analogRead(_port);
  	float resistance=(float)(1023-a)*10000/a; //get the resistance of the sensor;
  	float temperature=1/(log(resistance/10000)/3975+1/298.15)-273.15;//convert to temperature via datasheet&nbsp;;
  	return (double) temperature;
}