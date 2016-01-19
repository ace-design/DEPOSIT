#ifndef _GROVE_TEMPERATURE_H_
#define _GROVE_TEMPERATURE_H_
#include "temperature.h"
class GroveTemperatureSensor: public TemperatureSensor
{
public:
	GroveTemperatureSensor(int port){
		_port = port;
	}
	virtual double readTemperature();
private:
		int _port;
};
#endif

double GroveTemperatureSensor::readTemperature() {
	int a = analogRead(_port);
  	float R = 1023.0/((float)a)-1.0;
    R = 100000.0*R;
    float temperature=1.0/(log(R/100000.0)/4275+1/298.15)-273.15;
  	return (double) temperature;
}