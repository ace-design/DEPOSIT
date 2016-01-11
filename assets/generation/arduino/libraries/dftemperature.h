#ifndef _DF_TEMPERATURE_H_
#define _DF_TEMPERATURE_H_
#include "temperature.h"
class DFTemperatureSensor: public TemperatureSensor
{
public:
	DFTemperatureSensor(int port){
		_port = port;
	}
	virtual double readTemperature();
private:
		int _port;
};
#endif

double DFTemperatureSensor::readTemperature() {
	int a = analogRead(_port);
  	return (500 * a)/1024;
}