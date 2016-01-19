#ifndef _TEMPERATURE_H_
#define _TEMPERATURE_H_
#include <math.h>
#include "Arduino.h"
class TemperatureSensor
{
	public:
		virtual double readTemperature() = 0;
	
};
#endif