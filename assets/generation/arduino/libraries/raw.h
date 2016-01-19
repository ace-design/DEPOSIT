#ifndef _RAW_H_
#define _RAW_H_
#include "Arduino.h"
class RawSensor
{
public:
	RawSensor(int port){
		_port = port;
	}
	int readValue();
private:
		int _port;
};
#endif


int RawSensor::readValue(){
	return analogRead(_port);
}