#ifndef MAXMIN_HXX
#define MAXMIN_HXX

#include <stdio.h>
#include <stdlib.h>

class MaxMin
{
   
public:
   
   
     MaxMin();
     ~MaxMin();
     
     void init(double initMin, double initMax, double initMissingValue);
     
     void checkValue(double value);
     void checkValue(MaxMin maxMin);
     void checkValue(MaxMin maxMin[], long length);
     
     
     
     double getMaxValue();
     double getMinValue();
     
     void print();
     
private:
   
     double maxValue;
     double minValue;
     double missingValue;
};


#endif
