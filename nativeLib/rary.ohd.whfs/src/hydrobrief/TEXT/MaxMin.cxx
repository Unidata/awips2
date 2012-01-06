
#include "MaxMin.H"


//**********************************************************************

MaxMin::MaxMin()
{

     return;   
}

//**********************************************************************

MaxMin::~MaxMin()
{
   
     return;   
}

//**********************************************************************

void MaxMin::checkValue(double value)
{

   if (value != missingValue)
   {
      
      if (value < minValue)
      {
	 minValue = value; 	
      }
      
      if (value > maxValue)
      {
	 maxValue = value; 	
      }
      
   }
   
     return;   
}

//**********************************************************************

void MaxMin::checkValue(MaxMin maxMin)
{

     checkValue(maxMin.getMinValue());
     checkValue(maxMin.getMaxValue());
     
     return;   
}

//**********************************************************************

void MaxMin::checkValue(MaxMin maxMin[], long length)
{
     long i;
     
     for (i = 0; i < length; i++)
     {
          checkValue(maxMin[i]);
     }
     
     return;   
}

//**********************************************************************

double MaxMin::getMaxValue()
{
     return maxValue;      
}

//**********************************************************************

double MaxMin::getMinValue()
{
     return minValue;   
}

//**********************************************************************

void MaxMin::init(double initMin, double initMax, double initMissingValue)
{
     minValue = initMin;
     
     maxValue = initMax;
   
     missingValue = initMissingValue;
     return;
}

//**********************************************************************

void MaxMin::print()
{
   
     printf("Max = %f  Min = %f\n", maxValue, minValue);
   
     return;
}

//**********************************************************************
