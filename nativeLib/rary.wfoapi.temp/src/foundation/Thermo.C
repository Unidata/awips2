// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// Thermo.C
// Contains funtions that calculate various meteorological parameters.
//
// Author: lefebvre
// ---------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const Thermo_C_Id =
  "$Id$";
#endif

// -- module -----------------------------------------------------------------
// This class contains simple static functions that calculate various
// meteorological parameters. 
// -- implementation ---------------------------------------------------------
// This implementation is based on the FORTRAN thermoLib routines, authored
// primarily by Don Baker.  See this library for more details and source for
// the equations.
// ---------------------------------------------------------------------------
#include "LogStream.H"
#include "Thermo.H"
#include <math.h>

// -- public -----------------------------------------------------------------
// Thermo::relativeHum()
// Returns the relative humidity in percent, given the temperature and the
// dewPoint in degrees Fahrenheit.
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
float Thermo::relativeHum(float temp, float dewPoint) 
    {
    temp = Thermo::fToC(temp);
    dewPoint = Thermo::fToC(dewPoint);
    return Thermo::satVapPres(dewPoint) / Thermo::satVapPres(temp) * 100.0;
    }

// -- public -----------------------------------------------------------------
// Thermo::dewPoint()
// Returns the dewPoint in degrees Fahrenheit, given the temperature in 
// degrees Fahrenheit and the relative humidity.
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
float Thermo::dewPoint(float temp, float rh) 
    {
    // Convert from F to C
    temp = Thermo::fToC(temp);

    float antiRH = 1.0 - 0.01 * rh;

    // Calculate the dewPoint depression
    float dewDep = (14.55 + 0.114 * temp) * antiRH
      + pow(((2.5 + 0.007 * temp) * antiRH), 3)
      + (15.9 + 0.117 * temp) * pow(antiRH, 14);

    // Compute the dewpoint and convert back to degrees F
    float dewPoint = temp - dewDep;

    return Thermo::cToF(dewPoint);
    }

// -- public -----------------------------------------------------------------
// Thermo::fToC()
// Returns the temperature in Celcius, given the temperature in Fahrenheit
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
float Thermo::fToC(float f)
    {
    return (f - 32) * 5 / 9;
    }

// -- public -----------------------------------------------------------------
// Thermo::cToF()
// Returns the temperature in Fahrenheit, given the temperature in Celcius
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
float Thermo::cToF(float c)
    {
    return c * 9 / 5 + 32;
    }

// -- private ----------------------------------------------------------------
// Thermo::satVapPress()
// Returns the saturation vapor pressure in millibars, given the temperature
// in degrees Celcius.
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
float Thermo::satVapPres(float temp) 
    {
    float tk = temp + 273.15;    // Temp in Kelvin
    float p1 = 11.344 - 0.0303998 * tk;
    float p2 = 3.49149 - 1302.8844 / tk;
    float c1 = 23.832241 - 5.02808 * log10(tk);
    float esat = pow(10.0, (c1 - 0.00000013816 * pow(10.0, p1)
      + 0.0081328 * pow(10.0, p2) - 2949.076) / tk);
    return esat;
    }

