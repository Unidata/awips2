// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// Thermo.H
// Contains funtions that calculate various meteorological parameters.
// 
// Author: lefebvre
//-----------------------------------------------------------------------------
#ifndef _Thermo_H
#define _Thermo_H

#ifdef IDENT_H
static const char* const Thermo_H_Id =
 "$Id$";
#endif

class Thermo
    {
    public:
        static float relativeHum(float temp, float dewPoint);
        static float dewPoint(float temp, float rh);
        static float fToC(float f);
        static float cToF(float c);
        
    private:
        static float satVapPres(float temp);
    };

#endif
