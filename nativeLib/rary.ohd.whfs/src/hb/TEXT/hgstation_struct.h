/************************************************************************
   hgstation_struct.h
   
   PURPOSE
   Include file for structures containing hgstation information.
   
   ***********************************************************************/

#ifndef HGSTATION_STRUCT_H
#define HGSTATION_STRUCT_H

typedef struct
{ 
    char     lid[LOC_ID_LEN + 1];
    char     pe[SHEF_PE_LEN + 1];
    char     ts[SHEF_TS_LEN + 1];
    char     fcstts[SHEF_TS_LEN + 1];
} hgstation_struct;    

#endif
