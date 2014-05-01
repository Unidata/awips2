/*******************************************************************************
* FILENAME:              empe_util.h
*
* DESCRIPTION:         This file contains utility function prototypes
*                      for the empe_fieldgen program.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         August 01, 2006
* ORGANIZATION:          HSEB / OHD
* MACHINE:               Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef EMPE_UTIL_H
#define EMPE_UTIL_H

/*
 *  function prototypes
 */

void    hpe_fieldgen_printUsage();
int     hpe_fieldgen_isDigits(const char* );
char*   hpe_fieldgen_toLowerCase(char* );
int     hpe_fieldgen_getAppsDefaults(const char*, char*);
void    hpe_fieldgen_getCurrentTime(char* strTime) ;

void    hpeOpenLogFile(const time_t tDate) ;
void    hpeDeleteLogFile() ;

void    printLogMessage(const char *) ;
void    hpe_fieldgen_printMessage(const char *) ;
void    shutdown(const char *);

void buildCategoryName(const int timeValue, char categoryName[4]);

void hpe_fieldgen_hrap_to_latlon(const double hrap_x ,
                    const double hrap_y ,
                    double * flon ,
                    double * flat );

void hpe_fieldgen_hrapsize(const double flat , double * rmesh);

void LatLongToScaledHrap(const double lat ,
                         const double lon ,
                         const double factor,
                         double * scale_row ,
                         double * scale_col );

void ScaledHrapToLatLong(const double scale_row,
                         const double scale_col,
                         const double factor,
                         double * lat ,
                         double * lon );

int compare_radar_id ( void * search_value,
                       void * array_value );

void fill2DFloatArray(float ** pArray, 
                      const float value,
                      const int rows,
                      const int cols );

short * init1DShortArray(const short defaultValue,
                         const int rowSize);
int * init1DIntArray(const int defaultValue,
                     const int rowSize);
float * init1DFloatArray(const float defaultValue,
                         const int rowSize);
double * init1DDoubleArray(const double defaultValue,
                           const int rowSize);


short ** init2DShortArray(const short defaultValue,
                          const int rowSize,
                          const int colSize );
int ** init2DIntArray(const int defaultValue,
                      const int rowSize,
                      const int colSize );
float ** init2DFloatArray(const float defaultValue,
                          const int rowSize,
                          const int colSize );
double ** init2DDoubleArray(const double defaultValue,
                            const int rowSize,
                            const int colSize );

void free1DShortArray(short * array );
void free1DIntArray(int * array );
void free1DFloatArray(float * array );
void free1DDoubleArray(double * array );

void free2DShortArray(short ** array, 
                      const int rowSize );
void free2DIntArray(int ** array, 
                    const int rowSize );
void free2DFloatArray(float ** array, 
                      const int rowSize );
void free2DDoubleArray(double ** array, 
                       const int rowSize );


void convertDoubleArray(const int orig_x ,
                        const int orig_y ,
                        double ** source ,
                        const double missingValue ,
                        const int target_x ,
                        const int target_y ,
                        double ** target );

void convertFloatArray(const int orig_x ,
                       const int orig_y ,
                       float ** orig ,
                       const float missingValue ,
                       const int target_x ,
                       const int target_y ,
                       float ** target );

void convertShortArray(const int orig_x ,
                       const int orig_y ,
                       short ** orig,
                       const short missingValue ,
                       const int target_x ,
                       const int target_y ,
                       short ** target );

void interpolateArray(const int rows ,
                      const int cols ,
                      float ** prev_array ,
                      float ** post_array ,
                      const float missingValue ,
                      const float prev_weight ,
                      const float post_weight ,
                      float ** target );

#endif /* #ifndef EMPE_UTIL_H */
