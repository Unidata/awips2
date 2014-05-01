#ifndef DISPLAY_PDC_TSL_JNI_H_
#define DISPLAY_PDC_TSL_JNI_H_

#include "jni.h"

void startjavavm();
void closejavavm();

JNIEnv * getJavaEnv ( );
JavaVM * getJavaVM ( ); 

void callPDCTimeSeriesLiteThroughJNI( const char *jdbcUrl,
                                      char *stationId,
                                      char *stationParm);                                 

jclass getClassName(const char* javaClassName);

void callColorManagerThroughJNI(const char *jdbcUrl,
                                const char *logFilePath,
                                const char *applicationName,
                                const char *userID,
                                const char *defaultColorFilePath,
                                const char *rgbFilePath);

long getColorManagerSaveTimeThroughJNI( );


#endif /*DISPLAY_PDC_TSL_JNI_H_*/
