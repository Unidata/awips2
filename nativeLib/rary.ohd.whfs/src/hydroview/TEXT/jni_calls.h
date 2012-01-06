#ifndef JNI_CALLS_H_
#define JNI_CALLS_H_

#include "jni.h"


void startjvm();
void closejvm();
void callTimeSeriesLiteThroughJNI(const char *locationId,
                                  const char *obsParamCode,
                                  const char *fcstParamCode,
                                  const char *jdbcUrl);

void callColorManagerThroughJNI( const char *jdbcUrl,
                                 const char *logFilePath,
                                 const char *userID,
                                 const char *applicationName,
                                 const char *defaultColorFile,
                                 const char *rgbFilePath);
				 
long getColorManagerSaveTimeThroughJNI( );				 

jclass getClass(const char* javaClassName);


#endif /*JNI_CALLS_H_*/
