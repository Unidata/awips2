#ifndef MPE_LOG_UTILS_H
#define MPE_LOG_UTILS_H

#include <stdio.h>
#include <stdarg.h>

void logMessage(const char* format, ...);
void flogMessage(FILE * stream, const char* format, ...);
void openLogFile();
void closeLogFile();

#endif
