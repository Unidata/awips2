#ifndef _AFBUFRCOMMON_H
#define _AFBUFRCOMMON_H
#include "afbufr_structures.h"
void setCycleAndType(char *outputFormatIn, char *cycle, char *airmetType,
                    char *outputFormatStr);

void buildFilenameAndDateStampFmIssTime(char *issTimeStr, char *outputformat,
                                        char *bufrfilename, char *dateStamp);
#endif
