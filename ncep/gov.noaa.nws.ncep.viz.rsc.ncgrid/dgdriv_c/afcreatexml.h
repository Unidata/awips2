#ifndef _AFCREATEXML_H
#define _AFCREATEXML_H

/*
 *  Global definitions
 */
#define NUM_TYPES       (3)             /* Number of FA areas */
#define ONEBLOCK        2048
#define XML_HDR         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
#define SCM_FILE \
"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
xsi:noNamespaceSchemaLocation=\"file:$GEMTBL/xsd/airmet.xsd\""
#define XSD             "xsd"
#define FOUR_THOUSAND   ( 40 )          /* or 040 in AWC usage */

void af_create_prexml(int ntypes, char *types[3], char *refDate,
                      char *cycle, int nin, VG_DBStruct *el_in,
                      Boolean filterTimeSmears, char *issTimeStr,
                      char *string[NUM_TYPES],int *iret );

void loadXmlfile(char **xmlString, char *filename,int *ier);

void writeToXmlfile(char *xmlString, char *issTimeStr, char
    *outputformat, int *ier);

#endif
