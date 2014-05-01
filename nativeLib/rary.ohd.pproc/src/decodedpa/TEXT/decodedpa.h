#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE 1
#define FALSE 0

#define LEVEL_OUT_OF_RANGE 255

#define NUM_DPA_COLS 131
#define NUM_DPA_ROWS 131

FILE *dpafile;

char radid[4];

/*--------------------------*/
/*  function prototypes     */
/*--------------------------*/

void get_radid_from_product(char[]);
int  get_radid_from_filename(char[]);
char *convertJulianDate(short);
void decodeDPA(char *, int *, int);
int getadapt(int *, float [], char []);
int getsuppl(short int, int *, int *, int *, float *, float *, int *, int *,
             int *, int *, int *, float *, float *, float *,
             int *, int *, char []);
int check_radid();
void wrtodb_adapt(short int, char [], float [], char []);
void wrtodb_suppl(short int, char [], short *, float *, float *, float *,
                  char *, char *, char *, char *, char *, int *, int *, int *,
                  float *, float *, int *, int *, int *, int *,
                  int *, int *, int *, float *, float *, float *,
                  char [], int *, char *);
