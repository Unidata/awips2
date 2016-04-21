#include <stdio.h>
#include <datetime.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

void calpc(char *, char *, int *, int *, char *, int *, FILE *);
void calpc624(char *, char *, int *, int *, int *, FILE *);
void calpp(int *, short int , int *, int *, int *, char *, FILE *);
void filterpc(char *,  int *, int *, int *, char *, FILE *);
void main_process(char *, int *, char *, char *, short int, short int, int, int *, int *, int *, int *, int *, int *, FILE *);
void post_procprecip(char *, char *, short int *, char *, float *, char *, short int *, char *, char *,
                     short int, FILE *);
void search_prev_hr(char *, char *, char *, int *, float *,  FILE *);

char  **dtpc;
char  **dtimepc;
char  **dtimepp;
char  **orig_dt1;
char  **orig_dt2;
double *value;
float  *valpc;
float  *calval;
short int  *idurat;
