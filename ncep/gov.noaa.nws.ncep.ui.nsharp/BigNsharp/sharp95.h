#include "structures.h"
#include "globals.h"
#include "readdata.h"
#include "decoder.h"

/*
Moved farther down so this would compile with gcc, otherwise
the following error is obtained: 
In file included from sharp95.h:5,
                 from parameterization.c:2:
hpgl.h:1: parse error before `*'

#include "hpgl.h"
*/

#include "winter.h"
#ifndef _WIN32
#include "xwvideo.h"
#include "save.h"
#endif
#include "globals_xw.h"
#include "config.h"
#include "textsave.h"
#include "sndglib.h"
#ifndef _WIN32
#include "hpgl.h"
#endif

void write_scheme_file(char st[80]);
void write_hail_file(char st[80]);
float sig_hail(float mucape, float mumixr, float lr75, float t500, float shr6, float fzlh, float mucin, float davc, float davcb, float ic, float mlcape);
int read_nsharp_config(void);
float mpi_func1 ( float x);
float mpi_func2 ( float x);
float mpi_func3 ( float x);
float spechum (float pres, float temp);
float itemp (float x);
float idwpt (float x);
float ihght (float x);
int cave_cvgust(float dpd, float ui);
