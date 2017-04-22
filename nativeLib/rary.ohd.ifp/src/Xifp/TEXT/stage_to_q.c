/* File: stage_to_q.c
 *
 *  stage_to_q() - This function takes a single stage value as input
 *                 and returns the corresponding discharge value
 *                 after conversion by the FHQS1 subroutine.
 *                 Units are assumed to be metric (m and cms).
 *
 *  written by gfs, 9/11/91
 */
#include "c_call_f/fhqs1.h"

float   stage_to_q(stage)

float   stage;
{
 float  discharge;
 int    iconv;    /* metric to English flag */
 int    ibug;     /* debug flag */
 int    needex;   /* the following are used for       */
 int    lowext;   /* hydraulic extension computations */
 int    iupext;
 int    nw;
 int    nrange;
 int    mising;
 float  carryo[4];

 iconv = 1;
 ibug = needex = lowext = iupext = nw = nrange = mising = 0;
 carryo[0] = carryo[1] = carryo[2] = carryo[3] = 0.0;

 FHQS1(&stage, &discharge, &iconv, &ibug, &needex, &lowext, &iupext,
       &nw, &nrange, &mising, carryo);

 return(discharge);
}

/*
 *  q_to_stage() - This function takes a single discharge value as input
 *                 and returns the corresponding stage value
 *                 after conversion by the FHQS1 subroutine.
 *                 Units are assumed to be metric (m and cms).
 *  written by gfs, 9/11/91
 */

float   q_to_stage(discharge)

float   discharge;
{
 float  stage;
 int    iconv, ibug, needex, lowext, iupext, nw, nrange, mising;
 float  carryo[4];

 iconv = 2;
 ibug = needex = lowext = iupext = nw = nrange = mising = 0;
 carryo[0] = carryo[1] = carryo[2] = carryo[3] = 0.0;

 FHQS1(&stage, &discharge, &iconv, &ibug, &needex, &lowext, &iupext,
       &nw, &nrange, &mising, carryo);

 return(stage);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/stage_to_q.c,v $";
 static char rcs_id2[] = "$Id: stage_to_q.c,v 1.2 2002/02/11 19:45:28 dws Exp $";}
/*  ===================================================  */

}
