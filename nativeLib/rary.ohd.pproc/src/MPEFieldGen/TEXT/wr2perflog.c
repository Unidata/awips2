#include <stdio.h>
#include <string.h>
#include "PerfLog.h"
#include "current_GMT_dt.h"

void wr2perflog(int *nproc, double elt, int *cpu, int *ret)
{
/*
   this subroutine writes information about each execution of
     mpe_fieldgen to the perflog table
*/

      dtime_t current_time_GMT;
      PerfLog fppp;

      *ret = current_GMT_dt(&current_time_GMT);
      if(*ret != 0) return;

      strcpy(fppp.process,"rfcwgen");
      fppp.start_time = current_time_GMT;
      fppp.num_processed = *nproc;
      fppp.elapsed_time = (double) elt;
      fppp.cpu_time = (float) *cpu;

      fppp.num_reads = 0;
      fppp.num_inserts = 0;
      fppp.num_updates = 0;
      fppp.num_deletes = 0;
      fppp.io_time = 0;

      *ret = PutPerfLog(&fppp);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/wr2perflog.c,v $";
 static char rcs_id2[] = "$Id: wr2perflog.c,v 1.1 2007/10/17 16:44:23 varmar Exp $";}
/*  ===================================================  */

}  /* end function wr2perflog  */
