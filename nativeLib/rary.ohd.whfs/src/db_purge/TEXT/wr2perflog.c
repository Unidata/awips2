#include <string.h>
#include <sqlca.h>
#include <stdio.h>

#include "PerfLog.h"

      void wr2perflog(char type[10], dtime_t *itime, int *nproc,
	          float *elt)
{
/*
   this subroutine writes information about each execution of
     a process to the PerfLog table                  
*/

      PerfLog fppp;
      int ret;

      strcpy(fppp.process,type);
      fppp.start_time = *itime;
      fppp.num_processed = *nproc;
      fppp.elapsed_time = *elt;

      fppp.cpu_time = 0;
      fppp.num_reads = 0;
      fppp.num_inserts = 0;
      fppp.num_updates = 0;
      fppp.num_deletes = 0;
      fppp.io_time = 0;

      ret = PutPerfLog(&fppp);
      if(ret != 0)
      {
	   printf("PostgreSQL error %s attempting insert into PerfLog table\n",sqlca.sqlstate);
      }


}  /* end function wr2perflog  */
