#include <string.h>
#include "PerfLog.h"

      int writePerfLog(char process[10], dtime_t *itime)             
{
/*
   this subroutine writes information about each execution of the
     application specified by process variable to records in the
     PerfLog table
*/

      PerfLog fppp;
      int status;

      bool isUnique;

      strcpy(fppp.process,process);
      fppp.start_time = *itime;

      fppp.num_processed = 0;
      fppp.elapsed_time = 0;
      fppp.cpu_time = 0;
      fppp.num_reads = 0;
      fppp.num_inserts = 0;
      fppp.num_updates = 0;
      fppp.num_deletes = 0;
      fppp.io_time = 0;

/*      ret = PutPerfLog(&fppp); */
      status = InsertIfUniquePerfLog(&fppp, &isUnique);

      if ( !isUnique)
      {
      }

      return status;
}
