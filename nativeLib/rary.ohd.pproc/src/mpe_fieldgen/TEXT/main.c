#include <stdio.h>
#include <stdlib.h>

#include "main_mpe_fieldgen.h"

int mpe_fieldgen_main(int argc, const char ** argv)
{
   if(argc > 0 && argv != NULL)
   {
      main_mpe_fieldgen_for_calls_from_editor(argc, argv);
   }
   else
   {
      printf("No arguements supplied\n");
      exit(0);
   }

   return 0;
}
