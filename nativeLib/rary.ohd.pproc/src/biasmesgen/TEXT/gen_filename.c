#include <string.h>
#include "bias_table.h"
#include "mesg_hdr.h"

void gen_filename(char ddhhmm[7], char dirname[100], char filename[128])

/*

   function to generate filename for the bias table message

   filename is of the form 

   radid.siteid.mescode.blockid.ddhhmm.temp

   where  radid = numeric radar id
          siteid = numeric site id
          mescode = message code                                              
                  = 15
          block id = type of message
                   = currenty equal to 1
          ddhhmm = generation time

   radid is read from the num_radid field of the radarloc table
   siteid is read from the num_hsa field of the admin table

   ".temp" extension is removed when file is closed

   calling routine:  create_biastable_mesg

*/

{

   /*------------------------------------*/
   /*   generate filename                */
   /*------------------------------------*/

   sprintf(filename,"%s/%03d.%03d.%d.%d.%6s.temp",dirname,
            MesgHdr.dest_id, MesgHdr.src_id, MesgHdr.mesg_code, biastable.block_id, ddhhmm);
   printf("File generated to contain bias table message: \n%s\n", filename);	    

}
