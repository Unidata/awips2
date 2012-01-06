#include <stdio.h>
#include "string.h"
#include "mesg_hdr.h"
#include "bias_table.h"

#define MSG_CODE    15       /* Message Code */
#define NUM_BLOCKS   2       /* No. blocks in Bias Table Message */

/*--------------------------------------------------------------*/
/*          Standard Message Header Generation Routine          */
/*--------------------------------------------------------------*/

void create_mesg_hdr( short int gen_date_hour[6], short int btabl_block_len )

/*
   Create the standard NEXRAD Message Header

   mesg_hdr_struct = structure containing fields of standard NEXRAD
                     Message Header preceding products and messages
*/

{
   short int  ix_hr = 3, ix_mn = 4, ix_sc = 5;

   /*------------------------------*/
   /*     Define message code      */
   /*------------------------------*/

    MesgHdr.mesg_code = MSG_CODE;
   
   /*-------------------------------------*/
   /*     Print generation date & time    */
   /*-------------------------------------*/

    /*
    printf("gen_date_hour: ");
    for ( n = 0; n < 6; n++)
    {
       printf("%d ",gen_date_hour[n]);
    }
    printf("\n");
    */

   /*------------------------------------------------*/
   /*     Estimate message date (modified Julian)    */
   /*------------------------------------------------*/

    MesgHdr.mesg_date = modif_julian( gen_date_hour );

   /*----------------------------------------------------*/
   /*     Determine Message time (secs after midnight)   */
   /*----------------------------------------------------*/

    MesgHdr.mesg_time = (gen_date_hour[ix_hr] * 3600) + (gen_date_hour[ix_mn] * 60) + gen_date_hour[ix_sc];

   /*-----------------------------*/
   /*     Set message length      */
   /*-----------------------------*/

    MesgHdr.mesg_len = sizeof(short)*5 + sizeof(int)*2;
    MesgHdr.mesg_len += btabl_block_len;
   
   /*----------------------------------*/
   /*     Set no. blocks in message    */
   /*----------------------------------*/

    MesgHdr.num_blks = NUM_BLOCKS;
   
   /*--------------------------------------*/
   /*     Print Message Header contents    */
   /*--------------------------------------*/

    /*
    printf("Message Header Fields:\n");
    printf("  Message Code: %d\n", MesgHdr.mesg_code); 
    printf("  Message Date: %d\n", MesgHdr.mesg_date); 
    printf("  Message Time: %d\n", MesgHdr.mesg_time); 
    printf("      Block 2 Length: %d\n", btabl_block_len); 
    printf("  Message Length: %d\n", MesgHdr.mesg_len); 
    printf("  Source ID: %d\n", MesgHdr.src_id); 
    printf("  Dest. ID: %d\n", MesgHdr.dest_id); 
    printf("  No. Blocks: %d\n", MesgHdr.num_blks); 
    */

}
