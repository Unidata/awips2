#include <unistd.h>
#include "decodedpa.h"
#include "convert_to_LittleEndian.h"
#include "GeneralUtil.h"
#include "GetOS.h"
#include "startdb.h"
#include "closedb.h"

/*-----------------------------------------------------------------------------*/
/*  main program for decoding DPA products                                     */
/*                                                                             */
/*  product name is passed from the command line into this program             */
/*                                                                             */
/*  DPA products have a filename of the form                                   */
/*        XXXXX/DPAYYY.yyyymmddhhmmss                                          */
/*         where XXXX = directory name                                         */
/*               YYY = radar id                                                */
/*               yyyymmddhhmmss = date/time from system clock                  */
/*                                                                             */
/*  if running on Linux, raw DPA product is converted to Little Endian format, */
/*     saved to a file with the name *.LE and then deleted at end of           */
/*     decoding process                                                        */
/*  if running on HP or Linux, archived product is in Big Endian format        */
/*                                                                             */
/*  return codes from decodedpa executable to the Run_DecodeDPA script         */
/*  0 -- valid product                                                         */
/*       (includes case of supplemental message = "no precip detected")        */
/*  1 -- valid product, no decoded file created                                */
/*       (non-top-of-hour product, supplemental message = "bad rate scan", etc)*/
/*  2 -- invalid product                                                       */
/*       (loss of data, unexpected EOF, invalid date or time, etc)             */
/*  3 -- problem outside of product                                            */
/*       (error opening db, error opening product                              */
/*  4 -- radar id not listed in RadarLoc table                                 */
/*  5 -- failure in Big Endian to Little Endian conversion                     */
/*-----------------------------------------------------------------------------*/

int decode_dpa_main( int argc,const char **argv)
{
   int pcipflg, ichk_rid_flg, ireturn, c;
   int write_to_db = 1;
   long int irc;
   char rawfname[256], LittleEfname[256];
   OperSys os;

   /*------------------------------------------------------------*/
   /*   check any command line arguments                         */
   /*------------------------------------------------------------*/

   while ( ( c = getopt ( argc , argv , "i" ) ) != -1 )
   {
      switch( c )
      {
	 case 'i' :
	    write_to_db = 0;
	    break;
      }
   }

   if ( write_to_db == 0 )
   {
      argv[1] = argv[2];
   }

   /*------------------------------------------------------------*/
   /*   open database                                            */
   /*------------------------------------------------------------*/

   if ( write_to_db == 1 )
   {
      startdb(&irc);
      if(irc !=0)
      {
	 printf("PostgreSQL error# %ld ",irc);
	 printf(" occurred attempting to open database\n");
	 exit(3);
      }
   }

   /*-------------------------------------------------------*/
   /*   determine radar id                                  */
   /*                                                       */
   /*   if the first 7 char of the filename = "81", then   */
   /*      prev 3 char contain radar id                     */
   /*     (get_radid_from_filename)                         */
   /*   else open file and look for "DPA" in header         */
   /*     next 3 char contain radar id                      */
   /*     (get_radid_from_product)                          */
   /*-------------------------------------------------------*/

   if(get_radid_from_filename(argv[1]) == 1)
   {
      get_radid_from_product(argv[1]);
   }

   /*--------------------------------------------------------------------*/
   /*   check radid against entries in RadarLoc table                    */
   /*   if the radar id is not in the table OR the use_radar field='F'   */
   /*    then radar will not be decoded                                  */
   /*--------------------------------------------------------------------*/

   if ( write_to_db == 1 )
   {
      ichk_rid_flg = check_radid();
      if(ichk_rid_flg > 0)
      {
	 if(ichk_rid_flg == 1)
	    printf("%s radar identifier not in RadarLoc table -- product not decoded\n",radid);
	 else
	    printf("%s appears in RadarLoc table with use_flag=F -- product not decoded\n",radid);

	 /*-------------------------------*/
	 /*   close database              */
	 /*-------------------------------*/

	 closedb(&irc);
	 if(irc !=0)
	 {
	    printf("PostgreSQL error# %ld ",irc);
	    printf(" occurred attempting to close database \n");
	 }

	 exit(4);
      }
   }

   /*-------------------------------------------------------------------------*/
   /*   if running on Linux, then convert raw product to Little Endian format */
   /*   new file is created with same filename as original file but with      */
   /*     ".LE" appended to it                                                */
   /*-------------------------------------------------------------------------*/

   strcpy(rawfname,argv[1]);
   os = GetOS();
   if(os == OS_LINUX)
   {
      ireturn = convert_to_LittleEndian(rawfname, LittleEfname);

      if(ireturn != 0)
      {
         if ( write_to_db == 1 )
         {
	    closedb(&irc);
         }
	 if(irc !=0)
	 {
	    printf("PostgreSQL error# %ld ",irc);
	    printf(" occurred attempting to close database \n");
	 }

	 exit(5);
      }

      strcpy(rawfname, LittleEfname);
   }

   /*-------------------------------------------------------------------*/
   /*   decode DPA product                                              */
   /*   if running on Linux, decode the Little Endian format product    */
   /*   if running on HP, decode the Big Endian format product          */
   /*-------------------------------------------------------------------*/

   decodeDPA(rawfname, &pcipflg, write_to_db);

   /*-----------------------------------------*/
   /*   close database                        */
   /*-----------------------------------------*/

   if ( write_to_db == 1 )
   {
      closedb(&irc);
      if(irc !=0)
      {
	 printf("PostgreSQL error# %ld ",irc);
	 printf(" occurred attempting to close database \n");
      }
   }

   /*------------------------------------------------------------------------*/
   /* exit with a status of 0 if this is a valid product with either precip  */
   /*  (pcipflg=0) or no precip detected (=4)                                */
   /* otherwise, exit with a status of 1 for the cases of bad rate           */
   /*  scan (=1), not enough data in hour (=2), and disk error (=3)          */
   /*  in the supplemental message                                           */
   /*------------------------------------------------------------------------*/

   if(pcipflg == 0 || pcipflg == 4)
      exit(0);
   else if (pcipflg == 1 || pcipflg == 2 || pcipflg == 3)
      exit(1);

   return 0;

}
