/***********************************************************************
* Filename: main_decode.c
*
* Original Author: adopted from DPA Decoder
*
* File Creation Date:
*
* Development Group: OHD
*
* Description:
* main program for decoding radar DHR/DSP products
*
* Modules:
* main
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <unistd.h>
#include "closedb.h"
#include "decode_radar_product.h"
#include "GetOS.h"
#include "startdb.h"

/***********************************************************************
* Module Name: main
*
* Original Author: adopted from DPA Decoder
*
* Module Creation Date:
*
* Description:
*   main program for decoding radar DHR/DSP products and outputting data
*   in quarter HRAP grids.
*
*  product name is passed from the command line into this program
*
*  radar products have a filename of the form
*        XXXXX/NNNYYY.yyyymmddhhmmss
*         where XXXX = directory name
*               NNN = radar product: DHR or DSP
*               YYY = radar id
*               yyyymmddhhmmss = date/time from system clock
*
*  if running on Linux, raw radar product is converted to Little Endian format,
*     saved to a file with the name *.LE and then deleted at end of
*     decoding process
*  if running on HP or Linux, archived product is in Big Endian format
*
*  return codes from decode_dhr_dsp executable to
*  the Run_DecodeDHR and Run_DecodeDSP script
* -1 -- Invalid filename
*       (file name does not include dsp, dhr, or dpa)
*  0 -- valid product
*       (includes case of supplemental message = "no precip detected")
*  1 -- valid product, no decoded file created
*       (non-top-of-hour product, supplemental message = "bad rate scan", etc)
*  2 -- invalid product
*       (loss of data, unexpected EOF, invalid date or time, etc)
*  3 -- problem outside of product
*       (error opening db, error opening product
*  4 -- radar id not listed in RadarLoc table
*  5 -- failure in Big Endian to Little Endian conversion
*
*   calling function:
*   functions called: get_radid_from_filename, get_radid_from_product, decodeDHR,
*                     decodeDSP, check_radid
*
* Calling Arguments:
* Name         Input/Output Type          Description
*
* filename     Input        char *        raw DHR/DSP file name
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
*
*
* Return Value:
* Type          Description
* None
*
* Error Codes/Exceptions:
*
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date        Developer     Action
* 7/20/2006   Guoxian Zhou  Build operational version
*
***********************************************************************/

int decode_dhr_dsp_main( int argc, const char ** argv)
{
	int status, c;
	int write_to_db = 1;
	long int irc;
	char raw_filename[FILEPATH_LEN] = {'\0'};
//	char LittleE_filename[FILEPATH_LEN] = {'\0'};
	OperSys os;
	productType type = unknown;
	char radid[4] = {'\0'};

	/*
	 * check any command line arguments
	 */

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

	/*
	 * open database
	 */

	if ( write_to_db == 1 )
	{
		startdb(&irc);
		if(irc != 0)
		{
			printf("PostgreSQL error# %ld ", irc);
			printf(" occurred attempting to open database\n");
			exit(3);
		}
	}

	/*
	 * determine radar id
	 *
	 * if the first 3 char of the filename = "DHR", then
	 * next 3 char contain radar id
	 * (get_radid_from_filename)
	 * else open file and look for "DHR" in header
	 * next 3 char contain radar id
	 * (get_radid_from_product)
	 */

	type = decode_dhr_dsp_get_radid_from_filename(argv[1], radid);

	if(type == unknown)
	{
		type = decode_dhr_dsp_get_radid_from_product(argv[1], radid);
	}

	if(type == unknown)
	{
		printf("INVALID filename: %s\nProgram exit.\n", argv[1]);
		exit(-1);
	}

	/*
	 * check radid against entries in RadarLoc table
	 * if the radar id is not in the table OR the use_radar field='F'
	 * then radar will not be decoded
	 */

	if ( write_to_db == 1 )
	{
		status = decode_dhr_dsp_check_radid(radid);
		if(status > 0)
		{
			if(status == 1)
				printf("%s radar identifier not in RadarLoc table"
						" -- product not decoded\n", radid);
			else
				printf("%s appears in RadarLoc table with use_flag=F "
						"-- product not decoded\n", radid);

			/*
			 * close database
			 */

			closedb(&irc);
			if(irc != 0)
			{
				printf("PostgreSQL error# %ld ",irc);
				printf(" occurred attempting to close database \n");
			}

			exit(4);
		}
	}

	/*
	 * if running on Linux, then convert raw product to Little Endian format
	 * new file is created with same filename as original file but with
	 * ".LE" appended to it
	 */

	strcpy(raw_filename, argv[1]);
	os = GetOS();

	/*
	 * decode radar products
	 *
	 * if running on Linux, decode the Little Endian format product
	 * if running on HP, decode the Big Endian format product
	 */

	if ( type == dhr )
	{
		decodeDHR(radid, raw_filename, write_to_db, os);
	}
	else if(type == dsp)
	{
		decodeDSP(radid, raw_filename, write_to_db, os);
	}

	/*
	 * close database
	 */

	if ( write_to_db == 1 )
	{
		closedb(&irc);
		if(irc != 0)
		{
			printf("PostgreSQL error# %ld ", irc);
			printf(" occurred attempting to close database \n");
		}
	}

	return 0;

}
