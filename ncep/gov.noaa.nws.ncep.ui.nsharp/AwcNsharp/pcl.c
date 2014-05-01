/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  Epson 8-pin Dot-Matrix Printer Output.  These routines     */
/*  assume the DOS Video Mode is being used.                   */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*                                                             */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  PRINT_SOUNDING_PCL                                         */
/*  PCOPEN_PRINTER                                             */
/*  PCPRINTER_SETUP                                            */
/*  TO_PCLRASTER                                               */
/*  PCCLOSE_PRINTER                                            */
/*                                                             */
/***************************************************************/
	#include <stdio.h>
	#include <stdlib.h>
	#include <sharp95.h>
	#include <math.h>
	#include <string.h>

	/*NP*/
	void print_sounding_pcl( void )
	/*************************************************************/
	/*  PRINT_SOUNDING_PCL                                       */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Prints skewt/hodograph to LPT1.                          */
	/*************************************************************/
	{
	short row, col, maxrow, maxcol;
	FILE *fp;
	char st[80];

	/* ----- Define area of screen to print ----- */
	maxrow = 408;
	maxcol = 408;

	/* ----- Open Printer ----- */
	fp = open_printer();
	if(fp==NULL)
	   {
	   printf("\a");
	   return;
	   }

	/* ----- Set Printer to PCL mode ----- */
	pcprinter_setup(fp);

	/* ----- Draw image, one line at a time ----- */
	for(row=1; row<=maxrow; row++)
	   { to_pclraster( row, maxcol, fp ); }

	pcclose_printer( fp );
	}

	/*NP*/
	void pcprinter_setup( FILE *fp )
	/*************************************************************/
	/*  PCPRINTER_SETUP                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Sets PCL printer to raster graph mode.                   */
	/*                                                           */
	/*  fp         - File handle                                 */
	/*************************************************************/
	{

	/* ----- Reset Printer ----- */
	fputs( "\033E", fp );

	/* ----- Move lpt cursor to 100,100 and set to raster mode ----- */
	fputs( "\033*p100x100Y", fp);
	fputs( "\033*t75R", fp);
	fputs( "\033*r0F", fp);
	fputs( "\033*r1A", fp);
	}


	/*NP*/
	void to_pclraster( short row, short maxcol, FILE *fp )
	/*************************************************************/
	/*  TO_PCLRASTER                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts one line of screen pixels into PCL raster data  */
	/*  and sends it to the printer.  A test for EOF character   */
	/*  is included...as testing shows this to be a problem.     */
	/*                                                           */
	/*  fp         - File handle                                 */
	/*************************************************************/
	{
	short v, z, i, col, nb;

	nb=0; for(col=1; col<=maxcol; col += 8) { nb++; }

	/* ----- Send Header info for each line ----- */
	fprintf( fp, "\033*b%dW", nb);

	for(col=1; col<=maxcol; col += 8)
	   {
	   z = 0;
	   v = 0;
	   for(i=0; i<8; i++)
	      {
	      /*v = _getpixel(col+i, row);*/
	      if(v > 0) {z = z + (short)pow(2, 7-i); }
	      }

	   /* ----- Avoid EOF character being sent to printer ----- */
	   if(z==26) z=25;

	   fputc( z, fp);
	   }
	}



	/*NP*/
	void pcclose_printer( FILE *fp )
	/*************************************************************/
	/*  PCCLOSE_PRINTER                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Close printer stream.                                    */
	/*************************************************************/
	{
	/* ----- End Raster Graphics ----- */
	fputs( "\033*rC", fp);

	fclose(fp);
	}

