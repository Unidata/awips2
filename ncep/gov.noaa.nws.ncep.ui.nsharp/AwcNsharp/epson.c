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
/*  PRINT_SOUNDING_EPSON                                       */
/*  EPOPEN_PRINTER                                             */
/*  EPPRINTER_SETUP                                            */
/*  TO_RASTER                                                  */
/*  EPCLOSE_PRINTER                                            */
/*                                                             */
/***************************************************************/
	#include <stdio.h>
	#include <stdlib.h>
	#include <sharp95.h>
	#include <math.h>
	#include <string.h>


	/*NP*/
	void print_sounding_epson( void )
	/*************************************************************/
	/*  PRINT_SOUNDING_EPSON                                     */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Prints skewt/hodograph to LPT1.                          */
	/*************************************************************/
	{
	short row, col, maxrow, maxcol, n1, n2;
	FILE *fp;
	char st[80];

	/* ----- Define area of screen to print ----- */
	maxrow = 408;
	maxcol = 418;

	n2 = maxcol/256;
	n1 = maxcol%256;

	/* ----- Open Printer ----- */
	fp = open_printer();
	if(fp==NULL)
	   {
	   printf("\a");
	   return;
	   }

	/* ----- Set Printer to EPSON mode ----- */
	epprinter_setup(fp);

	for(row=1; row<=maxrow; row += 8)
	   {

	   /* ----- Set 8-pin standard density graphics ----- */
	   fputc(27, fp);
	   fputc(75, fp);
	   fputc(n1, fp);
	   fputc(n2, fp);

	   /* ----- Draw image, one short vertical line at a time ----- */
	   for(col=1; col<=maxcol; col++)
	      { to_raster( row, col, fp ); }

	   /* ----- Send a LF and begin a new line ----- */
	   fputc(10, fp);
	   }

	epclose_printer( fp );
	}


	/*NP*/
	void epprinter_setup( FILE *fp )
	/*************************************************************/
	/*  EPPRINTER_SETUP                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Sets Epson printer to raster graph mode.                 */
	/*                                                           */
	/*  fp         - File handle                                 */
	/*************************************************************/
	{
	/* ----- Set paper feed to 24/180 inch ----- */
	fputc(27, fp);
	fputc(51, fp);
	fputc(24, fp);
	}


	/*NP*/
	void to_raster( short row, short col, FILE *fp )
	/*************************************************************/
	/*  TO_RASTER                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts 8 vertical pixels (starting at col,row) into    */
	/*  a byte of data and sends it to the printer.  A test for  */
	/*  EOF character is included...as testing shows this to be  */
	/*  a problem.                                               */
	/*                                                           */
	/*  fp         - File handle                                 */
	/*************************************************************/
	{
	short v, z, i;

	v = 0;
	z = 0;
	for(i=0; i<8; i++)
	   {
	   /*v = _getpixel(col, row+i);*/
	   if(v > 0) {z = z + (short)pow(2, 7-i); }
	   }

	/* ----- Avoid EOF character being sent to printer ----- */
	if(z==26) z=25;

	fputc( z, fp);
	}



	/*NP*/
	void epclose_printer( FILE *fp )
	/*************************************************************/
	/*  EPCLOSE_PRINTER                                          */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Close printer stream.                                    */
	/*************************************************************/
	{
	fclose(fp);
	}

