/*******************************************************************************
* FILENAME:            readRDMosaicRadars.c
*
* Purpose:
* reads the radar precip accumulation array
* if radar has a dual-pol product use it else use single-pol product
*
* calling function: run_rdmosaic
* functions called: haveUsefulDAARadar, haveUsefulDPARadar       
*
* input variables
*
* datetime - date and time of current run in timestamp format
*          - minutes and seconds set to 00
*
* dpa_wind -  parameters
* daa_wind -  parameters
*
* output variables
*
* radar - two-dimensional array of radar data
*
* radarAvailFlag - DAA/DPA radar availability flag
*                - for radar fields in the RDMOSAIC field
*                - used to define the rad_avail field in the DAARadarResult table
*                - used to define the iflarad_dp[n] values used by the GUI
*                - note that this flag can pertain to a DPA or a DAA product
*     = 0 -- no radar data available OR radar data product available but field set to missing
*     = 1 -- radar data available (some values > 0.0)
*     = 2 -- radar data available (all values = 0.0)
*
* daa_avail_flag - DAA radar availability flag
*                - used for DAA MFB generation
*                - note that this flag is different from the radarAvailFlag (above)
*     = 0 -- DAA radar product not available
*     = 1 -- DAA radar product available
*
* ignoreDAARadar - Ignore DAA Radar Product Flag
*                - the DAA Radar product can be set to "ignored" via the 4-panel "single-site" window 
*                = 0 -- ignore flag not set (use radar) (default)
*                = 1 -- ignore flag set via 4-panel window 
*
* ignoreDPARadar - Ignore DPA Radar Product Flag
*                - the DPA Radar product can be set to "ignored" via the 4-panel "single-site" window	
*                = 0 -- ignore flag not set (use radar) (default)
*                = 1 -- ignore flag set via 4-panel window
*
* useDAA - Use DAA radar product flag
*        - combines ignoreDAARadar flag with check if useful DAA product is available
*        = 0 -- DAA product not available or ignored
*        = 1 -- DAA product available 
*
* useDPA - Use DPA radar product flag
*        - combines ignoreDPARadar flag with check if useful DPA product is available
*        = 0 -- DPA product not available or ignored
*        = 1 -- DPA product available
*
* haveGoodRadarProduct - DAA/DPA good radar product flag
*                      = 0 -- good radar product is NOT available (set field to all missing)
*                      = 1 -- good radar product available (could be all 0.0 product)
**
********************************************************************************
*/

#include "mpe_fieldgen.h"
#define ACC_MIN 0.01

void readRDMosaicRadars(const char * radarID,
                const char * datetime,
                const int    dpa_wind, 
                const int    daa_wind, 
                const int    minCoverageDur, 
                const int    ignoreDPARadar,
		const int    ignoreDAARadar,
                float radar [ ] [ NUM_DPA_COLS ] ,
                int *    radarAvailFlag,
                short *    daa_avail_flag)
{
    char    fname[FNAME_LEN] = "" ;
    int     i, j ;
    int     itim, coverageDur, nullProductFlag;
    int len, ierr ;
    double  bias, xmax ;
    long int irc = 0 ;
    int useDAA = 0;
    int useDPA = 0;
    
    /**
     * the dpa product is coded as a value between 0 and 255.   0 and 255
     * are special values. when converting from level (0-255) to dba, a 
     * level of "0" is set to -98 dba.  -98 dba = 1.58489e-10 mm of 
     * accumulation.   this small positive value must be set to zero.
     * the acc_min theshold is used to check for values less than the 
     * minimum detectible value so that they can be set to zero.
     **/

    *radarAvailFlag = 0 ;
    *daa_avail_flag = 0;
      
   if(ignoreDAARadar == 1)
   {
      sprintf ( message , "STATUS: DAA radar product ignored") ;
      printMessage(message, logFile);
   }

   if(ignoreDPARadar == 1)
   {
      sprintf ( message , "STATUS: DPA radar product ignored") ;
      printMessage(message, logFile);
   }

   useDAA = (! ignoreDAARadar && haveUsefulDAARadar(radarID, datetime, daa_wind, minCoverageDur, radar,
                                                      radarAvailFlag) );
   
   if (useDAA)
   {
       *daa_avail_flag = 1;
   }
   
   else //try DPA
   {
       useDPA = (! ignoreDPARadar && haveUsefulDPARadar(radarID, datetime, dpa_wind, radar, radarAvailFlag));
        
       if ( ! useDPA )
       {
           
          /*---------------------------*/
          /* set radar to all missing  */
          /*---------------------------*/
       
          for( i = 0; i < NUM_DPA_COLS; i++)
          {
              for( j = 0; j < NUM_DPA_COLS; j++)
	      {
                  radar[i][j] = RADAR_DEFAULT ;
              }
          }
	  
	  sprintf ( message , "STATUS: radar marked as missing .") ;
          printMessage(message, logFile);
        
       }

   }
   
   return;
   
}
   
//------------------------------------------------------------------------------------------
   
int haveUsefulDAARadar(const char * radarID, const char * datetime, const int daa_wind,
                const int minCoverageDur, float radar [ ] [ NUM_DPA_COLS ] , 
                int * radarAvailFlag)


{
    char    fname[FNAME_LEN] = "" ;
    int     i, j ; 
    int     itim, coverageDur, nullProductFlag;
    int len, ierr ;
    int haveGoodRadarProduct = 0;
    double  bias, xmax ;
    long int irc = 0 ; 

    readDAARadar(radarID, datetime, daa_wind, &xmax, &bias, fname, &itim,
                 &coverageDur, &nullProductFlag, &irc) ;
		 
    if(irc == 0) //DAA is available
    {
        
        sprintf ( message , "DAA product found for radar = %s\n", radarID) ;
        printMessage(message, logFile);
	
        *radarAvailFlag = 1 ;
	
        if(itim != 0)
        {
            sprintf ( message , "STATUS: no top-of-hour product found -- %d, "
                "product used instead.", itim ) ;
            printMessage(message, logFile);
        }
        
        len = strlen(fname) ;
        ierr = 0 ;
  

       /*---------------------------------------*/
       /* check if product is a null product    */
       /*---------------------------------------*/
       if(nullProductFlag == 0) //not a null product
       {
          sprintf ( message , "Maximum radar value = %7.2f mm", xmax ) ;
          printMessage(message, logFile);

          sprintf ( message , "reading DAA decoded product with filename = %s\n", fname);
          printMessage(message, logFile);

          read_daa_decoded(fname, &len, radar, &ierr) ;
          if(ierr != 0)
          {
             /*-------------------------------------------------------*/
             /* error reading DAA product - missing data substituted  */
             /*-------------------------------------------------------*/
              sprintf ( message , "ERROR: #%d encountered reading radar file = %s"
                          " -- missing data substituted.", ierr, fname ) ;
              printMessage(message, logFile);
              *radarAvailFlag = 0 ;

              for( i = 0; i < NUM_DPA_COLS; i++)
              {
                  for( j = 0; j < NUM_DPA_COLS; j++)
                      radar[i][j] = RADAR_DEFAULT ;
              }
	      
	      haveGoodRadarProduct = 0;
	      
          }
	  else //good product
	  {
	       haveGoodRadarProduct = 1;
               *radarAvailFlag = 1 ;
	  }
       }
       else  //this is a null product
       {

          //consider it enough info to say it is 0.0
           if(nullProductFlag == 5 && coverageDur >= minCoverageDur)  
              /*-----------------------------------------------------------------------------------------*/
              /* if null_product_flag = 5 and coverageDur >= min_coverage_dur then set field to all 0.0  */
              /*-----------------------------------------------------------------------------------------*/
           {
              haveGoodRadarProduct = 1; 
              *radarAvailFlag = 2 ;
              sprintf ( message , "null product flag = 5 and coverage > min coverage --  all 0.0 field substituted.");
              printMessage(message, logFile);

              for( i = 0; i < NUM_DPA_COLS; i++)
              {
                  for( j = 0; j < NUM_DPA_COLS; j++)
                      radar[i][j] = 0.0 ;
              }
           }
           else //consider it missing
           {

              haveGoodRadarProduct = 0; 
              *radarAvailFlag = 0 ;
              sprintf ( message , "null product with coverage < min coverage or unspecified --  all missing field substituted.");
              printMessage(message, logFile);

              for( i = 0; i < NUM_DPA_COLS; i++)
              {
                  for( j = 0; j < NUM_DPA_COLS; j++)
                      radar[i][j] = RADAR_DEFAULT ;
              }
           }
       } //end if null product        
  	
    }
    else //error reading DAA product
    {    
         haveGoodRadarProduct = 0; 
         *radarAvailFlag = 0 ;
    }
   
    return haveGoodRadarProduct;

}
   
//------------------------------------------------------------------------------------------
int haveUsefulDPARadar(const char * radarID, const char * datetime, const int dpa_wind,
                float radar [ ] [ NUM_DPA_COLS ] , int *    radarAvailFlag)
   
{
    char    fname[FNAME_LEN] = "" ;
    int     i, j ; 
    int     itim;
    int len, ierr ;
    int haveGoodRadarProduct = 0;
    double  bias, xmax ;
    long int irc = 0 ; 

     irc = 0;
     readDPARadar(radarID, datetime, dpa_wind, &xmax, &bias, fname, &itim, &irc);
     
     if(irc == 0) //should be a radar file available
     {
          sprintf ( message , "DPA product found for radar = %s\n", radarID) ;
          printMessage(message, logFile);
	  
     }
     
     if((irc != 0) || (xmax == -99.0)) // dpa radar
     {

        sprintf ( message , " no radar data for current hour"
                            " -- missing data substituted\n");
        printMessage(message, logFile);


       haveGoodRadarProduct = 0;

     }
     else //read DPA product
     {
           /*-----------------------------------------*/
           /* xmax > 0.0 -- read decoded DPA product  */
           /*-----------------------------------------*/

           if(xmax > 0.0)
           {

              len = strlen(fname) ;
              ierr = 0 ;

              sprintf ( message , "Maximum radar value = %7.2f mm", xmax ) ;
              printMessage(message, logFile);

              read_stage1_decoded_(fname, &len, radar, &ierr) ;

              if(ierr != 0)
              {
	          haveGoodRadarProduct  = 0;
                  
		  sprintf ( message , "ERROR: #%d encountered reading radar file = %s"
                           " -- missing data substituted.", ierr, fname ) ;
                  printMessage(message, logFile);
                  *radarAvailFlag = 0 ;

                  for( i = 0; i < NUM_DPA_COLS; i++)
                  {
                      for( j = 0; j < NUM_DPA_COLS; j++)
                          radar[i][j] = RADAR_DEFAULT ;
                  }
		  
              }
              else  //product is good, but just check for below minimum and correct to 0.0
              {
	          haveGoodRadarProduct = 1;
		
                  for( i = 0; i < NUM_DPA_COLS; i++)
                  {
                      for( j = 0; j < NUM_DPA_COLS; j++)
                      {
                          if(radar[i][j] < ACC_MIN)
                              radar[i][j] = 0.0 ;
                      }
                  }
		  
		
              }
           }
	   
	   else //zero product
	   {
	      haveGoodRadarProduct = 1;
	      
              *radarAvailFlag = 2 ;

              sprintf ( message , "STATUS: radar data all zero for current hour.") ;
              printMessage(message, logFile);

              if(itim != 0)
              {
                  sprintf ( message , "STATUS: no top-of-hour product found -- %d, "
                      "product used instead.", itim ) ;
                  printMessage(message, logFile);
              }

              for( i = 0; i < NUM_DPA_COLS; i++)
              {
                  for( j = 0; j < NUM_DPA_COLS; j++)
                      radar[i][j] = 0.0 ;
              }

           }
     }
     
    return haveGoodRadarProduct;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/readRDMosaicRadars.c,v $";
 static char rcs_id2[] = "$Id: readRDMosaicRadars.c,v 1.4 2012/09/10 19:38:47 pst Exp $";}
/*  ===================================================  */

}
