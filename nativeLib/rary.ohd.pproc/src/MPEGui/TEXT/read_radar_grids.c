/*=========================================================================*/
/*                         FILE NAME:  read_radar_grids.c                  */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:  read_radar_grids                    */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "drawa.h"
#include "mpe_log_utils.h"
#include "read_dpa_fname.h"
#include "read_radar_grids.h"
#include "read_stage1_decoded.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/***************************************************************************/
/*  FUNCTION NAME:   read_radar_grids                                      */
/*  FUNCTION:        read gridded radar data                               */
/***************************************************************************

Function type:
   void

Called by function:
 newhour_RFCW                
 initialize_data_RFCW
 rerun_rfcwgen

Functions called:
   read_dpa_fname
   read_stage1_decoded

Local variables:
   iflign = ignore radar flag 

   iflarad = radar availability flag array read from RWRadarResult table
         = 0 -- field available (some values > 0.0)
         = 1 -- field not available
         = 2 -- field available (all values = 0.0)

   datafile[n][0] = NOT USED                                     

   datafile[n][1] = 0/1 flag for gage availabilty (num_gages field in rwradarresult record)
                  = 0 -- >0 gages available
                  = 1 -- no gages available

   datafile[n][2] = 0/1 flag for radar data availabilty
                  = 0 -- radar data available
                  = 1 --   "     "  not available
    where n = radar number in list

   datafile[n][2] and datafile[n][1] are also used to determine the color of the radar ring

   stage1i array contains raw radar field
   stage1u array contains unbiased radar field (= raw radar * bias)
   both arrays are read in as mm and multiplied by 100 to make them look like 
   RMOSAIC

************************************* BEGIN read_radar_grids ***********/

void read_radar_grids ( int ss_number )
{
   int          i ; 
   int          j ;
   int          ierr ;
   long         ierr_long;
   int          length ;
   char         st1fn[17],dtprod[22];
   float        rdata[NUM_DPA_COLS][NUM_DPA_COLS];

  logMessage("reading radar grids for radar %s, number %d.\n" ,
           nexrad [ ss_number ].id , ss_number );

   if ( iflarad [ ss_number ] == 0 )
   {

     /*----------------------------------------------------------------------*/
     /*   read filename containing gridded radar data from DPARadar table    */
     /*----------------------------------------------------------------------*/

     strcpy ( dtprod , datetime_radar_prod [ ss_number ] ) ;

     read_dpa_fname ( nexrad [ ss_number ].id , dtprod , st1fn , 
                      & ierr_long ) ;

     /*----------------------------------------------*/
     /*   if read of filename successful, then       */
     /*   read gridded radar data from flat file     */
     /*----------------------------------------------*/

     ierr = ( int ) ierr_long ;

     if ( ierr == 0 )
     {
        strcpy ( st1fname , st1fn ) ;
        length = strlen ( st1fname ) ;
        read_stage1_decoded_ ( st1fname , & length , rdata , & ierr ) ;
     }
    
     flogMessage ( stdout , "The bias used for radar %d is %5.7f\n" ,
                         ss_number , siibiasu [ ss_number ] ) ;

     if ( ierr == 0 )
     {
        datafile [ ss_number ] [ 2 ] = 0;
        
        for ( i = 0 ; i < NUM_DPA_COLS ; i++ )
        {
          for ( j = 0 ; j < NUM_DPA_COLS ; j++ )
          {
            stage1i[ ss_number ] [ i ] [ j ] = rdata [ i ] [ j ] * 100 ;
            stage1u[ ss_number ] [ i ] [ j ] = rdata [ i ] [ j ] * 
                                         siibiasu [ ss_number ] * 100 ;
          }
        }
      }
      else
      {
        datafile [ ss_number ] [ 2 ] = 1 ;

        for ( i = 0 ; i < NUM_DPA_COLS ; i++ )
        {
          for ( j = 0 ; j < NUM_DPA_COLS ; j++ )
          {
            stage1i [ ss_number ] [ i ] [ j ] = -1 ;
            stage1u [ ss_number ] [ i ] [ j ] = -1 ;
          }
        }
      }
    }
    else if ( iflarad [ ss_number ] == 2 )
    {
      datafile [ ss_number ] [ 2 ] = 0 ;

      for (i=0;i<NUM_DPA_COLS;i++)
      {
        for (j=0;j<NUM_DPA_COLS;j++)
        {
         stage1i [ ss_number ] [ i ] [ j ] = 0 ;
         stage1u [ ss_number ] [ i ] [ j ] = 0 ;
        }
      }
    }
    else
    {
      datafile [ ss_number ] [ 2 ] = 1 ;

      for ( i = 0 ; i < NUM_DPA_COLS ; i++ )
      {
        for ( j = 0 ; j < NUM_DPA_COLS ; j++ )
        {
          stage1i [ ss_number ] [ i ] [ j ] = -1 ;
          stage1u [ ss_number ] [ i ] [ j ] = -1 ;
        }
      }
    }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

/****************************************** END read_radar_grids ***********/

