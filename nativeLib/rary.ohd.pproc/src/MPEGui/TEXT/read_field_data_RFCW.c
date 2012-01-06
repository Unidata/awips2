/*=========================================================================*/
/*                         FILE NAME:   read_field_data_RFCW.c             */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   read_field_data                    */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "display_field_data_RFCW.h"
#include "drawa.h"
#include "mpe_log_utils.h"
#include "ReadSPE.h"
#include "read_xmrg.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "Swap2Bytes.h"
#include "TestByteOrder.h"
#include "TestXmrg.h"

/***************************************************************************/
/*  FUNCTION NAME:   display_field_data_RFCW()                             */
/*       FUNCTION:   display field data                                    */
/***************************************************************************

Function type:
   void

Called by function:

********************************* BEGIN display_field_data_RFCW **************/

void display_field_data_RFCW ( enum DisplayFieldData display_data ,
			       int ** data_array_tmp , date_struct date , 
			       int addition_flag )
{
 char                 dirname [ 100 ] ;
 char                 fname [ 128 ] ;
 int                  ida ;
 int                  idate ;
 int                  ihr ;
 int                  im ;
 int                  imo ;
 int                  is ;
 int                  iyr ;
 
 int                  len ;
 int                  len_fname ;
 int                  status ;
 int                  tdiff ;
 int                  tunit ;

 switch ( display_data ) {

     case display_rMosaic :
         len = strlen("rfcwide_rmosaic_dir");
	 get_apps_defaults("rfcwide_rmosaic_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"RMOSAIC");	 
	 break;

     case display_avgrMosaic :
         len = strlen("rfcwide_avg_rmosaic_dir");
	 get_apps_defaults("rfcwide_avg_rmosaic_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"AVGRMOSAIC");	 
	 break;

     case display_maxrMosaic :
         len = strlen("rfcwide_max_rmosaic_dir");
	 get_apps_defaults("rfcwide_max_rmosaic_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"MAXRMOSAIC");	 
	 break;
	 
     case display_bMosaic:
         len = strlen("rfcwide_bmosaic_dir");
	 get_apps_defaults("rfcwide_bmosaic_dir",&len,dirname,&len);
	 strcpy(cv_use_tmp,"BMOSAIC");
	 break;
	 
     case display_mMosaic:
         len = strlen("rfcwide_mmosaic_dir");
	 get_apps_defaults("rfcwide_mmosaic_dir",&len,dirname,&len);
	 strcpy(cv_use_tmp,"MMOSAIC");
	 break;

     case display_rfcMosaic:
         len = strlen ( "gaq_xmrg_1hr_dir" );
         get_apps_defaults("gaq_xmrg_1hr_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"RFCMOSAIC");
         break;

     case display_mlMosaic:
         len = strlen("rfcwide_mlmosaic_dir");
         get_apps_defaults("rfcwide_mlmosaic_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"MLMOSAIC");
         break;
	 
     case display_lMosaic:
         len = strlen("rfcwide_lmosaic_dir");
	 get_apps_defaults("rfcwide_lmosaic_dir",&len,dirname,&len);
	 strcpy(cv_use_tmp,"LMOSAIC");
	 break; 
	      
     case display_Xmrg:
         len = strlen("rfcwide_xmrg_dir");
	 get_apps_defaults("rfcwide_xmrg_dir",&len,dirname,&len);
	 strcpy(cv_use_tmp,"xmrg");
	 break;
	 
     case display_satPrecip:
         len = strlen("rfcwide_satpre_dir");
	 get_apps_defaults("rfcwide_satpre_dir",&len,dirname,&len);
	 strcpy(cv_use_tmp,"SATPRE") ;
	 break;	

     case display_lsatPrecip:
         len = strlen("rfcwide_lsatpre_dir");
         get_apps_defaults("rfcwide_lsatpre_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"LSATPRE");
         break;

     case display_srMosaic:
         len = strlen("mpe_srmosaic_dir");
         get_apps_defaults("mpe_srmosaic_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"SRMOSAIC");
         break;

     case display_sgMosaic:
         len = strlen("mpe_sgmosaic_dir");
         get_apps_defaults("mpe_sgmosaic_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"SGMOSAIC");
         break;

     case display_srgMosaic:
         len = strlen("mpe_srgmosaic_dir");
         get_apps_defaults("mpe_srgmosaic_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"SRGMOSAIC");
         break;
	  
     case display_gageOnly:
         len = strlen("rfcwide_gageonly_dir");
	 get_apps_defaults("rfcwide_gageonly_dir",&len,dirname,&len);
	 strcpy(cv_use_tmp,"GAGEONLY");
	 break;	 

     case display_p3Mosaic :
         len = strlen("rfcwide_p3lmosaic_dir");
	 get_apps_defaults("rfcwide_p3lmosaic_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"P3LMOSAIC");	 
	 break;

     case display_rfcbMosaic :
         len = strlen("mpe_rfcbmosaic_dir");
	 get_apps_defaults("mpe_rfcbmosaic_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"RFCBMOSAIC");	 
	 break;

     case display_rfcmMosaic :
         len = strlen("mpe_rfcmmosaic_dir");
	 get_apps_defaults("mpe_rfcmmosaic_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"RFCMMOSAIC");	 
	 break;
	 
     case display_subValue:
         len = strlen("rfcwide_gageonly_dir");
	 get_apps_defaults("rfcwide_gageonly_dir",&len,dirname,&len);
	 strcpy(cv_use_tmp,"SUBVALUE");
	 return;
	 break;	
	  	
     case display_missing:
	 return;	 
	 	 	 	 
     default:
        logMessage("ERROR: invalid selection");
         return;
 }	 

 status = strcmp ( cv_use_tmp , "xmrg" ) ;

 if ( status == 0 ) 
 {
     idate = date.month*1000000 + date.day*10000 + date.year;
     sprintf(fname,"%s/%s%08d%02dz",dirname,cv_use_tmp,idate,date.hour); 
 }
 else if ( display_data == display_satPrecip )
 {
     iyr = date.year ;
     imo = date.month ;
     ida = date.day ;
     ihr = date.hour ;
     im = 0 ;
     is = 0 ; 
     tdiff = -1 ;
     tunit = 2 ;
     TADJ ( & iyr , & imo , & ida , & ihr , & im , & is , & tdiff , & tunit ) ;
     sprintf ( fname , "%s/%4d%02d%02d_%02d00.multi" , dirname , iyr , imo ,
           ida , ihr ) ;
    
 }
 else if ( display_data == display_rfcMosaic )
 {
    sprintf(fname,"%s/%s01%sz",dirname,cv_use_tmp,date.cdate);
 }
 else
 {
     sprintf(fname,"%s/%s%sz",dirname,cv_use_tmp,date.cdate);
 }
     
 if ( display_data != display_satPrecip )
 {
    len_fname = strlen ( fname ) ;
    display_field_read_xmrg ( data_array_tmp , fname, addition_flag );
 }
 else
 {
    /* Special logic to process the satellite image. */
    display_field_read_spe ( data_array_tmp , fname, addition_flag ) ;
 }

}
 
void display_field_read_xmrg (int ** data_array_tmp , char * fname,  
				              int addition_flag )
{
 int     		len_fname;
 int            	i, j, ifile;
 short        	      **temp = NULL ;
 enum TestByteResult  	result ;
 
 len_fname = strlen(fname);
 temp = ( short ** ) malloc ( MAXY * sizeof ( short *) ) ;

 if ( temp == NULL )
 {
    flogMessage ( stderr , "In routine \"display_field_data_RFCW\":\n"
                       "Cannot allocate memory for the \"temp\"\n"
                       "variable.  Aborting the display of the field\n"
                       "data.\n" ) ;
    return ;
 }

 for ( i = 0 ; i < MAXY ; ++ i )
 {
    temp [ i ] = ( short * ) malloc ( MAXX * sizeof ( short ) ) ;

    if ( temp [ i ] == NULL )
    {
       flogMessage ( stderr , "In routine \"display_field_data_RFCW\":\n"
                          "Cannot allocate memory for element %d of\n"
                          "the \"temp\" array.  Aborting the display\n"
                          "of the field data.\n" , i ) ;

       for ( j = 0 ; j < i ; ++ j )
       {
          free ( temp [ j ] ) ;
       }

       free ( temp ) ;
       temp = NULL ;
       return ;
    }
    
 }

 /* Test whether or not the bytes need to be "swapped" in the
    file to be read in to match the memory architecture of the
    operating system that this program is running on. */
 TestXmrgByteOrder_ ( fname , & XOR , & result ) ;

 if ( result == FlipTestFailed )
 {
    flogMessage ( stderr , "In routine \"display_field\":\n"
                       "The call to \"TestXmrgByteOrder_\" failed.\n"
                       "Cannot read file \"%s\".\n" , fname ) ;
     
    if ( addition_flag == 0 )
    { 
       for (i=0;i<MAXX;i++)
       {
          for (j=0;j<MAXY;j++)
          {
        
             data_array_tmp[i][j] = -999;
	  }
       }
    }
 
    /* Free the memory used by the "temp" array. */
    for ( i = 0 ; i < MAXY ; ++ i )
    {
       free ( temp [ i ] ) ;
    }

    free ( temp ) ;
    temp = NULL ;

    return ;
 }

 for ( i = 0 ; i < MAXY ; ++ i  )
 {
   read_xmrg ( & MAXX , & MAXY , & i , fname , & len_fname , & ifile , 
               temp [ i ] ) ;

   if (ifile != 0)
   {
     logMessage("error reading %s -- missing data substituted....\n",fname);
      break;
   }

   if ( result == FlipBytes )
   {
      Swap2Bytes_ ( temp [ i ] , ( size_t * ) & MAXX ) ;
   }
 } 

 if (ifile == 0)
 {
   for (i=0;i<MAXX;i++)
   for (j=0;j<MAXY;j++)
   {
     if ( addition_flag == 1 )
     {
        if (temp [j][i] >= 0 )
	{ 
           data_array_tmp[i][j] += temp[j][i];
	}
     }
     else 
     {
        data_array_tmp[i][j] = temp[j][i];
     }
   }
     
 }
 else
 {
   for (i=0;i<MAXX;i++)
     for (j=0;j<MAXY;j++)
     {
        if ( addition_flag == 0 ) 
	{
           data_array_tmp[i][j] = -999;
	}
     }
       
 }

 /* Free the memory used by the "temp" array. */
 for ( i = 0 ; i < MAXY ; ++ i )
 {
    free ( temp [ i ] ) ;
 }

 free ( temp ) ;
 temp = NULL ;
 return ;

}

void display_field_read_spe ( int ** data_array_tmp , char * fname,  
			                  int addition_flag )
{
 float                  mm_factor ;
 int            	i , j , ifile ;
 int                    spe_index ;
 short int              * site_spe = NULL ;
 short int              u_thres ;
 short int              x_factor ;
 
 site_spe = ( short int * ) malloc ( sizeof ( short int ) * MAXX * MAXY ) ;

 if ( site_spe == NULL )
 {
    flogMessage ( stderr , "\nIn routine 'display_field_read_spe':\n"
                       "Could not allocate memory for the site_spe\n"
                       "array.\n" ) ;
    return ;
 }

 ReadSPE ( fname , & XOR , & YOR , & MAXX , & MAXY , site_spe , 
           & x_factor , & u_thres , & mm_factor ,  & ifile ) ;
 
 if ( ifile == 0 )
 {
   for ( i = 0 ; i < MAXY ; i++ )
   {
      spe_index = MAXX * i ;
   
      for ( j = 0 ; j < MAXX ; j++ )
      {
         if ( addition_flag == 1 )
         {
            if ( site_spe [ j + spe_index ] >= 0 )
	    {    
               data_array_tmp[j][i] += site_spe [ j + spe_index ] ;
   	    }
         }
         else 
         {
            data_array_tmp[j][i] = site_spe [ j + spe_index ] ;
         }
      }
   }
     
 }
 else
 {
   for ( i = 0 ; i < MAXX ; i++ )
   {
     for ( j = 0 ; j < MAXY ; j++ )
     {
        if ( addition_flag == 0 )
	{
           data_array_tmp [ i ] [ j ] = -999;
	}
     }
   }
 }

 if ( site_spe != NULL )
 {
    free ( site_spe ) ;
    site_spe = NULL ;
 }

 return ;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/read_field_data_RFCW.c,v $";
 static char rcs_id2[] = "$Id: read_field_data_RFCW.c,v 1.16 2007/05/24 13:07:24 whfs Exp $";}
/*  ===================================================  */

}
