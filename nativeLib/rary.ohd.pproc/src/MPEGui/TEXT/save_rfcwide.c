/*================================================================*/
/*                         FILE NAME:   save_rfcwide.c            */
/*                                                                */
/*  FUNCTIONS CONTAINED IN THIS FILE:   save_rfcwide              */
/*                                      save_merged_RFCW          */
/*================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <X11/cursorfont.h>
#include <sys/utsname.h>


#include "drawa.h"
#include "map_library.h"
#include "mpegui_save_gif.h"
#include "mpegui_save_jpeg.h"
#include "mpe_log_utils.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "save_grib.h"
#include "save_netcdf.h"
#include "save_rfcwide.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "update_rwr_save.h"
#include "write_xmrg.h"

static char * version_number = "OB7.2";

/******************************************************************/
/*   FUNCTION NAME:  save_rfcwide()                               */
/*       FUNCTION:   calls functions to save gridded precip       */
/*                     in various formats                         */
/*******************************************************************

Function type:
   void

Called by function:
   callback from Save Data option

Functions called:
   get_apps_defaults
   update_rwr_save 
   save_merged_RFCW
   mpegui_save_gif
   mpegui_save_jpeg
   save_grib
   save_netcdf
*******************************************************************/

void save_rfcwide ( Widget w , XtPointer clientdata , XtPointer calldata )

{
   static int first = 1;
   static Boolean transmit_rfc_bias = False;
   static Boolean transmit_bias_on_save = False;
   static char precip_proc_bin_dir [ 120 ] = {'\0'};
   int len , lenfn , lenfnx , lenfng , idate ;
   int map_number;
   int status;
   int int_irc = 0;
   long irc = 0  ;
   char command_string [ 120 ];
   extern char RFC [ ] ;
   char save_flag [ 7 ] , cdate [ 11 ] ;
   char dirname [ 128 ] , filename [ 128 ] ;
   static char send_to_sbn [ 128 ] = {'\0'};
   static char qpe_sbn_dir [ 128 ] = {'\0'};
   static char transmit_on_save [ 128 ];
   static char transmit_bias [ 128 ];
   char fnamgif[35],fnamnet[35],fnamgrib[35],fnamjpeg[35];
   char ftype[20], rfc[9],runtype[2];
   char * pGribCommand = NULL;
   char proc_flag_local[9] = "MPM01   ";
   char proc_flag_sbn[9] = "QPE01   ";

   map_number = (int) clientdata;

   if ( first == 1 )
   {
      len = strlen ( "mpe_send_qpe_to_sbn" );
      get_apps_defaults ( "mpe_send_qpe_to_sbn", & len,
                          send_to_sbn, &len );
      len = strlen ( "mpe_qpe_sbn_dir" );
      get_apps_defaults ( "mpe_qpe_sbn_dir", &len,
                          qpe_sbn_dir, &len );
      len = strlen ( "mpe_transmit_bias" );
      get_apps_defaults ( "mpe_transmit_bias", &len,
                          transmit_bias, &len );

      status = strcasecmp ( transmit_bias, "ON" );

      if ( status == 0 )
      {
         transmit_rfc_bias = True;
      }

      len = strlen ( "transmit_bias_on_save" );
      get_apps_defaults ( "transmit_bias_on_save", &len,
                          transmit_on_save, &len );

      status = strcasecmp ( transmit_on_save, "YES" );
      
      if ( status == 0 )
      {
         transmit_bias_on_save = True;
      }

      len = strlen ( "pproc_bin" );
      get_apps_defaults ( "pproc_bin", &len,
                          precip_proc_bin_dir, &len ); 

      if ( ( len == 0 ) && 
           ( transmit_bias_on_save == True ) &&
           ( transmit_rfc_bias == True ) )
      {
         flogMessage ( stdout, "Could not find a value for token "
                           "'pproc_bin'.  Can't send rfc bias.\n" );
         transmit_bias_on_save = False;
         transmit_rfc_bias = False;
      }

      first = 0;
   }

   /*--------------------------------------------------------------*/
   /*   Display watch cursor and flush the display buffer.         */
   /*--------------------------------------------------------------*/
   mSetCursor ( M_WATCH ) ;
   XFlush(XtDisplay(rad_data[map_number].w));

   /*----------------------------------------------------------*/
   /*   create date in desired format for use in xmrg filename */
   /*----------------------------------------------------------*/
   strcpy ( cdate , date_st3.cdate ) ;

   if ( strcmp ( date_form , "mdY" ) == 0 )
   {
      idate = date_st3.month * 1000000 + date_st3.day * 10000 + date_st3.year ;
      sprintf ( cdate , "%08d%02d" , idate , date_st3.hour ) ;
   }

   /*----------------------------------------------------------*/
   /*   create full pathname of binary file (input to MAPX)    */
   /*   filename_xmrg is used by grib encoder                  */
   /*----------------------------------------------------------*/

   len = strlen("rfcwide_xmrg_dir");
   get_apps_defaults("rfcwide_xmrg_dir",&len,dirname,&len); 
   memset(filename,' ',128);
   sprintf(filename,"%s/xmrg%sz",dirname,cdate);
   sprintf(filename_xmrg,"xmrg%sz",cdate);

  /*----------------------------------------------------------*/
  /*   update RWResult table                                  */
  /*   write merged (mosaicked) field to file                 */
  /*   replace missing values with 0.0                        */
  /*----------------------------------------------------------*/

  strcpy(rfc,RFC);
  strcpy(runtype,"F");

  update_rwr_save(rfc, & date_st3, rad_data[map_number].cv_use, &irc);

  if ( irc != 0 )
  {
   logMessage("Informix error %ld attempting to update RwResult table\n",irc);
    return;
  }

  lenfn = strlen(filename);
  save_merged_RFCW(filename,lenfn,map_number,proc_flag_local,&irc);

  /* Is the mpe_send_qpe_to_sbn token set to ON? If so create a second
     QPE file with the proc_flag set to QPE01. */
  if ( send_to_sbn [ 1 ] == 'n' || send_to_sbn [ 1 ] == 'N' )
  {
     memset(filename,'\0',128);
     sprintf(filename,"%s/xmrg%sz",qpe_sbn_dir,cdate);
     lenfn = strlen ( filename );
     save_merged_RFCW(filename,lenfn,map_number,proc_flag_sbn,&irc);
  } 

  /*-----------------------------------------------------------*/
  /*   generate and save files depending on values of          */
  /*     mpe_save_... tokens read from .Apps_defaults          */
  /*                                                           */
  /*   create filenames for netCDF, gif, grib and jpeg files   */
  /*   if mpe_xxxx_id token not found or blank, then no string */
  /*     is prepended to filename                              */
  /*   in all cases, filenames contain date in form yyyymmddhh */
  /*-----------------------------------------------------------*/

  strcpy(cdate,date_st3.cdate);

       /*--------------------------------------*/
       /*  generate and save gif image         */
       /*--------------------------------------*/

  len = strlen("mpe_save_gif");
  memset(save_flag,'\0',7);
  get_apps_defaults("mpe_save_gif",&len,save_flag,&len);

  if(strcmp(save_flag,"save") == 0)
  {
    len = strlen("mpe_gif_id");
    get_apps_defaults("mpe_gif_id",&len,ftype,&len);
    if(len == 0)
       sprintf(fnamgif,"%sz",cdate);
    else
       sprintf(fnamgif,"%s%sz",ftype,cdate);

    mpegui_save_gif(fnamgif, map_number);
  }
  else
  {
   logMessage("gif file not saved \n");
  }

       /*--------------------------------------*/
       /*  generate and save jpeg image        */
       /*--------------------------------------*/

  len = strlen("mpe_save_jpeg");
  memset(save_flag,'\0',7);
  get_apps_defaults("mpe_save_jpeg",&len,save_flag,&len);

  if(strcmp(save_flag,"save") == 0)
  {
    len = strlen("mpe_jpeg_id");
    get_apps_defaults("mpe_jpeg_id",&len,ftype,&len);
    if(len == 0)
       sprintf(fnamjpeg,"%sz",cdate);
    else
       sprintf(fnamjpeg,"%s%sz",ftype,cdate);

    mpegui_save_jpeg(fnamjpeg,map_number);
  }
  else
  {
   logMessage("jpeg file not saved \n");
  }

       /*--------------------------------------*/
       /*  generate and save netCDF file       */
       /*--------------------------------------*/

  len = strlen("mpe_save_netcdf");
  memset(save_flag,'\0',7);
  get_apps_defaults("mpe_save_netcdf",&len,save_flag,&len);

  if(strcmp(save_flag,"save") == 0)
  {
    len = strlen("mpe_netcdf_id");
    get_apps_defaults("mpe_netcdf_id",&len,ftype,&len);
    if(len == 0)
       sprintf(fnamnet,"%sz.nc",cdate);
    else
       sprintf(fnamnet,"%s%sz.nc",ftype,cdate);

    len = strlen("mpe_netcdf_dir");
    get_apps_defaults("mpe_netcdf_dir", &len, dirname, &len);
   logMessage("Saving netcdf file in %s/%s\n",dirname,fnamnet);

    len = strlen(fnamnet);

    save_netcdf(fnamnet, &len, &MAXX, &MAXY, filename, &lenfn, & int_irc);

    irc = int_irc;

    if(irc != 0)
    {
       if(irc == 1)
         logMessage("malloc for precip array failed - netCDF file not saved\n");
       else if(irc == 2) 
         logMessage("error reading %s -- netCDF file not saved\n",filename);
       else if(irc == 3)
       {
         logMessage("error attempting to create netCDF file (from nc_create routine)");       
         logMessage(" -- netCDF file not saved\n");
       }
    }
  }
  else
  {
   logMessage("netCDF file not saved \n");
  }

         /*--------------------------------------*/
         /*  generate and save grib file         */
         /*--------------------------------------*/

  len = strlen("mpe_save_grib");
  memset(save_flag,'\0',7);
  get_apps_defaults("mpe_save_grib",&len,save_flag,&len);

  if(strcmp(save_flag,"save") == 0)
  {
     len = strlen("mpe_grib_id");
     get_apps_defaults("mpe_grib_id",&len,ftype,&len);
     if(len == 0)
        sprintf(fnamgrib,"%sz.grib",cdate);
     else
        sprintf(fnamgrib,"%s%sz.grib",ftype,cdate);

     lenfnx = strlen(filename_xmrg);
     lenfng = strlen(fnamgrib);

     len = strlen("mpe_grib_dir");
     get_apps_defaults("mpe_grib_dir", &len, dirname, &len);
    logMessage("Saving grib encoded file in %s/%s\n",dirname,fnamgrib);

     pGribCommand = save_grib(filename_xmrg, &lenfnx, fnamgrib, &lenfng);

     if ( pGribCommand != NULL )
     {
       logMessage("process_grib_files script called using command '%s'.\n",
               pGribCommand );
        free ( pGribCommand );
        pGribCommand = NULL;
     }
  }
  else
  {
   logMessage("grib encoded file not saved \n");
  }

  /* Check if the RFC Bias needs to be sent across the WAN. */
  if ( ( transmit_rfc_bias == True ) && ( transmit_bias_on_save == True ) )
  { 
     sprintf ( command_string, "%s/transmit_rfc_bias %s",
               precip_proc_bin_dir, cdate );
     flogMessage ( stdout, "Invoking transmit_rfc_bias script using command:\n"
                       "%s\n", command_string ); 
     system ( command_string );
  } 

  num_prev_poly = 0;
  num_draw_precip_poly = 0;
  deletepoly_flag = 0;

  mSetCursor ( M_NORMAL ) ; 
}

/******************************************************************/
/*   FUNCTION NAME:  save_merged_RFCW                             */
/*       FUNCTION:   save merged (mosaicked) data in xmrg file    */
/*******************************************************************


Function type:
   void

Called by function:
   save_rfcwide 

Functions called:
   write_xmrg 
*******************************************************************/

void save_merged_RFCW ( char filename [ ] , int len , int map_number,
                        char * proc_flag, long * irc )
{
 extern char LOGNAME [ ] ;
 extern short int * xmrgfile_row_array;
 char user[11],dttms[20],os[3],system[6],vernumc[10];
 float vernumf;
 int i,j,lenus,lenpf,maxxmrg,ircc;
 struct utsname  uts_struct;

logMessage("Saving binary file %s \n",filename);

 *irc = 0;
 lenpf = strlen(proc_flag);

 /*-------------------------------------*/
 /*  define user field                  */
 /*  HP or LX will be in first 2 chars  */
 /*-------------------------------------*/

 uname(&uts_struct);
 memset(user, '\0', 11);
 system[5]='\0';
 os[2]='\0';
 strcpy(system, (char *)uts_struct.sysname);
 if(strcmp(system,"HP-UX") == 0)
    sprintf(user,"HP%s",LOGNAME);
 else
    sprintf(user,"LX%s",LOGNAME);

 lenus = strlen(user);

 strcpy(dttms,datetime_save_xmrg);
 strcpy(vernumc,version_number);
 vernumf = atof(vernumc);
 maxxmrg = 0.;

 /*--------------------------------------------------------------*/
 /* write the xmrg file for use in NWSRFS                        */  
 /* replace missing values with 0.0                              */  
 /*                                                              */  
 /* write_xmrg is a FORTRAN subroutine                           */
 /* datetime variable contains valid time                        */
 /*--------------------------------------------------------------*/

 for (i=0; i<MAXY; i++)
 {

   for (j=0; j<MAXX; j++)
   {
     xmrgfile_row_array[j] = (short int) rad_data[map_number].data_array[j][i];
     if(xmrgfile_row_array[j] < 0.0) xmrgfile_row_array[j] = 0.0;
   }

   WRITE_XMRG(&XOR, &YOR, &MAXX, &MAXY, &i, filename, &len,
             user,&lenus,dttms,proc_flag, &lenpf, xmrgfile_row_array,
             datetime,&maxxmrg,&vernumf,&ircc);
   if(ircc != 0) break;
 }
 
 *irc = ( long ) ircc ;
 if(*irc != 0)
  logMessage("Error -- xmrg file not created due to FORTRAN error %ld\n",*irc);

 DataSaved = TRUE; 
 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/save_rfcwide.c,v $";
 static char rcs_id2[] = "$Id: save_rfcwide.c,v 1.5 2007/06/06 14:07:43 whfs Exp $";}
/*  ===================================================  */

}
