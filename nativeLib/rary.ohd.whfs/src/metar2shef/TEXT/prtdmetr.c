#include "metar.h"                               /* dgb:09/16/01 */
/********************************************************************/
/*                                                                  */
/*  Title:         prtDMETR                                         */
/*  Organization:  W/OSO242 - GRAPHICS AND DISPLAY SECTION          */
/*  Date:          15 Sep 1994                                      */
/*  Programmer:    CARL MCCALLA                                     */
/*  Language:      C/370                                            */
/*                                                                  */
/*  Abstract:  prtDMETR    prints, in order of the ASOS METAR       */
/*             format, all non-initialized members of the structure */
/*             addressed by the Decoded_METAR pointer.              */
/*                                                                  */
/*  External Functions Called:                                      */
/*                 None.                                            */
/*                                                                  */
/*  Input:         Mptr - ptr to a decoded_METAR structure.         */
/*                                                                  */
/*  Output:        NONE                                             */
/*                                                                  */
/*  Modification History:                                           */
/*                 None.                                            */
/*                                                                  */
/*  dgb:  added FP and METRIC switch for precip                     */
/*        12/02/96 dgb                                              */
/*                 Add SDO and SCD changes.                         */
/*        06/04/97 DGB                                              */
/*                 Add check for MAXINT < 0 for 24hr precip         */
/*        11/28/97 DGB                                              */
/*                 Check for endless while loops.                   */
/*        09/16/01 DGB                                              */
/*                 Change <metar.h> to "metar.h"                    */
/*        11/20/04 DGB                                              */
/*                 add 24 hour new snow                             */
/********************************************************************/
#define MAX_LOOP 5000                                      /* dgb:11/28/97 */
extern int METRIC;
extern int SDO, SCD;                           /* dgb:12/02/96 */
extern int metar_error_handler( char function[] );
void prtDMETR( Decoded_METAR *Mptr, FILE *fp ) /* dgb: FP added */
{
 
   /***************************/
   /* DECLARE LOCAL VARIABLES */
   /***************************/
 
   int i;
   /*************************/
   /* START BODY OF ROUTINE */
   /*************************/
   
   /* Accomodate SDO and SCD type of reports - If an SDO or SCD type
      of report exists, reset the wind, temp and dew point of the 
      pseudo ob to missing or non-existent
   */
   
   if ( SDO || SCD )                                        /* dgb:12/02/96 */
   {
       Mptr->prevail_vsbyM     = (float) MAXINT;
       Mptr->winData.windDir   = MAXINT; 
       Mptr->winData.windSpeed = MAXINT;
       Mptr->winData.windGust  = MAXINT;
       Mptr->temp              = MAXINT;
       Mptr->dew_pt_temp       = MAXINT;
       Mptr->winData.windUnits[ 0 ] = '\0';  
   }
   
   fprintf(fp,"\n\n\n/*******************************************/\n");
   fprintf(fp,"/*    THE DECODED METAR REPORT FOLLOWS     */\n");
   fprintf(fp,"/*******************************************/\n\n");
 
   if( Mptr->codeName[ 0 ] != '\0' )
      fprintf(fp,"REPORT CODE NAME    : %s\n",Mptr->codeName);
 
   if( Mptr->stnid[ 0 ] != '\0' )
      fprintf(fp,"STATION ID          : %s\n",Mptr->stnid);
 
   if( Mptr->ob_date != MAXINT )
      fprintf(fp,"OBSERVATION DAY     : %d\n",Mptr->ob_date);
 
   if( Mptr->ob_hour != MAXINT )
      fprintf(fp,"OBSERVATION HOUR    : %d\n",Mptr->ob_hour);
 
   if( Mptr->ob_minute != MAXINT )
      fprintf(fp,"OBSERVATION MINUTE  : %d\n",Mptr->ob_minute);
 
   if( Mptr->NIL_rpt )
      fprintf(fp,"NIL REPORT          : TRUE\n");
 
   if( Mptr->AUTO )
      fprintf(fp,"AUTO REPORT         : TRUE\n");
 
   if( Mptr->COR )
      fprintf(fp,"CORRECTED REPORT    : TRUE\n");
 
   if( Mptr->winData.windVRB )
      fprintf(fp,"WIND DIRECTION VRB  : TRUE\n");
 
   if( Mptr->winData.windDir != MAXINT )
      fprintf(fp,"WIND DIRECTION      : %d\n",Mptr->winData.windDir);
 
   if( Mptr->winData.windSpeed != MAXINT )
      fprintf(fp,"WIND SPEED          : %d\n",Mptr->winData.windSpeed);
 
   if( Mptr->winData.windGust != MAXINT )
      fprintf(fp,"WIND GUST           : %d\n",Mptr->winData.windGust);
 
   if( Mptr->winData.windUnits[ 0 ] != '\0' )
      fprintf(fp,"WIND UNITS          : %s\n",Mptr->winData.windUnits);
 
   if( Mptr->minWnDir != MAXINT )
      fprintf(fp,"MIN WIND DIRECTION  : %d\n",Mptr->minWnDir);
 
   if( Mptr->maxWnDir != MAXINT )
      fprintf(fp,"MAX WIND DIRECTION  : %d\n",Mptr->maxWnDir);
 
   if( Mptr->prevail_vsbyM != (float) MAXINT )
      fprintf(fp,"PREVAIL VSBY     (M): %f\n",Mptr->prevail_vsbyM);
 
   if( Mptr->prevail_vsbyKM != (float) MAXINT )
      fprintf(fp,"PREVAIL VSBY    (KM): %f\n",Mptr->prevail_vsbyKM);
 
   if( Mptr->prevail_vsbySM != (float) MAXINT )
      fprintf(fp,"PREVAIL VSBY    (SM): %.3f\n",Mptr->prevail_vsbySM);
 
   if( Mptr->charPrevailVsby[0] != '\0' )
      fprintf(fp,"PREVAIL VSBY  (CHAR): %s\n",Mptr->charPrevailVsby);
 
   if( Mptr->vsby_Dir[ 0 ] != '\0' )
      fprintf(fp,"VISIBILITY DIRECTION: %s\n",Mptr->vsby_Dir);
 
   if( Mptr->RVRNO )
      fprintf(fp,"RVRNO               : TRUE\n");
 
   for ( i = 0; i < 12; i++ )
   {
      if( Mptr->RRVR[i].runway_designator[0] != '\0' )
         fprintf(fp,"RUNWAY DESIGNATOR   : %s\n",
                 Mptr->RRVR[i].runway_designator);
 
      if( Mptr->RRVR[i].visRange != MAXINT )
         fprintf(fp,"R_WAY VIS RANGE (FT): %d\n",
                 Mptr->RRVR[i].visRange);
 
      if( Mptr->RRVR[i].vrbl_visRange )
         fprintf(fp,"VRBL VISUAL RANGE   : TRUE\n");
 
      if( Mptr->RRVR[i].below_min_RVR )
         fprintf(fp,"BELOW MIN RVR       : TRUE\n");
 
      if( Mptr->RRVR[i].above_max_RVR )
         fprintf(fp,"ABOVE MAX RVR       : TRUE\n");
 
      if( Mptr->RRVR[i].Max_visRange != MAXINT )
         fprintf(fp,"MX R_WAY VISRNG (FT): %d\n",
                 Mptr->RRVR[i].Max_visRange);
 
      if( Mptr->RRVR[i].Min_visRange != MAXINT )
         fprintf(fp,"MN R_WAY VISRNG (FT): %d\n",
                 Mptr->RRVR[i].Min_visRange);
 
   }
 
 
   if( Mptr->DVR.visRange != MAXINT )
      fprintf(fp,"DISPATCH VIS RANGE  : %d\n",
              Mptr->DVR.visRange);
 
   if( Mptr->DVR.vrbl_visRange )
      fprintf(fp,"VRBL DISPATCH VISRNG: TRUE\n");
 
   if( Mptr->DVR.below_min_DVR )
      fprintf(fp,"BELOW MIN DVR       : TRUE\n");
 
   if( Mptr->DVR.above_max_DVR )
      fprintf(fp,"ABOVE MAX DVR       : TRUE\n");
 
   if( Mptr->DVR.Max_visRange != MAXINT )
      fprintf(fp,"MX DSPAT VISRNG (FT): %d\n",
              Mptr->DVR.Max_visRange);
 
   if( Mptr->DVR.Min_visRange != MAXINT )
      fprintf(fp,"MN DSPAT VISRNG (FT): %d\n",
              Mptr->DVR.Min_visRange);
 
   i = 0;
   while ( Mptr->WxObstruct[i][0] != '\0' && i < MAXWXSYMBOLS )
   {
      fprintf(fp,"WX/OBSTRUCT VISION  : %s\n",
         Mptr->WxObstruct[i] );
      i++;
      if ( i > MAX_LOOP ) metar_error_handler("prtdmetr"); /* dgb:11/28/97 */
   }
 
   if( Mptr->PartialObscurationAmt[0][0] != '\0' )
      fprintf(fp,"OBSCURATION AMOUNT  : %s\n",
            &(Mptr->PartialObscurationAmt[0][0]));
 
   if( Mptr->PartialObscurationPhenom[0][0] != '\0' )
      fprintf(fp,"OBSCURATION PHENOM  : %s\n",
            &(Mptr->PartialObscurationPhenom[0][0]));
 
 
   if( Mptr->PartialObscurationAmt[1][0] != '\0' )
      fprintf(fp,"OBSCURATION AMOUNT  : %s\n",
            &(Mptr->PartialObscurationAmt[1][0]));
 
   if( Mptr->PartialObscurationPhenom[1][0] != '\0' )
      fprintf(fp,"OBSCURATION PHENOM  : %s\n",
            &(Mptr->PartialObscurationPhenom[1][0]));
 
   i = 0;
   while ( Mptr->cldTypHgt[ i ].cloud_type[0] != '\0' &&
                     i < 6 )
   {
      if( Mptr->cldTypHgt[ i ].cloud_type[0] != '\0' )
         fprintf(fp,"CLOUD COVER         : %s\n",
            Mptr->cldTypHgt[ i ].cloud_type);
 
      if( Mptr->cldTypHgt[ i ].cloud_hgt_char[0] != '\0' )
         fprintf(fp,"CLOUD HGT   (CHARAC): %s\n",
            Mptr->cldTypHgt[ i ].cloud_hgt_char);
 
      if( Mptr->cldTypHgt[ i ].cloud_hgt_meters != MAXINT)
         fprintf(fp,"CLOUD HGT   (METERS): %d\n",
            Mptr->cldTypHgt[ i ].cloud_hgt_meters);
 
      if( Mptr->cldTypHgt[ i ].other_cld_phenom[0] != '\0' )
         fprintf(fp,"OTHER CLOUD PHENOM  : %s\n",
            Mptr->cldTypHgt[ i ].other_cld_phenom);
 
      i++;
      if ( i > MAX_LOOP ) metar_error_handler("prtdmetr"); /* dgb:11/28/97 */
   }
 
   if( Mptr->temp != MAXINT )
      fprintf(fp,"TEMP. (CELSIUS)     : %d\n", Mptr->temp);
 
   if( Mptr->dew_pt_temp != MAXINT )
      fprintf(fp,"D.P. TEMP. (CELSIUS): %d\n", Mptr->dew_pt_temp);
 
   if( Mptr->A_altstng )
      fprintf(fp,"ALTIMETER   (INCHES): %.2f\n",
         Mptr->inches_altstng );
 
   if( Mptr->Q_altstng )
      fprintf(fp,"ALTIMETER  (PASCALS): %d\n",
         Mptr->hectoPasc_altstng );
 
   if( Mptr->TornadicType[0] != '\0' )
      fprintf(fp,"TORNADIC ACTVTY TYPE: %s\n",
         Mptr->TornadicType );
 
   if( Mptr->BTornadicHour != MAXINT )
      fprintf(fp,"TORN. ACTVTY BEGHOUR: %d\n",
         Mptr->BTornadicHour );
 
   if( Mptr->BTornadicMinute != MAXINT )
      fprintf(fp,"TORN. ACTVTY  BEGMIN: %d\n",
         Mptr->BTornadicMinute );
 
   if( Mptr->ETornadicHour != MAXINT )
      fprintf(fp,"TORN. ACTVTY ENDHOUR: %d\n",
         Mptr->ETornadicHour );
 
   if( Mptr->ETornadicMinute != MAXINT )
      fprintf(fp,"TORN. ACTVTY  ENDMIN: %d\n",
         Mptr->ETornadicMinute );
 
   if( Mptr->TornadicDistance != MAXINT )
      fprintf(fp,"TORN. DIST. FROM STN: %d\n",
         Mptr->TornadicDistance );
 
   if( Mptr->TornadicLOC[0] != '\0' )
      fprintf(fp,"TORNADIC LOCATION   : %s\n",
         Mptr->TornadicLOC );
 
   if( Mptr->TornadicDIR[0] != '\0' )
      fprintf(fp,"TORNAD. DIR FROM STN: %s\n",
         Mptr->TornadicDIR );
 
   if( Mptr->TornadicMovDir[0] != '\0' )
      fprintf(fp,"TORNADO DIR OF MOVM.: %s\n",
         Mptr->TornadicMovDir );
 
 
   if( Mptr->autoIndicator[0] != '\0' )
         fprintf(fp,"AUTO INDICATOR      : %s\n",
                          Mptr->autoIndicator);
 
   if( Mptr->PKWND_dir !=  MAXINT )
      fprintf(fp,"PEAK WIND DIRECTION : %d\n",Mptr->PKWND_dir);
   if( Mptr->PKWND_speed !=  MAXINT )
      fprintf(fp,"PEAK WIND SPEED     : %d\n",Mptr->PKWND_speed);
   if( Mptr->PKWND_hour !=  MAXINT )
      fprintf(fp,"PEAK WIND HOUR      : %d\n",Mptr->PKWND_hour);
   if( Mptr->PKWND_minute !=  MAXINT )
      fprintf(fp,"PEAK WIND MINUTE    : %d\n",Mptr->PKWND_minute);
 
   if( Mptr->WshfTime_hour != MAXINT )
      fprintf(fp,"HOUR OF WIND SHIFT  : %d\n",Mptr->WshfTime_hour);
   if( Mptr->WshfTime_minute != MAXINT )
      fprintf(fp,"MINUTE OF WIND SHIFT: %d\n",Mptr->WshfTime_minute);
   if( Mptr->Wshft_FROPA != FALSE )
      fprintf(fp,"FROPA ASSOC. W/WSHFT: TRUE\n");
 
   if( Mptr->TWR_VSBY != (float) MAXINT )
      fprintf(fp,"TOWER VISIBILITY    : %.2f\n",Mptr->TWR_VSBY);
   if( Mptr->SFC_VSBY != (float) MAXINT )
      fprintf(fp,"SURFACE VISIBILITY  : %.2f\n",Mptr->SFC_VSBY);
 
   if( Mptr->minVsby != (float) MAXINT )
      fprintf(fp,"MIN VRBL_VIS    (SM): %.4f\n",Mptr->minVsby);
   if( Mptr->maxVsby != (float) MAXINT )
      fprintf(fp,"MAX VRBL_VIS    (SM): %.4f\n",Mptr->maxVsby);
 
   if( Mptr->VSBY_2ndSite != (float) MAXINT )
      fprintf(fp,"VSBY_2ndSite    (SM): %.4f\n",Mptr->VSBY_2ndSite);
   if( Mptr->VSBY_2ndSite_LOC[0] != '\0' )
      fprintf(fp,"VSBY_2ndSite LOC.   : %s\n",
                   Mptr->VSBY_2ndSite_LOC);
 
 
   if( Mptr->OCNL_LTG )
      fprintf(fp,"OCCASSIONAL LTG     : TRUE\n");
 
   if( Mptr->FRQ_LTG )
      fprintf(fp,"FREQUENT LIGHTNING  : TRUE\n");
 
   if( Mptr->CNS_LTG )
      fprintf(fp,"CONTINUOUS LTG      : TRUE\n");
 
   if( Mptr->CG_LTG )
      fprintf(fp,"CLOUD-GROUND LTG    : TRUE\n");
 
   if( Mptr->IC_LTG )
      fprintf(fp,"IN-CLOUD LIGHTNING  : TRUE\n");
 
   if( Mptr->CC_LTG )
      fprintf(fp,"CLD-CLD LIGHTNING   : TRUE\n");
 
   if( Mptr->CA_LTG )
      fprintf(fp,"CLOUD-AIR LIGHTNING : TRUE\n");
 
   if( Mptr->AP_LTG )
      fprintf(fp,"LIGHTNING AT AIRPORT: TRUE\n");
 
   if( Mptr->OVHD_LTG )
      fprintf(fp,"LIGHTNING OVERHEAD  : TRUE\n");
 
   if( Mptr->DSNT_LTG )
      fprintf(fp,"DISTANT LIGHTNING   : TRUE\n");
 
   if( Mptr->LightningVCTS )
      fprintf(fp,"L'NING W/I 5-10(ALP): TRUE\n");
 
   if( Mptr->LightningTS )
      fprintf(fp,"L'NING W/I 5   (ALP): TRUE\n");
 
   if( Mptr->VcyStn_LTG )
      fprintf(fp,"VCY STN LIGHTNING   : TRUE\n");
 
   if( Mptr->LTG_DIR[0] != '\0' )
      fprintf(fp,"DIREC. OF LIGHTNING : %s\n", Mptr->LTG_DIR);
 
 
 
   i = 0;
   while( i < 3 && Mptr->ReWx[ i ].Recent_weather[0] != '\0' )
   {
      fprintf(fp,"RECENT WEATHER      : %s",
                  Mptr->ReWx[i].Recent_weather);
 
      if( Mptr->ReWx[i].Bhh != MAXINT )
         fprintf(fp," BEG_hh = %d",Mptr->ReWx[i].Bhh);
      if( Mptr->ReWx[i].Bmm != MAXINT )
         fprintf(fp," BEG_mm = %d",Mptr->ReWx[i].Bmm);
 
      if( Mptr->ReWx[i].Ehh != MAXINT )
         fprintf(fp," END_hh = %d",Mptr->ReWx[i].Ehh);
      if( Mptr->ReWx[i].Emm != MAXINT )
         fprintf(fp," END_mm = %d",Mptr->ReWx[i].Emm);
 
      fprintf(fp,"\n");
 
      i++;
      if ( i > MAX_LOOP ) metar_error_handler("prtdmetr"); /* dgb:11/28/97 */
   }
 
   if( Mptr->minCeiling != MAXINT )
      fprintf(fp,"MIN VRBL_CIG    (FT): %d\n",Mptr->minCeiling);
   if( Mptr->maxCeiling != MAXINT )
      fprintf(fp,"MAX VRBL_CIG    (FT): %d\n",Mptr->maxCeiling);
 
   if( Mptr->CIG_2ndSite_Meters != MAXINT )
      fprintf(fp,"CIG2ndSite      (FT): %d\n",Mptr->CIG_2ndSite_Meters);
   if( Mptr->CIG_2ndSite_LOC[0] != '\0' )
      fprintf(fp,"CIG @ 2nd Site LOC. : %s\n",Mptr->CIG_2ndSite_LOC);
 
   if( Mptr->PRESFR )
      fprintf(fp,"PRESFR              : TRUE\n");
   if( Mptr->PRESRR )
      fprintf(fp,"PRESRR              : TRUE\n");
 
   if( Mptr->SLPNO )
      fprintf(fp,"SLPNO               : TRUE\n");
 
   if( Mptr->SLP != (float) MAXINT )
      fprintf(fp,"SLP (hPa)           : %.1f\n", Mptr->SLP);
 
   if( Mptr->SectorVsby != (float) MAXINT )
      fprintf(fp,"SECTOR VSBY  (MILES): %.2f\n", Mptr->SectorVsby );
 
   if( Mptr->SectorVsby_Dir[ 0 ] != '\0' )
      fprintf(fp,"SECTOR VSBY OCTANT  : %s\n", Mptr->SectorVsby_Dir );
 
   if( Mptr->TS_LOC[ 0 ] != '\0' )
      fprintf(fp,"THUNDERSTORM LOCAT. : %s\n", Mptr->TS_LOC );
 
   if( Mptr->TS_MOVMNT[ 0 ] != '\0' )
      fprintf(fp,"THUNDERSTORM MOVMNT.: %s\n", Mptr->TS_MOVMNT);
 
   if( Mptr->GR )
      fprintf(fp,"GR (HAILSTONES)     : TRUE\n");
 
   if( Mptr->GR_Size != (float) MAXINT )
      fprintf(fp,"HLSTO SIZE  (INCHES): %.3f\n",Mptr->GR_Size);
 
   if( Mptr->VIRGA )
      fprintf(fp,"VIRGA               : TRUE\n");
 
   if( Mptr->VIRGA_DIR[0] != '\0' )
      fprintf(fp,"DIR OF VIRGA FRM STN: %s\n", Mptr->VIRGA_DIR);
 
   for( i = 0; i < 6; i++ ) {
      if( Mptr->SfcObscuration[i][0] != '\0' )
         fprintf(fp,"SfcObscuration      : %s\n",
                   &(Mptr->SfcObscuration[i][0]) );
   }
 
   if( Mptr->Num8thsSkyObscured != MAXINT )
      fprintf(fp,"8ths of SkyObscured : %d\n",Mptr->Num8thsSkyObscured);
 
   if( Mptr->CIGNO )
      fprintf(fp,"CIGNO               : TRUE\n");
 
   if( Mptr->Ceiling != MAXINT )
      fprintf(fp,"Ceiling (ft)        : %d\n",Mptr->Ceiling);
 
   if( Mptr->Estimated_Ceiling != MAXINT )
      fprintf(fp,"Estimated CIG   (FT): %d\n",Mptr->Estimated_Ceiling);
 
   if( Mptr->VrbSkyBelow[0] != '\0' )
      fprintf(fp,"VRB SKY COND BELOW  : %s\n",Mptr->VrbSkyBelow);
 
   if( Mptr->VrbSkyAbove[0] != '\0' )
      fprintf(fp,"VRB SKY COND ABOVE  : %s\n",Mptr->VrbSkyAbove);
 
   if( Mptr->VrbSkyLayerHgt != MAXINT )
      fprintf(fp,"VRBSKY COND HGT (FT): %d\n",Mptr->VrbSkyLayerHgt);
 
   if( Mptr->ObscurAloftHgt != MAXINT )
      fprintf(fp,"Hgt Obscur Aloft(FT): %d\n",Mptr->ObscurAloftHgt);
 
   if( Mptr->ObscurAloft[0] != '\0' )
      fprintf(fp,"Obscur Phenom Aloft : %s\n",Mptr->ObscurAloft);
 
   if( Mptr->ObscurAloftSkyCond[0] != '\0' )
      fprintf(fp,"Obscur ALOFT SKYCOND: %s\n",Mptr->ObscurAloftSkyCond);
 
 
   if( Mptr->NOSPECI )
      fprintf(fp,"NOSPECI             : TRUE\n");
 
   if( Mptr->LAST )
      fprintf(fp,"LAST                : TRUE\n");
 
   if( Mptr->synoptic_cloud_type[ 0 ] != '\0' )
      fprintf(fp,"SYNOPTIC CLOUD GROUP: %s\n",Mptr->synoptic_cloud_type);
 
   if( Mptr->CloudLow != '\0' )
      fprintf(fp,"LOW CLOUD CODE      : %c\n",Mptr->CloudLow);
 
   if( Mptr->CloudMedium != '\0' )
      fprintf(fp,"MEDIUM CLOUD CODE   : %c\n",Mptr->CloudMedium);
 
   if( Mptr->CloudHigh != '\0' )
      fprintf(fp,"HIGH CLOUD CODE     : %c\n",Mptr->CloudHigh);
 
   if( Mptr->SNINCR != MAXINT )
      fprintf(fp,"SNINCR (INCHES)     : %d\n",Mptr->SNINCR);
 
   if( Mptr->SNINCR_TotalDepth != MAXINT )
      fprintf(fp,"SNINCR (TOT. INCHES): %d\n",Mptr->SNINCR_TotalDepth);
 
   if( Mptr->snow_depth_group[ 0 ] != '\0' )
      fprintf(fp,"SNOW DEPTH GROUP    : %s\n",Mptr->snow_depth_group);


   if( Mptr->snow_depth != MAXINT )
   {
      if ( METRIC )                                        /* dgb:05/30/96 */
         fprintf(fp,"SNOW DEPTH      (CM): %d\n",Mptr->snow_depth);
      else
         fprintf(fp,"SNOW DEPTH  (INCHES): %d\n",Mptr->snow_depth);
   }

   if( Mptr->WaterEquivSnow != (float) MAXINT )
   {
      if ( METRIC )                                        /* dgb:05/30/96 */
         fprintf(fp,"H2O EquivSno   )(MM): %4.0f\n",Mptr->WaterEquivSnow * 100);
      else
         fprintf(fp,"H2O EquivSno(INCHES): %.2f\n",Mptr->WaterEquivSnow);
   }

   if( Mptr->DepthNewSnow != (float) MAXINT )              /* dgb:12/26/96 */
         fprintf(fp,"DEPTH 6 HOUR SNOW IN: %.2f\n",Mptr->DepthNewSnow);
 
   if( Mptr->DepthNewSnow24 != (float) MAXINT )            /* dgb:11/20/04 */
         fprintf(fp,"DEPTH 24 HOUR NEW SNOW IN: %.2f\n",Mptr->DepthNewSnow24); /* dgb:11/20/04 */
 
   if( Mptr->SunshineDur != MAXINT )
      fprintf(fp,"SUNSHINE   (MINUTES): %d\n",Mptr->SunshineDur);
 
   if( Mptr->SunSensorOut )
      fprintf(fp,"SUN SENSOR OUT      : TRUE\n");
 
   if( Mptr->hourlyPrecip != (float) MAXINT )
   {
      if ( METRIC )                                        /* dgb:05/30/96 */
         fprintf(fp,"HRLY PRECIP     (MM): %4.0f\n",Mptr->hourlyPrecip * 100);
      else
         fprintf(fp,"HRLY PRECIP (INCHES): %.2f\n",Mptr->hourlyPrecip);
   } 

   if( Mptr->precip_amt != (float) MAXINT)
   {
      if ( METRIC )                                        /* dgb:05/30/96 */
         fprintf(fp,"PRECIP AMT      (MM): %4.0f\n",Mptr->precip_amt * 100);
      else
         fprintf(fp,"PRECIP AMT  (INCHES): %.2f\n",Mptr->precip_amt);
   }

   
   if( Mptr->precip_24_amt !=  (float) MAXINT )
   {
      if ( Mptr->precip_24_amt < 0 )                                   /* dgb:06/04/97 */
      {
         if ( METRIC )
            fprintf(fp,"24HR PRECIP     (MM): M\n");
         else
            fprintf(fp,"24HR PRECIP (INCHES): M\n");
      }
      else
      {     
      if ( METRIC )                                        /* dgb:05/30/96 */
         fprintf(fp,"24HR PRECIP     (MM): %4.0f\n",Mptr->precip_24_amt * 100);
      else
         fprintf(fp,"24HR PRECIP (INCHES): %.2f\n",Mptr->precip_24_amt);
      }
   }

   if( Mptr->Indeterminant_24HrPrecip )
      fprintf(fp,"INDTRMN 24 HR PRECIP: TRUE\n");

   if( Mptr->Indeterminant3_6HrPrecip )
      fprintf(fp,"INDTRMN 3/6HR PRECIP: TRUE\n");
 
   if( Mptr->Temp_2_tenths != (float) MAXINT )
      fprintf(fp,"TMP2TENTHS (CELSIUS): %.1f\n",Mptr->Temp_2_tenths);
 
   if( Mptr->DP_Temp_2_tenths != (float) MAXINT )
      fprintf(fp,"DPT2TENTHS (CELSIUS): %.1f\n",Mptr->DP_Temp_2_tenths);
 
   if( Mptr->maxtemp !=  (float) MAXINT)
      fprintf(fp,"MAX TEMP   (CELSIUS): %.1f\n",
         Mptr->maxtemp);
 
   if( Mptr->mintemp !=  (float) MAXINT)
      fprintf(fp,"MIN TEMP   (CELSIUS): %.1f\n",
         Mptr->mintemp);
 
   if( Mptr->max24temp !=  (float) MAXINT)
      fprintf(fp,"24HrMAXTMP (CELSIUS): %.1f\n",
         Mptr->max24temp);
 
   if( Mptr->min24temp !=  (float) MAXINT)
      fprintf(fp,"24HrMINTMP (CELSIUS): %.1f\n",
         Mptr->min24temp);
 
   if( Mptr->char_prestndcy != MAXINT)
      fprintf(fp,"CHAR PRESS TENDENCY : %d\n",
         Mptr->char_prestndcy );
 
   if( Mptr->prestndcy != (float) MAXINT)
      fprintf(fp,"PRES. TENDENCY (hPa): %.1f\n",
         Mptr->prestndcy );
 
   if( Mptr->PWINO )
      fprintf(fp,"PWINO               : TRUE\n");
 
   if( Mptr->PNO )
      fprintf(fp,"PNO                 : TRUE\n");
 
   if( Mptr->CHINO )
      fprintf(fp,"CHINO               : TRUE\n");
 
   if( Mptr->CHINO_LOC[0] != '\0' )
      fprintf(fp,"CHINO_LOC           : %s\n",Mptr->CHINO_LOC);
 
   if( Mptr->VISNO )
      fprintf(fp,"VISNO               : TRUE\n");
 
   if( Mptr->VISNO_LOC[0] != '\0' )
      fprintf(fp,"VISNO_LOC           : %s\n",Mptr->VISNO_LOC);
 
   if( Mptr->FZRANO )
      fprintf(fp,"FZRANO              : TRUE\n");
 
   if( Mptr->TSNO )
      fprintf(fp,"TSNO                : TRUE\n");
 
   if( Mptr->DollarSign)
      fprintf(fp,"DOLLAR $IGN INDCATR : TRUE\n");
 
   if( Mptr->horiz_vsby[ 0 ] != '\0' )
      fprintf(fp,"HORIZ VISIBILITY    : %s\n",Mptr->horiz_vsby);
 
   if( Mptr->dir_min_horiz_vsby[ 0 ] != '\0' )
      fprintf(fp,"DIR MIN HORIZ VSBY  : %s\n",Mptr->dir_min_horiz_vsby);
 
   if( Mptr->CAVOK )
      fprintf(fp,"CAVOK               : TRUE\n");
 
 
   if( Mptr->VertVsby != MAXINT )
      fprintf(fp,"Vert. Vsby (meters) : %d\n",
                  Mptr->VertVsby );
 
   if( Mptr->charVertVsby[0] != '\0' )
      fprintf(fp,"Vert. Vsby (CHAR)   : %s\n",
                  Mptr->charVertVsby );
 
   if( Mptr->QFE != MAXINT )
      fprintf(fp,"QFE                 : %d\n", Mptr->QFE);
 
   if( Mptr->VOLCASH )
      fprintf(fp,"VOLCANIC ASH        : TRUE\n");
 
   if( Mptr->min_vrbl_wind_dir != MAXINT )
      fprintf(fp,"MIN VRBL WIND DIR   : %d\n",Mptr->min_vrbl_wind_dir);
   if( Mptr->max_vrbl_wind_dir != MAXINT )
      fprintf(fp,"MAX VRBL WIND DIR   : %d\n",Mptr->max_vrbl_wind_dir);
 
 
   fprintf(fp,"\n\n\n");
 
 
   return;
 
}
