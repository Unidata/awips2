
/*****************************XMRG FILE FORMAT****************************/
/* PLEASE SEE http://hsp.nws.noaa.gov/oh/hrl/pps/xmrg43.wpd FOR DETAILS. */
/*                                                                       */
/* THE LENGTH OF A XMRG FILE SHOULD BE                                   */
/* '4'+16+'4'+'4'+66+'4'+('4'+MAXX+'4')*MAXY BYTES                       */
/*                      OR                                               */
/* '4'+16+'4'+'4'+37+'4'+('4'+MAXX+'4')*MAXY BYTES                       */
/*                      OR                                               */
/* '4'+16+'4'+('4'+MAXX+'4')*MAXY BYTES                                  */
/*                                                                       */
/* '4' IS A 4-BYTE INTEGER REPRESENTS THE LENGTH OF THE RECORD IN WORDS. */
/* EVERY RECORD HAS THIS 4-BYTE INTEGER AT THE BEGINNING AND THE END.    */
/* WHEN READING THE RECORD IN FORTRAN BY RECORDS, THE 4-BYTE INTEGER     */
/* COULD BE IGNORED, ELSE IT SHOULD BE ACCOUNTED.---KWZ 7-11-02          */
/*************************************************************************/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "TimeDisAgg.h"
#include "messenger_inc/msgwrappers.h"

typedef struct HRAP {
        double   x;
        double   y;
        }       HRAP;

typedef char String[16];

/**********global declarations************/
int NumFiles;
FILE *in_files[101];

/********************************************************/
/* LatLongToHrap was from find_hrap.c 			*/
/* convert latitute and longtitute to hrap format	*/
/*							*/
/* variables i/o  description				*/
/* lat       i    latitute				*/
/* lon       i    longtitute				*/
/********************************************************/

HRAP LatLongToHrap(double lat, double lon)
{
  double  degrad = 0.017453293;
  double  earthrad = 6371.2;
  double  stdlat = 60.;
  double  stdlon = 105.;
  double  mesh_len = 4.7625;
  double  tlat, re, latrad, lonrad, r, x, y;
  HRAP   hrap;
  
  writemsg(EVT_START_RTN+SEV_DEBUG + TIER_4,"%s\n","Enter subroutine LatLongToHrap.");
 
  tlat = stdlat*degrad;
  re = (earthrad*(1. + sin(tlat)))/mesh_len;
  latrad = lat * degrad;
  lonrad = (lon + 180. - stdlon) * degrad;
  r = re * cos(latrad) / (1. + sin(latrad));
  x = r * sin(lonrad);
  y = r * cos(lonrad);
  hrap.x = x + 401;
  hrap.y = y + 1601;
 
  writemsg(EVT_FINISH_RTN+TIER_4+SEV_DEBUG,"%s\n","Exit subroutine LatLongToHrap.");
 
  return hrap;
}

/***********************************************************/
/* Create file names from start time to end time in        */
/* xmrgMMDDYYHHz format for each hour. Use xmrgMMDDCCYYHHz */
/* format if time is before year 2000                      */
/*                                                         */
/* variables  i/o  description                             */
/* StartTime  i    begin time                              */
/* EndTime    i    end time                                */
/* FileNames1 o    file names list                         */
/* FileNames2 o    file names list                         */
/***********************************************************/
void CreateFileNames(time_t StartTime, time_t EndTime, String FileNames1[101],String FileNames2[101])
{
/* Input variable: StartTime, EndTime
 * Output variable: FileNames,NumFiles
 *
 * Create file names from start time and end time in xmrgMMDDYYHHz format
 */

  struct tm *TmpTime ;
  time_t TTime;
  int FileIndex,TokenLength,ValLength; /*julian hour*/
  char DateFormatToken[128] ; ;
  
  writemsg(EVT_START_RTN+TIER_4+SEV_DEBUG,"%s\n","Enter subroutine CreateFileNames.");
  
/*From start time to end includes end time*/
  NumFiles = (int)(difftime(EndTime,StartTime)/3600) + 1;
  
  /*if (NumFiles > 0  && NumFiles<=24)*/
  if (NumFiles > 0 && NumFiles < 101)
  {
    TTime = StartTime;
    
    TokenLength=13;
    get_apps_defaults("mpe_date_form", &TokenLength, DateFormatToken, &ValLength);
    
    for (FileIndex=0;FileIndex<NumFiles;FileIndex++)
    { 
       TmpTime = gmtime(&TTime);
       
/*some XMRG files use xmrgmmddyyhhz format and others use xmrgmmddccyyhhz format*/
      if (strcmp(DateFormatToken,"Ymd"))
      {/*use mdY format*/
        strftime(FileNames1[FileIndex],16,"xmrg%m%d%Y%Hz",TmpTime);/*4 digit year*/
	strftime(FileNames2[FileIndex],16,"xmrg%m%d%y%Hz",TmpTime);/*2 digit year*/
      }
      else 
      {/*use Ymd format*/
        strftime(FileNames1[FileIndex],16,"xmrg%Y%m%d%Hz",TmpTime);/*4 digit year*/
	strftime(FileNames2[FileIndex],16,"xmrg%y%m%d%Hz",TmpTime);/*2 digit year*/
      }

      TTime+=3600;/*add one hour*/
    }
  }
  else
  {
    NumFiles=0;
  }/**End of if (NumFiles>0)**/

  writemsg(EVT_FINISH_RTN+TIER_4+SEV_DEBUG,"%s\n","Exit subroutine CreateFileNames.");
}

/*********************************************************************/
/* open all xmrg files for reading. If unable open any file, it will */
/* close all opened files and return 0;                              */
/* return 0 for any file unable to open, 1 for all files opened      */
/*                                                                   */
/* variables i/o  description                                        */
/* StartTime i    begin time                                         */
/* EndTime   i    end time                                           */
/* in_files  0    A list of file pointers.                           */
/* NumFiles  0    Length of in_files.                                */
/*********************************************************************/
int OpenXMRGFiles (time_t StartTime, time_t EndTime, int Radius, char  XMRGDir[])
{
  int FileFlag,i,j;
  String FileNames1[101],FileNames2[101];
  char XMRGFile[128];
  
  writemsg(EVT_START_RTN+TIER_4+SEV_DEBUG,"%s\n","Enter subroutine OpenXMRGFiles.");
  
  CreateFileNames( StartTime,  EndTime, FileNames1,FileNames2);
  /*Radius=GetRadiusVal();*/
  
  if (NumFiles<=0)
    FileFlag=0;
  else
  { FileFlag=1;
    i=14;
    
    strcat(XMRGDir,"/");
    for (i=0;i<NumFiles;i++)
    { strcpy(XMRGFile,XMRGDir);
      strcat(XMRGFile,FileNames1[i]);/*check xmrg with 4 digit year*/
      if((in_files[i] = fopen(XMRGFile, "r")) == NULL)
      { 
        strcpy(XMRGFile,XMRGDir);
        strcat(XMRGFile,FileNames2[i]);/*if 4 digit year xmrg file not exist, then check xmrg with 2 digit year*/
        if((in_files[i] = fopen(XMRGFile, "r")) == NULL)
        { 
	  FileFlag=0;
          break;
	}
      }
    }
  
    if (FileFlag==0)
    { 
      for (j=0;j<i;j++)
        fclose (in_files[j]);
    }
  }/*end of if (NumFiles<=0)*/
  
  if (FileFlag==0)
    writemsg(EVT_SOURCE_IO+DQ_BAD+PS_NON_CORRECT+SEV_ERROR,"%s\n","Unable to open files.");
  else
    writemsg(EVT_LOGGING + SEV_DEBUG + TIER_4,"%s\n","Files opened.");

  writemsg(EVT_FINISH_RTN+TIER_4+SEV_DEBUG,"%s\n","Exit subroutine OpenXMRGFiles.");
  
  return FileFlag;
}

/***************************************************************/
/* close all files in in_files                                 */
/*                                                             */
/* variables  i/o  description                                 */
/* NumFiles   i    A global variable. Length of in_files       */
/* in_files   i    A global variable. A list of file pointers. */
/***************************************************************/
void CloseXMRGFiles ()
{
  int j ;
  
  writemsg(EVT_START_RTN+TIER_4+SEV_DEBUG,"%s\n","Enter subroutine CloseXMRGFiles.");
  
  for (j=0;j<NumFiles;j++)
    fclose (in_files[j]);
  
  writemsg(EVT_LOGGING + SEV_INFO + TIER_4,"%s\n","Files closed.");
  writemsg(EVT_FINISH_RTN+TIER_4+SEV_DEBUG,"%s\n","Exit subroutine CloseXMRGFiles.");
}

/*******************************************************/
/* Get the disagg_radius token value, always return an */
/* odd number from 1 to 25                             */
/* If disagg_radius didn't exported, return 1          */
/* if disagg_radius is not a number, return 1          */
/* if disagg_radius has a decimal point or even number,*/
/* return the next odd number.                         */
/*******************************************************/
int GetRadiusVal()
{ int Length1,Length2, TmpRadius,ErrorFlag=0;
  char RadiusToken[128] ;
  
  

  writemsg(EVT_START_RTN+TIER_4+SEV_DEBUG,"%s\n","Enter subroutine GetRadiusVal.");
  
  Length1 = 13;
  get_apps_defaults("disagg_radius", &Length1, RadiusToken, &Length2);  
  if (Length2==0)
  { TmpRadius=1 ;
    ErrorFlag=-1; /*disagg_radius not exported*/
  }
  else
  { TmpRadius = 0 ;
    for (Length1=0;Length1<Length2;Length1++)
      if (RadiusToken[Length1] == '.')
        ErrorFlag = 1; /*It has a decimal point*/
      else
        if ((RadiusToken[Length1] > '9') || (RadiusToken[Length1] < '0'))
        { TmpRadius=1; ErrorFlag = -2; break;} /*disagg_radius is not a number*/
    
    if (ErrorFlag == 0)
    { TmpRadius=(int)(atof(RadiusToken) + 0.5); /*return the round up number*/
      if ((TmpRadius % 2) != 1)
      { TmpRadius += 1 ; ErrorFlag = 2;}/*It must be odd number*/
      if (TmpRadius<1)
      { TmpRadius = 1; ErrorFlag = 3;}
      else if (TmpRadius>25)
      { TmpRadius = 25; ErrorFlag = 4;}
    }
  }

/****report errors and warnings******/
  switch(ErrorFlag){
    case -2 :
      writemsg(EVT_DATA+DQ_BAD+PS_CORRECT+SEV_ERROR,"%s\n","Error: token disagg_radius is not a number");
      break;
    case -1 :
      writemsg(EVT_DATA+DQ_BAD+PS_CORRECT+SEV_ERROR,"%s\n","Error: token disagg_radius not exported.");
      break;
    case 0 :
    
      writemsg(EVT_LOGGING + SEV_DEBUG+TIER_4,"%s\n","Good disagg_radius token value.");
      break;
    case 1 :
      writemsg(EVT_LOGGING + SEV_WARNING + TIER_2,"%s\n","Warning: token disagg_radius is not a whole number.");
      break;
    case 2 :
      writemsg(EVT_LOGGING + SEV_WARNING + TIER_2,"%s\n","Warning: token disagg_radius is not a odd number.");
      break;
    case 3 :
      writemsg(EVT_LOGGING + SEV_WARNING + TIER_2,"%s\n","Warning: token disagg_radius is less than 1.");
      break;
    case 4 :
      writemsg(EVT_LOGGING + SEV_WARNING + TIER_2,"%s\n","Warning: token disagg_radius is greater than 25.");
      break;
    default :
      writemsg(EVT_DATA+DQ_BAD+PS_CORRECT+SEV_ERROR,"%s\n","Unknown disagg_radius token error.  Contact developer.");
  }

  writemsg(EVT_FINISH_RTN+TIER_4+SEV_DEBUG,"%s\n","Exit subroutine GetRadiusVal.");
  
  return TmpRadius ;
}

/***************************************************************/
/* return the average value centered by pos in radius          */
/*                                                             */
/* variables  i/o  description                                 */
/* in_files   i    A global variable. A list of file pointers. */
/* StartTime  i    begin time                                  */
/* CurrTime   i    current time                                */
/* Pos        i    where the center of xmrg data will read     */
/* Radius     i    Dimemsion of xmrg data will read            */
/***************************************************************/
float GetXMRGVal (time_t StartTime, time_t CurrTime,
                           struct PosStruct Pos, int Radius)
{
  int index;
  HRAP Location;
  int NumWord,xor,yor,maxx,maxy,X,Y,SkipLength,Sum,StartX,StartY,EndX,EndY ;
  short int value, WarningVal;
  float XMRGVal;
  long int SecRecLength ; /*length of second record in the xmrg file*/
  
  writemsg(EVT_START_RTN+TIER_4+SEV_DEBUG,"%s\n","Enter subroutine GetXMRGVal.");
  
  Location=LatLongToHrap(Pos.lat, Pos.lon);
  index = (CurrTime-StartTime)/3600; /*calculate the index of in_files to be used*/
  X = (int) (Location.x + .5);
  Y = (int) (Location.y + .5);
    
  WarningVal=0;
  if((index<0) || (index>=NumFiles))
  { 
    XMRGVal=-999.00 ;
    WarningVal=-1; /*file not open*/
  }
  else if ((Radius<1) || ((Radius%2)==0))
  {
    XMRGVal=-999.00 ;
    WarningVal=-2; /*bad disagg_radius token value*/
  }
  else
  {
    rewind(in_files[index]); /*goto beginning on the file*/
    fread(&NumWord, sizeof(int), 1, in_files[index]);
    fread(&xor, sizeof(int), 1, in_files[index]);
    fread(&yor, sizeof(int), 1, in_files[index]);
    fread(&maxx, sizeof(int), 1, in_files[index]);
    fread(&maxy, sizeof(int), 1, in_files[index]); /*read header information*/
    fread(&NumWord, sizeof(int), 1, in_files[index]);
    
    if ((X<xor) || (X>=(xor+maxx)) || (Y<yor) || (Y>=(yor+maxy))){
      XMRGVal=-999.00 ;
      WarningVal=-3; /*lid not locate on the grid*/
    }
    else
    {
      StartX=X-xor-Radius/2;
      if (StartX < 0) StartX=0;
      StartY=Y-yor-Radius/2;
      if (StartY < 0) StartY=0;
      EndX=X-xor+Radius/2+1;
      if (EndX > maxx) EndX=maxx;
      EndY=Y-yor+Radius/2+1;
      if (EndY > maxy) EndY=maxy;/*calculate where the information will be read*/
    
      if (EndX <= StartX || EndY <= StartY){
        XMRGVal=-999.00;
        WarningVal=-4;/*highly impossible error. invalid reading umbrella*/
      }
      else
      {
        Sum=0;
    
        fread(&NumWord, sizeof(int), 1, in_files[index]);
        if (NumWord==37 || NumWord==66)
          SecRecLength = NumWord + 4;/*XMRG FILE format after 1997*/
        else
        { SecRecLength = 0 ;
          fseek (in_files[index],-4,SEEK_CUR); /*XMRG FILE format before 1997*/
        }

        SkipLength = StartY * maxx * 2 + StartY * 8 + SecRecLength;
        fseek (in_files[index],(long)  SkipLength,SEEK_CUR); /*skip some uneccessary bytes*/
       
        for (Y=StartY;Y<EndY;Y++)
        { fseek (in_files[index],(long) (StartX*2+4),SEEK_CUR);/*Skip to StartX*/
          for (X=StartX;X<EndX;X++)
          {
            fread(&value, 2 , 1,  in_files[index]);
  	    Sum +=value ;
           
          }
          fseek (in_files[index],(long) ((maxx-EndX)*2+4),SEEK_CUR);/*Skip to end of row*/
        }
        XMRGVal = (float)((Sum/(EndX-StartX)*(EndY-StartY))) / 100.0; /*find the average & round to nearest integer*/
      }
    }
  }
  
  if (WarningVal == -1)
    writemsg(EVT_DATA+DQ_BAD+PS_NON_CORRECT+SEV_WARNING,"%s\n",
                    "Attempted reading file not open or not exist.");
  else if (WarningVal == -2)
    writemsg(EVT_DATA+DQ_BAD+PS_NON_CORRECT+SEV_WARNING,"%s\n",
                    "Invalid disagg_radius token value.");
  else if (WarningVal == -3)
    writemsg(EVT_DATA+DQ_BAD+PS_NON_CORRECT+SEV_WARNING,"%s\n",
                    "Lid is not located on the grid.");
  else if (WarningVal == -4)
    writemsg(EVT_DATA+DQ_BAD+PS_NON_CORRECT+SEV_WARNING,"%s\n",
                    "Invalid reading umbrella.  See developer.");
  else if (XMRGVal<0.0)
    writemsg(EVT_DATA+DQ_BAD+PS_NON_CORRECT+SEV_WARNING,"%s\n",
                    "Bad xmrg value read from file.");
/*  else
    writemsg(EVT_DATA+DQ_GOOD+PS_NONE+SEV_INFO,"%s\n","Good value.");
*/
  writemsg(EVT_LOGGING + SEV_INFO + TIER_3,
                     "XMRGVal = %f\n",XMRGVal);
  writemsg(EVT_FINISH_RTN+TIER_4+SEV_DEBUG,"%s\n","Exit subroutine GetXMRGVal.");
  
  return XMRGVal;
}
