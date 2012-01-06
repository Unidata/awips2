#include <time.h>
#include <messenger_inc/msgwrappers.h>

/*setup and start api message*/
/* OutFile   out  output file name use to construct API file name*
 *LevelToken out  API level token name        */
/*********************************************************/
 
void setupapi(char *OutFile, int *OutFileLen, char *LevelToken, int *LvlLen)
{

  int   lendir, len, lendebug_level;
  int   reqdlev[] = {1, 1, TIER_3, TIER_3, 1, 1, 1, 1, 1, 1, 1};
  int   defevt = -1;
 
  int   debugLevels,i,index;
  
  char  debug_level[5],filename[200], TmpFileName[200],LvlTknStr[51];
  char  *TmpStr;


/*---------------------------------------------*/
/*   Check if the parameters valid             */
/*---------------------------------------------*/
  if (*LvlLen<1 || *LvlLen>50)
    strcpy(LvlTknStr,"");
  else
  { strncpy(LvlTknStr,LevelToken,*LvlLen);
    LvlTknStr[*LvlLen]=0;/*set null to end of string*/
  }

  if (*OutFileLen<1 || *OutFileLen>200)
    strcpy(TmpFileName,"");
  else
  { strncpy(TmpFileName,OutFile,*OutFileLen);
    TmpFileName[*OutFileLen]='\0';/*set null to end of string*/
  }
  
  /*because fortran could add some spaces to end of OutFile, So must eliminate it*/
  i=strlen(TmpFileName)-1;
  while ((TmpFileName[i]==' ') && (i >= 0))
  { TmpFileName[i]='\0';
    i--;
  }

/*---------------------------------------------*/
/*   Construct the API log file name           */
/* The logic is to add _Api before 1st dot(.)  */
/* or end of file and set api file name to ""  */
/* if OutFile=="" or NULL.                     */
/*---------------------------------------------*/

  if (*TmpFileName==0)  /*0 is null*/
    strcpy(filename,""); /*Will use standard output*/
  else if (strcmp(TmpFileName,"")==0)
    strcpy(filename,""); /*Will use standard output also*/
  else
  {
    index=strcspn(TmpFileName,".");/*Looking for index of 1st dot(.)*/
	strncpy(filename,TmpFileName,index);
	filename[index]=0;  /*Add null because strncpy might not do it.*/
	strcat(filename,"_api");/*insert _api*/

	TmpStr=strpbrk(TmpFileName,".");
	if (TmpStr != 0)
	  strcat(filename,TmpStr);/*add rest of string*/
  }

/*---------------------------------------------*/
/* set up debug level                          */
/*---------------------------------------------*/
  lendebug_level = strlen(LvlTknStr);
  get_apps_defaults(LvlTknStr,&lendebug_level,debug_level,&lendebug_level);
  debugLevels = atoi(debug_level);
  if (debugLevels <0 || debugLevels >127)
  {
     debugLevels=1;
  }
/*---------------------------------------------*/
/*   start initializing API msg                */
/*---------------------------------------------*/ 

  for(i=0;i<=10;i++)
  {
    reqdlev[i] =  debugLevels; 
  }
  initmsg(&defevt, NULL, filename, reqdlev);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/ndfd2rfs_sub/RCS/SetupApi.c,v $";
 static char rcs_id2[] = "$Id: SetupApi.c,v 1.1 2004/09/27 13:27:58 dsa Exp $";}
/*  ===================================================  */

}
