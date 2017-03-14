#include <stdio.h>

/*  Create for the unit hydrograph change mod enhancement.
*
* ModDates[][0] input List of all start dates from mods
* ModDates[][1] input List of all end dates from mods
* ndtuhg     input length of ModDates[][0] and ModDates[][1]
* SRunDate   input Start run date of the segment
* ERunDate   input End run date of the segment
* *Interval   input time *Interval
* PLLength   input length of PList
* PList      output sorted list
*/
/*
ModDates[][0],ModDates[][1], and ERunDate are excluded, SRunDate is included

/*This subroutine identifies which mods should be used on what dates.
  last in wins.
  updated by Edwin Welles from original version which sorted by dates.
  */

void sortingmods (int ModDates[20][2], int *ndtuhg, int *SRunDate, int *ERunDate,
          int *Interval, int *PList)
/*                 int *Interval, int *PList, char op[20][8])*/
{

  int PLLength,i,j,l,m;
  int StartPos,EndPos;


  PLLength = ((*ERunDate - *SRunDate) / *Interval)+1 ;
  
  /*initialize the PList*/

  for ( j=0;j<PLLength;j++)
        PList[j]=0; /*0 stands for the base ordinates*/

 for ( j=0;j<*ndtuhg;j++){
    StartPos = (ModDates[j][0]-*SRunDate) / *Interval-1; /*include startrun time*/
/*printf("cew  startpos ======> %d \n",StartPos);*/
    if (StartPos < 0) StartPos=0;
    EndPos = (ModDates[j][1]-*SRunDate) / *Interval +1 ;
    if (EndPos > 1000) EndPos=1000;
/*printf("cew  endpos ======> %d \n",EndPos);*/
    for ( i=StartPos;i<EndPos;i++)
      PList[i]=j+1 ;    
  }

/*printf ("kwz---plist=");
for ( i=0;i<PLLength;i++)
  printf(" %d",PList[i]);
printf("\n");
printf("plength %d \n",PLLength);
*/


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/SortingMods.c,v $";
 static char rcs_id2[] = "$Id: SortingMods.c,v 1.4 2004/11/10 22:43:53 edwin Exp $";}
/*  ===================================================  */

}
