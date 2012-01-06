#include <stdio.h>

/*
This procedure extract the current uhgchng operation mods.

variable: description
input variables
op: all the basin names of uhgchng mod of this segment.
keyop: basin name of this operation.
UHGVAL: all converted uhgchng mod base ordinates.
ndtuhg: length of UHGVAL
jdtuhg: a list of start dates and end dates of uhgchng mod of this segment.
NVLUHG: a list of number of ordinates of each mod.

output variables
TmpUHGVAL: base ordinates for this operation.
TmpNumUHG: The length of TmpUHGVAL,TmpDates,and TmpUhgLen
TmpDates: a list of start dates and end dates of uhgchng mod of this operation.
TmpUhgLen: a list of number of ordinates of each mod of this operation.
*/

void getthisop(char op[20][8], char keyop[8],
               float UHGVAL[100][20], int *ndtuhg,
			   int jdtuhg[20][3],
			   int NVLUHG[20],
               float TmpUHGVAL[100][20], int *TmpNumUHG,
			   int TmpDates[20][2],
			   int TmpUhgLen[20])
{
	int i,j;

	*TmpNumUHG=0;
	for (i=0;i<*ndtuhg;i++)
	{
		if(strncmp (op[i],keyop,8)==0  || strncmp (op[i],"        ",8)==0)
		{
			for (j=0;j<NVLUHG[i];j++)
				TmpUHGVAL[j][*TmpNumUHG]=UHGVAL[j][i];
			
			TmpDates[*TmpNumUHG][0]=jdtuhg[i][0];
			TmpDates[*TmpNumUHG][1]=jdtuhg[i][1];
			TmpUhgLen[*TmpNumUHG]=NVLUHG[i];
			
			(*TmpNumUHG)++;
		}/*end of if*/
	}/*end of for i loop*/

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/getThisOp.c,v $";
 static char rcs_id2[] = "$Id: getThisOp.c,v 1.2 2004/08/05 17:48:14 wkwock Exp $";}
/*  ===================================================  */

}/*end of getthisop procedure*/
