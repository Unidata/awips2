
/**************sets comparison********************/
/*This procedure returns 0 if [LB,UB] is totally included in SetList,
return -1 if totally not included, else return 1.
That is, by the unions rule:
if ([LB,UB] and [SetList[][0],SetList[][1]]) == [LB,UB], returns 0,
if == {}, returns -1 and add set [LB,UB], else returns 1.
*/

void setcmp(int SetList[][2],int *SetLength, int *LB, int *UB, int *Stat)
/*
  SetList = a list of sets where SetList[][0] is lower boundary and SetList[][1] is
  upper boundary.
  SetLength = length of SetList.
  LB = Lower Boundary of the set.
  UB = Upper Boundary of the set.
*/
{
	int i,TmpLb,TmpUb;/*loop index, temporary LB, and temporary UB*/

	/*****an error trap****/
	if (*SetLength<0 || *SetLength>100){ *Stat=1; return ;}

	if (*SetLength==0)
	{	SetList[*SetLength][0]=*LB ; /*update the SetList*/
		SetList[*SetLength][1]=*UB ;
		*SetLength++;
		*Stat=-1;
		return ;
	}
	
	TmpLb=*LB;
	TmpUb=*UB;
	for (i=0;i<*SetLength;i++)
	{
		if (SetList[i][0] <= TmpLb && SetList[i][1] >= TmpLb)
			TmpLb = SetList[i][1] + 1;

		if (SetList[i][0] <= TmpUb && SetList[i][1] >= TmpUb)
			TmpUb = SetList[i][0] - 1;
		
		if(TmpLb>TmpUb) break ;
	}

	if (TmpLb>TmpUb)/*([LB,UB] and [SetList[][0],SetList[][1]]) == [LB,UB]*/
		*Stat=0;
	else if(TmpLb==*LB && TmpUb==*UB)
	/*([LB,UB] and [SetList[][0],SetList[][1]]) == {}*/
	{	if (*SetLength<100)
		{	SetList[*SetLength][0]=*LB ; /*update the SetList*/
			SetList[*SetLength][1]=*UB ;
			*SetLength++;
		}
		
		*Stat=-1;
	}
	else
		*Stat=1;

	return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/setcmp.c,v $";
 static char rcs_id2[] = "$Id: setcmp.c,v 1.2 2004/01/29 21:29:26 wkwock Exp $";}
/*  ===================================================  */

}
