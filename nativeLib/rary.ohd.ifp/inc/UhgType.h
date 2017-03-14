typedef struct
{
	char ModName[10]; /*Name of this mod.  Options are BASE, UHGCHNG, or UHGCDATE*/
	int startDate,endDate,validDate;
	int NumOrd ;  /*Number of ordinates*/
	int Ordinates[100];  /*ordinates*/
	int NumBasin; /*Number of basins.  Warning if more than 20.*/
	char BasinName[20][10];  /*Setup 20 basins name should be enough.*/
}UhgType;
