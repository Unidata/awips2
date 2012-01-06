#include "decodedpa.h"

char *convertJulianDate(short int jdate)
{
	static char cdate[9];
	 int days[]={31,28,31,30,31,30,31,31,30,31,30,31} ;
	 int year,month,ndays,date,total,leap_year;

	 total=0;
	 for (year=1970;year<2050;year++)
	    {
			   leap_year=FALSE;
			   if ((((year%4) == 0) && ((year%100) != 0)) || (year%400==0))
	               leap_year=TRUE;
	     for (month=0;month<12;month++)
			  {
			   total = total+days[month];
			   if (month==1 && leap_year==TRUE)
			       total++;
			   if (total >= jdate)
			     {
			      ndays=days[month];
			      if (month==1 && leap_year==TRUE) ndays++;
			      date=ndays - (total-jdate);
			      month=month+1;
			      sprintf(cdate,"%02d%02d%04d",month,date,year);
			      return cdate;
			     }
			  }
	    }

	 return NULL;
}
