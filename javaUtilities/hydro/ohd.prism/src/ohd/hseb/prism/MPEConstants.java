package ohd.hseb.prism;

public interface MPEConstants
{
   double PI=3.141592654;
   double D2RAD=PI/180.0;
   double EARTHR=6371.2;
   double REF_LAT=60.0;
   double REF_LON=105.0;
   double RMESH=4.7625;
   double TLAT=REF_LAT*D2RAD;
   double RE=(EARTHR * ( 1.0 + Math.sin(TLAT)))/RMESH;
   double HRAP_BASE_COL=401.0;
   double HRAP_BASE_ROW=1601.0;
   double MISSING=-9999.0;
   double MISSING_SAVE=-999.0;
   int MINIMUM_COMMAND_LINE_ARGS=3;
}
