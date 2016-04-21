/****************************************************************************
//
//  writeFFGnetCDF.c
//
//  Description:
//      This free-standing function uses the netCDF c interface to
//      write FFG grid output data to netCDF files.
//
//   History
//	May 2002	R. Erb		created
//
*****************************************************************************/

#include <time.h>

#include "writeFFGnetCDF.h"

int writeFFGnetCDF(char *fileName, float zeroLat, float zeroLon,
		   int xSize, int ySize, unsigned char *ucGrid)
{
   int netCDFfileID;
   int xDim, yDim;
   int dimids[2];
   int imageId, validTimeId;
   int attId[2];
   int zeroLatId, zeroLonId;
   long startGridAt[2];
   long readGridTo[2];
   double product_sec;

   const int NETCDF_ERROR = -1;
   const int FAILED=0, SUCCESS=1;

time_t     currentTime = 0;

   /* Set NetCDF header variable to allow errors to be non-fatal and 
   non-verbose (so they won't write to the screen). */
   ncopts = 0;

   /* Create netCDF file and go into define mode. */
   netCDFfileID = nccreate(fileName, NC_CLOBBER);
   if (netCDFfileID == NETCDF_ERROR)
   {
	printf("Cannot create file '%s' -- NetCDF error # %d\n",
		fileName, ncerr);
	return FAILED;
   }

   /* Define netCDF dimensions */
   yDim = ncdimdef(netCDFfileID, "y", ySize); 
   xDim = ncdimdef(netCDFfileID, "x", xSize); 

  if (xDim == NETCDF_ERROR || yDim == NETCDF_ERROR)
   {
	printf("NetCDF error # %d defining NetCDF dimensions for '%s'\n",
		ncerr, fileName);
	if (ncclose (netCDFfileID) == NETCDF_ERROR)
	{
		printf("Cannot close '%s' fileName -- NetCDF error # %d\n",
			fileName, ncerr);
	}
	return FAILED;
   }

   dimids[0] = yDim;
   dimids[1] = xDim;

   /* Define netCDF variables. */
   imageId = ncvardef(netCDFfileID, "image", NC_BYTE, 2, dimids);
   validTimeId = ncvardef(netCDFfileID, "validTime", NC_DOUBLE, 0, 0);

   if (imageId == NETCDF_ERROR || validTimeId == NETCDF_ERROR) 
   {
	printf("NetCDF error # %d defining netCDF variables for %s\n",
		ncerr, fileName);
      if (ncclose (netCDFfileID) == NETCDF_ERROR)
      {
		printf("Cannot close '%s' fileName -- NetCDF error # %d\n",
			fileName, ncerr);
      }
      return FAILED;
   }

   /* Assign netCDF attributes for variables. */
   attId[0] = ncattput(netCDFfileID, validTimeId, "units", NC_CHAR, 39,
	(void *)"seconds since 1970-1-1 00:00:00.00 0:00");
   attId[1] = ncattput(netCDFfileID, validTimeId, "long_name", NC_CHAR,10,
	(void *)"Valid Time");

   if (attId[0] == NETCDF_ERROR || attId[1] == NETCDF_ERROR) 
   {
	printf("NetCDF error # %d writing netCDF attributes into %s\n",
		ncerr, fileName);
	if (ncclose (netCDFfileID) == NETCDF_ERROR)
	{
		printf("Cannot close '%s' fileName -- NetCDF error # %d\n",
			fileName, ncerr);
	}
	return FAILED;
   }  


   /* Assign netCDF global attributes for the whole file. */
   zeroLatId = ncattput(netCDFfileID, NC_GLOBAL, "lat00", NC_FLOAT, 1, (void *) &zeroLat);
   zeroLonId = ncattput(netCDFfileID, NC_GLOBAL, "lon00", NC_FLOAT, 1, (void *) &zeroLon);
   if (zeroLatId == NETCDF_ERROR || zeroLonId == NETCDF_ERROR)
   {
	printf("NetCDF error # %d writing netCDF global attributes into %s\n",
		ncerr, fileName);
	if (ncclose (netCDFfileID) == NETCDF_ERROR)
	{
		printf("Cannot close '%s' fileName -- NetCDF error # %d\n",
			fileName, ncerr);
	}
	return FAILED;
   }  



   /* End of define mode section. */
   if ((ncendef (netCDFfileID)) == NETCDF_ERROR)
   {
	printf("NetCDF error # %d leaving define mode in %s\n",
		ncerr, fileName);
	if (ncclose (netCDFfileID) == NETCDF_ERROR)
	{
		printf("Cannot close '%s' fileName -- NetCDF error # %d\n",
			fileName, ncerr);
	}
	return FAILED;
   }		
 
   /* Define where to write the values into the netCDF variable image */
   startGridAt[0] = 0;
   startGridAt[1] = 0;
   readGridTo[0] = ySize;
   readGridTo[1] = xSize;

   /* Store image grid. */
   if ((ncvarput (netCDFfileID, imageId, startGridAt, readGridTo, (void *) ucGrid)) == NETCDF_ERROR)
   {
	printf("NetCDF error # %d writing image array in %s\n",
		ncerr, fileName);
      
	if (ncclose (netCDFfileID) == NETCDF_ERROR)
	{
		printf("Cannot close '%s' fileName -- NetCDF error # %d\n",
			fileName, ncerr);
	}
	return FAILED;
   }		

   /* Store seconds since 0000Z Jan 1 1970 from input product time stamp. */
   time(&currentTime);
   /* Assign a "double" value for the Julian seconds. */
   product_sec = (double) currentTime;
   if (ncvarput1 (netCDFfileID, validTimeId, (long *)0, (void *) &product_sec) == NETCDF_ERROR)
   {
	printf("NetCDF error # %d writing time stamp in %s\n",
		ncerr, fileName);
   
	if (ncclose (netCDFfileID) == NETCDF_ERROR)
	{
		printf("Cannot close '%s' fileName -- NetCDF error # %d\n",
			fileName, ncerr);
	}
	return FAILED;
   }	

   /* Close the netCDF file. */
   if (ncclose (netCDFfileID) == NETCDF_ERROR)
   {
	printf("Cannot close '%s' fileName -- NetCDF error # %d\n",
		fileName, ncerr);

	return FAILED;
   }

   return SUCCESS;

} /* end of function writeFFGNetCdfFile */
