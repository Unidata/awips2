#ifndef WRITE_FFG_NETCDF_H
#define WRITE_FFG_NETCDF_H

#include <netcdf.h>
#include <stdio.h>
#include <stdlib.h>

/* function prototype */
int writeFFGnetCDF(char *fileName, float zeroLat, float zeroLon,
		   int xSize, int ySize, unsigned char *ucGrid);

#endif
