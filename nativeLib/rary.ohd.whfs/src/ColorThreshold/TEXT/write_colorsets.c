#include <unistd.h>
#include <stdio.h>
#include "write_colorsets.h"

/* 
Used to output default colors/scales to a specified output file
*/

void writeColorDataSetToOutputFile( FILE * outputFilePtr,
                                    const char * colorUseDisplayName,
                                    const char * colorUseDatabaseName,
                                    const char * colorNames[], const double scaleValues[],
                                    int numOfValues )
{
    int i = 0;

    fprintf( outputFilePtr, "%s|%s|%d\n", colorUseDisplayName, colorUseDatabaseName, numOfValues );
    for ( i = 0; i < numOfValues; i++ )
    { 
        fprintf( outputFilePtr, "%s %1.2f\n", colorNames[ i ], scaleValues[ i ] );
    }
    fprintf( outputFilePtr, "\n" ); 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/whfs_lib/src/ColorThreshold/RCS/write_colorsets.c,v $";
 static char rcs_id2[] = "$Id: write_colorsets.c,v 1.1 2007/07/17 14:00:20 gsood Exp $";}
/*  ===================================================  */

}


