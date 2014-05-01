/*****************************************************************************************
 * COPYRIGHT (c), 2009, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

/*
 * Header for thin wrapper around NCEP decoder
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

#ifndef __g2clibWrapper_H__
#define __g2clibWrapper_H__
#include <stdlib.h>
#include <string.h>
#include "grib2.h"


extern void baopenr_(int* arg1, char fileName[],int *ios);

int getMetadata(FILE * fptr, int recordNumber,int fieldNumber, int metadata[]);

int getData(FILE * fptr, int recordNumber, int fieldNumber, int idSection[],
	int localUseSection[], int gdsTemplate[],int pdsTemplate[],float data[], 
	int bitMap[], int list_opt[],float coord_list[]);

int getRecord(FILE * fptr,gribfield ** gfld, int recordNumber, int fieldNumber, int unpack);

#endif
