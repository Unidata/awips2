/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef READ_STAGE1_DATA_H
#define READ_STAGE1_DATA_H

#define NUM_DPA_COLS 131
#define NUM_DPA_ROWS 131
#define NUM_DPA_ELEMENTS ( NUM_DPA_COLS * NUM_DPA_ROWS )

void read_stage1_decoded_ ( const char * filename ,
                            const int * fname_length ,
                            float radar [ ] [ NUM_DPA_COLS ] ,
                            int * ierr ) ;

void read_daa_decoded_file ( const char * filename ,
                            const int * fname_length ,
                            float radar [ ] [ NUM_DPA_COLS ] ,
                            int * ierr ) ;
#endif /* #ifndef READ_STAGE1_DATA_H */
