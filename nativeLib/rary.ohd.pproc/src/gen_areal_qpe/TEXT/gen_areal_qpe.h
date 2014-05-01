#ifndef GEN_AREAL_QPE_H
#define GEN_AREAL_QPE_H

#include <float.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "mpe_write_xmrg.h"
#include "time_convert.h"

/* defines */
#define PRECIP_FACTOR 100.0
#define FAIL 1
#define SUCCESS 0
#define FOUND 1
#define NOT_FOUND 0
#define RFC_MASK_ROWS 881
#define RFC_MASK_COLS 1121
#define RFC_MASK_MISSING_VALUE -9999

#define MISSING_VAL -9999.

#define MAX_RFCS     13

#define MAX_DURATIONS 5

#define MAX_PATH_LEN 200
#define MAX_FILE_LEN 200 

#define PROC_FLAG_LEN 8

#define SITE_ID_LEN  10

#define RFC_QPE_BASE_STRING "RFCMOSAIC"
#define XMRG_MISSING -999

#define YEAR_LEN 4
#define MONTH_LEN 2
#define DAY_LEN 2
#define HOUR_LEN 2

#define GAQ_NETCDF_QPE1 "qpe1"
#define GAQ_NETCDF_QPE6 "qpe6"
#define GAQ_NETCDF_QPE24 "qpe24"

#define FILL_VALUE_FLOAT FLT_MAX 

/* define a structure for holding all the user options */

typedef struct
{
   int	num_rfcs;
   char rfcid[MAX_RFCS][RFC_NAME_LEN +1 ];
   int	num_durations;
   int	durations[MAX_DURATIONS];
   
   /* Path to the RFC netCDF QPE files */
   char	input_data_path[MAX_PATH_LEN + 1];

   /* Path to the cropped 1 hr QPE xmrg files. */
   char input_xmrg_path_1hr[MAX_PATH_LEN + 1];

   /* Path to the cropped 6 hr QPE xmrg files. */
   char input_xmrg_path_6hr[MAX_PATH_LEN + 1];

   /* Path to the cropped 24 hr QPE xmrg files. */
   char input_xmrg_path_24hr[MAX_PATH_LEN + 1];

   /* Path to the temporary QPE xmrg directory. */
   char	output_xmrg_path[MAX_PATH_LEN + 1];
   
   /* Path to the coord.dat file for this office. */
   char	geo_coord_path[MAX_PATH_LEN + 1];  

   /* The id of this office. */
   char site_id[SITE_ID_LEN + 1];  


}  gaq_options_struct;

/* Define the enumeration for the RFCs.  The values in this enumeration
   are based on those used within the NPVU RFC mask. */
enum RFCmaskValue { ABRFC_MASK=150,
                    APRFC_MASK=151,
                    CBRFC_MASK=152,
                    CNRFC_MASK=153,
                    LMRFC_MASK=154,
                    MARFC_MASK=155,
                    MBRFC_MASK=156,
                    NCRFC_MASK=157,
                    NERFC_MASK=158,
                    NWRFC_MASK=159,
                    OHRFC_MASK=160,
                    SERFC_MASK=161,
                    WGRFC_MASK=162,
                    NUM_RFC_MASK_VALUES=13 };

/* define a structure for holding the file info */
typedef struct
{
  char 		    name[MAX_FILE_LEN + 1];
  char 		    rfc[RFC_NAME_LEN + 1];
  char              proc_flag[ PROC_FLAG_LEN + 1 ];
  int 		    dur;
  time_t	    endtime;
  enum RFCmaskValue mask_value;
} qpe_info_struct;

/* The structure which maps an RFC name to a NPVU mask file value. */
typedef struct 
{
   char * rfc_name;
   enum RFCmaskValue mask_value;
}  rfc_mask_val_map;

/* prototypes */

int process_qpe_files(const gaq_options_struct * options);


int process_single_qpe(const gaq_options_struct * options,
                       int rfc_mask [][RFC_MASK_COLS],
                       const geo_data_struct * coord_info,
		       const qpe_info_struct * qpe_info,
                       float ** pXmrgGrid );
			
void get_coord_info(const gaq_options_struct * options,
                    geo_data_struct	*coord_info);

void get_rfc_mask ( int rfc_mask [ ] [ RFC_MASK_COLS ] );

const char * build_saved_xmrg_filename ( const gaq_options_struct * options,
                                         const qpe_info_struct * 
                                         qpe_info_struct );

const char * build_temp_xmrg_filename ( const gaq_options_struct * options,
                                        const qpe_info_struct * 
                                        qpe_info_struct );

int read_saved_xmrg_grid ( const gaq_options_struct * options,
                           const geo_data_struct * coord_info,
                           const qpe_info_struct * qpe_info_struct,
                           float ** pXmrgGrid);

int check_if_saved_mosaic ( const gaq_options_struct * options,
                            const qpe_info_struct * qpe_info );

int process_rfc_qpe_grid ( const gaq_options_struct * options,
                           int rfc_mask[][RFC_MASK_COLS],
                           const geo_data_struct * coord_info,
                           const qpe_info_struct * qpe_info,
                           float ** pXmrgGrid );

int write_saved_xmrg_grid ( const gaq_options_struct *  options,
                            const geo_data_struct * coord_info,
                            const qpe_info_struct * qpe_info,
                            float ** pXmrgGrid);


/* prototype for FORTRAN functions */

#define LLGD llgd_

void LLGD();


				  			  
#endif
