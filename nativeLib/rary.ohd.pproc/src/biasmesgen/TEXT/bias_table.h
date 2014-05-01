#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct         {
                         short int block_divdr;
                         short int block_id;
                         short int version_num;
                         short int block_len;
                         char site_id[4];
                         char radid[4];
                         short int obs_datehr[6];
                         short int gen_datehr[6];
                         short int nrows;
                         int mem_span;
                         int num_pairs;
                         int sumgag;
                         int sumrad;
                         int bias;
                       } bias_table_struct;

bias_table_struct        biastable;


void close_file(char[], FILE *);
int cnvrt1000(double);
void create_biastable_mesg();
void create_mesg_hdr( short int[], short int );
void get_apps_defaults();
void gen_filename(char [], char[], char[]);
void get_radarnum();                         
void get_site();
short int modif_julian(short int[]);
void read_rwbiasstat(    const char * fxa_local_site_id, 
                         float *min_gr_value,
                         int *npair_bias_select,
                         int *npair_svar_update,
                         int *std_cut,
                         int *lag_cut,
                         int *init_span,
                         int *bias_qc_opt,
                         int *num_span,
                         float mem_span_values[10]);
char * get_radar_bias_source ( const char * radar_id );

#define ID_LEN   4            /* Length of station IDs */
#define NROWS   10            /* No. Rows in Bias Table */
#define NCOLS    5            /* No. Cols in Bias Table */
