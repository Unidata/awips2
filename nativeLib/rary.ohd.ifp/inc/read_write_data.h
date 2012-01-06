/*=========================================================================*/
/*                    FILE PATH/NAME:   /home/lef/s3/stage3.h              */
/*                                                                         */
/*                              HEADER FILE                                */
/*                                                                         */
/*=========================================================================*/

#ifndef read_write_data_h
#define read_write_data_h

#define PI 3.141593


void            initialize_data();
void            create_gage_table();
void            Mosaic();
void            ReadGageData();
void            ReadRadarData();
void            ReadParameters();
void            writemosaic();

int             CalculateDifference();
int             Difference();
int             Interactive();
int             InOut();
int             smax();
int             WriteGageData();
char *          build_filename();
char *          GetSource();

char            ***loc_basin;
char            ***loc_cty;
char            *HOME;

int             NRADARS;
int             **datafile;
int             dbg;
int             ngages;
int             nummap;
int             numfg;
int             numrivers;
int             numrfc;
int             numstates;
int             GAGERAD;
int             MAXDIFF;
int             NSCANS;
int             ORD_FG;
int             ORD_MOS;
int             ORD_SS;
int             ORD_ZM;
int             XOR;
int             YOR;
int             MAXX;
int             MAXY;
int             num_gage_edit, num_rad_edit;
int             istate, icity, iriver, ibound, icounty;
int             save_gif;
int             numcounty;
int             numpseudo;

short int       ***gageonly;
short int       **MosBuf;
short int       ***Multi;

#endif







