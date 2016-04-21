/*=========================================================================*/
/*                              FILE NAME:   siii_shared.h                 */
/*                                                                         */
/*                              HEADER FILE                                */
/*                                                                         */
/*   contains declarations shared between stageiii and post analysis       */
/*=========================================================================*/

short int durcode;

char           *LOGNAME;

/*---------------------------*/
/*  levels,colors for legend */
/*---------------------------*/

char        app_name[21];
float       level_value[30];

char     *color_list_overlays[30];
char     *color_list_levels[30];

void    set_colorvalues();
void    set_coloroverlays();

/*-------------------------------------*/
/*  routines for updating gage values  */
/*-------------------------------------*/

void    update_gagrad(char[], char[], short, float, float *, int *, char[][4], long int *);
void    update_pseudo(char[], char[], short, float, int *, char[][4], long int *);
void    update_precip(char [], char[], char [], short *, float, int *);
void    insert_reject(char [], char[], char [], short, float *);
