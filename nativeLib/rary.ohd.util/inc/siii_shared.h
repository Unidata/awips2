/*=========================================================================*/
/*  FILE NAME:   siii_shared.h  			                   */
/*                                                                         */
/*                                                                         */
/*   contains declarations shared between stageiii and post analysis       */
/*=========================================================================*/

#ifndef SIII_SHARED_H
#define SIII_SHARED_H

short int durcode;

/*---------------------------*/
/*  levels,colors for legend */
/*---------------------------*/

float       level_value[30];

char     color_list_levels[40][26];

void    set_colorvalues();
void    set_coloroverlays();

/*-------------------------------------*/
/*  routines for updating gage values  */
/*-------------------------------------*/

void    update_gagrad(char[], char[], short, float, float *, int *, 
                      char[][4], long int *);
void    update_pseudo(char[], char[], short, float, int *, char[][4],
                      long int *);
void    update_precip(char [], char[], char [], short *, float, int *);
void    insert_reject(char [], char[], char [], short, float *);


#endif
