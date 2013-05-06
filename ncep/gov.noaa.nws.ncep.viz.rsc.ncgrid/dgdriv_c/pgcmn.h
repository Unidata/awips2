
/************************************************************************
 * pgcmn.h  								*
 * This header file contains structures and globals common to product	*
 * generation.								*
 *                                                                      *
 **                                                                     *
 * Log:									*
 * S. Law/GSC		02/00	created					*
 * S. Law/GSC		04/00	moved MAXTBLNAME from nmap_pgsigw.c	*
 * R. Curtis/EAI	05/00	changed MAXNOPT from 20 to 30           *
 * E. Safford/GSC	12/00	move group table structures here	*
 * M. Li/GSC		12/00	added color to group table structure	*
 * S. Jacobs/NCEP	 2/01	Changed MAXNOPT from 30 to 50		*
 * H. Zeng/EAI          03/01   removed group type stuff                *
 ***********************************************************************/
#ifndef PGCMN_H
#define PGCMN_H


/*
 *  option menu structures and definitions
 */

#define MAXTBLNAME	 80	/* max size for any name from table */
#define MAXNOPT		 50	/* max number of option menu items  */

struct optMenuStrc {
    int		current;
    Widget	form;
    Widget	label;
    Widget	menu;
    Widget	pb[MAXNOPT];
};

void    pgutls_setOptMenu (     char            curr_label[],
                                char            labels[][MAXTBLNAME],
                                int             nlabels,
                                struct optMenuStrc *omstrc );


#endif
