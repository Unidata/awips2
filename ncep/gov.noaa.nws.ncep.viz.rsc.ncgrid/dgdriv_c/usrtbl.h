/************************************************************************
 * usrtbl.h                                                             *
 *                                                                      *
 * This file contains structure definitions used by GEMPAK to store     *
 * user settings retrieved from "users.tbl"				*
 *                                                                      *
 **                                                                     *
 * E. Wehner/EAi        3/97    Created					*
 ***********************************************************************/


#define MAX_USERS		20
#define LEN_USERNAME		20
#define LEN_FNAME		100



/* 
 * Users info - Information for each user that can be accessed through
 * product generation.
 */
typedef struct user_data
{
    int         user_id;
    char 	user_text[LEN_USERNAME];
    char	user_fname[LEN_FNAME];
    char 	user_pallette[LEN_USERNAME];
} user_t[MAX_USERS], sglusr_t;



