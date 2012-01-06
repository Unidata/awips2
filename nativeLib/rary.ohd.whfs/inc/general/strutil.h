/******************************************************************************
 *
 * Header: strutil.h
 *
 * Description: This header contains prototypes for the strutil module
 *
 * Created by: Heather Friedeman 
 *
 * History:
 *
 * Bryon Lawrence       09/28/2004         Moved the chgupper and the chglower
 *                                         routines to the GeneralUtil 
 *                                         library.
 *****************************************************************************/

/* $Id: strutil.h,v 1.2 2004/12/10 20:36:16 lawrence Exp $ */

/* check if string is blank */

extern int is_blank(char *str);

/* remove non space characters from a string */

extern void strip_leading_blanks(char *str);
extern void strip_trailing_blanks(char *str);
extern void strip_leading_trailing_blanks(char *str);

/* convert a string into a float value */

extern int getFloat(char *,float *);



