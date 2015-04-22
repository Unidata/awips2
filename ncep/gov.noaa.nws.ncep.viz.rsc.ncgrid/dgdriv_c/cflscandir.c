#include "geminc.h"
#include "gemprm.h"


char _cflrdirPath[LLPATH];	/* directory path */
char *_cflrdirSchstr=NULL;	/* search string */


int cfl_scandir ( const char *dir, const char *search,
			int (*filter)(const struct dirent *),
			int (*compar)(const void *, const void *),
			struct dirent ***namelist )
/************************************************************************
 * cfl_scandir								*
 *									*
 * This function scans the directory 'dir', calling 'filter()' on each	*
 * directory entry.  Entries for which 'filter()' returns non-zero are	*
 * stored in strings allocated via 'malloc()', sorted using 'qsort()'	*
 * with	the comparison function 'compar()', and collected in an arrary	*
 * 'namelist' which is allocated via 'malloc()'.  If 'filter()' is NULL,*
 * all entries are selected.  						*
 *									*
 * NOTE: The directory name, 'dir', may include an environment variable.*
 *									*
 * The 'search' parameter is either a search pattern for the pre-defined*
 * filters 1 to 3 to further refine the filter OR a template for 	*
 * pre-defined filter 4, '_selecttmplt'.  A match is made when a file or*
 * subdirectory name contains the search pattern string exactly as given*
 * OR matches the template in filter 4.  See 'cst_ptmt' for more details*
 * on how to create a template.          				*
 *									*
 * NOTE: Wild-card characters are not supported in the search parameter.*
 *	 The 'search', 'filter', and 'compar' parameters can be NULL.	*
 *	 See 'man scandir' for more info.   				*
 * 									*
 * Pre-defined 'filter' functions:					*
 * 1)  _selectdir	Excludes all '.' files				*
 * 2)  _selectdirOnly	Excludes all '.' files & all non-directory files* 
 * 3)  _selectdirExcd	Excludes all '.' files & all directory files	*
 * 4)  _selecttmplt	Excludes all '.' files & files that do NOT match*
 *			    the template given in the 'search' parameter*
 *									*
 * Pre-defined 'compar' functions:					*
 * 1)  _alphasort 	Performs alphabetical sort by strcmp		*
 * 2)  _ralphasort 	Performs reverse alphabetical sort by strcmp	*
 * 3)  _datesort 	Performs sort based upon modified time of file	*
 * 4)  _rdatesort 	Performs reverse date sort			*
 *									*
 * int cfl_scandir ( search, dir, filter, compar, namelist )		*
 *									*
 * Input parameters:							*
 *	*dir		const char	Directory name			*
 *	*search		const char	Search pattern			*
 *	*filter()	int		Filter function for scandir	*
 *	*compar()	int		Compare function for scandir	*
 *									*
 * Output parameters:							*
 *	***namelist	struct dirent	Directory content		*
 *	cfl_scandir	int		Number of entries in namelist	*
 **									*
 * Log:									*
 * T. Piper/SAIC	04/07	Created.  Modelled after cfl_rdir	*
 * m.gamazaychikov/CWS	01/10	Added AWIPS II DB functionality 	*
 ***********************************************************************/
{
    int iret, n_entries=0;
/*---------------------------------------------------------------------*/

   /*
    * If the directory name contains A2DB get the directory content from
    * db functions
    if ( strstr (dir, "A2DB") != NULL ) {
          n_entries = db_gFileNames ( search, namelist);
    }
    else {
    */
       css_envr(dir, _cflrdirPath, &iret);
       if ( iret == G_NORMAL ) {
	  if ( search != NULL ) {
	      G_MALLOC(_cflrdirSchstr, char, strlen(search) + 1,
				 "cfl_scandir:  _cflrdirSchstr");
	      strcpy(_cflrdirSchstr, search);
	  }
#ifdef HPUX
	  n_entries = scandir(_cflrdirPath, namelist, filter,
	     (int(*)(const struct dirent **, const struct dirent **))compar);
#else
	  n_entries = scandir(_cflrdirPath, namelist, filter, compar);
#endif
	  if ( _cflrdirSchstr != NULL ) G_FREE(_cflrdirSchstr, char);
       }
    //}
    return(n_entries);
}

/*=====================================================================*/

/************************************************************************
 *                                                                      *
 *      This is the start of the filter functions for scandir.          *
 *                                                                      *
 ***********************************************************************/

#ifdef IRIX
int _selectdir ( struct dirent *check )
#else
int _selectdir ( const struct dirent *check )
#endif
/************************************************************************
 * _selectdir								*
 *									*
 * This function is a filter for SCANDIR.  It allows SCANDIR to search	*
 * only for the files and subdirectories that match the given criteria.	*
 *									*
 * int _selectdir ( check )						*
 *									*
 * Input parameters:							*
 *	*check	const struct dirent	Structure used by SCANDIR	*
 *									*
 * Output parameters:							*
 *	_selectdir	int		Decision about the given entry	*
 *					  0 = entry does not match	*
 *					  1 = entry matches		*
 **									*
 * Log:									*
 * T. Piper/SAIC	04/07	Modified from cfl_rdir			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    if (check->d_name[0] == '.')
/*
 *  Exclude all the files starting with '.'.
 */
	return(0);
    else if ( _cflrdirSchstr == NULL )
/*
 *  Include all files if no search string.
 */
	return(1);
    else if (strstr(check->d_name, _cflrdirSchstr) != NULL)
/*
 *  Include those files that do include the substring _cflrdirSchstr.
 */
	return(1);
    else
/*
 *  Exclude those files that do not include the substring _cflrdirSchstr.
 */
	return(0);
}

/*=====================================================================*/

#ifdef IRIX
int _selectdirOnly ( struct dirent *check )
#else
int _selectdirOnly ( const struct dirent *check )
#endif
/************************************************************************
 * _selectdirOnly							*
 *									*
 * This function is a filter for SCANDIR.  It allows SCANDIR to search	*
 * only for the subdirectories that match the given criteria.		*
 *									*
 * int _selectdirOnly ( check )						*
 *									*
 * Input parameters:							*
 *	*check	const struct dirent	Structure used by SCANDIR	*
 *									*
 * Output parameters:							*
 *	_selectdirOnly	int		Decision about the given entry	*
 *					  0 = entry does not match	*
 *					  1 = entry matches		*
 **									*
 * Log:									*
 * T. Piper/SAIC	04/07	Modified from cfl_rdir			*
 ***********************************************************************/
{
char fname[FILE_FULLSZ];
struct stat buf;

/*---------------------------------------------------------------------*/
    if (check->d_name[0] == '.')
/*
 *  Exclude all the files starting with '.'.
 */
	return(0);
    else {
	sprintf(fname, "%s/%s", _cflrdirPath, check->d_name);
	if ( stat(fname, &buf) != 0 )
/*
 *  Exclude file; cannot read its attributes.
 */
	    return(0);
	else if (!S_ISDIR(buf.st_mode))
/*
 *  Exclude all non-directory files.
 */         
	    return(0);
	else if (_cflrdirSchstr == NULL)
/*
 *  Include all subdirectories if no search string.
 */
	    return(1);
	else if (strstr(check->d_name, _cflrdirSchstr) != NULL)
/*
 *  Include those files that do include the substring _cflrdirSchstr.
 */
	    return(1);
	else
/*
 *  Exclude those files that do not include the substring _cflrdirSchstr.
 */
        return(0);
    }
}

/*=====================================================================*/

#ifdef IRIX
int _selectdirExcd ( struct dirent *check )
#else
int _selectdirExcd ( const struct dirent *check )
#endif
/************************************************************************
 * _selectdirExcd							*
 *									*
 * This function is a filter for SCANDIR.  It allows SCANDIR to search	*
 * entries other than subdirectories and also match the given criteria.	*
 *									*
 * int _selectdirExcd ( check )						*
 *									*
 * Input parameters:							*
 *	*check	const struct dirent	Structure used by SCANDIR	*
 *									*
 * Output parameters:							*
 *	_selectdirExcd	int		Decision about the given entry	*
 *					  0 = entry does not match	*
 *					  1 = entry matches		*
 **									*
 * Log:									*
 * T. Piper/SAIC	04/07	Modified from cfl_rdir			*
 ***********************************************************************/
{
char fname[FILE_FULLSZ];
struct stat buf;

/*---------------------------------------------------------------------*/
    if (check->d_name[0] == '.')
/*
 *  Exclude all the files starting with '.'.
 */
	return(0);
    else {
	sprintf(fname, "%s/%s", _cflrdirPath, check->d_name);
	if ( stat(fname, &buf) == 0 ) {
	    if (!S_ISDIR(buf.st_mode)) {
/*
 *  If file is NOT a directory, continue...
 */
		if (_cflrdirSchstr != NULL) {
		    if (strstr(check->d_name, _cflrdirSchstr) != NULL) {
/*
 *  A valid entry is found when the substring _cflrdirSchstr is present 
 *  in the file name.
 */
			return(1);
		    }
		    else {
			return(0);
		    }
		}
		else {
/*
 *  A valid entry is found when _cflrdirSchstr not present.
 */                 
		    return(1);
		}
	    }
	    else {
		return(0);
	    }
	}
	else {
	    return(0);
	}
    }
}

/*=====================================================================*/

#ifdef IRIX
int _selecttmplt ( struct dirent *check )
#else
int _selecttmplt ( const struct dirent *check )
#endif
/************************************************************************
 * _selecttmplt								*
 *                                                                      *
 * This function is a filter for SCANDIR.  It allows SCANDIR to search  *
 * entries other than subdirectories and also match the given criteria. *
 *                                                                      *
 * int _selecttmplt ( check )                                         	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *check		const struct dirent  Structure used by SCANDIR  *
 *                                                                      *
 * Output parameters:                                                   *
 *      _selecttmplt	int	Decision about the given entry  	*
 *                                        0 = entry does not match      *
 *                                        1 = entry matches             *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	04/07	Created					*
 ***********************************************************************/
{
    int ier, match;

/*---------------------------------------------------------------------*/
    if (check->d_name[0] == '.')
/*
 *  Exclude all the files starting with '.'.
 */
        return(0);
    else if ( _cflrdirSchstr == NULL )
/*
 *  Include all files if no template specfied.
 */
	return(1);
    else {
	cst_ptmt(check->d_name, _cflrdirSchstr, &match, &ier);
/*
 *  Include or exclude files based upon template match.
 */
	return(match);
    }
}

/*=====================================================================*/

/************************************************************************
 *									*
 *	This is the start of the compare functions for scandir.		*
 *									*
 ***********************************************************************/

#ifdef IRIX
int _alphasort ( void *vp1, void *vp2 )
#else
int _alphasort ( const void *vp1, const void *vp2 )
#endif
/************************************************************************
 * _alphasort                                                       	*
 *                                                                      *
 * This function is a compare function for SCANDIR.  It uses strcmp to	*
 * sort the directory names in alphabetical order.			*
 *                                                                      *
 * int _alphasort ( *vp1, *vp2 )                                      	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*vp1		void		compare string 1		*
 *	*vp2		void		compare string 2		*
 *                                                                      *
 * Output parameters:                                                   *
 *      _alphasort	int		Decision about the given entry  *
 *					 -1 = s1 less than s2		*
 *                                        0 = s1 equals s2		*
 *                                        1 = s1 greater than s2	*
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	04/07	Created					*
 ***********************************************************************/
{
        const struct dirent **d1, **d2;
/*---------------------------------------------------------------------*/
    d1 = (const struct dirent **)vp1;
    d2 = (const struct dirent **)vp2;
    return( strcmp( (*d1)->d_name, (*d2)->d_name ) );
}

/*=====================================================================*/

#ifdef IRIX
int _ralphasort ( void *vp1, void *vp2 )
#else
int _ralphasort ( const void *vp1, const void *vp2 )
#endif
/************************************************************************
 * _ralphasort                                                          *
 *                                                                      *
 * This function is a compare function for SCANDIR.  It uses strcmp to  *
 * sort the directory names in reverse alphabetical order.              *
 *                                                                      *
 * int _ralphasort ( *vp1, *vp2 )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *	*vp1		void		compare string 1                *
 *	*vp2		void		compare string 2                *
 *                                                                      *
 * Output parameters:                                                   *
 *	_ralphasort	int		Decision about the given entry  *
 *                                       -1 = s1 less than s2           *
 *                                        0 = s1 equals s2              *
 *                                        1 = s1 greater than s2        *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        04/07   Created                                 *
 ***********************************************************************/
{
    const struct dirent **d1, **d2;
/*---------------------------------------------------------------------*/
    d1 = (const struct dirent **)vp1;
    d2 = (const struct dirent **)vp2;
    return( strcmp( (*d2)->d_name, (*d1)->d_name ) );
} 

/*=====================================================================*/

#ifdef IRIX
int _datesort ( void *vp1, void *vp2 )
#else
int _datesort ( const void *vp1, const void *vp2 )
#endif
/************************************************************************
 * _datesort                                                            *
 *                                                                      *
 * This function is a compare function for SCANDIR.  It sorts the	*
 * directory files by date with the newest date first.           	*
 *                                                                      *
 * int _datesort ( *vp1, *vp2 )                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      *vp1            void            compare string 1                *
 *      *vp2            void            compare string 2                *
 *                                                                      *
 * Output parameters:                                                   *
 *      _datesort      int             Decision about the given entry	*
 *                                       -1 = s1 less than s2           *
 *                                        0 = s1 equals s2              *
 *                                        1 = s1 greater than s2        *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        04/07   Created                                 *
 ***********************************************************************/
{
    char fname[FILE_FULLSZ];
    struct stat buf1, buf2;
    const struct dirent **d1, **d2;
/*---------------------------------------------------------------------*/
/*
 *  Retrieve file info for file 1.
 */
    d1 = (const struct dirent **)vp1;
    sprintf(fname, "%s/%s", _cflrdirPath, (*d1)->d_name);
    stat(fname, &buf1);

/*
 *  Retieve file info for file 2.
 */
    d2 = (const struct dirent **)vp2;
    sprintf(fname, "%s/%s", _cflrdirPath, (*d2)->d_name);
    stat(fname, &buf2);

/*
 *  Compare the modification times.
 */
    if ( buf1.st_mtime < buf2.st_mtime ) {
	return(-1);
    }
    else if ( buf1.st_mtime == buf2.st_mtime ) {
	return (0);
    }
    else {
	return (1);
    }
}

/*=====================================================================*/

#ifdef IRIX
int _rdatesort ( void *vp1, void *vp2 )
#else
int _rdatesort ( const void *vp1, const void *vp2 )
#endif
/************************************************************************
 * _rdatesort                                                           *
 *                                                                      *
 * This function is a compare function for SCANDIR.  It sorts the       *
 * directory files by date with the oldest date first.            	*
 *                                                                      *
 * int _rdatesort ( *vp1, *vp2 )                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *vp1            void            compare string 1                *
 *      *vp2            void            compare string 2                *
 *                                                                      *
 * Output parameters:                                                   *
 *      _rdatesort      int             Decision about the given entry  *
 *                                       -1 = s1 less than s2           *
 *                                        0 = s1 equals s2              *
 *                                        1 = s1 greater than s2        *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        04/07   Created                                 *
 ***********************************************************************/
{
    char fname[FILE_FULLSZ];
    struct stat buf1, buf2;
    const struct dirent **d1, **d2;
/*---------------------------------------------------------------------*/
/*
 *  Retrieve file info for file 1.
 */
    d1 = (const struct dirent **)vp1; 
    sprintf(fname, "%s/%s", _cflrdirPath, (*d1)->d_name);
    stat(fname, &buf1); 

/*
 *  Retieve file info for file 2.
 */
    d2 = (const struct dirent **)vp2; 
    sprintf(fname, "%s/%s", _cflrdirPath, (*d2)->d_name);
    stat(fname, &buf2); 

/*
 *  Compare the modification times.
 */
    if ( buf2.st_mtime < buf1.st_mtime ) {
        return(-1);
    }
    else if ( buf1.st_mtime == buf2.st_mtime ) {
        return (0);
    }
    else {
        return (1);
    }
}
