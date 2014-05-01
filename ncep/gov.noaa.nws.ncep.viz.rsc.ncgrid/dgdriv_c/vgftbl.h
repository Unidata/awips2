/************************************************************************
 * vgftbl.h								*
 *                                                                      *
 * This header file defines the VGF user table structures.		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		05/97						*
 * S. Law/GSC		 3/00	Removed usrnam from the data structure	*
 * S. Jacobs/NCEP	 3/00	Renamed					*
 ***********************************************************************/

/*
 * VGF user info data struct
 */
typedef struct {
        char	*title;		/* title name */
        char	*usrpath;	/* full path to this user's VGF dir */
} vgfusr_ent_t;

/*
 * VGF user info table struct
 */
typedef struct {
    int			nitems;	/* total # of users*/
    vgfusr_ent_t	*items;	/* pointer to the array of user items */
} vgfutbl_t;

#ifdef VGFTBL_GLOBAL

vgfutbl_t _vgfUsrTbl;

#else

extern vgfutbl_t _vgfUsrTbl;

#endif

void vtbl_readUsrTbl (	char		*tblname,
			int		*iret );
