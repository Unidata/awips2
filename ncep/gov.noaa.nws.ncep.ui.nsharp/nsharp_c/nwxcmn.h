/************************************************************************
 * nwxcmn.h								*
 *									*
 * This header file declares and defines structures, variables and 	*
 * macros for use in the NWX program.					*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * C. Lin/EAI	 	 8/95						*	
 ***********************************************************************/

#define	MAXARRY	90
#define	MAXCOLR	3


struct date_time_info {
	int	year;
	int	month;
	int	day;
	int	hour;
	int	minute;
};

struct srch_strng_info {
	char	search_string[49];
	char	start_of_text[2][9];
	char	end_of_text[2][9];
};

struct data_file_info { 
	FILE	*fptr;
	char	filnam[133];
	int	file_len;
};

struct directory_info {
	char	dirpath[73];        
	int	nent;		    
	int	cfilnum;	    
	char	filnam[NFILES][73]; 
};
