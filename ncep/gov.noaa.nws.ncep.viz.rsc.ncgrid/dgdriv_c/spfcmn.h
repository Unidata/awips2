/************************************************************************
 * SPFCMN.H								*
 *									*
 * This header file declares the global variables used in SPF library	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	 6/01	create						*
 * J. Wu/GSC	 4/02	increase data buffer length to 512 bytes	*
 * J. Wu/GSC	 4/02	increase data buffer length to 1024 bytes	*
 ***********************************************************************/

#define	DEF_DATA	"NONE"
#define	TAGDATA_BUF	1024

#ifdef SPF_GLOBAL

    char    *_spfBuffer;
    int     _initSPF;
    
#else

    extern char    *_spfBuffer;
    extern int     _initSPF;

#endif

