        SUBROUTINE DG_NFIL  ( gdfile, gdoutf, iret ) 
C************************************************************************
C* DG_NFIL                                                              *
C*                                                                      *
C* This subroutine opens grid files and initializes the grid            *
C* diagnostics package.							*
C*                                                                      *
C* DG_NFIL  ( GDFILE, GDOUTF, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*      GDFILE          CHAR*           Grid file name or template      *
C*      GDOUTF          CHAR*           Output grid file name           *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -30 = error opening file        *
C*                                      -33 = too many files to open    *
C*                                      -51 = path associated with      *
C*                                            template does not exist	*
C*                                      -62 = grid file open failed     *
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC		 3/06	Fortran wrapper of DGC_NFIL		*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER*(*)   gdfile, gdoutf
C*
	CHARACTER	tmpgdi*(LLMXLN), tmpgdo*(LLMXLN)
C------------------------------------------------------------------------
	CALL ST_NULL ( gdfile, tmpgdi, nt, ier )
	print *, 'DG_NFIL LLMXLN = ', LLMXLN
	print *, 'DG_NFIL ST_NULL 1, ier = ', ier
	CALL ST_NULL ( gdoutf, tmpgdo, nt, ier )
	print *, 'DG_NFIL ST_NULL 2, ier = ', ier
	print *, 'DG_NFIL gdfile = ', gdfile
	CALL DGC_NFIL ( tmpgdi, tmpgdo, iret )
	print *, 'DG_NFIL DGC_NFIL, ier = ', iret
C*
        RETURN
        END
