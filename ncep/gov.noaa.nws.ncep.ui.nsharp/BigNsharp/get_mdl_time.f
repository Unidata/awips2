	SUBROUTINE GET_MDL_TIMES(snfile, time_list, ntimf)
C
C 10/10/99 removed initialization stuff (IN_BDTA and GG_INIT) from 
C this routine so that we don't screw up initialization elsewhere
C -mkay
C  
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile
	CHARACTER	time_list(LLMXGT)*20
	CHARACTER	lasttm*20
	CHARACTER*512	gdcur
	REAL		anl(LLNANL), rnav(LLNNAV)
	LOGICAL  	ermiss
	INTEGER   	ntimf

	ermiss = .false.

C
C*	Open the input file.
C
        gdcur = ' '
CJL        CALL GR_OPEN(snfile, .false., gdcur , igdfln, lasttm,
CJL     +       anl, rnav, numgrd, maxgrd, newfile, ier)
CJL	print *, "get_mdl_time.f: GR_OPEN ier = ", ier
	CALL GD_OPEN(snfile, .false., 255, 255, igdfln,
     +            anl, rnav, maxgrd, ier)
CJL	print *, "get_mdl_time.f: GD_OPEN ier = ", ier
C
C*	Get input times and pointers.
C
	CALL GD_GTIM(igdfln, LLMXTM, time_list, ntimf, ier)
CJL	print *, "get_mdl_time.f: GD_GTIM ier = ", ier
CJL	print *, "get_mdl_time.f: GD_GTIM ntimf = ", ntimf
CJL	print *, "get_mdl_time.f: GD_GTIM igdfln = ", igdfln
C
	CALL GD_CLOS(igdfln, iret)
CJL	print *, "get_mdl_time.f: GD_CLOS iret = ", iret
C*
	END
