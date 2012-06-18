	SUBROUTINE GD_INIT  ( iret )
C************************************************************************
C* GD_INIT								*
C*									*
C* This subroutine initializes the grid library common area.	 	*
C*									*
C* GD_INIT  ( IRET )							*
C*									*
C* Input parameters:							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal              	*
C**									*
C* Log:									*
C* R. Tian/SAIC		 1/04						*
C* R. Tian/SAIC		 3/04		Modified init nflnum to MMFILE	*
C* R. Tian/SAIC		 3/05		Added init for mgrid		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'grdcmn.cmn'
C------------------------------------------------------------------------
	iret  = 0

	DO i = 1, MMFILE
	  mgrid (i) = 0
	  kgrid (i) = 0
	  gdwrt (i) = .false.
	  khdrln (i) = 0
	  lnavbl (i) = 0
	  lanlbl (i) = 0
	  ktgrid (i) = 0
	  gdflnm (i) = ' '
	  iflacc (i) = 0
 
	  DO j = 1, 256
	    savnav (j,i) = 0. 
	  END DO
 
	  DO j = 1, 128
	    savanl (j,i) = 0.
	  END DO
 
	  DO j = 1, LLMXGT
	    igdatm (1,j,i) = 0
	    igdatm (2,j,i) = 0
	    igdatm (3,j,i) = 0
	    ndattm (j,i) = 0
	  END DO
 
	  DO j = 1, MMHDRS
	    ksrtl (1,j,i) = 0
	    ksrtl (2,j,i) = 0
	  END DO
	END DO

	DO i = 1, LLGDHD
	  kbfhdr (i) = 0
	END DO

	nflnum = MMFILE
	nucode = .false.
C*
	RETURN
	END
