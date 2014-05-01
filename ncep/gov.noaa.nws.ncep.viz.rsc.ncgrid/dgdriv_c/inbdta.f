	SUBROUTINE IN_BDTA  ( iret )
C************************************************************************
C* IN_BDTA								*
C*									*
C* This subroutine serves as a BLOCKDATA statement, initializing	*
C* variables in GEMPAK common blocks.  This subroutine is called	*
C* by IP_INIT.  If a GEMPAK program does not call IP_INIT, it must	*
C* call IN_BDTA directly.						*
C*									*
C* IN_BDTA  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 7/90	Initialize LUNGEM for lun flags		*
C* M. desJardins/NMC	 1/92	Initiaize mbchan			*
C* K. Brill/NMC		01/92	Remove CALL IN_BOLD			*
C* M. desJardins/NMC	 8/94	Initialize IESTAT			*
C* C. Lin/EAI		 8/95	Added IMFTYP				*
C* S. Jacobs/NCEP	 7/96	Added IFINVD				*
C* K. Tyle/GSC		12/96	Replace IESTAT with call to ER_STAT;	*
C*				added ER_INIT call			*
C* R. Tian/SAIC		 1/04	Added GD_INIT call			*
C* T. Piper/SAIC	12/07	Removed GD_INIT				*
C* T. Lee/NCEP		 4/10	Write error message to buffer		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'ADBUFF.CMN'
	INCLUDE		'IMGDEF.CMN'
C------------------------------------------------------------------------
	iret= 0
C
C*	Initialize IM variables.
C
	imftyp = IFINVD
C
C*	Initialize DP variables.
C
	DO  i = 1, MMFLDP
	    iflgdp (i) = 0
	END DO
C
C*	Initialize IP variables.
C
	lunp = 0
	lunf = 0
C
C*	Initialize PC variables.
C
	tablrd = .false.
	dsflg  = .false.
	stnflg = .false.
	pptabl = .false.
C
C*	Initialize SF, SN, GD DM file numbers and LUNs.
C
	DO  i = 1, MMFILE
	    isfcfn (i) = -1
	    isndfn (i) = -1
	    igrdfn (i) = -1
	    lundm (i)  = -1
	END DO
C
C*	Initialize DM variables
C
	kclast = 0
	DO i = 1, MCACHE
	    kwrite (i) = .false.
	    kcflno (i) = 0
	    kcrecn (i) = 0
	END DO
C
C*	Initialize logical unit number flags.
C
	DO  i = 1, 10
	    lungem (i) = 0
	END DO
C
C*	Initialize channel number for GPLT mailbox, so that an error
C*	can be generated for missing call to GINITP.
C
	mbchan = -999
C
C*	Initialize ERCMN.CMN and ERCMN.H files.
C
	CALL ER_STAT ( 0, 1, .false., iret )
	CALL ER_INIT ( iret )
C*
 	RETURN
	END
