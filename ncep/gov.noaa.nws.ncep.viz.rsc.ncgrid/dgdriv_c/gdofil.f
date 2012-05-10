	SUBROUTINE GD_OFIL  ( filnam, wrtflg, shrflg, igdfln, navsz, 
     +			      rnvblk, ianlsz, anlblk, ihdrsz, maxgrd, 
     +			      iret )
C************************************************************************
C* GD_OFIL								*
C*									*
C* This subroutine opens an existing GEMPAK grid file.			*
C*									*
C* GD_OFIL  ( FILNAM, WRTFLG, SHRFLG, IGDFLN, NAVSZ, RNVBLK, IANLSZ,	*
C*            ANLBLK, IHDRSZ, MAXGRD, IRET )				*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name 			*
C*	WRTFLG		LOGICAL		Write access flag		*
C*	SHRFLG		LOGICAL		Shared access flag		*
C*									*
C* Output parameters:							*
C*	IGDFLN		INTEGER		File number			*
C*	NAVSZ		INTEGER		Navigation block length		*
C*	RNVBLK (NAVSZ)	REAL		Navigation block		*
C*	IANLSZ		INTEGER		Analysis block length		*
C*	ANLBLK (IANLSZ)	REAL		Analysis block			*
C*	IHDRSZ		INTEGER		Grid header length		*
C*	MAXGRD		INTEGER		Maximum number of grids		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = file open error		*
C*					 -7 = not a GEMPAK4 grid file	*
C*					 -8 = nav cannot be read	*
C*					-13 = grid header too long	*
C*					-15 = too many files open	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	 9/88	Added MAXGRD				*
C* M. desJardins/GSFC	11/88	Save analysis and navigations blocks	*
C* M. desJardins/GSFC	 4/89	Changed sorting				*
C* K. Brill/NMC		02/92	Use LLNNAV, LLNANL			*
C* K. Brill/HPC		12/03	Detect DM error -3 & return error -15	*
C* R. Tian/SAIC		 1/04	Added nucode check			*
C* R. Tian/SAIC		 3/04	Removed nucode check			*
C* T. Lee/SAIC		 1/05	Tidy error string			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C
	CHARACTER*(*) 	filnam
	LOGICAL		wrtflg, shrflg
	REAL		rnvblk (*), anlblk (*)
C
	CHARACTER	parm*4, kcolnm(10)*4, kfilnm(10)*4, fname*128
	LOGICAL		ok
C
	DATA		kcolnm / 'GDT1', 'GTM1', 'GDT2', 'GTM2', 
     +				 'GLV1', 'GLV2', 'GVCD', 'GPM1',
     +				 'GPM2', 'GPM3' /
c        real    t0, t1, elapsed
        CHARACTER       message*720, funcnm*8, loglevel*6
C------------------------------------------------------------------------
        loglevel = "debug"
        funcnm="GD_OFIL"
        CALL ST_NULL ( funcnm,   funcnm,   lenq, ier )
        CALL ST_NULL ( loglevel, loglevel, lenq, ier )
C*	Open the file.
C
	CALL ST_RMBL  ( filnam, fname, nf, ier  )
c        CALL CPU_TIME ( t0 )
        message = "calling dm_open with filnam=" // filnam
        CALL ST_NULL ( message,  message,  lenq, ier )
        CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
	CALL DM_OPEN  ( filnam, wrtflg, shrflg, igdfln, iftype, 
     +	                iflsrc, nrow, ncol, nprt, nfhdrs, iret )
        WRITE (message, 1001 ) iret
 1001   FORMAT ("after dm_open iret=", I5 )
        CALL ST_NULL ( message,  message,  lenq, ier )
        CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
        message = "after dm_open with filnam=" // filnam
        CALL ST_NULL ( message,  message,  lenq, ier )
        CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
c        CALL CPU_TIME ( t1 )
c        elapsed = (t1-t0)*1000000.0
c        print *, "Time spent in DM_OPEN =", elapsed,  " usec"
	IF ( iret .eq. -3 ) THEN
C
C*	    Return so that a file can be closed.
C
	    iret = -15
	    RETURN
	END IF
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DM', iret, fname (:nf), ier )
	    iret = -2
	    CALL ER_WMSG  ( 'GD', iret, fname (:nf), ier )
	    RETURN
	END IF
C
C*	Set MAXGRD.
C
	maxgrd = ncol
C
C*	Update common with this file number.
C
	igrdfn ( igdfln ) = igdfln
	gdwrt  ( igdfln ) = wrtflg
C
C*	Check that this is a grid file.
C
	IF  ( iftype .ne. MFGD )  THEN
	    iret = -7
	    CALL ER_WMSG  ( 'GD', iret, fname (:nf), ier )
	    CALL GD_CLOS  ( igdfln, ier )
	    RETURN
	END IF
C
C*	Check that one part contains grid data.
C
	CALL DM_PART  ( igdfln, 'GRID', lenhdr, ityprt, nparms, parm, 
     +			iscale, ioffst, nbits, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DM', iret, fname (:nf), ier )
	    iret = -7
	    CALL ER_WMSG  ( 'GD', iret, fname (:nf), ier )
	    CALL GD_CLOS  ( igdfln, ier )
	    RETURN
	  ELSE IF  ( lenhdr .gt. LLGDHD )  THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Save user header length in common.
C
	khdrln ( igdfln ) = lenhdr - 2
	ihdrsz = khdrln ( igdfln )
C
C*	Check that column names are correct.
C
	CALL DM_KEYS  ( igdfln, nrkeys, parm, nckeys, kfilnm, ier )
c       faking
        nrkeys = 1
        nckeys = 10

	ok = .true.
C	DO  i = 1, 10
	    IF  ( kfilnm (1) .ne. kcolnm (1) ) ok = .false.
C	END DO
c       faking
	ok = .true.
	IF  ( ( nrkeys .ne. 1 ) .or. ( nckeys .ne. 10 ) .or.
     +	      ( .not. ok ) )  THEN
	    CALL ER_WMSG  ( 'DM', iret, fname (:nf), ier )
	    iret = -7
	    CALL ER_WMSG  ( 'GD', iret, fname (:nf), ier )
	    CALL GD_CLOS  ( igdfln, ier )
	    RETURN
	END IF
C
C*	Retrieve navigation block.
C
	CALL DM_RFHR  ( igdfln, 'NAVB', LLNNAV, rnvblk, navsz, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -8
	    CALL ER_WMSG ( 'GD', iret, ' ', ier )
	    CALL GD_CLOS ( igdfln, ier )
	    RETURN
	  ELSE
	    DO  ij = 1, navsz
		savnav ( ij, igdfln ) = rnvblk ( ij )
	    END DO
	    lnavbl ( igdfln ) = navsz
	END IF
C
C*	Retrieve analysis block.
C
	CALL DM_RFHR  ( igdfln, 'ANLB', LLNANL, anlblk, ianlsz, ier )
	IF  ( ier .ne. 0 )  ianlsz = 0
	DO  ij = 1, ianlsz
	    savanl ( ij, igdfln ) = anlblk ( ij )
	END DO
	lanlbl ( igdfln ) = ianlsz
C
C*	Sort grids in common.
C
	kgrid  ( igdfln ) = 0
	ktgrid ( igdfln ) = 0
	CALL GD_SRTT  ( igdfln, ier )
C
C*	Set first value in sorted list to 0 to indicate that grid list
C*	is not yet sorted.
C
	ksrtl ( 1, 1, igdfln ) = 0
C*
	RETURN
	END
