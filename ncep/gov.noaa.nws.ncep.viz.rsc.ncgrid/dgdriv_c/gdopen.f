        SUBROUTINE GD_OPEN  ( ffnam, wrtfg, mxanl, mxnav, iacss,
     +                        bkanl, bknav, mxgrd, iret )
C************************************************************************
C* GD_OPEN                                                              *
C*                                                                      *
C* This subroutine opens a managed grid file.                           *
C*									*
C* This subroutine manages the available DM file slots.  GD_OPEN should	*
C* be called before any operation is done on a file.  If GD_OPEN has	*
C* been called for a different file previously, then GD_OPEN should be	*
C* called again even if GD_OPEN has been called before for the same	*
C* file.  Good practice is to always call GD_OPEN before accessing a	*
C* file.								*
C*                                                                      *
C* GD_OPEN  ( FFNAM, WRTFG, MXANL, MXNAV, IACSS, BKANL, BKNAV, 		*
C*            MXGRD, IRET )                                             *
C*                                                                      *
C* Input parameters:                                                    *
C*	FFNAM		CHAR*		Fully qualified file name	*
C*	WRTFG		LOGICAL		Flag set true if write needed	*
C*	MXANL		INTEGER		Number of analysis block return	*
C*	MXNAV		INTEGER		NUmber of nav block return	*
C*                                                                      *
C* Output parameters:                                                   *
C*	IACSS		INTEGER		Access number to do I/O on file	*
C*	BKANL (MXANL)	REAL		Analysis block data array	*
C*	BKNAV (MXNAV)	REAL		Navigation block data array	*
C*	MXGRD		INTEGER		Maximum number of grids		*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -14 = file name is blank        *
C*                                      -17 = file number limit reached *
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC		 1/04						*
C* R. Tian/SAIC		 3/04		Recoded				*
C* R. Tian/SAIC		 3/05		Added mgrid			*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'GMBDTA.CMN'
        INCLUDE         'grdcmn.cmn'
C
        CHARACTER*(*) 	ffnam 
	LOGICAL		wrtfg
        REAL            bkanl (*), bknav (*)
C*
	LOGICAL		avail, shrfg
	REAL		rnvblk (LLNNAV), anlblk (LLNANL)
        CHARACTER       message*720, funcnm*8, loglevel*6
C------------------------------------------------------------------------
        loglevel = "debug"
        funcnm="GD_OPEN"
        CALL ST_NULL ( funcnm,   funcnm,   lenq, ier )
        CALL ST_NULL ( loglevel, loglevel, lenq, ier )
        message = "ffnam=" // ffnam
        CALL ST_NULL ( message,  message,  lenq, ier )
        CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
	iret = 0
	iacss = 0
	mxgrd = 0
	nucode = .true.
	shrfg = .false.
C
C*	Check for blank file name.
C
	IF ( ffnam .eq. ' ' ) THEN
	    iret = -14
	    RETURN
	END IF
C
C*	Check if the file has already been opened.
C
	avail = .false.
	DO i = 1, MMFILE
	    IF ( ffnam .eq. gdflnm (i) ) THEN
	      igdfln = i
	      iacss = iflacc (i)
	      avail = .true.
	    END IF
	END DO
        WRITE (message, 1001 ) avail
 1001   FORMAT ("avail=", L1 )
        CALL ST_NULL ( message,  message,  lenq, ier )
        CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
C
C*	Try to open the file if it is not opened.
C
	IF ( .not. avail ) THEN
	    ier2 = 0
            CALL GD_OFIL  ( ffnam, wrtfg, shrfg, igdfln, navsz,
     +		            rnvblk, ianlsz, anlblk, ihdrsz, 
     +                      mxgrd, ier1 )

            
C
C*	    MMFILE files have been opened, close the least recently
C*	    used file and open again.
C
	    IF ( ier1 .eq. -15 ) THEN 
		ier1 = 0
	        ismlst = MXFLNM
	        DO i = 1, MMFILE
		    IF ( iflacc(i) .ne. 0 .and. iflacc(i) .lt. ismlst )
     +              THEN
	                ismlst = iflacc (i)
		    END IF
	        END DO
	        CALL GD_CLOS ( ismlst, ier )
                CALL GD_OFIL  ( ffnam, wrtfg, shrfg, igdfln, navsz, 
     +    		        rnvblk, ianlsz, anlblk, ihdrsz, 
     +                          mxgrd, ier2 )
	    END IF
	    IF ( ier1 .ne. 0 .or. ier2 .ne. 0 ) THEN
		iret = -2
	        RETURN
	    END IF
C
	    mgrid (igdfln) = mxgrd
            nflnum = nflnum + 1
            IF ( nflnum .gt. MXFLNM ) THEN
                iret = -17
                RETURN
            END IF
            iflacc ( igdfln ) = nflnum
            gdflnm ( igdfln ) = ffnam 
	    iacss = nflnum
	END IF
C
	mxgrd = mgrid (igdfln)
	IF ( mxanl .gt. 0 ) THEN
	    DO i = 1, MIN ( mxanl, LLNANL )
	        bkanl (i) = savanl (i,igdfln)
	    END DO
	END IF
C
	IF ( mxnav .gt. 0 ) THEN
	    DO i = 1, MIN (mxnav, LLNNAV )
	        bknav (i) = savnav (i,igdfln)
	    END DO
	END IF
C*
	RETURN
	END
