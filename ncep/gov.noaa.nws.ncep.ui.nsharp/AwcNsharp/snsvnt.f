	SUBROUTINE SNSVNT ( vint, vline, vflag, vlvl, nvlvl, ivcolr,
     +			    iret )
C************************************************************************
C* SNSVNT								*
C*									*
C* This subroutine returns the colors for color coded wind vectors	*
C*									*
C* SNSVNT ( VINT, VLINE, VFLAG, VLVL, NVLVL, IVCOLR, IRET )		*
C*									*
C* Input parameters:							*
C*	VINT		CHAR(*)		Input wind speed intervals	*
C*	VLINE		CHAR(*)		Input wind spd intervals colors	*
C*									*
C* Output parameters:							*
C*	VFLAG		LOGICAL		Color coded winds flag		*
C*	VLVL(*)		REAL		Wind speed intervals		*
C*	NVLVL		INTEGER		Number of wind speed intervals	*
C*	IVCOLR(*)	INTEGER		Wind spd intervals colors	*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* J. Whistler/AWC     11/95						*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	CHARACTER*(*)	vint, vline
	LOGICAL		vflag
	REAL		vlvl (*)
	INTEGER		ivcolr (*)
C*
	INTEGER		ivltyp(LLCLEV), ivlwid(LLCLEV), ivlabl(LLCLEV)
	REAL		smooth, linfltr
	LOGICAL		onelev
        CHARACTER       clbl(LLCLEV)*24
        LOGICAL         scflag
C------------------------------------------------------------------------
	iret = NORMAL
        scflag = .false.
C
C*	Get color coded wind intervals.
C
	vflag = .true.
	CALL IN_CINT ( vint, value, 1, 0., 200., vlvl, nvlvl, clbl,
     +                 rint, iret )
C
C*      Make sure there are no duplicate levels.
C
        ilvl = 1
        DO  i = 2, nvlvl
            IF  ( vlvl (i) .ne. vlvl (i-1) )  THEN
                ilvl = ilvl + 1
                vlvl (ilvl) = vlvl (i)
            END IF
        END DO
        nvlvl = ilvl
C
C*      Get the colors
C
        IF  ( nvlvl .eq. LLCLEV )  THEN
            nvlvl = nvlvl - 1
        END IF
        nvlvl1 = nvlvl + 1
        CALL IN_LINE ( vline, vlvl, nvlvl1, ivcolr, ivltyp, ivlwid, 
     +		       ivlabl, smooth, linfltr, scflag, iret )
C
C*      Check that at least one line has a color.
C
        onelev = .false.
        DO  i = 1, nvlvl
            IF  ( ivcolr (i) .gt. 0 )onelev = .true.
        END DO
        IF  ( .not. onelev )  THEN
            nvlvl = 0
	    vflag = .false.
        ELSE
C
C*          Sort the levels from smallest to largest.
C
            DO  i = 1, nvlvl - 1
                DO  j = i+1, nvlvl
                    IF  ( vlvl (i) .gt. vlvl (j) )  THEN
                        jcol = ivcolr (i)
                        vsav = vlvl (i)
                        ivcolr (i) = ivcolr (j)
                        vlvl   (i) = vlvl   (j)
                        ivcolr (j) = jcol
                        vlvl   (j) = vsav
                    END IF
                END DO
            END DO
        END IF
C*
	RETURN
	END
