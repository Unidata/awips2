	SUBROUTINE PC_INIT  ( ivert, nparm, parms, iret )
C************************************************************************
C* PC_INIT								*
C*									*
C* This subroutine initializes the parameter conversion software.	*
C* Information about the current data set is saved.  It must be the	*
C* first PC subroutine called.						*
C*									*
C* PC_INIT  ( IVERT, NPARM, PARMS, IRET )				*
C*									*
C* Input parameters:							*
C*	IVERT		INTEGER		Vertical coordinate type	*
C*					  0 = NONE              	*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*	NPARM		INTEGER		Number of parameters		*
C*	PARMS (NPARM)	CHAR*4		Parameter names			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = invalid NPARM		*
C*					  -2 = invalid IVERT		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 7/90	Initialize station parms, ...		*
C* T. Lee/GSC		 8/97	Added TVRC to output parameter		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	parms (*)
C*
	LOGICAL		flgint
C--------------------------------------------------------------------------
C*	Set values which are no longer input.
C
	isf = 0
	flgint = .true.
C
C*	Initialize common variables.
C
	DO  i = 1, MAXTBL
	    tabflg (i) = .false.
	END DO
C*
	basics (1) = 'PRES'
	basics (2) = 'TMPC'
	basics (3) = 'DWPC'
	basics (4) = 'UWND'
	basics (5) = 'VWND'
	basics (6) = 'HGHT'
	DO  i = 7, MAXPRM
	    basics (i) = ' '
	END DO
C*
	vparms (1) = 'PRES'
	vparms (2) = 'THTA'
	vparms (3) = 'HGHT'
	vparms (4) = 'SGMA'
	vparms (5) = 'TMPC'
	vparms (6) = 'DWPC'
	DO  i = 7, MAXVRT
	    vparms (i) = ' '
	END DO
C*
	splist (1) = 'PRES'
	splist (2) = 'TMPC'
	splist (3) = 'DWPC'
	splist (4) = 'UWND'
	splist (5) = 'VWND'
	splist (6) = 'HGHT'
	splist (7) = 'TVRC'
	sindxf = .false.
C*
	inton  = .true.
	doint  = .true.
	bsonly = .false.
	range  = 100.
	prmint = 'PRES'
	inttyp = 1
	ksprm  = 0
C
C*	Save all input information in common
C
	IF  ( ( nparm .lt. 1 ) .or. ( nparm .gt. MAXPRM ) ) THEN
	    iret = -1
	  ELSE IF ( (ivert .eq. 1) .and. (parms (1) .ne. 'PRES') ) THEN
	    iret = -2
	  ELSE IF ( (ivert .eq. 2) .and. (parms (1) .ne. 'THTA') ) THEN
	    iret = -2
	  ELSE IF ( (ivert .eq. 3) .and. ((parms (1) .ne. 'HGHT') .and.
     +		    (parms (1) .ne. 'DHGT') .and. 
     +		    (parms (1) .ne. 'MHGT')))  THEN
	    iret = -2
	  ELSE IF  ( (ivert .eq. 4) .and. (parms (1) .ne. 'SGMA') ) THEN
	    iret = -2
	  ELSE IF  ( (ivert .lt. 0) .or. (ivert .gt. 4) ) THEN
	    iret = -2
	  ELSE
	    iret   = 0
	    dsflg  = .true.
	    jcord  = ivert
	    jdsprm = nparm
	    DO  i = 1, nparm
		dsparm (i) = parms (i)
	    ENDDO
	    jsfflg = isf
	    jntflg = flgint
	ENDIF
C
C*	See if there is height information in dataset.
C
	CALL ST_FIND  ( 'HGHT', parms, nparm, ipos, ier )
	IF  ( ipos .ne. 0 )  THEN
	    vparms (3) = 'HGHT'
	  ELSE
	    CALL ST_FIND  ( 'MHGT', parms, nparm, ipos, ier )
	    IF  ( ipos .ne. 0 )  THEN
		vparms (3) = 'MHGT'
	      ELSE
		CALL ST_FIND  ( 'DHGT', parms, nparm, ipos, ier )
		IF  ( ipos .ne. 0 )  THEN
		    vparms (3) = 'DHGT'
		  ELSE
		    vparms (3) = 'XXXX'
		END IF
	    END IF
	END IF
	jhght = ipos
C
C*	Set up retrieval of vertical coordinates.
C
	IF  ( jcord .ne. 0 )  CALL PC_SVRT  ( ier )
C*
	RETURN
	END
