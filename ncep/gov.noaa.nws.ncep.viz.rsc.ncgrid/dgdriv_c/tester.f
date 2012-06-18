	PROGRAM TESTER
C************************************************************************
C* TESTER								*
C*									*
C* This program tests the ER library subroutines.			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 6/84						*
C* M. desJardins/NMC	 8/94	Restructured				*
C* K. Tyle/GSC		12/96	Added TIMFLG to call to ER_MMSG; added	*
C*				ER_LMSG and ER_WBUF; added to ER_STAT	*
C* J. Wu/SAIC	 	01/04	Added er_gnumerr & er_gerrmsg		*
C* J. Wu/SAIC	 	02/04	Printed results from er_gerrmsg		*
C* T. Piper/SAIC	10/06	Increased errmsg to 512			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'ercmn.cmn'
C*
	CHARACTER	errgrp*12, errstr*72, outmsg*80, errmsg*512
	LOGICAL 	timflg
C-------------------------------------------------------------------------
	CALL IN_BDTA  ( ier )
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
	    WRITE(6,20)
20	    FORMAT (
     +      '  1 = ER_WMSG   2 = ER_LMSG   3 = ER_MMSG  4 = ER_STAT'/
     +      '  5 = ER_WBUF   6 = ER_GNUMERR  7 = ER_GERRMSG' ) 
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier ) 
	IF ( ier .eq. 2 ) THEN
	   iostat = -1
	   numsub = -1
	END IF	
C--------------------------------------------------------------------------
	    IF (numsub .eq. 1) THEN
		WRITE (6,*) 'Enter ERRGRP'
		READ  (5,2)  errgrp
		WRITE (6,*) 'Enter NUMERR'
		READ  (5,*)  numerr
		WRITE (6,*) 'Enter ERRSTR'
		READ  (5,2)  errstr
		CALL ER_WMSG  ( errgrp, numerr, errstr, iret )
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'ER', iret, ' ', ier )
C--------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 2) THEN
		WRITE (6,*) 'Enter LEVERR'
		READ  (5,*)  leverr 
		WRITE (6,*) 'Enter ERRGRP'
		READ  (5,2)  errgrp
		WRITE (6,*) 'Enter NUMERR'
		READ  (5,*)  numerr
		WRITE (6,*) 'Enter ERRSTR'
		READ  (5,2)  errstr
		CALL ER_LMSG  ( leverr, errgrp, numerr, errstr, iret )
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'ER', iret, ' ', ier )
C--------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 3) THEN
		WRITE (6,*) 'Enter ERRGRP'
		READ  (5,2)  errgrp
		WRITE (6,*) 'Enter NUMERR'
		READ  (5,*)  numerr
		WRITE (6,*) 'Enter ERRSTR'
		READ  (5,2)  errstr
		WRITE (6,*) 'Enter TIMFLG'
		READ  (5,*) timflg
		CALL ER_MMSG  ( errgrp, numerr, errstr, timflg, 
     +		outmsg, iret )
		WRITE (6,*) 'IRET = ', iret
		WRITE (6,*) 'OUTMSG = ', outmsg
		CALL ER_WMSG  ( 'ER', iret, ' ', ier )
C--------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 4) THEN
		WRITE (6,*) 'Enter NELEVL'
		READ  (5,*)  nelevl
		WRITE (6,*) 'Enter NEBUFF'
		READ  (5,*)  nebuff
		WRITE (6,*) 'Enter TIMFLG'
		READ  (5,*)  timflg 
		CALL ER_STAT  ( nelevl, nebuff, timflg, iret )
		WRITE  (6,*)  ' IELEVL: ', ielevl, ' IEBUFF: ',iebuff 
		WRITE  (6,*)  ' TIMFLG: ', etmflg, ' iret: ',iret
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 5) THEN
		CALL ER_WBUF  ( iret )
		WRITE  (6,*)  ' IRET = ', iret
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6) THEN
		CALL ER_GNUMERR  ( numerr, iret )
		WRITE  (6,*) ' NUMERR = ', numerr, ' IRET = ', iret
		CALL ER_WMSG  ( 'ER', iret, ' ', ier )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 7) THEN
		WRITE (6,*) 'Enter error index'
		READ  (5,*)  index
		errmsg = ' '
		CALL ER_GERRMSG  ( index, errmsg, iret )
		WRITE (6,*)  ' IRET = ', iret
		WRITE (6,*)  ' ERRMSG = ', errmsg
		CALL ER_WMSG  ( 'ER', iret, ' ', ier )
C-------------------------------------------------------------------------
	    END IF
	END DO
C*
2	FORMAT (A)
	END
