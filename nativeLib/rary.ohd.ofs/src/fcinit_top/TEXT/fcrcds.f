C MODULE FCRCDS
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ INPUT RECOLDS, CHECK FOR INCLUDES AND WRITE TO FILE.
C
      SUBROUTINE FCRCDS (IUNIT,ISTAT)
C
      CHARACTER*8 TYPMSG
      CHARACTER*8 XINCL/'INCLUDE'/,XINCL2/'@INCLUDE'/,ZINCL
      CHARACTER*11 XNINCL/'@NOINCLUDES'/
      CHARACTER*72 FIELD1
      CHARACTER*80 RECOLD,RECNEW
C
      INCLUDE 'uiox'
      INCLUDE 'ufreex'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fcrcds.f,v $
     . $',                                                             '
     .$Id: fcrcds.f,v 1.4 2001/06/13 13:29:58 dws Exp $
     . $' /
C    ===================================================================
C
C
      LDEBUG=0
C
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (IPR,1)
         WRITE (IPR,*) 'ENTER FCRCDS'
         ENDIF
C
      ISTAT=0
C
      NRDCRD=0
      ICKINC=1
C
      REWIND ICDTMP
C
C  READ INPUT RECOLDS
10    READ (IUNIT,'(A)',END=60) RECOLD
      NRDCRD=NRDCRD+1
C
C  CHECK FOR COMMENT FIELD
      CALL UINDEX (RECOLD,LEN(RECOLD),'$',1,ICOLCM)
C
C  GET FIRST FIELD
      NSCAN=1
      CALL USCAN2 (RECOLD,' ',NSCAN,FIELD1,LFIELD1,IERR)
C
C  CHECK FOR INCLUDE COMMAND
      IF (FIELD1.EQ.XINCL.OR.FIELD1.EQ.XINCL2) THEN
         ICOLIN=0
         CALL UINDEX (RECOLD,LEN(RECOLD),XINCL,LEN(XINCL),ICOLIN)
         IF (ICOLIN.EQ.0) THEN
	    CALL UINDEX (RECOLD,LEN(RECOLD),XINCL2,LEN(XINCL2),ICOLIN)
	    ENDIF
         IF (ICOLIN.GT.0) THEN
C        PRINT CARD
            NBLINE=1
            IFORM=1
            NCARD=0
            CALL UPRCR2 (NBLINE,IFORM,IPR,RECOLD,NCARD)
            IF (ICOLCM.EQ.0) GO TO 30
            IF (ICOLCM.GT.ICOLIN) GO TO 30
            GO TO 20
            ENDIF
         ENDIF
C
C  CHECK FOR NOINCLUDE COMMAND
      IF (FIELD1.EQ.XNINCL) THEN
         CALL UINDEX (RECOLD,LEN(RECOLD),XNINCL,LEN(XNINCL),ICOLIN)
         IF (ICOLIN.GT.0) THEN
C        PRINT CARD
            NBLINE=1
            IFORM=1
            NCARD=0
            CALL UPRCR2 (NBLINE,IFORM,IPR,RECOLD,NCARD)
            IF (ICOLCM.EQ.0) ICKINC=0
            IF (ICOLCM.GT.ICOLIN) ICKINC=0
            GO TO 10
            ENDIF
         ENDIF
C
C  WRITE TO TEMPORARY FILE
20    WRITE (ICDTMP,'(A)') RECOLD
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    IF (ICKINC.EQ.0) GO TO 10
C
C  EXPAND INCLUDE
C
      WRITE (ICDTMP,'(A)') RECOLD
      BACKSPACE ICDTMP
C
      NSTLVL=0
C
C  CHECK FOR 'INCLUDE' STATEMENT
40    ICKCOL=0
      IPRERR=1
      TYPMSG='ERROR'
      CALL UINDEX (RECOLD,LEN(RECOLD),XINCL2,LEN(XINCL2),ICOLIN)
      IF (ICOLIN.GT.0) THEN
         ZINCL=XINCL2
         ELSE
            ZINCL=XINCL
         ENDIF
      CALL UINCLD (ZINCL,RECOLD,ICKCOL,NSTLVL,RECNEW,LRECNEW,
     *   IPRERR,TYPMSG,LP,IERR)
      IF (IERR.GT.0) GO TO 50
C
C  CHECK IF END OF INPUT FROM 'INCLUDE' STATEMENT
      IF (NSTLVL.EQ.-1) GO TO 50
C
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (IPR,1)
         WRITE (IPR,*) 'NSTLVL=',NSTLVL,
     *      ' RECNEW=',RECNEW(1:LENSTR(RECNEW))
         ENDIF
C
C  CHECK IF ANY 'INCLUDE' STATEMENTS FOUND
      IF (NSTLVL.GT.0) THEN
         WRITE (ICDTMP,'(A)') RECNEW
         RECOLD=RECNEW
         GO TO 40
         ENDIF
C
50    GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RESET UNIT FROM WHICH RECORD IMAGES WILL BE READ
60    IUNIT=ICDTMP
      ICD=ICDTMP
      REWIND ICD
C
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (IPR,1)
         WRITE (IPR,*) 'EXIT FCRCDS'
         ENDIF
C
      RETURN
C
      END
