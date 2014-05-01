C MODULE UEROR
C-----------------------------------------------------------------------
C
      SUBROUTINE UEROR (NUNIT,NLINES,NUMERR)
C
C  ROUTINE UEROR COUNTS THE NUMBER OF ERROR MESSAGES GENERATED.
C
      INCLUDE 'uiox'
      INCLUDE 'upagex'
      INCLUDE 'uerorx'
      INCLUDE 'ucmdbx'
      INCLUDE 'uoptnx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/ueror.f,v $
     . $',                                                             '
     .$Id: ueror.f,v 1.2 2001/06/13 08:27:36 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UEROR'
         ENDIF
C
      IUNIT=IABS(NUNIT)
      LINES=IABS(NLINES)
C
      IF (IUNIT.EQ.0) GO TO 50
C
C  CHECK IF PAGE HEADER HAS BEEN PRINTED
      IF (NPSPAG(IUNIT).EQ.0) CALL UPAGE (NUNIT)
C
      DO 10 LUNIT=1,MXUERR
         IF (LUEROR(LUNIT).EQ.IUNIT) GO TO 20
         IF (LUEROR(LUNIT).EQ.0) THEN
            LUEROR(LUNIT)=IUNIT
            GO TO 20
            ENDIF
10       CONTINUE
      CALL ULINE (IUNIT,2)
      WRITE (IUNIT,120) IUNIT,MXUERR
      GO TO 80
C
C  CHECK IF EXCEEDED MAXIMUM ERRORS ALLOWED
20    IF (MAXERR.GT.0) THEN
         IF (IUNIT.GT.0) THEN
            IF (NTERR(LUNIT).GE.MAXERR) THEN
               CALL ULINE (IUNIT,2)
               WRITE (IUNIT,140)
               NTIME=NOVPRT
               IF (NTIME.EQ.0) NTIME=1
               DO 30 I=1,NTIME
                  CALL ULINE (IUNIT,0)
                  WRITE (IUNIT,160) MAXERR
30                CONTINUE
               MAXCDE=16
               ISTOP=0
               CALL USTOP (IUNIT,ISTOP)
               ENDIF
            ENDIF
         ENDIF
C
C  CHECK IF LINES WILL FIT ON PAGE
      NCHK=LINES
      IF (NLINES.GT.0) NCHK=NCHK+1
      CALL ULINEL (IUNIT,NCHK,IRETRN)
      IF (IRETRN.GT.0) CALL UPAGE (NUNIT)
C
      IF (NLINES.GT.0) THEN
         CALL ULINE (IUNIT,1)
         IF (NOVPRT.GT.0) WRITE (IUNIT,130)
CCC         IF (NOVPRT.EQ.0) WRITE (IUNIT,140)
         ENDIF
C
      IF (NOVPRT.LE.0) GO TO 50
C
C  OVERPRINT '*** ERROR'
      IF (NLINES.GT.0) THEN
         CALL ULINE (IUNIT,0)
         WRITE (IUNIT,130)
         ENDIF
      DO 40 I=1,NOVPRT
         CALL ULINE (IUNIT,0)
         WRITE (IUNIT,150)
40       CONTINUE
C
C  COUNT NUMBER OF ERRORS ENCOUNTERED
50    IF (NUMERR.GE.0) NUMERR=NUMERR+1
C
C  UPDATE THE LINES PRINTED
      IF (LINES.GT.0) CALL ULINE (IUNIT,LINES)
C
      NERR(LUNIT)=NERR(LUNIT)+1
      NTERR(LUNIT)=NTERR(LUNIT)+1
C
      IF (ICMDBG.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,100) IUNIT,LP
         ENDIF
C
      IF (NPSPAG(IUNIT).LT.0) GO TO 70
C
C  STORE PAGE ON WHICH ERROR OCCURRED
      NTIME=MPGERR(LUNIT)
      DO 60 I=1,NTIME
         IF (ICMDBG.GT.1) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*) 'LUNIT=',LUNIT,
     *         ' I=',I,
     *         ' IUNIT=',IUNIT,
     *         ' NPSPAG(IUNIT)=',NPSPAG(IUNIT),
     *         ' LPGERR(LUNIT,I)=',LPGERR(LUNIT,I)
            ENDIF
         IF (LPGERR(LUNIT,I).EQ.NPSPAG(IUNIT)) GO TO 70
         IF (LPGERR(LUNIT,I).NE.0) GO TO 60
            LPGERR(LUNIT,I)=NPSPAG(IUNIT)
            NPGERR(LUNIT)=NPGERR(LUNIT)+1
            GO TO 70
60       CONTINUE
C
70    IF (MAXCDE.LT.8) MAXCDE=8
C
80    IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UEROR'
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT (' IUNIT=',I2,3X,'LP=',I2)
120   FORMAT ('0*** WARNING - IN UEROR - UNIT ',I2,' CANNOT HAVE ',
     *   'ERRORS COUNTED BECAUSE MAXIMUM UNIT ',
     *   'IS ',I2,'.')
130   FORMAT (' ')
140   FORMAT ('0')
150   FORMAT ('+*** ERROR')
160   FORMAT ('+*** FATAL ERROR - MAXIMUM NUMBER OF ERRORS ALLOWED (',
     *   I4,') EXCEEDED.')
C
      END
