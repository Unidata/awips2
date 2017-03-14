C MODULE UWARN
C-----------------------------------------------------------------------
C
      SUBROUTINE UWARN (NUNIT,NLINES,NUMWRN)
C
C  ROUTINE UWARN COUNTS THE NUMBER OF WARNINGS MESSAGES GENERATED.
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
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/uwarn.f,v $
     . $',                                                             '
     .$Id: uwarn.f,v 1.2 2001/06/13 08:28:14 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UWARN'
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
      WRITE (IUNIT,100) IUNIT,MXUERR
      GO TO 80
C
C  CHECK IF EXCEEDED MAXIMUM WARNINGS ALLOWED
20    IF (MAXWRN.GT.0) THEN
         IF (IUNIT.GT.0) THEN
            IF (NTWRN(LUNIT).GE.MAXWRN) THEN
               CALL ULINE (IUNIT,2)
               WRITE (IUNIT,120)
               NTIME=NOVPRT
               IF (NTIME.EQ.0) NTIME=1
               DO 30 I=1,NTIME
                  CALL ULINE (IUNIT,0)
                  WRITE (IUNIT,140) MAXWRN
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
         IF (NOVPRT.GT.0) WRITE (IUNIT,110)
CCC         IF (NOVPRT.EQ.0) WRITE (IUNIT,120)
         ENDIF
C
      IF (NOVPRT.LE.0) GO TO 50
C
C  OVERPRINT '*** WARNING'
      IF (NLINES.GT.0) THEN
         CALL ULINE (IUNIT,0)
         WRITE (IUNIT,110)
         ENDIF
      DO 40 I=1,NOVPRT
         CALL ULINE (IUNIT,0)
         WRITE (IUNIT,130)
40       CONTINUE
C
C  COUNT NUMBER OF WARNINGS ENCOUNTERED
50    IF (NUMWRN.GE.0) NUMWRN=NUMWRN+1
C
C  UPDATE THE LINES PRINTED
      IF (LINES.GT.0) CALL ULINE (IUNIT,LINES)
C
      NWRN(LUNIT)=NWRN(LUNIT)+1
      NTWRN(LUNIT)=NTWRN(LUNIT)+1
C
      IF (NPSPAG(IUNIT).LT.0) GO TO 70
C
C  STORE PAGE ON WHICH WARNING OCCURRED
      NTIME=MPGWRN(LUNIT)
      DO 60 I=1,NTIME
         IF (ICMDBG.GT.1) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*) 'LUNIT=',LUNIT,
     *         ' I=',I,
     *         ' IUNIT=',IUNIT,
     *         ' NPSPAG(IUNIT)=',NPSPAG(IUNIT),
     *         ' LPGWRN(LUNIT,I)=',LPGWRN(LUNIT,I)
            ENDIF
         IF (LPGWRN(LUNIT,I).EQ.NPSPAG(IUNIT)) GO TO 70
         IF (LPGWRN(LUNIT,I).NE.0) GO TO 60
            LPGWRN(LUNIT,I)=NPSPAG(IUNIT)
            NPGWRN(LUNIT)=NPGWRN(LUNIT)+1
            GO TO 70
60       CONTINUE
C
70    IF (MAXCDE.LT.4) MAXCDE=4
C
80    IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UWARN'
         ENDIF
C
      IF (ICMTRC.GT.0.OR.ICMDBG.GT.0) THEN
         CALL ULINE (IUNIT,1)
         WRITE (IUNIT,*) ' '
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT ('0*** WARNING - IN UEROR - UNIT ',I2,' CANNOT HAVE ',
     *   'ERRORS COUNTED BECAUSE MAXIMUM UNIT IS ',I2,'.')
110   FORMAT (' ')
120   FORMAT ('0')
130   FORMAT ('+*** WARNING')
140   FORMAT ('+*** FATAL ERROR - MAXIMUM NUMBER OF WARNINGS ALLOWED (',
     *   I4,') EXCEEDED.')
C
      END
