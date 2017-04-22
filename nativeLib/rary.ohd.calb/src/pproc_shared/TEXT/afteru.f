C MODULE AFTERU
C-----------------------------------------------------------------------
C
      SUBROUTINE AFTERU (LATSGN,ITYPEU,UCHAR,LABEL,LTYPE,ISTAT,
     *   LENGTH,INDERR)
C
      INCLUDE 'ufreex'
      INCLUDE 'common/ionum'
C
      CHARACTER*4 UCHAR,LABEL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pproc_shared/RCS/afteru.f,v $
     . $',                                                             '
     .$Id: afteru.f,v 1.4 1998/07/06 15:03:18 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTAT.NE.1) GO TO 10
C
      IF (LTYPE.EQ.3) GO TO 70
      GO TO 90
C
10    IF (ISTAT.EQ.3) THEN
C     END OF INPUT ENCOUNTERED
         CALL UEROR (IPR,2,-1)
         WRITE (IPR,100)
         IUSTOP=0
         CALL USTOP (IPR,IUSTOP)
         ENDIF
C
      IF (ISTAT.EQ.7) THEN
C     NON-COMMENT CHARACTERS FOUND AFTER LAST COLUMN TO BE PROCESSED
         CALL UWARN (IPR,2,-1)
         WRITE (IPR,110) ICDSTP,ICDBUF(1:80)
         ENDIF
C
C  CHECK IF '@' FOUND IN FIRST CHARACTER OF FIELD
      IF (LATSGN.EQ.1) GO TO 50
C
20    IF (LTYPE.EQ.2) GO TO 90
      IF (LTYPE.EQ.ITYPEU) GO TO 90
C
      IF (LTYPE.NE.3) GO TO 30
         CALL UEROR (IPR,2,-1)
         WRITE (IPR,120) LABEL,UCHAR
         ISTAT=7
         GO TO 80
30    IF (LTYPE.NE.0) GO TO 40
         CALL UEROR (IPR,2,-1)
         WRITE (IPR,130) 'INTEGER',UCHAR,LABEL(1:LENSTR(LABEL))
         ISTAT=8
         GO TO 80
40    IF (ITYPEU.LT.1) GO TO 90
      CALL UEROR (IPR,2,-1)
      WRITE (IPR,130) 'REAL',UCHAR,LABEL(1:LENSTR(LABEL))
      ISTAT=9
      GO TO 80
C
50    IF (LENGTH.NE.2) GO TO 20
C
      IF (LTYPE.EQ.3) GO TO 60
         ISTAT=6
         GO TO 90
60    IF (LABEL.EQ.UCHAR) GO TO 90
C
70    CALL UEROR (IPR,2,-1)
      WRITE (IPR,120) LABEL,UCHAR
      ISTAT=7
C      
80    IF (INDERR.EQ.0) THEN
         INDERR=1
         CALL ULINE (IPR,2)
         WRITE (IPR,140)
         ENDIF
C
90    RETURN
C
100   FORMAT ('0*** ERROR - UNEXPECTED END OF INPUT ENCOUNTERED.')
110   FORMAT ('0*** WARNING - NON-COMMENT CHARACTERS FOUND AFTER LAST ',
     *      'CARD COLUMN TO BE PROCESSED (',I2,') ',
     *      'ON THE FOLLOWING CARD:' /
     *   ' ',16('----+') /
     *   ' ',A)
120   FORMAT ('0*** ERROR - CARD LABEL ',A,' WAS EXPECTED AND ',A,' ',
     *   'WAS FOUND.')
130   FORMAT ('0*** ERROR - ',A,' VALUE WAS EXPECTED AND ',A,' ',
     *   'WAS FOUND ON CARD LABEL ',A,'.')
140   FORMAT ('0*** NOTE - PROGRAM WILL TERMINATE DUE TO INPUT ERROR.')
C
      END
