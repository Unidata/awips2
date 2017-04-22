C MODULE PDFVR
C-----------------------------------------------------------------------
C
      SUBROUTINE PDFVRR (ITIM,VAL,IPER,ISKIP,MIN,IFUT,IREV,IRRBUF,
     *   IFRBUF,ISTAT)
C     
C  THIS ROUTINE FINDS A VALUE IN A RRS RECORD.  IF ITIM IS
C  NEGATIVE, THE VALUE IS DELETED. THE VALUE IS REPLACED IF THE TIME
C  AND PERIOD ARE FOUND.  IF NOT FOUND THE VALUE IS INSERTED.
C
C   ARGUMENT LIST:
C
C     NAME    TYPE   I/O   DIM   DESCRIPTION
C     ----    ----   ---   ---   -----------
C     ITIM      I     I     1    TIME OF DATA (JULIAN HOUR)
C     VAL       R     I     1    DATA VALUE
C     IPER      I     I     1    TIME PERIOD OF DATA
C     ISKIP     I     I     1    NUMBER OF WORDS IN OBSERVATION
C     IRRBUF    I    I/O    ?    ARRAY FOR RRS RECORD
C     IFRBUF    I    I/O    ?    ARRAY FOR FREE POOL RECORD
C     ISTAT     I     O     1    STATUS INDICATOR:
C                                  0=NORMAL RETURN
C                                  1=DATA NOT WRITTEN OR DELETED
C                                  2=READ/WRITE ERROR
C
      DIMENSION IRRBUF(1),IFRBUF(1)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      COMMON /PDBREV/ IREVSN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdfvrr.f,v $
     . $',                                                             '
     .$Id: pdfvrr.f,v 1.3 2002/02/11 19:53:36 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.1) WRITE (IOGDB,*) 'ENTER PDFVRR'
C
      IF (IRRBUF(8).EQ.0) GO TO 120
      ITIME=IABS(ITIM)
      IREVSN=IREV
C
      IEPOS=IRRBUF(1)
      ISTRT=IRRBUF(9)
      IEND =IRRBUF(11)
      IF (ISTRT.LE.IEND) GO TO 20
      IEND=IEPOS-ISKIP+1
C
C  SEARCH FROM EARLIEST TO END OF DATA
20    DO 30 I=ISTRT,IEND,ISKIP
         IF (ITIME.LT.JULMIN(ISKIP,IRRBUF(I))) GO TO 50
         IF (ITIME.GT.JULMIN(ISKIP,IRRBUF(I))) GO TO 30
C        TIME MATCH - CHECK PERIOD
            IF (ISKIP.EQ.2.OR.IPER.EQ.IRRBUF(I+2)) GO TO 60
            IF (IPER.GT.IRRBUF(I+2)) GO TO 50
30       CONTINUE
C
C  SEARCH WRAP AROUND IF ANY
      IF (IEND.EQ.IRRBUF(11)) GO TO 40
      ISTRT=IRRBUF(1)-IRRBUF(7)*ISKIP+1
      IEND=IRRBUF(11)
      GO TO 20
C
C  SET POSITION TO END OF DATA
40    I=IEND+ISKIP

50    IF (ITIM.LT.0) GO TO 110
C
C  TIME NOT FOUND
      CALL PDMVDR (ITIME,VAL,IPER,ISKIP,MIN,IFUT,IREV,I,IRRBUF,IFRBUF,
     *   ISTAT)
      IF (ISTAT.NE.0) GO TO 90
      CALL PDSTAR (0,ITIME,VAL,0,MIN,IFUT,IRRBUF,IFRBUF,ISTAT)
      IF (ISTAT.EQ.1) GO TO 110
      IF (ISTAT.NE.0) GO TO 90
      GO TO 120
C
C  REPLACE VALUE
60    IF (ITIM.LT.0) GO TO 80
      IF (IPDDB.GT.1) WRITE (IOGDB,70) ITIME,VAL,I,MIN,IFUT,
     *   (IRRBUF(M),M=1,17),(IFRBUF(M),M=1,2)
70    FORMAT (' IN PDFVRR - ITIME=',I6,', VAL=',F9.2,
     *   ' I=',I7,',MIN=',I5,' IFUT=',I6 /
     * ' IRRBUF(1-17)=',I5,1X,2A4,1X,I6,1X,A4,1X,I2,2(1X,I4),5(1X,I6),
     *                  1X,I2,2(1X,I6),1X,I2 /
     * ' IFRBUF(1-2)=',2I11)
      CALL PDSTAR (0,ITIME,VAL,I,MIN,IFUT,IRRBUF,IFRBUF,ISTAT)
      IF (ISTAT.EQ.1) GO TO 110
      IF (ISTAT.NE.0) GO TO 90
      GO TO 120
C
C  DELETE A VALUE
80    IF (IREV.NE.1) GO TO 110
      IOVAL=IRRBUF(I+1)
      CALL PDDLRR (I,IFUT,ITIME,IRRBUF,IFRBUF,ISTAT)
      CALL PDSTAR (0,ITIME,IOVAL,-I,MIN,IFUT,IRRBUF,IFRBUF,ISTAT)
      IF (ISTAT.EQ.1) GO TO 110
      IF (ISTAT.EQ.0) GO TO 120
C
C  ERROR
90    WRITE (LP,100) ITIM
100   FORMAT ('0**ERROR** IN PDFVRR - SYSTEM ERROR FOR TIME OF ',I8)
      ISTAT=2
      GO TO 120
C
C  DATA NOT WRITTEN OR DELETED
110   ISTAT=1
C
120   IF (IPDTR.GT.1) WRITE (IOGDB,*) 'EXIT PDFVRR - ISTAT=',ISTAT
C
      RETURN
C
      END
