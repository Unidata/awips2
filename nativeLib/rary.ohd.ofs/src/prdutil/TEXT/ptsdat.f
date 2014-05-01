C MODULE PTSDAT
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT TIME SERIES. THE DATA IS PRINTED STARTING AT THE 
C  FIRST TIME PERIOD AFTER THE HOUR OF 12Z.
C
      SUBROUTINE PTSDAT (JHOUR,RBUF,LRBUF,ITSTEP,NVPTS,NDEC)
      
C  ARGUMENT LIST:
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ----      ----  ---   ---   -----------
C       JHOUR      I     I     1    JULIAN HOUR OF FIRST DATA
C       RBUF       R     I     ?    ARRAY CONTAINING TS DATA
C       LRBUF      I     I     1    NUMBER OF DATA WORDS IN RBUF
C       ITSTEP     I     I     1    TIME STEP OF DATA
C       NVPTS      I     I     1    NUMBER OF VALUES PER TIME STEP
C       NDEC       I     I     1    NUMBER OF DECIMAL PLACES
C
      CHARACTER*116 XLINE,TLINE
C
      DIMENSION RBUF(LRBUF)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/ptsdat.f,v $
     . $',                                                             '
     .$Id: ptsdat.f,v 1.3 1999/04/23 20:01:12 page Exp $
     . $' /
C    ===================================================================
C
C
      IPOS=0
      IBPOS=1
      IPFLG=0
      NRBUF=LRBUF
C
C  SET UP SPACING VARIABLES
      NVPDAY=24/ITSTEP*NVPTS
      NDPL=(LEN(XLINE)-1)/(NVPDAY*9)
      NVPL=NDPL*NVPDAY
      IF (NVPL.GT.0) GO TO 10
C
C  MORE THAN ONE PRINTED LINE PER DAY OF DATA
      NDPL=1
      NVPL=12
C
C  CHECK WHERE TO START
10    NVTPR=NVPDAY*NDPL
      CALL DDGHC2 (JHOUR,IYR,IMO,IDAY,IHR)
      IF (ITSTEP.EQ.24) GO TO 40
      IF (IHR.EQ.0.AND.ITSTEP.EQ.12) GO TO 40
      IF (IHR.GT.12) GO TO 30
      IF (IHR.LT.12) GO TO 20
C
C  HOUR IS 12
      IPOS=NVPDAY-NVPTS
      IHR=IHR-(24/ITSTEP-1)* ITSTEP
      IF (IHR.GE.0) GO TO 40
         IHR=IHR+24
         IDAY=IDAY-1
         GO TO 40
C
C  HOUR IS < 12
20    IHR=IHR+ITSTEP
      IPOS=IPOS+1
      IF (IHR.LE.12) GO TO 20
      IDAY=IDAY-1
      IPOS=NVPDAY-IPOS*NVPTS
      IF (IHR.NE.24) GO TO 40
         IHR=IHR-ITSTEP
         IDAY=IDAY+1
         GO TO 40
C
C  HOUR IS > 12
30    IHR=IHR-ITSTEP
      IPOS=IPOS+1
      IF (IHR.GT.12) GO TO 30
         IHR=IHR+ITSTEP
         IPOS=(IPOS-1)*NVPTS
C
40    CALL DDGCH2 (LHOUR,IYR,IMO,IDAY,IHR)
      CALL DDGHC2 (LHOUR,IYR,IMO,IDAY,IHR)
C
C  PUT IN NONE FOR FIRST VALUES IF NOT START AT 18Z
      NWDS=NVPL
      ILPOS=1
      CALL UREPET (' ',XLINE,LEN(XLINE))
      IF (IPOS.EQ.0) GO TO 80
      DO 70 I=1,IPOS
         NCHAR=8
         CALL UMOVEX ('**NONE**',1,XLINE,ILPOS,NCHAR)
         ILPOS=ILPOS+NCHAR+1
         XLINE(ILPOS-1:ILPOS-1)=' '
         IF (ILPOS.LT.NVPL*9+1) GO TO 70
         IF (IPFLG.EQ.1) GO TO 50
            IPFLG=1
            CALL ULINE (LP,1)
            JYR=IYR-(IYR/100)*100
            WRITE (LP,130) IMO,IDAY,JYR,IHR,XLINE
            GO TO 60
50       CALL ULINE (LP,1)
         WRITE (LP,140) XLINE
60       CALL UREPET (' ',XLINE,LEN(XLINE))
         ILPOS=1
70       CONTINUE
      NVTPR=NVTPR-IPOS
      IPOS=MOD(IPOS,NVPL)
      NWDS=NVPL-IPOS
C
C  CONVERT FROM REAL TO CHARACTER
80    IF (NVTPR-NWDS.LT.0) NWDS=NVTPR
      NVTPR=NVTPR-NWDS
      IF (NWDS.GT.NRBUF) NWDS=NRBUF
      ITPOS=1
      NCHAR=8
      IPRERR=1
      DO 90 I=1,NWDS
         NDECT=NDEC
         IF (RBUF(IBPOS).GT.999999.0.AND.NDEC.GT.0) NDECT=0
         CALL UFF2A (RBUF(IBPOS),TLINE,ITPOS,NCHAR,NDECT,IPRERR,LP,
     *      IERR)
         IBPOS=IBPOS+1
         ITPOS=ITPOS+NCHAR
90       CONTINUE
C
C  PUT VALUE AND SLASHES IN PRINT ARRAY
      ITPOS=1
      DO 100 I=1,NWDS
         CALL UMOVEX (TLINE,ITPOS,XLINE,ILPOS,NCHAR)
         ITPOS=ITPOS+NCHAR
         ILPOS=ILPOS+NCHAR+1
         XLINE(ILPOS-1:ILPOS-1)=' '
         IF (MOD((ILPOS-1)/(NCHAR+1),NVPDAY).EQ.0)
     *      XLINE(ILPOS-1:ILPOS-1)='/'
100      CONTINUE
C
      IF (IPFLG.EQ.0) THEN
         IPFLG=1
         CALL ULINE (LP,1)
         JYR=IYR-(IYR/100)*100
         WRITE (LP,130) IMO,IDAY,JYR,IHR,XLINE
         GO TO 110
         ENDIF
      IF (NVTPR.EQ.0.AND.NWDS.EQ.MOD(NVPDAY,NVPL))
     *   XLINE(ILPOS-1:ILPOS-1)='/'
      CALL ULINE (LP,1)
      WRITE (LP,140) XLINE
C
110   NLINEL=5
      CALL ULINEL (LP,NLINEL,IRETRN)
      IF (IRETRN.EQ.1) CALL UPAGE (LP)
C
      NRBUF=NRBUF-NWDS
      IF (NRBUF.LE.0) GO TO 120
C
      CALL UREPET (' ',XLINE,LEN(XLINE))
      NWDS=NVPL
      ILPOS=1
      IF (NVTPR.NE.0) GO TO 80
         LHOUR=LHOUR+24*NDPL
         CALL DDGHC2 (LHOUR,IYR,IMO,IDAY,IHR)
         NVTPR=NVPDAY*NDPL
         IPFLG=0
      GO TO 80
C
120   RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT (1X,3(I2.2,'/'),I2.2,'Z  ',A)
140   FORMAT (15X,A)
C
      END
