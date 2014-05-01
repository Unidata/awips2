C MEMBER RPDBCI
C  (from old member PRDRWCIO)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 12/16/94.09:40:30 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPDBCI (ISTAT)
C
C          ROUTINE:  RPDBCI
C
C             VERSION:  1.0.0
C
C              AUTHOR:  SONJA R. SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  THIS ROUTINE READS CONTROL INFORMATION FROM PROCESSED DATA BASE
C  DATA FILES INTO COMMON BLOCKS.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         ISTAT     I    O     1     STATUS INDICATOR
C                                      0=NORMAL RETURN
C                                      1=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/ptsctl'
      INCLUDE 'urcommon/urmaxm'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urftbl'
      INCLUDE 'urcommon/urtscl'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION ARRAY1(360),ARRAY2(360)
      DIMENSION KIUNTS(5),KRUNTS(5)
C
      EQUIVALENCE (ARRAY1(1),DATFIL(1,1))
      EQUIVALENCE (ARRAY2(1),IDATFL(1,1))
      EQUIVALENCE (KIUNTS(1),KMAPTS)
      EQUIVALENCE (KRUNTS(1),KUMAPT)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/rpdbci.f,v $
     . $',                                                             '
     .$Id: rpdbci.f,v 1.2 1996/12/09 20:46:31 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,70)
C
      ISTAT=0
C
C  SET UNIT NUMBER
      IF (IAMORD.EQ.0) IUNIT=KRFCPR
      IF (IAMORD.EQ.1) IUNIT=KUFCPR
C
C  READ GENERAL PARAMETERS
      IREC=1
      IF (IAMORD.EQ.0) CALL UREADT (IUNIT,IREC,USERPR(1),ISTAT)
      IF (IAMORD.EQ.1) CALL UREADT (IUNIT,IREC,INAMRF(1),ISTAT)
      IF (ISTAT.GT.0) GO TO 50
C
C  READ DATA TYPE DIRECTORY RECORDS
      IPOS=1
      IF (IAMORD.EQ.0) NTYPE=MAXDTP
      IF (IAMORD.EQ.1) NTYPE=MXTYPE
      LRECLP=240
      NWORDS=LRECLP/4
      NREC=(NTYPE*18)/NWORDS
      IF (NREC*NWORDS.LT.NTYPE*18) NREC=NREC+1
      DO 10 IREC=2,NREC+1
         IF (IAMORD.EQ.0) CALL UREADT (IUNIT,IREC,ARRAY1(IPOS),ISTAT)
         IF (IAMORD.EQ.1) CALL UREADT (IUNIT,IREC,ARRAY2(IPOS),ISTAT)
         IF (ISTAT.GT.0) GO TO 50
         IPOS=IPOS+NWORDS
10       CONTINUE
C
C  READ TIME SERIES DATA FILE CONTROL RECORDS
      IREC=1
      DO 20 NUNIT=1,NMPRDF
         IF (IAMORD.EQ.0) IUNIT=KIUNTS(NUNIT)
         IF (IAMORD.EQ.1) IUNIT=KRUNTS(NUNIT)
         IF (IAMORD.EQ.0) CALL UREADT (IUNIT,IREC,TSCNTR(1,NUNIT),ISTAT)
         IF (IAMORD.EQ.1) CALL UREADT (IUNIT,IREC,ITSCNT(1,NUNIT),ISTAT)
         IF (ISTAT.GT.0) GO TO 50
20       CONTINUE
C
C  SET LAST RECORD READ TO 1 FOR READ ROUTINE
      DO 40 I=1,NMPRDF
         IF (IAMORD.EQ.0) TSCNTR(5,I)=1
         IF (IAMORD.EQ.1) ITSCNT(5,I)=1
40       CONTINUE
C
      IF (IPRDB.GT.0) THEN
         IF (IAMORD.EQ.0) WRITE (IOGDB,80) USERPR
         IF (IAMORD.EQ.1) WRITE (IOGDB,90) INAMRF
         WRITE (IOGDB,*) 'MAXDTP=',MAXDTP,
     *      ' MAXTMS=',MAXTMS,
     *      ' NUMDTP=',NUMDTP,
     *      ' NUMTMS=',NUMTMS
         ENDIF
      GO TO 60
C
C  ERROR
50    WRITE (LPE,100) IREC,IUNIT
C
60    IF (IPRTR.GT.0) WRITE (IOGDB,110) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER RPDBCI')
80    FORMAT (' USERPR=',2A4)
90    FORMAT (' INAMRF=',2A4)
100   FORMAT ('0*** ERROR - IN RPDBCI - DAIO ERROR READING RECORD ',I6,
     *   ' FROM UNIT ',I2,'.')
110   FORMAT (' *** EXIT RPDBCI : ISTAT=',I2)
C
      END
