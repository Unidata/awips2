C MEMBER WPDBCO
C  (from old member PRDRWCIO)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 12/16/94.09:40:30 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE WPDBCO (ISTAT)
C
C          ROUTINE:  WPDBCO
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
C  THIS ROUTINE WRITES CONTROL INFORMATION TO PROCESSED DATA BASE
C  DATA FILES FROM COMMON BLOCKS.
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/wpdbco.f,v $
     . $',                                                             '
     .$Id: wpdbco.f,v 1.2 1996/12/09 20:47:17 dws Exp $
     . $' /
C    ===================================================================
C
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
C  WRITE GENERAL PARAMETERS
      IREC=1
      IF (IAMORD.EQ.0) CALL UWRITT (IUNIT,IREC,USERPR(1),ISTAT)
      IF (IAMORD.EQ.1) CALL UWRITT (IUNIT,IREC,INAMRF(1),ISTAT)
      IF (ISTAT.GT.0) GO TO 50
C
C  RESET POINTER TO INCORE DATA TYPE
      DO 10 I=1,MAXDTP
         IF (IAMORD.EQ.0) DATFIL(10,I)=0
         IF (IAMORD.EQ.1) IDATFL(10,I)=0
10       CONTINUE
C
C  WRITE DATA TYPE DIRECTORY RECORDS
      IPOS=1
      IF (IAMORD.EQ.0) NTYPE=MAXDTP
      IF (IAMORD.EQ.1) NTYPE=MXTYPE
      LRECLP=240
      NWORDS=LRECLP/4
      NREC=(NTYPE*18)/NWORDS
      IF (NREC*NWORDS.LT.NTYPE*18) NREC=NREC+1
      DO 20 IREC=2,NREC+1
         IF (IAMORD.EQ.0) CALL UWRITT (IUNIT,IREC,ARRAY1(IPOS),ISTAT)
         IF (IAMORD.EQ.1) CALL UWRITT (IUNIT,IREC,ARRAY2(IPOS),ISTAT)
         IPOS=IPOS+NWORDS
20       CONTINUE
C
C  RESET LAST RECORD AND INDEX ACCESS COUNTER
      DO 30 I=1,NMPRDF
         IF (IAMORD.EQ.0) TSCNTR(5,I)=1
         IF (IAMORD.EQ.1) ITSCNT(5,I)=1
         IF (IAMORD.EQ.0) TSCNTR(6,I)=0
         IF (IAMORD.EQ.1) ITSCNT(6,I)=0
30       CONTINUE
C
C  WRITE TIMES FILE SERIES CONTROL RECORDS
      IREC=1
      DO 40 NUNIT=1,NMPRDF
         IF (IAMORD.EQ.0) IUNIT=KIUNTS(NUNIT)
         IF (IAMORD.EQ.1) IUNIT=KRUNTS(NUNIT)
         IF (IAMORD.EQ.0) CALL UWRITT (IUNIT,IREC,TSCNTR(1,NUNIT),ISTAT)
         IF (IAMORD.EQ.1) CALL UWRITT (IUNIT,IREC,ITSCNT(1,NUNIT),ISTAT)
         IF (ISTAT.GT.0) GO TO 50
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
70    FORMAT (' *** ENTER WPDBCO')
80    FORMAT (' USERPR=',2A4)
90    FORMAT (' INAMRF=',2A4)
100   FORMAT ('0*** ERROR - IN WPDBCO - DAIO ERROR WRITING RECORD ',I6,
     *   ' TO UNIT ',I2,'.')
110   FORMAT (' *** EXIT WPDBCO : ISTAT=',I2)
C
      END
