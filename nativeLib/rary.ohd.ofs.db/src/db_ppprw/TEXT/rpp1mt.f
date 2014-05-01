C MEMBER PPRPP1MT
C-----------------------------------------------------------------------
C
       SUBROUTINE RPP1MT (IPTRMT,TMPMAX,TMPMIN,ISTAT)
C
C          SUBROUTINE:  RPP1MT
C
C 8-17-83 CHANGE TO READ TENTHS OF A DEGREE SRS.
C             VERSION:  1.0.0
C
C                DATE:  12-9-82
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL READ THE MEAN MONTHLY TEMPS FOR 1 STATION
C    FROM THE RESERVED MMMT RECORDS IN THE PPPDB               *
C    IT READS 2*12 VALUES, TWO FOR EACH MONTH.
C
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IPTRMT      I    I     1    SUBSCRIPT IN MMMT RECORD FOR THIS
C                                       STATION
C       TMPMAX       R    O    12    MEAN MONTHLY TEMPS, 1/MONTH
C       ISTAT       I    O     1    STATUS CODE
C                                      0=SUCCESSFUL READ
C                                      1=SYSTEM READ ERROR
C                                      2=MMMT NOT DEFINED
C                                      3=INVALID POINTER VALUE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/pppdta'
      INCLUDE 'pppcommon/ppunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION TMPMAX(12),TMPMIN(12),ICNTL(16)
      INTEGER*2 WORK(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/rpp1mt.f,v $
     . $',                                                             '
     .$Id: rpp1mt.f,v 1.1 1995/09/17 18:45:12 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LMMMT/4hMMMT/
C
C***********************************************************************
C
C
      IF (IPPTR.EQ.1) WRITE (IOGDB,2000)
2000  FORMAT(' ENTERING SUBROUTINE RPP1MT')
C
      ISTAT=0
      DO 4 I=1,12
         TMPMAX(I)=-999.
         TMPMIN(I)=-999.
4        CONTINUE
      LREC2=LRECPP*2
C
C GET MEAN MONTHLY TEMPS CONTROL RECORD
      IDXDAT=IPCKDT(LMMMT)
      ICREC=IPDTDR(3,IDXDAT)
C
C  CHECK THAT MMMTS IS VALID IN THIS SYSTEM
      IF (ICREC.NE.0) GO TO 5
         IF (IPPDB.EQ.1) WRITE (LPE,2005)
2005  FORMAT(' **ERROR** IN RPP1MT. MEAN MONTHLY TEMPS NOT DEFINED IN ',
     *  'THIS DATA BASE')
         ISTAT=2
         GO TO 999
5     LUFILE=KPPRMU(IPDTDR(2,IDXDAT))
      CALL UREADT (LUFILE,ICREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF (IPPDB.EQ.1) WRITE (IOGDB,2002) ICNTL
2002  FORMAT(' SUB RPP1MT. MMMT CONTROL:',4I4,2X,A4,2X,11I4)
C
C  CHECK FOR VALID POINTER VALUE
      IF (IPTRMT.GE.1.AND.IPTRMT.LE.ICNTL(9)) GO TO 15
         IF (IPPDB.EQ.1) WRITE (LPE,2006) IPTRMT
2006  FORMAT (' **ERROR** IN RPP1MT. INVALID POINTER VALUE : ',I5)
         ISTAT=3
         GO TO 999
C
C COMPUTE THE RECORD AND SLOT NUMBER
15    IREC=IUNRCD(IPTRMT,LREC2)+ICNTL(2)-1
      JSLOT=IPTRMT-(IREC-ICNTL(2))*LREC2
      IF (IPPDB.EQ.1) WRITE (IOGDB,2010) IREC,JSLOT
2010  FORMAT(' SUB RPP1MT, FIRST RECORD=',I6,' JSLOT=',I6)
C
C  READ VALUES FOR EACH MONTH
      DO 250 IMO=1,12
         CALL UREADT (LUFILE,IREC,WORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 900
C       GET MAXTEMP FOR THIS MONTH, DIVIDE BY 10 TO GET BACK TO REAL
         TMPMAX(IMO)=WORK(JSLOT)
         TMPMAX(IMO)=TMPMAX(IMO)/10
         IF (IPPDB.EQ.1) WRITE (IOGDB,2009) IMO,WORK(JSLOT),TMPMAX(IMO)
         IREC=IREC+ICNTL(3)
         CALL UREADT (LUFILE,IREC,WORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 900
C       GET MINTEMP FOR THIS MONTH, DIVIDE BY 10 TO GET BACK TO REAL
         TMPMIN(IMO)=WORK(JSLOT)
         TMPMIN(IMO)=TMPMIN(IMO)/10
         IF (IPPDB.EQ.1) WRITE (IOGDB,2009) IMO,WORK(JSLOT),TMPMIN(IMO)
2009  FORMAT(' SUB RPP1MT. MONTH, FILE VALUE:',2I6,' TEMP=',F6.2)
         IREC=IREC+ICNTL(3)
250   CONTINUE
      GO TO 999
C
C  READ/WRITE ERROR
900   IF (IPPDB.EQ.1) WRITE (LPE,2003) LUFILE,ISTAT
2003  FORMAT(' **ERROR** IN RPP1MT. READING FROM FILE',I4,
     *     ' ISTAT=',I5)
      ISTAT=1
C
999   IF (IPPDB.EQ.1.OR.IPPTR.EQ.1) WRITE (IOGDB,2004) ISTAT
2004  FORMAT(' SUB RPP1MT COMPLETE, STATUS=',I5)
C
      RETURN
C
      END
