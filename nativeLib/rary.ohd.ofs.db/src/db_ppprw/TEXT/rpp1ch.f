C MEMBER PPRPP1CH
C-----------------------------------------------------------------------
C
       SUBROUTINE RPP1CH (IPTRCH,PXCHR,ISTAT)
C
C          SUBROUTINE:  RPP1CH
C
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
C    THIS ROUTINE WILL READ THE CHARACTERISTICS FOR 1 STATION
C    TO THE RESERVED CHARACTERISTIC RECORDS IN THE PPPDB
C    IT READS 12 VALUES, ONE FOR EACH MONTH.
C
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IPTRCH      I    I     1    SUBSCRIPT IN CHAR RECORD FOR THIS
C                                       STATION
C       PXCHR       R    O    12    CHARACTERISTICS, 1/MONTH
C       ISTAT       I    O     1    STATUS
C                                      0=SUCCESSFUL READ
C                                      1=SYSTEM READ ERROR
C                                      2=CHAR NOT DEFINED
C                                      3=INVALID VALUE OF POINTER
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
      DIMENSION PXCHR(12),ICNTL(16)
      INTEGER*2 WORK(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/rpp1ch.f,v $
     . $',                                                             '
     .$Id: rpp1ch.f,v 1.1 1995/09/17 18:45:11 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LCHAR/4hCHAR/
C
C***********************************************************************
C
C
      IF (IPPTR.EQ.1) WRITE (IOGDB,2000)
2000  FORMAT(' ENTERING SUBROUTINE RPP1CH')
C
      ISTAT=0
      DO 4 I=1,12
         PXCHR(I)=-999.
4        CONTINUE
      LREC2=LRECPP*2
C
C GET CHARACTERISTICS CONTROL RECORD
      IDXDAT=IPCKDT(LCHAR)
      ICREC=IPDTDR(3,IDXDAT)
C
C  CHECK THAT CHARS IS VALID IN THIS SYSTEM
      IF (ICREC.NE.0) GO TO 5
         IF (IPPDB.EQ.1) WRITE (LPE,2005)
2005  FORMAT(' **ERROR** IN RPP1CH. CHARACTERISTICS NOT DEFINED IN ',
     *  'THIS DATA BASE')
         ISTAT=2
         GO TO 999
5     LUFILE=KPPRMU(IPDTDR(2,IDXDAT))
      CALL UREADT (LUFILE,ICREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF (IPPDB.EQ.1) WRITE (IOGDB,2002) ICNTL
2002  FORMAT(' SUB RPP1CH. CHAR CONTROL:',4I4,2X,A4,2X,11I4)
C
C  CHECK FOR VALID POINTER VALUE
      IF (IPTRCH.GE.1.AND.IPTRCH.LE.ICNTL(9)) GO TO 15
         IF (IPPDB.EQ.1) WRITE (LPE,2006) IPTRCH
2006  FORMAT (' **ERROR** IN RPP1CH. INVALID POINTER VALUE : ',I5)
         ISTAT=3
         GO TO 999
C
C  COMPUTE THE RECORD AND SLOT NUMBER
15    IREC=IUNRCD(IPTRCH,LREC2)+ICNTL(2)-1
      JSLOT=IPTRCH-(IREC-ICNTL(2))*LREC2
      IF (IPPDB.EQ.1) WRITE (IOGDB,2010) IREC,JSLOT
2010  FORMAT(' SUB RPP1CH, FIRST RECORD=',I6,' JSLOT=',I6)
C
C  GET VALUES FOR EACH MONTH
      DO 250  IMO=1,12
         CALL UREADT (LUFILE,IREC,WORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 900
C       GET CHAR FOR THIS MONTH, DIVIDE BY 100 TO GET BACK TO REAL
         PXCHR(IMO)=WORK(JSLOT)
         PXCHR(IMO)=PXCHR(IMO)/100
         IF (IPPDB.EQ.1) WRITE (IOGDB,2009) IMO,WORK(JSLOT),PXCHR(IMO)
2009  FORMAT(' SUB RPP1CH. MONTH, FILE VALUE:',2I6,' PXCH=',F6.2)
         IREC=IREC+ICNTL(3)
250   CONTINUE
      GO TO 999
C
C  READ/WRITE ERROR
900   IF (IPPDB.EQ.1) WRITE (LPE,2003) LUFILE,ISTAT
2003  FORMAT(' **ERROR** IN RPP1CH. READING FROM FILE',I4,
     *     ' ISTAT=',I5)
      ISTAT=1
C
999   IF (IPPDB.EQ.1.OR.IPPTR.EQ.1) WRITE (IOGDB,2004) ISTAT
2004  FORMAT(' SUB RPP1CH COMPLETE, STATUS=',I5)
C
      RETURN
C
      END
