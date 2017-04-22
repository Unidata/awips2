C MEMBER HPTRCD
C  (from old member HPTRCD)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/29/94.08:28:17 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HPTRCD (ITYPE,NWORDS,BUFFER,ISTAT)
C
C
C          ROUTINE:  HPTRCD
C
C             VERSION:  1.0.1 5-25-82 ADDED ENTRY HPTRDN
C
C             VERSION:  1.0.0
C
C                DATE: 8-27-81
C
C              AUTHOR:  JIM ERLANDSON
C
C***********************************************************************
C
C          DESCRIPTION:
C
C   ROUTINE WILL ENTER A RECORD INTO THE APPROPRIATE HCL FILE AND
C   UPDATE THE CORRESPONDING INDEX FILE.
C   CHECKS FOR UNIQUENESS OF NAME AND FOR SPACE IN DESTINATION FILE.
C   ALSO CHECKS FOR VALID RECORD TYPE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         ITYPE    I    INPUT  1     RECORD TYPE   +=LOCAL
C                                                  -=GLOBAL
C                                                  1=PROCEDURE
C                                                  2=FUNCTION
C                                                  3=TECHNIQUE
C                                                  4=NAMED OPTS
C                                                  4=DEFAULTS
C         NWORDS   I   INPUT   1     NUMBER OF WORDS IN BUFFER
C         BUFFER   I   I/O     ?     RECORD TO BE ENTERED
C         ISTAT    I   OUTPUT  1     STATUS INDICATOR
C                                                  0=RECORD ENTERED
C                                                  1=NAME NOT UNIQUE
C                                                  2=FILE FULL
C                                                  3=BAD RECORD TYPE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hcntrl'
      INCLUDE 'hclcommon/hwords'
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION I3(2),NAME(2)
      INTEGER BUFFER(NWORDS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hptrcd.f,v $
     . $',                                                             '
     .$Id: hptrcd.f,v 1.1 1995/09/17 18:42:55 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      ISTAT=0
      IF (IHCLDB.EQ.3) WRITE (IOGDB,20)
20    FORMAT (' HPTRCD ENTERED')
C
C     CHECK FOR VALID TYPE
C
      CALL UMEMOV (BUFFER(4),NAME,2)
      IF (ITYPE.EQ.-4) GO TO 30
      IF (ITYPE.EQ.0) GO TO 30
      IF (ITYPE.LT.-5.OR.ITYPE.GT.5) GO TO 30
C
C     DETERMINE FILES TO USE
C
      IXUNIT=KINDXL
      IDUNIT=KDEFNL
      ISUB=1
      CALL UMEMOV (LLOCAL,I3,2)
C
      IF (ITYPE.GT.0) GO TO 50
      IXUNIT=KINDXG
      IDUNIT=KDEFNG
      ISUB=2
      CALL UMEMOV (LGLOBL,I3,2)
C
      GO TO 50
30    CONTINUE
C
C     ERROR - INVALID TYPE
C
      WRITE (LPE,40) NAME,ITYPE
40    FORMAT (' **ERROR** IN HPTRCD. RECORD ',2A4,' HAS INVALID TYPE ',
     *      I2,' WAS PASSED-VALID RANGE IS -5, -3 TO -1, 1 TO 5')
      ISTAT=3
      GO TO 160
C
50    CONTINUE
C
C     SEE IF NAME EXISTS IN INDEXES
C
      IF (ITYPE.EQ.-5.OR.ITYPE.EQ.5) GO TO 80
      ITYP=ITYPE
      CALL HFNDDF(NAME,IREC,ITYP,IXREC)
      IF (IREC.EQ.0) GO TO 80
      CALL UMEMOV (LGLOBL,I3,2)
      IF (ITYP.GT.0) CALL UMEMOV (LLOCAL,I3,2)
C
      WRITE (LPE,60) NAME,I3
60    FORMAT (' **ERROR** IN HPTRCD. ',2A4,' ALREADY DEFINED AS ',2A4)
      ISTAT=1
      GO TO 160
C
80    CONTINUE
C
C     CHECK FOR ROOM IN FILE
C
      LRECL=HCNTL(4,ISUB)
      NLREC=(NWORDS+(LRECL-1))/LRECL
      BUFFER(1)=NLREC
C
      ISREC=HCNTL(7,ISUB)+1
      IF (ISREC+NLREC.LT.HCNTL(6,ISUB)) GO TO 100
C
C     ERROR - NOT ENOUGH ROOM IN FILE
C
      WRITE (LPE,90) IDUNIT,NLREC,NAME
90    FORMAT (' **ERROR** IN HPTRCD. NOT ENOUGH ROOM IN UNIT # ',
     *    I2,' FOR ',I2,' RECORDS FOR RECORD ',2A4)
      ISTAT=2
      GO TO 160
C
C     CHECK INDEX AND UPDATE POINTERS
C
100   CONTINUE
      IF (ITYPE.EQ.-5.OR.ITYPE.EQ.5) GO TO 120
C
C        UPDATE INDEX FOR NEW RECORD
C
      INUM=0
      IF (IABS(ITYPE).EQ.2) INUM=BUFFER(2)
      CALL HWIXEN(NAME,INUM,IXUNIT,ISREC,ITYPE,ISTA)
      BUFFER(2)=INUM
      IF (ISTA.NE.0) GO TO 140
C
C     READY TO WRITE RECORD - ZERO FILL LAST LOGICAL RECORD FIRST
C
120   CONTINUE
      NZEROS=MOD(NWORDS,LRECL)
      IF (NZEROS.EQ.0) GO TO 130
      NUM=LRECL-NZEROS
      ISPO=NWORDS+1
      CALL UMEMST(0,BUFFER(ISPO),NUM)
C
130   CONTINUE
      CALL WVLRCD(IDUNIT,ISREC,NLREC,BUFFER,LRECL,ISTA)
      IF (ISTA.NE.0) GO TO 140
      HCNTL(7,ISUB)=HCNTL(7,ISUB)+NLREC
      GO TO 160
C
140   CONTINUE
      WRITE (LPE,150) NAME,I3
150   FORMAT (' **ERROR** IN HPTRCD. SYSTEM PROBLEM WITH RECORD ',
     *    2A4,1X,2A4)
      ISTAT=4
C
160   CONTINUE
C
C     WRITE DEBUG AND RETURN
C
      IF (IHCLDB.EQ.3) WRITE (IOGDB,170) ISTAT,NAME,IXUNIT,IDUNIT,
     *   ISREC,NLREC,(BUFFER(I),I=1,NWORDS)
170   FORMAT (' ISTAT =',I2,' NAME=',2A4,' INDEX UNIT=',I2,
     *  ' RECORD UNIT=',I2,' RECORD #=',I6,' # OF RECORDS=',
     2    I2/' RECORD ',3I4,2A4,2X,A4,14I4/(1X,20I4))
C
      RETURN
C
      END
