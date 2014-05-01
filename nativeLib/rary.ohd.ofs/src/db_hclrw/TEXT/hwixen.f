C MEMBER HWIXEN
C  (from old member HPTRCD)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/29/94.08:28:17 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HWIXEN (NAME,INUM,IUNIT,IREC,ITYPE,ISTAT)
C
C          ROUTINE:  HWIXEN
C
C             VERSION:  1.0.1  5-25-82  ADD SEARCH FOR DELETED SLOT
C
C             VERSION:  1.0.0
C
C                DATE: 7-28-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO UPDATE AN INDEX FOR HCL DEFINITIONS.
C    SEARCHES FOR A DELETED SLOT IN THE APPROPRIATE SECTION OF
C    THE HCL INDEX. IF NONE ARE FOUND IT USES THE NEXT RECORD
C    AND UPDATES THE POINTER. IT PLACES THE RECORD NUMBER AS THE
C    INTERNAL NUMBER EXCEPT FOR FUNCTIONS.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NAME       I     I     2     NAME OF RECORD
C
C       INUM       I    I/O    1     FUNCTION NUMBER OR 0
C                                    INTERNAL NUMBER IS RETURNED
C
C       IUNIT      I     I     1     UNIT OF INDEX
C
C
C       IREC       I     I     1     RECORD # OF RECORD IN FILE
C
C       ITYPE      I     I     1     TYPE OF RECORD
C
C       ISTAT      I     O     1     STATUS INDICATOR
C                                        0=NORMAL RETURN
C                                        1=DAIO ERROR
C                                        2=INDEX SECTION FULL
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'udebug'
      INCLUDE 'uio'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION NAME(2),IARR(4),LDEL(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hwixen.f,v $
     . $',                                                             '
     .$Id: hwixen.f,v 1.1 1995/09/17 18:43:18 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LDEL/4HDELE,4HTED /
C
C***********************************************************************
C
C
      IFLG=0
      ISTAT=0
C
      ISUB=ITYPE
      IF (ITYPE.LT.0) ISUB=IABS(ITYPE)+4
C
C        SEARCH INDEX FOR A DELETED SLOT
C
      IS=HINDEX(2,ISUB)
      IE=HINDEX(3,ISUB)
      IF (IE.LT.IS) GO TO 20
      DO 10 I=IS,IE
          CALL UREADT (IUNIT,I,IARR,ISTAT)
          IF (ISTAT.NE.0) GO TO 60
          CALL UNAMCP(LDEL,IARR,ISTAT)
          IF (ISTAT.EQ.0) GO TO 30
10        CONTINUE
C
C     COULD NOT FIND A SLOT
C
20    CONTINUE
      IFLG=1
      I=IE+1
      IF (I.GT.HINDEX(1,ISUB)) GO TO 40
30    CONTINUE
C
C        SET UP INDEX RECORD
C
      CALL UMEMOV (NAME,IARR,2)
      IARR(3)=IREC
      IARR(4)=I-HINDEX(2,ISUB)+1
      IF (INUM.NE.0) IARR(4)=INUM
      INUM=IARR(4)
C
      CALL UWRITT(IUNIT,I,IARR,ISTAT)
      IF (ISTAT.NE.0) GO TO 60
C
      IF (IFLG.EQ.1) HINDEX(3,ISUB)=I
      GO TO 80
C
40    CONTINUE
C
C     INDEX SECTION FULL NOOOOOOOOOOOOOOOOOOOOOO!
C
      WRITE (LPE,50) ITYPE
50    FORMAT (' **ERROR** IN HWIXEN. INDEX SECTION IS FULL FOR TYPE ',
     *   I2)
      ISTAT=3
      GO TO 80
C
60    CONTINUE
C
C     DAIO ERROR
C
      WRITE (LPE,70) NAME
70    FORMAT (' **ERROR** IN HWIXEN. DAIO ERROR FOR RECORD ',2A4)
      ISTAT=2
C
80    CONTINUE
      IF (IHCLDB.EQ.3) WRITE (IOGDB,90) ISTAT,I,IUNIT,NAME
90    FORMAT (' HWIXEN EXECUTED. STATUS=',I2,' RECORD # ',I5,
     *  ' WRITTEN TO UNIT # ',I2,' FOR RECORD ',2A4)
C
      RETURN
C
      END
