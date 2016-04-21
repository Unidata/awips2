C MEMBER HFNDDF
C  (from old member HCLUTIL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/21/94.10:39:00 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HFNDDF (NAME,IREC,ITYPE,IXREC)
C
C          ROUTINE:  HFNDDF
C
C             VERSION:  1.0.0
C
C                DATE: 7-30-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO SEARCH THE HCL INDEXES FOR A DEFINITION RECORD
C    ROUTINE FIRST CHECKS THE GLOBAL INDEX THEN THE LOCAL INDEX
C    CHECKS FOR VALID RECORD TYPE RANGE IS -3 TO -1, 1 TO 4
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NAME        I    I     2     NAME OF RECORD
C
C       IREC        I    O     1     RECORD NUMBER IF FOUND
C                                    0 IF NOT FOUND
C
C       ITYPE       I    I/O   1     TYPE OF RECORD - WHEN RETURND,
C                                    GLOBAL OR LOCAL IS INDICATED
C
C       IXREC       I     O    1     INDEX RECORD #  0 IF NOT FOUND
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hunits'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION NAME(2), IARR(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hfnddf.f,v $
     . $',                                                             '
     .$Id: hfnddf.f,v 1.1 1995/09/17 18:42:19 dws Exp $
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
      IXREC=0
      IREC=0
      ITYPA=IABS(ITYPE)
      IF (ITYPA.LT.1.OR.ITYPA.GT.4) GO TO 80
C
C     SEARCH GLOBAL INDEX
C
      ISUB=ITYPA+4
      IF (ISUB.EQ.8) GO TO 30
C
      I1=HINDEX(2,ISUB)
      I2=HINDEX(3,ISUB)
      IF (I2.LT.I1) GO TO 30
C
      DO 10 I=I1,I2
           CALL UREADT (KINDXG,I,IARR,ISTAT)
           IF (ISTAT.NE.0) GO TO 60
           CALL UNAMCP (NAME,IARR,ISTAT)
           IF (ISTAT.EQ.0) GO TO 20
10         CONTINUE
      GO TO 30
20    CONTINUE
C
C     FOUND IT IN GLOBAL INDEX
C
      IREC=IARR(3)
      ITYPE=-ITYPA
      IXREC=I
      GO TO 100
30    CONTINUE
C
C     TRY THE LOCAL INDEX
C
      I1=HINDEX(2,ITYPA)
      I2=HINDEX(3,ITYPA)
      IF (I2.LT.I1) GO TO 100
C
      DO 40 I=I1,I2
          CALL UREADT (KINDXL,I,IARR,ISTAT)
          IF (ISTAT.NE.0) GO TO 60
          CALL UNAMCP (NAME,IARR,ISTAT)
          IF (ISTAT.EQ.0) GO TO 50
40        CONTINUE
      GO TO 100
50    CONTINUE
C
C     I FOUND IT
C
      IREC=IARR(3)
      ITYPE=ITYPA
      IXREC=I
      GO TO 100
C
60    CONTINUE
C
C     DAIO ERROR
C
      WRITE (LPE,70) NAME
70    FORMAT (' **ERROR** IN HFNDDF - DAIO ERROR FOR RECORD ',2A4)
      GO TO 100
C
80    CONTINUE
C
C     INVALID TYPE
C
      WRITE (LPE,90) NAME,ITYPE
90    FORMAT (' **ERROR** IN HFNDDF - INVALID TYPE FOR RECORD ',2A4,
     *    ' TYPE PASSED WAS ',I2)
C
100   CONTINUE
C
C     WRITE DEBUG AND RETURN
C
      IF (IHCLTR.GT.2) WRITE (IOGDB,110) NAME,ITYPE,IREC
110   FORMAT (' EXIT HFNDDF - NAME=',2A4,' ITYPE=',I3,
     *    ' IREC=',I5)
C
      RETURN
C
      END
