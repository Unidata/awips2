C MEMBER HFDFNM
C  (from old member HCLFDFNM)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/29/94.08:13:34 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HFDFNM (IFNUM,IFNAM,IFREC)
C
C          ROUTINE:  HFDFNM
C
C             VERSION:  1.0.0
C
C                DATE:  1-25-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO FIND THE NAME OF A FUNCTION WHEN THE INTERNAL
C    NUMBER IS KNOWN.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IFNUM      I     I     1    INTERNAL FUNCTION NUMBER
C
C       IFNAM      A8    O     2    FUNCTION NAME
C
C       IFREC      I     O     1    RECORD NUMBER IN INDEX OF FUNCTION
C                                   0 IF NOT FOUND
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hunits'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER IFNAM(2),IXBUF(4),IDELET(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hfdfnm.f,v $
     . $',                                                             '
     .$Id: hfdfnm.f,v 1.1 1995/09/17 18:42:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IDELET(1)/4HDELE/,IDELET(2)/4HTED /
C
C***********************************************************************
C
C
      IFREC = 0
      CALL UMEMST(IBLNK,IFNAM,2)
C
C        GET UNITS AND SUBSCRIPT OF INDEX
C
      ISUB = 2
      IUNIT = KDEFNL
      IXUNIT = KINDXL
      IF (IFNUM.GE.500) GO TO 10
C GLOBAL
      ISUB = 6
      IUNIT = KDEFNG
      IXUNIT = KINDXG
10    CONTINUE
C
C        SEARCH INDEX FOR MATCHING FUNCTION NUMBER
C
      IS = HINDEX(2,ISUB)
      IE = HINDEX(3,ISUB)
      IF (IE.LT.IS) GO TO 30
      DO 20 I = IS,IE
         CALL UREADT (IXUNIT,I,IXBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 40
C SEE IF SAME NUMBER
         IF (IFNUM.NE.IXBUF(4)) GO TO 20
C SEE IF DELETED
         CALL UNAMCP(IDELET,IXBUF(1),ISTAT)
         IF (ISTAT.EQ.0) GO TO 20
C FOUND IT
         CALL UMEMOV (IXBUF(1),IFNAM,2)
         IFREC = I
         GO TO 60
20       CONTINUE
C
C     DID NOT FIND IT
C
30    CONTINUE
      GO TO 60
C
C        ERROR
C
40    CONTINUE
      WRITE (LPE,50)
50    FORMAT (' **ERROR** SYSTEM ERROR IN HFDFNM')
C
C        RETURN
C
60    CONTINUE
C
      RETURN
C
      END
