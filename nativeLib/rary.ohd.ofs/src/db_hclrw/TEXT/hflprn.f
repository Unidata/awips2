C MEMBER HFLPRN
C  (from old member HCLCUTIL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/21/94.14:13:32 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HFLPRN (ISPO,IEPO,IFLPRN)
C
C          ROUTINE:  HFLPRN
C
C             VERSION:  1.0.0
C
C                DATE: 7-31-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO FIND THE POSITION IN IBUF OF A LEFT PARENTHSIS
C   A RANGE IS SPECIFIED AND ROUTINE CHECKS FOR VALID RANGE
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ISPO        I    I     1    STARTING POSITION IN IBUF
C
C       IEPO        I    I     1    ENDING POSITION IN IBUF
C
C       IFLPRN      I    O     1    POSITION LEFT PARENTHSIS FOUND
C                                   0 IF NOT FOUND
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'hclcommon/hwords'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hflprn.f,v $
     . $',                                                             '
     .$Id: hflprn.f,v 1.1 1995/09/17 18:42:16 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
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
      IFLPRN=0
C
      IF (ISPO.EQ.0) GO TO 30
      IF (ISPO.GT.80.OR.IEPO.GT.80) GO TO 30
      IF (ISPO.GT.IEPO) GO TO 30
C
      DO 10 I =ISPO,IEPO
          IF (IBUF(I).EQ.LLPARN) GO TO 20
10        CONTINUE
C
      GO TO 50
C
C     FOUND  A LEFT PARNENTHESES
C
20    CONTINUE
      IFLPRN=I
      GO TO 50
C
C     INVALID SEARCH POSITIONS
C
30    CONTINUE
      WRITE (LPE,40) ISPO,IEPO
40    FORMAT (' **ERROR** IN HFLPRN - INVALID SEARCH POSITIONS - ',
     1   'STARTING POSITION=',I2,' ENDING POSITION=',I2)
C
C     WRITE DEBUG AND RETURN
C
50    CONTINUE
      IF (IHCLTR.GT.2) WRITE (IOGDB,60) IFLPRN
60    FORMAT (' EXIT HFLPRN - IFLPRN=',I2)
C
      RETURN
C
      END
