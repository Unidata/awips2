C MEMBER HPFLDF
C  (from old member HCLPRFUN)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/04/95.13:04:49 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HPFLDF (LDF,LGFLAG,DBUF,LDBUF,IWHAT)
C
C          ROUTINE:  HPFLDF
C
C             VERSION:  1.0.0
C
C                DATE:  6-8-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE REPLACES THE GLOBAL DEFAULT FOR A FUNCTION'S
C    TECHNIQUE WITH THE LOCAL DEFAULT IF THERE IS ONE. THE TYPE
C    OF DEFAULT IS RETURNED
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       LDF        I     I     1    FLAG INDICATING A LOCAL DEFAULT
C
C       LGFLAG     I     I     1    LOCAL/GLOBAL FLAG FOR FUNCTION
C
C       DBUF       I    I/O    ?    DEFAULT BUFFER
C
C       LDBUF      I     I     ?    LOCAL DEFAULT BUFFER
C
C       IWHAT      I     O     2    LOCAL GLOBAL INDICATOR OF DEFAULT
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hwords'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER DBUF(*),LDBUF(*),IWHAT(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hpfldf.f,v $
     . $',                                                             '
     .$Id: hpfldf.f,v 1.1 1995/09/17 18:42:41 dws Exp $
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
      CALL UMEMOV (LLOCAL,IWHAT,2)
      IF (LGFLAG.EQ.0) CALL UMEMOV (LGLOBL,IWHAT,2)
      IF (LDF.EQ.0) GO TO 30
C
C        SEARCH LOCAL DEFAULT FOR TECHNIQUE
C
      IPOS=8
      NUMDEF=LDBUF(7)
      DO 10 I=1,NUMDEF
         IF (DBUF(1).EQ.LDBUF(IPOS)) GO TO 20
         IPOS=IPOS+2
10       CONTINUE
C
C     NOT FOUND
C
      GO TO 30
C
C         MOVE IT IN
C
20    DBUF(2)=LDBUF(IPOS+1)
      CALL UMEMOV (LLOCAL,IWHAT,2)
30    CONTINUE
C
      RETURN
C
      END
