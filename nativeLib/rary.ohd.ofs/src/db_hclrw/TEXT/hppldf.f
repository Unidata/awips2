C MEMBER HPPLDF
C  (from old member HCLPRPRC)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/04/95.09:39:04 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HPPLDF (LDF,LGFLAG,DBUF,DBUFL,ITYPE)
C
C          ROUTINE:  HPPLDF
C
C             VERSION:  1.0.0
C
C                DATE:  6-7-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE REPLACES THE GLOBAL DEFAULT FOR A PROCEDURE
C    OR TECHNIQUE WITH THE LOCAL DEFAULT IF THERE IS ONE. THE TYPE
C    OF DEFAULT IS RETURNED.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       LDF        I     I     1    FLAG INDICATING A LOCAL DEFAULT
C
C       LGFLAG     I     I     1    LOCAL/GLOBAL FLAG FOR PROC OR TECH
C
C       DBUF       I    I/O    ?    DEFAULT BUFFER
C
C       DBUFL      I     I     ?    LOCAL DEFAULT BUFFER
C
C       ITYPE      I    I/O    2    LOCAL/GLOBAL INDICATOR
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
      INTEGER DBUF(*),DBUFL(*),ITYPE(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hppldf.f,v $
     . $',                                                             '
     .$Id: hppldf.f,v 1.1 1995/09/17 18:42:45 dws Exp $
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
      CALL UMEMOV (LLOCAL,ITYPE,2)
      IF (LGFLAG.LE.0) CALL UMEMOV (LGLOBL,ITYPE,2)
C
C        IF NO LOCAL DEFAULT - RETURN
C
      IF (LDF.EQ.0) GO TO 30
C
C        SEARCH FOR PARM IN LOCAL DEFAULT
C
      IPOS=8
      NUMDEF=DBUFL(7)
      DO 10 I=1,NUMDEF
          IF (DBUFL(IPOS).EQ.DBUF(1)) GO TO 20
          IPOS=IPOS+DBUFL(IPOS+1)+2
10        CONTINUE
C
C     NOT FOUND
C
      GO TO 30
C
C        MOVE LOCAL DEFAULT INTO DEFAULT
C
20    CALL UMEMOV (DBUFL(IPOS+2),DBUF(3),DBUFL(IPOS+1))
      CALL UMEMOV (LLOCAL,ITYPE,2)
C
30    RETURN
C
      END
