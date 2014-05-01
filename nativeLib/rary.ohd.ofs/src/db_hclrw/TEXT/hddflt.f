C MEMBER HDDFLT
C  (from old member HCLSETDF)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/04/95.09:39:56 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HDDFLT (IGL)
C
C
C          ROUTINE:  HDDFLT
C
C             VERSION:  1.0.0
C
C                DATE:  4-15-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE PARSES THE FIRST CARD FOR A SETDEFAULT COMMAND
C    AND CALLS THE ROUTINE FOR PROC, FUNC, OR TECH.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IGL        I     I     1    LOCAL/GLOBAL INDICATOR
C                                     @SETGDFLT=-1
C                                     @SETLDFLT=1
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hcomnd'
      INCLUDE 'hclcommon/hwords'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER NAME(2),LEFT(2),RIGHT(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hddflt.f,v $
     . $',                                                             '
     .$Id: hddflt.f,v 1.1 1995/09/17 18:41:52 dws Exp $
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
C
C        READ FIRST CARD
C
      NNCARD=1
      CALL HCARDR (NNCARD,ISTAT)
      IF (ISTAT.NE.0) GO TO 70
      IF=3
      CALL HNMPSW (IF,NAME,IPASS,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
C
C       CHECK TO MAKE SURE OF PASSWORD
C
      IF (IGL.EQ.-1) GO TO 10
      IF (IPASS.EQ.IBLNK) GO TO 10
      CALL HFEQLS (IF-1,LEFT,RIGHT,ISTRT)
      IF (LEFT(1).EQ.LPASS) GO TO 10
      IF=IF - 1
      IPASS=IBLNK
10    CONTINUE
C
C        CHECK THE KEYWORD AND CALL THE ROUTINE
C
      IF (KEYWRD.GE.1.AND.KEYWRD.LE.3) GO TO 30
      CALL ULINE (LP,2)
      WRITE (LP,20)
20    FORMAT ('0**ERROR** PROC, FUNC, OR TECH MUST BE SPECIFIED.')
      ISTAT=1
      GO TO 90
30    CONTINUE
      GO TO (40,50,60),KEYWRD
40    CONTINUE
C
C        SET DEFAULT FOR PROC
C
      CALL HDFLTP (IGL,IF,NAME,IPASS,NNCARD)
      GO TO 90
50    CONTINUE
C
C        SET DEFAULT FOR FUNC
C
      CALL HDFLTF (IGL,IF,NAME,IPASS,NNCARD)
      GO TO 90
60    CONTINUE
C
C        SET DEFAULT FOR TECH
C
      CALL HDFLTT (IGL,IF,NAME,IPASS,NNCARD)
      GO TO 90
70    CONTINUE
C
C        SYSTEM ERROR
C
      CALL ULINE (LP,2)
      WRITE (LP,80)
80    FORMAT ('0**ERROR** SYSTEM ERROR.')
C
90    RETURN
C
      END
