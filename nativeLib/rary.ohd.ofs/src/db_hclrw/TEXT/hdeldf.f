C MEMBER HDELDF
C  (from old member HCLDELET)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/18/94.12:44:49 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HDELDF (IGL)
C
C
C          ROUTINE:  HDELDF
C
C             VERSION:  1.0.1 4-19-82 JCE ADD DELG AND DELL COMMANDS
C
C             VERSION:  1.0.0
C
C                DATE:  12-9-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO DELETE PROC FUNC AND TECH. GETS NAME AND
C    PASSWORD THEN CALLS APPROPRIATE ROUTINE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         IGL      I     I     1    DELG / DELL INDICATOR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'common/where'
      INCLUDE 'ufreei'
      INCLUDE 'hclcommon/hcomnd'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER NAME(2),PASS,IROUT(2),ITEMP(2),LEFT(2),RIGHT(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdeldf.f,v $
     . $',                                                             '
     .$Id: hdeldf.f,v 1.1 1995/09/17 18:41:53 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IROUT(1)/4HHDEL/,IROUT(2)/4HDF  /
C
C***********************************************************************
C
C
      CALL UMEMOV (OPNAME,ITEMP,2)
      CALL UMEMOV (IROUT,OPNAME,2)
      IF (KEYWRD.GE.1.AND.KEYWRD.LE.4) GO TO 20
      WRITE (LP,10)
10    FORMAT ('0**ERROR** INVALID KEYWORD FOR DELETE COMMANDS')
      CALL ERROR
      GO TO 100
20    CONTINUE
      IFLD=3
      NNCARD=1
C
      CALL HCARDR (NNCARD,ISTAT)
      IF (ISTAT.NE.0) GO TO 80
C
C        GET NAME AND PASSWORD
C
      CALL HNMPSW(IFLD,NAME,PASS,ISTAT)
      IF (ISTAT.EQ.0) GO TO 30
C
      CALL ERROR
      GO TO 100
30    CONTINUE
C
C          FIND # OF CHARACTERS IN NAME
C
      CALL HFEQLS(3,LEFT,RIGHT,ISRT)
      NUMCAR=IFSTOP(3)-ISRT+1
      GO TO (40,50,60,70),KEYWRD
40    CONTINUE
C
C          DELETE A PROC
C
      CALL HDELPR(IGL,NAME,PASS,NUMCAR)
      GO TO 100
50    CONTINUE
C
C          DELETE A FUNCTION
C
      CALL HDELFN(IGL,NAME,PASS,NUMCAR)
      GO TO 100
60    CONTINUE
C
C         DELETE A TECHNIQUE
C
      CALL HDELTN(IGL,NAME,PASS)
      GO TO 100
70    CONTINUE
C
C        DELETE A NAMED OPTION
C
      CALL HDELOP(IGL,NAME,PASS)
      GO TO 100
80    CONTINUE
C
C         SYSTEM ERROR
C
      WRITE (LP,90)
90    FORMAT ('0**ERROR** SYSTEM ERROR')
      CALL ERROR
100   CONTINUE
C
C        END
C
      CALL UMEMOV (ITEMP,OPNAME,2)
      IF (IHCLDB.EQ.3) WRITE (IOGDB,110)
110   FORMAT ('  HDELDF EXECUTED')
C
      RETURN
C
      END
