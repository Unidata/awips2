C MEMBER HNMPSW
C  (from old member HCLCUTIL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/21/94.14:13:32 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HNMPSW (IFIELD,NAME,IPASS,ISTAT)
C
C          ROUTINE:  HNMPSW
C
C             VERSION:  1.0.0
C
C                DATE: 7-24-81
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL EXTRACT THE DEFINITION NAME AND PASSWORD
C    AND RETURN THE VALUES PACKED IN 'NAME AD IPASS'.  IT ASSUMES
C    THAT CARD HAS BEEN PARSED BY UFREE AND INFORMATION IS IN UFREEI
C    PASSWORD CANNOT BE 'Y, N OR FUNXXX'.
C
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IFIELD     I     I-O   1    FIRST FIELD TO ANALYZE, RESET
C                                       TO NEXT FIELD(IF NAME AND PASS
C                                       WORD FOUND, +2)
C                                           IF ONLY NAME, +1
C       NAME       I     O     2    NAME OF DEFINITION
C       IPASS      I     O     1    4 CHAR (OR LESS) PASSWORD
C                                       -1=NO PASSWORD FOUND
C       ISTAT      I     O     1    STATUS,0=OK
C                                          1=FIRST FIELD NOT A NAME
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'hclcommon/hwords'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION NAME(2),IRIGHT(2),LEFT(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hnmpsw.f,v $
     . $',                                                             '
     .$Id: hnmpsw.f,v 1.1 1995/09/17 18:42:38 dws Exp $
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
C START IN IFIELD
C
      CALL HFEQLS (IFIELD,LEFT,IRIGHT,ISTRGT)
      IF (LEFT(1).EQ.LNAME.OR.LEFT(1).EQ.IBLNK.OR.LEFT(1).EQ.LETN)
     1    GO TO 20
      WRITE (LPE,10) LEFT
10    FORMAT (' **ERROR** POSITIONAL PARAMETER UNRECOGNIZED: ',2A4)
      ISTAT=1
      GO TO 50
C
C
C MOVE INTO NAME
C
20    CONTINUE
      IF (IFSTOP(IFIELD) - ISTRGT+1.LE.8) GO TO 40
      WRITE (LPE,30) IRIGHT
30    FORMAT (' **WARNING** NAME TRUNCATED TO ',2A4)
      CALL WARN
40    CONTINUE
      CALL UMEMOV (IRIGHT,NAME,2)
      IFIELD=IFIELD+1
      IF (IFIELD.GT.NFIELD) GO TO 70
C
C NOW WORK ON PASSWORD
C
50    CONTINUE
      CALL HFEQLS (IFIELD,LEFT,IRIGHT,ISTRGT)
      IF (LEFT(1).EQ.LPASS) GO TO 60
      IF (LEFT(1).NE.IBLNK) GO TO 70
C
C IF LEFT IS BLANK, MARE SURE RIGHT ISN'T FUNXXX OR Y OR N
C   OR ARG: OR A:
C
      IF (IRIGHT(1).EQ.LETN.OR.IRIGHT(1).EQ.LETY.OR.IRIGHT(1).EQ.LARGCL)
     1   GO TO 70
      ITEMP=IBLNK
      CALL UMOVE (IRIGHT(1),ITEMP,3)
      IF (ITEMP.EQ.LFUN) GO TO 70
      ITEMP=IBLNK
      CALL UMOVE (IRIGHT(1),ITEMP,2)
      IF (ITEMP.EQ.LACOLN) GO TO 70
C
C PASSWORD IS OK
C
60    IPASS=IRIGHT(1)
      IFIELD=IFIELD+1
      GO TO 80
C
C NO PASSWORD
C
70    IPASS=IBLNK
C
80    CONTINUE
      IF (IHCLTR.GT.2) WRITE (IOGDB,90) IFIELD,NAME,IPASS,ISTAT
90    FORMAT (' EXIT HNMPSW - IFIELD=',I3,' NAME=',2A4,
     1         ' IPASS=',A4,' ISTAT=',I3)
C
      RETURN
C
      END
