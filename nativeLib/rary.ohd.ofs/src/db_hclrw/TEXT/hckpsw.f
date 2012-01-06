C MEMBER HCKPSW
C  (from old member HCLCUTIL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/21/94.14:13:32 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HCKPSW (IGL,IPASS,ISTAT)
C
      INCLUDE 'uio'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hcntrl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hckpsw.f,v $
     . $',                                                             '
     .$Id: hckpsw.f,v 1.1 1995/09/17 18:41:42 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IGL.GT.0) GO TO 50
      IF (IPASS.NE.IBLNK) GO TO 10
      WRITE (LPE,40)
      GO TO 30
C
C CHECK THAT GLOBAL PASSWORD IS A MATCH
C
10    IF (IPASS.EQ.HCNTL(5,2)) GO TO 50
      WRITE (LPE,20) IPASS
20    FORMAT (' **ERROR** INVALID PASSWORD ',A4)
30    ISTAT=1
      CALL ERROR
40    FORMAT (' **ERROR**  GLOBAL DEFINITION MUST HAVE PASSWORD')
C
50    RETURN
C
      END
