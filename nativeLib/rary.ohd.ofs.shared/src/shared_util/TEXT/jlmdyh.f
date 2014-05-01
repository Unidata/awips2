C MODULE JLMDYH
C-----------------------------------------------------------------------
C
       SUBROUTINE JLMDYH (JULHR,IMO,IDAY,IYR,IHR)
C
C  THIS ROUTINE CONVERTS AN INPUT JULIAN HOUR TO THE CALENDAR
C  MONTH,DAY,YEAR AND HOUR BY CALLING MDYH2 WHICH USES THE TIME
C  ZONE CODE FOR ITS CONVERSION.
C
C    ARGUMENT LIST:
C
C       NAME    TYPE  I/O   DIM   DESCRIPTION
C       ------  ----  ---   ---   -----------
C       JULHR    I     I     1    JULIAN HOUR
C       IMO      I     O     1    CALENDAR MONTH 
C       IDAY     I     O     1    CALENDAR DAY
C       IYR      I     O     1    CALENDAR YEAR (2 DIGITS)
C       IHR      I     O     1    CALENDAR HOUR
C
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/jlmdyh.f,v $
     . $',                                                             '
     .$Id: jlmdyh.f,v 1.2 2000/07/21 18:32:54 page Exp $
     . $' /
C    ===================================================================
C
C
C  CONVERT JULIAN HOUR TO CALENDAR DATE
C
      JUL=JULHR-NHOPDB
      JHDAY=JUL/24+1
      JHR=MOD(JUL,24)
      IF (JHR.LT.0) JHR=0
      IF (JUL.LT.0) JHDAY=0
      CALL MDYH2 (JHDAY,JHR,IMO,IDAY,IYR,IHR,ITZ,IDSAV,TIME(3))
      IYR=MOD(IYR,100)
      RETURN
      END
