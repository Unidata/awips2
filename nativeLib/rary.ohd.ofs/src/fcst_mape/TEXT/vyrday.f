C MODULE VYRDAY
C-----------------------------------------------------------------------
C
C  THIS ROUTINE SETS THE CURRENT JULIAN PLUS ONE.
C
C  INPUTS:
C    - JULIAN DAY SINCE 1/1/1900 (JULDAY)
C
C  OUTPUTS:
C    - CURRENT DAY OF THE YEAR PLUS ONE (JDAYYR)
C
      SUBROUTINE VYRDAY (JULDAY)
C    
      INCLUDE 'common/pudbug'
      COMMON /VTIME/ JDAYO,IYRS,MOS,JDAMOS,IHRO,IHRS,JDAMO,JDAYYR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mape/RCS/vyrday.f,v $
     . $',                                                             '
     .$Id: vyrday.f,v 1.3 1999/07/06 16:35:50 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPTRCE.GT.0) WRITE (IOPDBG,*) 'ENTER VYRDAY'
C
      IDEBUG=IPBUG('VYRD')
C
      IF (IDEBUG.GT.0) WRITE (IOPDBG,*) 'JULDAY=',JULDAY
C
      CALL DDGDJ2 (JULDAY,JDAYYR,IYY)
      JDAYYR=JDAYYR+1
C
      IF (IDEBUG.GT.0) WRITE (IOPDBG,*) 'JDAYYR=',JDAYYR
C
      IF (IPTRCE.GT.0) WRITE (IOPDBG,*) 'EXIT VYRDAY'
C
      RETURN
C
      END
