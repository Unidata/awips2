C MODULE VDAY
C-----------------------------------------------------------------------
C
C  THIS PROGRAM COMPUTES THE TRIG FUNCTIONS OF THE SOLAR DECLINATION 
C  ANGLE.
C
C  INPUTS:
C    - DAY OF THE YEAR
C
C  OUTPUTS:
C    - TRIG FUNCTIONS COSINE, SINE, SIN SQUARE AND 2SINCOS
C................................................................
C
      SUBROUTINE VDAY (JDAY)
C
      COMMON /VSLR/ COSX,SINX,SINSQ,SIN2CS
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mape/RCS/vday.f,v $
     . $',                                                             '
     .$Id: vday.f,v 1.2 1999/07/06 16:15:57 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPTRCE.GT.0) WRITE(IOPDBG,*) 'ENTER VDAY'
C
      IDEBUG=IPBUG('VDAY')
C
      IF (IDEBUG.GT.0) WRITE(IOPDBG,*) 'JDAY=',JDAY
C
      X=0.0172*JDAY
      COSX=COS(X)
      SINX=SIN(X)
      SINSQ=SINX*SINX
      SIN2CS=2*SINX*COSX
C
      IF (IPTRCE.GT.0) WRITE(IOPDBG,*) 'EXIT VDAY'
C
      RETURN
C
      END
