C MODULE SBGRDL
C-----------------------------------------------------------------------
C
      SUBROUTINE SBGRDL (YCENTR,GRIDL,ISTAT)
C
C  THIS ROUTINE COMPUTES THE GRID LENGTH CORRESPONDING TO THE
C  LATITUDE AT THE CENTER OF THE BASIN
C.
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbgrdl.f,v $
     . $',                                                             '
     .$Id: sbgrdl.f,v 1.2 1999/07/06 11:41:39 page Exp $
     . $' /
C    ===================================================================
C
      DATA STLAT/60./
      DATA DEGRAD/.01745329/
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBGRDL'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LDEBUG=ISBUG('GRID')
C
      RLAT=YCENTR*DEGRAD
      TLAT=STLAT*DEGRAD
      S=(1.+SIN(TLAT))/(1.+SIN(RLAT))
      GRIDL=4.7625/S
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'YCENTR=',YCENTR,' GRIDL=',GRIDL
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBGRDL'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
