C MODULE SBFCTR
C-----------------------------------------------------------------------
C
      SUBROUTINE SBFCTR (NGRID,LFACTR,ISTAT)
C
C  THIS ROUTINE COMPUTES THE FACTOR TO BE USED IN DETERMINING HOW
C  MANY GRID POINTS WILL BE STORED FOR A BASIN.
C
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbfctr.f,v $
     . $',                                                             '
     .$Id: sbfctr.f,v 1.2 1999/07/06 11:40:15 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBFCTR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LDEBUG=ISBUG('GRID')
C
C
C  COMPUTE DENSITY OF THE STORED GRID POINTS - FOR EXAMPLE LFACTR=2
C  WILL STORE EVERY EVEN NUMBERED GRID POINT IN X AND Y DIRECTION
      NG=NGRID
      LFACTR=1
10    IF (NG.GT.200) THEN
         LFACTR=LFACTR+1
         NG=NGRID/LFACTR**2
         GO TO 10
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NGRID=',NGRID,' LFACTR=',LFACTR
         CALL SULINE (IOSDBG,1)
         ENDIF
C  
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBFCTR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
