C MODULE SBCNTR
C-----------------------------------------------------------------------
C
      SUBROUTINE SBCNTR (YMIN,YMAX,YCENTR,ISTAT)
C
C  THIS ROUTINE COMPUTES THE CENTER Y COORDINATE OF A BASIN BASED ON 
C  THE MINIMUM AND MAXIMUM Y VALUES.
C
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbcntr.f,v $
     . $',                                                             '
     .$Id: sbcntr.f,v 1.2 1999/07/06 11:39:54 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBCNTR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      ISTAT=0
C
      YDIF=YMAX-YMIN
      YCENTR=YMIN+YDIF*.5
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'YCENTR=',YCENTR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBCNTR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
