C MODULE SBMNUM
C-----------------------------------------------------------------------
C
      SUBROUTINE SBMNUM (MX,MY,MNUM,ISTAT)
C
C  THIS ROUTINE COMPUTES THE MDR BOX NUMBER FROM THE COLUMN AND ROW
C  OF THE BOX.
C
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbmnum.f,v $
     . $',                                                             '
     .$Id: sbmnum.f,v 1.2 1999/07/07 11:26:08 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBMNUM'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MX=',MX,' MY=',MY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      MNUM=((MY-1)*113)+MX
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MNUM=',MNUM
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBMNUM'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
