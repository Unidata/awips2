C MODULE SBNUMM
C-----------------------------------------------------------------------
C
      SUBROUTINE SBNUMM (MNUM,MX,MY,ISTAT)
C
C  THIS ROUTINE COMPUTES THE MDR COLUMN AND ROW FROM THE BOX NUMBER.
C
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbnumm.f,v $
     . $',                                                             '
     .$Id: sbnumm.f,v 1.2 1999/07/07 11:27:12 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBNUMM'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MNUM=',MNUM
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      MY=(MNUM/113)+1
      IF ((MY-1)*113.EQ.MNUM) MY=MY-1
      MX=MNUM-(MY-1)*113
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MX=',MX,' MY=',MY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBNUMM'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
