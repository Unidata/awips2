C MODULE SBMDRS
C-----------------------------------------------------------------------
C
      SUBROUTINE SBMDRS (X,Y,MDRX,MDRY,ISTAT)
C
C  THIS ROUTINE COMPUTES THE MDR BOX COLUMN AND ROW FOR A SET OF GRID
C  POINT COORDINATES.
C
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbmdrs.f,v $
     . $',                                                             '
     .$Id: sbmdrs.f,v 1.2 1999/07/07 11:25:17 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBMDRS'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,'(2(1X,A,F7.2))') 'X=',X,'Y=',Y
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  FIND MDR X VALUE.
      MDRX=((X-1)/10)+1
C
C  FIND MDR Y VALUE.
      MDRY=((Y-1)/10)+1
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MDRX=',MDRX,' MDRY=',MDRY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBMDRS'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
