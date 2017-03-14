C MODULE SUEROR
C-----------------------------------------------------------------------
C
      SUBROUTINE SUEROR
C
C  ROUTINE SUEROR COUNTS THE NUMBER OF ERROR MESSAGES.
C
      CHARACTER*1 CARCTL
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sueror.f,v $
     . $',                                                             '
     .$Id: sueror.f,v 1.3 2001/06/13 15:11:24 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GE.3) THEN
         WRITE (IOSDBG,*) 'ENTER SUEROR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      NERR=NERR+1
      NTERR=NTERR+1
      NRERR=NRERR+1
C
C  STORE PAGE ON WHICH ERROR OCCURRED
      DO 10 I=1,MPGERR
         IF (LPGERR(I).EQ.NPSPAG) GO TO 20
         IF (LPGERR(I).NE.0) GO TO 10
            LPGERR(I)=NPSPAG
            NPGERR=NPGERR+1
            GO TO 20
10       CONTINUE
C
20    IF (ICMCDE.LT.8) ICMCDE=8
      IF (KLCODE.LT.8) KLCODE=8
C
C  CHECK IF MAXIMUM ERRORS ALLOWED EXCEEDED
      IF (NTERR.GT.IOPMXE) THEN
         WRITE (LP,40)
         CALL SULINE (LP,2)
         CARCTL='+'
         WRITE (LP,50) CARCTL,IOPMXE
         IF (IOPOVP.EQ.1) THEN
            WRITE (LP,50) CARCTL,IOPMXE
            WRITE (LP,50) CARCTL,IOPMXE
            ENDIF
         CALL SULINE (LP,0)
         IF (IOPCLG(1).EQ.1) THEN
            IUNIT=IOPCLG(2)
            CARCTL=''
            WRITE (IUNIT,50) CARCTL,IOPMXE
            ENDIF
         ICMCDE=16
         KLCODE=16
         CALL SUEND
         STOP 16
         ENDIF
C
      IF (ISTRCE.GE.3) THEN
         WRITE (IOSDBG,*) 'EXIT SUEROR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT ('0')
50    FORMAT (A,'*** FATAL ERROR - MAXIMUM NUMBER OF ERRORS ALLOWED ',
     *   '(',I4,') EXCEEDED. PROGRAM EXECUTION STOPPED.')
C
      END
