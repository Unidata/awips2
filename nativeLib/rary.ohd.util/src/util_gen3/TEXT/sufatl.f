C MODULE SUFATL
C-----------------------------------------------------------------------
C
      SUBROUTINE SUFATL
C
C  ROUTINE SUFATL IS CALLED IF A FATAL ERROR IS ENCOUNTERED.
C
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/sufatl.f,v $
     . $',                                                             '
     .$Id: sufatl.f,v 1.2 2001/06/13 14:04:46 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GE.3) THEN
         WRITE (IOSDBG,*) 'ENTER SUFATL'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      NERR=NERR+1
      NTERR=NTERR+1
      NRERR=NRERR+1
C
C  STORE PAGE ON WHICH ERROR OCCURRED
      DO 20 I=1,MPGERR
         IF (LPGERR(I).EQ.NPSPAG) GO TO 30
         IF (LPGERR(I).NE.0) GO TO 20
            LPGERR(I)=NPSPAG
            NPGERR=NPGERR+1
            GO TO 30
20       CONTINUE
C
30    IF (ICMCDE.LT.16) ICMCDE=16
      IF (KLCODE.LT.16) KLCODE=16
C
      IF (ISTRCE.GE.3) THEN
         WRITE (IOSDBG,*) 'EXIT SUFATL'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
