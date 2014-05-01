C MEMBER STOPIN
C  (from old member FCSTOPIN)
C
      SUBROUTINE STOPIN(P,MP,C,MC,T,MT,NXP,NXC,NXT,LD,NWORK,LWMAX)
C.......................................
C     THIS SUBROUTINE IS THE INPUT SUBROUTINE FOR THE STOP OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ERIC ANDERSON - HRL     SEPTEMBER 1979
C.......................................
      DIMENSION P(MP),C(MC)
      INTEGER T(MT)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/stopin.f,v $
     . $',                                                             '
     .$Id: stopin.f,v 1.1 1995/09/17 18:49:00 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT (1H0,17H** STOPIN ENTERED)
C.......................................
C     MAKE AN ENTRY INTO THE P,C, AND T ARRAYS FOR STOP
C        IF THERE IS ROOM - THEN NXP,NXC, AND NXT SPACES ARE USED.
      IF (NXP.GT.MP) GO TO 91
      P(NXP)=-1.01
      GO TO 92
   91 NXP=MP
   92 IF (NXC.GT.MC) GO TO 93
      C(NXC)=-1.01
      GO TO 94
   93 NXC=MC
   94 IF (NXT.GT.MT) GO TO 95
      T(NXT)=-1
      GO TO 96
   95 NXT=MT
C
C     COMPUTE THE SPACE USED IN THE D ARRAY.
   96 LD=NWORK+LWMAX-1
C.......................................
      RETURN
      END
