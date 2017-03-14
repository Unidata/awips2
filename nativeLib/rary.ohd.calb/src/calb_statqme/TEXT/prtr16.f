C MEMBER PRTR16
C  (from old member MCEX16)
C
      SUBROUTINE PRTR16(W,IWS)
C.......................................................................
C     THIS SUBROUTINE PRINTS REAL DEBUG OUTPUT FOR THE STAT OPERATION.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL -- HRL  JULY 1980   VERSION1
C.......................................................................
      DIMENSION W(1)
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_statqme/RCS/prtr16.f,v $
     . $',                                                             '
     .$Id: prtr16.f,v 1.2 1996/07/11 19:41:53 dws Exp $
     . $' /
C    ===================================================================
C
      WRITE(IODBUG,10) (W(I),I=1,IWS)
   10 FORMAT(1H0,12F10.3)
      RETURN
      END
