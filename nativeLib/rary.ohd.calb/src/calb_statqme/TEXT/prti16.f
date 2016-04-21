C MEMBER PRTI16
C  (from old member MCEX16)
C
      SUBROUTINE PRTI16(IW,IWS)
C.......................................................................
C     THIS SUBROUTINE PRINTS INT. DEBUG OUTPUT FOR THE STAT OPERATION.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL -- HRL  JULY 1980   VERSION1
C.......................................................................
      DIMENSION IW(1)
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_statqme/RCS/prti16.f,v $
     . $',                                                             '
     .$Id: prti16.f,v 1.2 1996/07/11 19:36:58 dws Exp $
     . $' /
C    ===================================================================
C
      WRITE(IODBUG,10) (IW(I),I=1,IWS)
   10 FORMAT(1H0,12I10)
      RETURN
      END
