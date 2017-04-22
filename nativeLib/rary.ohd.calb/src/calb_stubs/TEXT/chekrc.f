C MEMBER CHEKRC
C  (from old member MCDUMMY)
C
      SUBROUTINE CHEKRC(RCID,IERR)
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_stubs/RCS/chekrc.f,v $
     . $',                                                             '
     .$Id: chekrc.f,v 1.1 1996/05/22 19:46:32 dws Exp $
     . $' /
C    ===================================================================
C
      IERR=1
      WRITE(IPR,600)
  600 FORMAT(1H0,10X,82H**ERROR** THE CAPABILITY TO USE RATING CURVES IS
     1 NOT INCLUDED IN THIS LOAD MODULE.)
      CALL ERROR
      RETURN
      END
