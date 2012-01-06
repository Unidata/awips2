C MODULE UTIL_TO_FCINIT
C-----------------------------------------------------------------------
C
      SUBROUTINE UTIL_TO_FCINIT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_main/RCS/util_to_fcinit.f,v $
     . $',                                                             '
     .$Id: util_to_fcinit.f,v 1.2 1998/07/08 16:02:45 page Exp $
     . $' /
C    ===================================================================
C
      RETURN
      END
C
C  CONVERT FROM UTILITY TO FCINIT ERROR AND WARNING COUNTING
C
C      SUBROUTINE UEROR (NUNIT,NLINES,NUMERR)
C      INCLUDE 'uoptnx'
C      INCLUDE 'common/errdat'
C      INCLUDE 'common/toterz'
C      INCLUDE 'common/killcd'
C      IF (NOVPRT.GE.0) WRITE (NUNIT,'(1H0)')
C      NERRS=NERRS+1
C      NERRST=NERRST+1
C      IF (KLCODE.LT.8) KLCODE=8
C      END
C
C      SUBROUTINE UWARN (NUNIT,NLINES,NUMWRN)
C      INCLUDE 'uoptnx'
C      INCLUDE 'common/errdat'
C      INCLUDE 'common/toterz'
C      INCLUDE 'common/killcd'
C     IF (NOVPRT.GE.0) WRITE (NUNIT,'(1H0)')
C      NWARN=NWARN+1
C      NWARNT=NWARNT+1
C      END
