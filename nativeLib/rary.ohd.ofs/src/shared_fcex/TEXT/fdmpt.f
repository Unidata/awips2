C MODULE FDMPT
C-----------------------------------------------------------------------
C
      SUBROUTINE FDMPT (ANAME,A,MA)
C
C  THIS ROUTINE PRINTS THE CONTENTS OF AN ARRAY.
C
C  SUBROUTINE INITIALLY WRITTEN BY - ERIC ANDERSON - HRL 9/1979
C
      CHARACTER*4 ANAME
      DIMENSION A(MA)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/flarys'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fdmpt.f,v $
     . $',                                                             '
     .$Id: fdmpt.f,v 1.2 2000/03/13 20:52:59 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.3) WRITE (IODBUG,*) 'ENTER FDMPT'
C
      CALL FDMPA (ANAME,A,MA)
C
      END
