C MEMBER FAPITR
C  (from old member DUMTOP)
C
C     This file is a combination of all of the "dummy" routines related to
C     "top" level routines.  These routines are either under development
C     or they are not needed at runtime but still need to be linked.
C
      SUBROUTINE fapitr
      include 'common/sionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/top_stubs/RCS/fapitr.f,v $
     . $',                                                             '
     .$Id: fapitr.f,v 1.1 1996/05/21 16:08:20 dws Exp $
     . $' /
C    ===================================================================
C
      write(ISTDERR,*) 'Enter FAPITR dummy.  Routine is not active.'
      END
