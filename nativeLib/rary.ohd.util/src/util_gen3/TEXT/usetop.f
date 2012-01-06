C MEMBER USETOP
C-----------------------------------------------------------------------
C
C  ROUTINE TO SET OPTIONS FOR UTILITY ROUTINES.
C
      SUBROUTINE USETOP (IPAGE,IERWRN,ICOND,ITIME)
C
      INCLUDE 'uiox'
      INCLUDE 'upagex'
      INCLUDE 'uerorx'
      INCLUDE 'utimrx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/usetop.f,v $
     . $',                                                             '
     .$Id: usetop.f,v 1.1 1995/09/17 19:04:07 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET PAGE NUMBER OPTION
      IF (IPAGE.EQ.0) IPSPAG(LP)=0
      IF (IPAGE.EQ.1) IPSPAG(LP)=1
C
C  SET ERROR AND WARNING PRINT OPTION
      IF (IERWRN.EQ.0) IERPRT=0
      IF (IERWRN.EQ.1) IERPRT=1
C
C  SET CONDITION CODE PRINT OPTION
      IF (ICOND.EQ.0) ICDPRT=0
      IF (ICOND.EQ.1) ICDPRT=1
C
C  SET CLOCK/CPU TIME PRINT OPTION
      IF (ITIME.EQ.0) ITMPRT=0
      IF (ITIME.EQ.1) ITMPRT=1
C
      RETURN
C
      END
