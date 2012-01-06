C MODULE USETO1
C-----------------------------------------------------------------------
C
C  ROUTINE TO SET OPTIONS FOR UTILITY ROUTINES.
C
      SUBROUTINE USETO1 (OPTION,ISTAT)
C
      CHARACTER*(*) OPTION
C
      INCLUDE 'uiox'
      INCLUDE 'upagex'
      INCLUDE 'uerorx'
      INCLUDE 'utimrx'
      INCLUDE 'uoptnx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/useto1.f,v $
     . $',                                                             '
     .$Id: useto1.f,v 1.2 2001/06/12 15:37:04 aivo Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
C  CHECK FOR PAGE HEADER PRINT OPTION
      IF (OPTION.EQ.'PAGHDR') THEN
         IPSPAG(LP)=1
         GO TO 10
         ENDIF
      IF (OPTION.EQ.'NOPAGHDR') THEN
         IPSPAG(LP)=-1
         GO TO 10
         ENDIF
C
C  CHECK FOR PAGE NUMBER PRINT OPTION
      IF (OPTION.EQ.'PAGNUM') THEN
         IPSPAG(LP)=1
         GO TO 10
         ENDIF
      IF (OPTION.EQ.'NOPAGNUM') THEN
         IPSPAG(LP)=0
         GO TO 10
         ENDIF
C
C  CHECK FOR LINE COUNT PRINT OPTION
      IF (OPTION.EQ.'LINCNT') THEN
         IPSLIN(LP)=1
         GO TO 10
         ENDIF
      IF (OPTION.EQ.'NOLINCNT') THEN
         IPSLIN(LP)=0
         GO TO 10
         ENDIF
C
C  CHECK FOR ERROR AND WARNING PRINT OPTION
      IF (OPTION.EQ.'ERWRPR') THEN
         IERPRT=1
         GO TO 10
         ENDIF
      IF (OPTION.EQ.'NOERWRPR') THEN
         IERPRT=0
         GO TO 10
         ENDIF
C
C  CHECK FOR CONDITION CODE PRINT OPTION
      IF (OPTION.EQ.'CONDPR') THEN
         ICDPRT=1
         GO TO 10
         ENDIF
      IF (OPTION.EQ.'NOCONDPR') THEN
         ICDPRT=0
         GO TO 10
         ENDIF
C
C  CHECK FOR CLOCK/CPU PRINT OPTION
      IF (OPTION.EQ.'TIMRPR') THEN
         ITMPRT=1
         GO TO 10
         ENDIF
      IF (OPTION.EQ.'NOTIMRPR') THEN
         ITMPRT=0
         GO TO 10
         ENDIF
C
C  CHECK FOR OVERPRINT OPTION
      IF (OPTION.EQ.'OVERPRINT') THEN
         NOVPRT=2
         GO TO 10
         ENDIF
      IF (OPTION.EQ.'NOOVERPRINT') THEN
         NOVPRT=0
         GO TO 10
         ENDIF
C
C  INVALID OPTION
      CALL UWARN (LP,1,-1)
      WRITE (LP,20) OPTION
      ISTAT=1
C
10    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT ('+*** WARNING - IN USETO1 - OPTION ',A,
     *   ' NOT RECOGNIZED.')
C
      END
