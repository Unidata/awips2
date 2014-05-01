C MODULE UINCL2
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK FOR INCLUDE KEYWORD.
C
      SUBROUTINE UINCL2 (XCARD,LXCARD,XSCAN,LXSCAN,ISTAT)
C
      CHARACTER*(*) XCARD,XSCAN
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uincl2.f,v $
     . $',                                                             '
     .$Id: uincl2.f,v 1.2 2002/02/11 20:22:01 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UINCL2'
         ENDIF
C
      ISTAT=0
C
      NSCAN=1
      CALL USCAN (XCARD,LXCARD,' ',1,NSCAN,XSCAN,LEN(XSCAN),LXSCAN,
     *   IERR)
      LXSCAN=LXSCAN+1
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'IERR=',IERR,
     *     ' LXSCAN=',LXSCAN,
     *     ' XSCAN=',XSCAN
         ENDIF
C
      IF (IERR.GT.0) ISTAT=1
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UINCL2'
         ENDIF
C
      RETURN
C
      END
