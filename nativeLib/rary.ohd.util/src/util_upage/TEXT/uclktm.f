C MEMBER UCLKTM
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET CLOCK TIME INFORMATION
C
      SUBROUTINE UCLKTM (JULDAY,NHRMNS,IERR)
C
      INCLUDE 'ucmdbx'
      INCLUDE 'upagex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/uclktm.f,v $
     . $',                                                             '
     .$Id: uclktm.f,v 1.1 1995/09/17 19:05:05 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         NPSNLN(ICMPRU)=NPSNLN(ICMPRU)+1
        WRITE (ICMPRU,25)
         ENDIF
C
C  GET CURRENT DATE AND TIME IN INTEGER FORM
      CALL UDATEI (NMO,NDA,NYR,NHRMIN,NSEC,JULDAY,IERR)
C
      NHR=NHRMIN/100
      NMIN=NHRMIN-NHR*100
      NHRMNS=(NHR*10000+NMIN*100)+(NSEC/100)
C
      IF (ICMDBG.GT.0) THEN
         NPSNLN(ICMPRU)=NPSNLN(ICMPRU)+1
         WRITE (ICMPRU,30) JULDAY,NHRMNS
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
25    FORMAT (' *** ENTER UCLKTM')
30    FORMAT (' *** IN UCLKTM : JULDAY=',I4,3X,'NHRMNS=',I8)
C
      END
