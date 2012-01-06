C MEMBER UPAGE
C-----------------------------------------------------------------------
C
C  ROUTINE TO STORE CLOCK AND CPU TIME INFORMATION IN COMMON
C  BLOCK AND CALL ROUTINE TO PRINT HEADER LINE.
C
      SUBROUTINE UPAGE (NUNIT)
C
      INCLUDE 'ucmdbx'
      INCLUDE 'upagex'
      INCLUDE 'utimrx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/upage.f,v $
     . $',                                                             '
     .$Id: upage.f,v 1.1 1995/09/17 19:05:16 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         NPSNLN(ICMPRU)=NPSNLN(ICMPRU)+1
         WRITE (ICMPRU,25) NUNIT
         ENDIF
C
C  CHECK IF FIRST TIME CALLED
      IF (ITMFIL.GT.0) GO TO 10
C
C  SET CPU TIMER
      NCUNIT=-1
      CALL UCPUTM (NCUNIT,ITMELA,ITMTOT)
C
C  GET CURRENT DATE AND TIME IN INTEGER FORM AND STORE IN COMMON BLOCK
      CALL UCLKTM (ISTDAY,ISTHMS,IERR)
      ITMFIL=1
      IF (ICMDBG.GT.0) THEN
         NPSNLN(ICMPRU)=NPSNLN(ICMPRU)+1
         WRITE (ICMPRU,30) ISTDAY,ISTHMS
         ENDIF
C
C  CHECK IF PAGE HEADER TO BE PRINTED
10    IF (NUNIT.EQ.0) GO TO 20
C
      CALL UPAGE2 (NUNIT)
C
20    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
25    FORMAT (' *** ENTER UPAGE - NUNIT=',I2)
30    FORMAT (' *** IN UPAGE - ISTDAY=',I4,3X,'ISTHMS=',I8)
C
      END
