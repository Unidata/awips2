C MEMBER ULINE2
C-----------------------------------------------------------------------
C
      SUBROUTINE ULINE2 (NUNIT,LINES)
C
C  ROUTINE TO COUNT LINES PRINTED TO THE SPECIFIED UNIT.
C
      INCLUDE 'ucmdbx'
      INCLUDE 'upagex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/uline2.f,v $
     . $',                                                             '
     .$Id: uline2.f,v 1.1 1995/09/17 19:05:14 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IUNIT=IABS(NUNIT)
C
C  CHECK IF LINES TO BE COUNTED
      IF (IPSLIN(IUNIT).EQ.0) GO TO 10
C
C  UPDATE LINE COUNTERS
      NPSNLN(IUNIT)=NPSNLN(IUNIT)+LINES
      NPSNLT(IUNIT)=NPSNLT(IUNIT)+LINES
C
10    RETURN
C
      END
