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
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/uline2.f,v $
     . $',                                                             '
     .$Id: uline2.f,v 1.1 2006/05/03 13:43:59 gsood Exp $
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
