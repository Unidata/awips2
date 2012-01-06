C MEMBER SNNTW2
C  (from old member SNNTWK)
C-----------------------------------------------------------------------
C
      SUBROUTINE SNNTW2 (LP,IPRINT)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_network/RCS/snntw2.f,v $
     . $',                                                             '
     .$Id: snntw2.f,v 1.1 1995/09/17 19:13:51 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRINT.EQ.1) RETURN
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
C
      WRITE (LP,10)
      CALL SULINE (LP,2)
10    FORMAT ('0',132('#'))
C
      IPRINT=1
C
      RETURN
C
      END
