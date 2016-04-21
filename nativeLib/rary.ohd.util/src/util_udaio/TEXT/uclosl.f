C MODULE UCLOSL
C-----------------------------------------------------------------------
C
C  ROUTINE TO CLOSE ALL OPEN FILES.
C
      SUBROUTINE UCLOSL()

      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'ufiles'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_udaio/RCS/uclosl.f,v $
     . $',                                                             '
     .$Id: uclosl.f,v 1.3 2001/06/13 09:43:49 mgm Exp $
     . $' /
C    ===================================================================
C

C  CLOSE ALL OPEN FILES
      DO 120 I=1,MFILES
         IF (IFILES(I).NE.1) GO TO 120
            IF (ICMDBG.GT.0) WRITE (ICMPRU,10) I
10    FORMAT (' CALLING UPCLOS TO CLOSE UNIT ',I3)
            CALL UPCLOS (I,'  ',ISTAT)
            IF (ISTAT.EQ.0) IFILES(I) = 0
120      CONTINUE

      RETURN

      END
