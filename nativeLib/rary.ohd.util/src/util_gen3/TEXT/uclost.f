C MODULE UCLOST
C-----------------------------------------------------------------------
C
C  ROUTINE TO CLOSE ONE FILE.
C
      SUBROUTINE UCLOST (IUNIT)
C
      INCLUDE 'ufiles'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/uclost.f,v $
     . $',                                                             '
     .$Id: uclost.f,v 1.3 2001/06/13 13:23:20 dws Exp $
     . $' /
C    ===================================================================
C
C
C  CHECK FOR VALID UNIT NUMBER
      IF (IUNIT.GE.1.AND.IUNIT.LE.MFILES) THEN
         IF (IFILES(IUNIT).EQ.2) THEN
C        REWIND SEQUENTIAL FILE
            REWIND IUNIT
            ELSE
C           CLOSE FILE
               CALL UPCLOS (IUNIT,'  ',ISTAT)
               IF (ISTAT.EQ.0) IFILES(IUNIT)=0
            ENDIF
         ENDIF
C
      RETURN
C
      END
