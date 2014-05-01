C MEMBER NMSQ50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C @PROCESS LVL(77)
      SUBROUTINE NMSQ50( QO, ISTART, IEND, NMISS_Q_DAYS, QAVE )
C
C    THIS ROUTINE CALCULATES THE NUMBER OF
C    VALUES MISSING IN THE OBSERVED STREAMFLOW
C    TIME SERIES. IT ASSUMES A DAILY TIME SERIES.
C    IT ALSO RETURNS THE AVERAGE VALUE OVER THE PERIOD
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C


C    PASSED ARGUMENTS
      DIMENSION QO(*)
      INTEGER ISTART, IEND, NMISS_Q_DAYS
      REAL QAVE

C    LOCAL ARGUMENTS
      INTEGER I, ITEST
      REAL SUM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/nmsq50.f,v $
     . $',                                                             '
     .$Id: nmsq50.f,v 1.1 1995/09/17 18:55:40 dws Exp $
     . $' /
C    ===================================================================
C

      NMISS_Q_DAYS = 0
      SUM = 0.0
      DO I = ISTART, IEND
         ITEST = IFMSNG(QO(I))
         IF (ITEST.EQ.1) THEN
            NMISS_Q_DAYS = NMISS_Q_DAYS + 1
         ELSE
            SUM = SUM + QO(I)
         END IF
      END DO

      IF ((IEND - ISTART - NMISS_Q_DAYS).LT.0.0) THEN
         WRITE( IPR, 901 )
         CALL ERROR
         RETURN
      END IF
      QAVE = SUM / (IEND - ISTART - NMISS_Q_DAYS + 1)
      RETURN


  901 FORMAT( 'THERE ARE NO VALID DAYS OF STREAMFLOW
     1 IN THE OBS DISCHARGE T.S.' )

      END
