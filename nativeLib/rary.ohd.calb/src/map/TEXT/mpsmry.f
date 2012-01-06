C MEMBER MPSMRY
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 02/16/95.12:27:05 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE MPSMRY (PXSUM,NBASIN,BMO,BYR,EMO,EYR,ANAME,AREAID,
     *   IMOW,IMOS,UNITS)
C
C
C  ROUTINE  MPSMRY  PRINTS A SUMMARY TABLE OF MONTHLY PRECIPITATION
C  VALUES ON AN ANNUAL OR WATER YEAR BASIS.
C
C
      DIMENSION    PXSUM(M2,M6,12),YAVG(12)
      DIMENSION    ANAME(5,M2),AREAID(3,M2)
      DIMENSION    MONTH(12)
C
      INTEGER      BMO,BYR,EMO,EYR
      INTEGER      DEBUG,DEBUGA
      REAL         MONTH
C
      INCLUDE 'common/ionum'
      COMMON /DIM/ M1,M2,M3,M4,M5,M6
      COMMON /MAP/ DEBUG,DEBUGA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/mpsmry.f,v $
     . $',                                                             '
     .$Id: mpsmry.f,v 1.1 1997/01/27 15:33:27 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA         MONTH/3HJAN,3HFEB,3HMAR,3HAPR,3HMAY,3HJUN,
     *                   3HJUL,3HAUG,3HSEP,3HOCT,3HNOV,3HDEC/
C
C
      IF (DEBUG.EQ.1) WRITE (IPR,240)
C
      NYR=EYR-BYR+1
      IYR=BYR+1900
      LYR=EYR+1900
      LMOW=IMOS-1
      LMOS=IMOW-1
      IF (DEBUG.EQ.1) WRITE (IPR,360) EYR,BYR,NYR
C
      ISMRY=0
      ISMRY=1
      ISMRY=2
      IF (ISMRY.EQ.2) GO TO 80
C
C
C  SUMMARY TABLES BY CALENDAR YEAR
C
      DO 70 I=1,NBASIN
         WRITE (IPR,250)
         WRITE (IPR,270) (ANAME(J,I),J=1,5),(AREAID(J,I),J=1,3),
     *      BMO,IYR,EMO,LYR
         WRITE (IPR,260)
         WRITE (IPR,310) MONTH
         WRITE (IPR,260)
         JYR=IYR
         DO 20 J=1,NYR
            SUM=0.0
               DO 10 K=1,12
                  IF (PXSUM(I,J,K).LT.-998.0) GO TO 10
                     SUM=SUM+PXSUM(I,J,K)
                     IF (DEBUG.EQ.1) WRITE (IPR,350) I,J,K,PXSUM(I,J,K)
10                CONTINUE
            WRITE (IPR,280) JYR,(PXSUM(I,J,K),K=1,12),SUM
            JYR=JYR+1
20          CONTINUE
C     COMPUTE MONTHLY AVERAGES AND SUM
         YTOT=0.0
         DO 40 N=1,12
            SUM=0.0
            NMO=0
               DO 30 M=1,NYR
                  IF (PXSUM(I,M,N).LT.-998.0) GO TO 30
                     NMO=NMO+1
                     SUM=SUM+PXSUM(I,M,N)
30                CONTINUE
            IF (NMO.GT.0) GO TO 35
               YAVG(N)=-999.0
               GO TO 40
35          YAVG(N)=SUM/NMO
            YTOT=YTOT+YAVG(N)
40          CONTINUE
         WRITE (IPR,260)
         WRITE (IPR,330) YAVG,YTOT,UNITS
C     COMPUTE SEASONAL SUMS OF MONTHLY AVERAGES
         SSUM=0.0
         WSUM=0.0
         NW=0
         NS=0
         DO 60 MO=1,12
            IF (YAVG(MO).LT.-998.0) GO TO 60
            IF (MO.GT.LMOW.AND.MO.LT.IMOW) GO TO 50
               WSUM=WSUM+YAVG(MO)
               NW=NW+1
               GO TO 60
50          SSUM=SSUM+YAVG(MO)
            NS=NS+1
60          CONTINUE
         IF (NW.EQ.0) WSUM=-999.0
         IF (NS.EQ.0) WSUM=-999.0
         WRITE (IPR,340) MONTH(IMOW),MONTH(LMOW),WSUM,UNITS,
     *      MONTH(IMOS),MONTH(LMOS),SSUM,UNITS
70       CONTINUE
C
      IF (ISMRY.EQ.1) GO TO 235
C
C  -    -    -    -    -    -    -    -    -    -    -    -    -    -
C
C  SUMMARY TABLES BY WATER YEAR
C
80    DO 230 I=1,NBASIN
         WRITE (IPR,250)
         WRITE (IPR,270) (ANAME(J,I),J=1,5),(AREAID(J,I),J=1,3),
     *      BMO,IYR,EMO,LYR
         WRITE (IPR,260)
         WRITE (IPR,320) (MONTH(J),J=10,12),(MONTH(J),J=1,9)
         WRITE (IPR,260)
         JYR=IYR
         IF (BMO.GE.10) JYR=JYR+1
         DO 140 J=1,NYR
            IF (J.GT.1) GO TO 100
            IF (BMO.GE.10) GO TO 100
               SUM=0.0
               DO 90 K=1,9
                  IF (PXSUM(I,J,K).LT.-998.0) GO TO 90
                     SUM=SUM+PXSUM(I,J,K)
                     IF (DEBUG.EQ.1) WRITE(IPR,350) I,J,K,PXSUM(I,J,K)
90                CONTINUE
               WRITE (IPR,290) JYR,(PXSUM(I,J,K),K=1,9),SUM
               JYR=JYR+1
100         SUM=0.0
            DO 110 K=10,12
               IF (PXSUM(I,J,K).LT.-998.0) GO TO 110
                  SUM=SUM+PXSUM(I,J,K)
                  IF (DEBUG.EQ.1) WRITE(IPR,350) I,J,K,PXSUM(I,J,K)
110            CONTINUE
            IF (J.EQ.NYR) GO TO 130
               JJ=J+1
               DO 120 K=1,9
                  IF (PXSUM(I,JJ,K).LT.-998.0) GO TO 120
                     SUM=SUM+PXSUM(I,JJ,K)
                     IF (DEBUG.EQ.1) WRITE(IPR,350) I,JJ,K,PXSUM(I,JJ,K)
120               CONTINUE
               WRITE (IPR,280) JYR,
     *            (PXSUM(I,J,K),K=10,12),(PXSUM(I,JJ,K),K=1,9),
     *            SUM
               JYR=JYR+1
               GO TO 140
130         IF (EMO.LT.10) GO TO 140
               WRITE (IPR,300) JYR,(PXSUM(I,J,K),K=10,12),SUM
140         CONTINUE
C     COMPUTE MONTHLY AVERAGES AND SUM
         YTOT=0.0
         N1=10
         N2=12
         IPASS=0
150      DO 170 N=N1,N2
            SUM=0.0
            NMO=0
            DO 160 M=1,NYR
               IF (PXSUM(I,M,N).LT.-998.0) GO TO 160
                  NMO=NMO+1
                  SUM=SUM+PXSUM(I,M,N)
160            CONTINUE
            IF (NMO.GT.0) GO TO 165
               YAVG(N)=-999.0
               GO TO 170
165         YAVG(N)=SUM/NMO
            YTOT=YTOT+YAVG(N)
170         CONTINUE
         IF (IPASS.EQ.1) GO TO 180
            N1=1
            N2=9
            IPASS=1
            GO TO 150
180      WRITE (IPR,260)
         WRITE (IPR,330) (YAVG(N),N=10,12),(YAVG(N),N=1,9),YTOT,UNITS
C     COMPUTE SEASONAL SUMS OF MONTHLY AVERAGES
         SSUM=0.0
         WSUM=0.0
         NW=0
         NS=0
         N1=10
         N2=12
         IPASS=0
190      DO 210 MO=N1,N2
            IF (YAVG(MO).LT.-998.0) GO TO 210
            IF (MO.GT.LMOW.AND.MO.LT.IMOW) GO TO 200
               WSUM=WSUM+YAVG(MO)
               NW=NW+1
               GO TO 210
200         SSUM=SSUM+YAVG(MO)
            NS=NS+1
210         CONTINUE
         IF (IPASS.EQ.1) GO TO 220
            N1=1
            N2=9
            IPASS=1
            GO TO 190
220      IF (NW.EQ.0) WSUM=-999.0
         IF (NS.EQ.0) SSUM=-999.0
         WRITE (IPR,340) MONTH(IMOW),MONTH(LMOW),WSUM,UNITS,
     *      MONTH(IMOS),MONTH(LMOS),SSUM,UNITS
230      CONTINUE
C
235    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
240   FORMAT (' *** ENTER MPSMRY')
250   FORMAT ('1')
260   FORMAT ('0')
270   FORMAT ( T5,'MAP SUBAREA SUMMARY FOR  ',5A4,'  (',3A4,').',
     *            '  PERIOD OF RECORD  ',I2.2,'/',I4,' THRU ',
     *            I2.2,'/',I4,'.'  )
280   FORMAT (' ',I4,5X,12F8.2,5X,F8.2,2X,A4)
290   FORMAT (' ',I4,5X,3(1X,'-999.00'),9F8.2,5X,F8.2)
300   FORMAT (' ',I4,5X,3F8.2,9(1X,'-999.00'),5X,F8.2)
310   FORMAT ('0','YEAR',T15,12(A3,5X),3X,'TOTAL')
320   FORMAT ('0','WATER',T15,12(A3,5X),3X,'TOTAL' / ' YEAR')
330   FORMAT (T11,12F8.2,5X,F8.2,2X,A4)
340   FORMAT (/'0',T77,'WINTER SEASON TOTAL  (',A4,'-',A4,')  =',F8.2,
     *         2X,A4  /
     *         '0',T77,'SUMMER SEASON TOTAL  (',A4,'-',A4,')  =',F8.2,
     *         2X,A4  )
350   FORMAT (' ',3I10,F10.2)
360   FORMAT ('0TRACE 1',5I10)
C
      END
