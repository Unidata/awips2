C-----------------------------------------------------------------------
      SUBROUTINE POLFIXS(NM,NX,KM,RLAT,RLON,IB,LO,GO)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  POLFIXS    MAKE MULTIPLE POLE SCALAR VALUES CONSISTENT
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM AVERAGES MULTIPLE POLE SCALAR VALUES
C           ON A LATITUDE/LONGITUDE GRID.  BITMAPS MAY BE AVERAGED TOO.
C        
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   07-03-05  EBISUZAKI - CHANGE SUGGESTED BY IREDELL TO ADD POLES FOR WAFS GRIDS
C
C USAGE:    CALL POLFIXS(NM,NX,KM,RLAT,RLON,IB,LO,GO)
C
C   INPUT ARGUMENT LIST:
C     NO       - INTEGER NUMBER OF GRID POINTS
C     NX       - INTEGER LEADING DIMENSION OF FIELDS
C     KM       - INTEGER NUMBER OF FIELDS
C     RLAT     - REAL (NO) LATITUDES IN DEGREES
C     RLON     - REAL (NO) LONGITUDES IN DEGREES
C     IB       - INTEGER (KM) BITMAP FLAGS
C     LO       - LOGICAL*1 (NX,KM) BITMAPS (IF SOME IB(K)=1)
C     GO       - REAL (NX,KM) FIELDS
C
C   OUTPUT ARGUMENT LIST:
C     LO       - LOGICAL*1 (NX,KM) BITMAPS (IF SOME IB(K)=1)
C     GO       - REAL (NX,KM) FIELDS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      PARAMETER(RLATNP=89.9995,RLATSP=-RLATNP)
      REAL RLAT(NM),RLON(NM)
      INTEGER IB(KM)
      REAL GO(NX,KM)
      LOGICAL*1 LO(NX,KM)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CMIC$ DO ALL AUTOSCOPE
      DO K=1,KM
        WNP=0.
        GNP=0.
        TNP=0.
        WSP=0.
        GSP=0.
        TSP=0.
C  AVERAGE MULTIPLE POLE VALUES
        DO N=1,NM
          IF(RLAT(N).GE.RLATNP) THEN
            WNP=WNP+1
            IF(IB(K).EQ.0.OR.LO(N,K)) THEN
              GNP=GNP+GO(N,K)
              TNP=TNP+1
            ENDIF
          ELSEIF(RLAT(N).LE.RLATSP) THEN
            WSP=WSP+1
            IF(IB(K).EQ.0.OR.LO(N,K)) THEN
              GSP=GSP+GO(N,K)
              TSP=TSP+1
            ENDIF
          ENDIF
        ENDDO
C  DISTRIBUTE AVERAGE VALUES BACK TO MULTIPLE POLES
        IF(WNP.GT.1) THEN
          IF(TNP.GE.1) THEN
            GNP=GNP/TNP
          ELSE
            GNP=0.
          ENDIF
          DO N=1,NM
            IF(RLAT(N).GE.RLATNP) THEN
              IF(IB(K).NE.0) LO(N,K)=TNP.GE.1
              GO(N,K)=GNP
            ENDIF
          ENDDO
        ENDIF
        IF(WSP.GT.1) THEN
          IF(TSP.GE.1) THEN
            GSP=GSP/TSP
          ELSE
            GSP=0.
          ENDIF
          DO N=1,NM
            IF(RLAT(N).LE.RLATSP) THEN
              IF(IB(K).NE.0) LO(N,K)=TSP.GE.1
              GO(N,K)=GSP
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
