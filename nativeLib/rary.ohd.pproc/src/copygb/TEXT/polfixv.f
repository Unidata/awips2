C-----------------------------------------------------------------------
      SUBROUTINE POLFIXV(NM,NX,KM,RLAT,RLON,IB,LO,UO,VO)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  POLFIXV    MAKE MULTIPLE POLE VECTOR VALUES CONSISTENT
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM AVERAGES MULTIPLE POLE VECTOR VALUES
C           ON A LATITUDE/LONGITUDE GRID.  BITMAPS MAY BE AVERAGED TOO.
C           VECTORS ARE ROTATED WITH RESPECT TO THEIR LONGITUDE.
C        
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   07-03-05  EBISUZAKI - CHANGE SUGGESTED BY IREDELL TO FIX MISSING POLES IN WAFS GRIDS
C
C USAGE:    CALL POLFIXV(NM,NX,KM,RLAT,RLON,IB,LO,UO,VO)
C
C   INPUT ARGUMENT LIST:
C     NO       - INTEGER NUMBER OF GRID POINTS
C     NX       - INTEGER LEADING DIMENSION OF FIELDS
C     KM       - INTEGER NUMBER OF FIELDS
C     RLAT     - REAL (NO) LATITUDES IN DEGREES
C     RLON     - REAL (NO) LONGITUDES IN DEGREES
C     IB       - INTEGER (KM) BITMAP FLAGS
C     LO       - LOGICAL*1 (NX,KM) BITMAPS (IF SOME IB(K)=1)
C     UO       - REAL (NX,KM) U-WINDS
C     VO       - REAL (NX,KM) V-WINDS
C
C   OUTPUT ARGUMENT LIST:
C     LO       - LOGICAL*1 (NX,KM) BITMAPS (IF SOME IB(K)=1)
C     UO       - REAL (NX,KM) U-WINDS
C     VO       - REAL (NX,KM) V-WINDS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      PARAMETER(RLATNP=89.9995,RLATSP=-RLATNP)
      REAL RLAT(NM),RLON(NM)
      INTEGER IB(KM)
      LOGICAL*1 LO(NX,KM)
      REAL UO(NX,KM),VO(NX,KM)
      PARAMETER(PI=3.14159265358979,DPR=180./PI)
      REAL CLON(NM),SLON(NM)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DO N=1,NM
        CLON(N)=COS(RLON(N)/DPR)
        SLON(N)=SIN(RLON(N)/DPR)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CMIC$ DO ALL AUTOSCOPE
      DO K=1,KM
        WNP=0.
        UNP=0.
        VNP=0.
        TNP=0.
        WSP=0.
        USP=0.
        VSP=0.
        TSP=0.
C  AVERAGE MULTIPLE POLE VALUES
        DO N=1,NM
          IF(RLAT(N).GE.RLATNP) THEN
            WNP=WNP+1
            IF(IB(K).EQ.0.OR.LO(N,K)) THEN
              UNP=UNP+(CLON(N)*UO(N,K)-SLON(N)*VO(N,K))
              VNP=VNP+(SLON(N)*UO(N,K)+CLON(N)*VO(N,K))
              TNP=TNP+1
            ENDIF
          ELSEIF(RLAT(N).LE.RLATSP) THEN
            WSP=WSP+1
            IF(IB(K).EQ.0.OR.LO(N,K)) THEN
              USP=USP+(CLON(N)*UO(N,K)+SLON(N)*VO(N,K))
              VSP=VSP+(-SLON(N)*UO(N,K)+CLON(N)*VO(N,K))
              TSP=TSP+1
            ENDIF
          ENDIF
        ENDDO
C  DISTRIBUTE AVERAGE VALUES BACK TO MULTIPLE POLES
        IF(WNP.GT.1) THEN
          IF(TNP.GE.1) THEN
            UNP=UNP/TNP
            VNP=VNP/TNP
          ELSE
            UNP=0.
            VNP=0.
          ENDIF
          DO N=1,NM
            IF(RLAT(N).GE.RLATNP) THEN
              IF(IB(K).NE.0) LO(N,K)=TNP.GE.1
              UO(N,K)=CLON(N)*UNP+SLON(N)*VNP
              VO(N,K)=-SLON(N)*UNP+CLON(N)*VNP
            ENDIF
          ENDDO
        ENDIF
        IF(WSP.GT.1) THEN
          IF(TSP.GE.1) THEN
            USP=USP/WSP
            VSP=VSP/WSP
          ELSE
            USP=0.
            VSP=0.
          ENDIF
          DO N=1,NM
            IF(RLAT(N).LE.RLATSP) THEN
              IF(IB(K).NE.0) LO(N,K)=TSP.GE.1
              UO(N,K)=CLON(N)*USP-SLON(N)*VSP
              VO(N,K)=SLON(N)*USP+CLON(N)*VSP
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
