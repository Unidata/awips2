C-----------------------------------------------------------------------
      SUBROUTINE IPXETAS(IDIR,M1,M2,KM,KGDS1,F1,KGDS2,F2,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  IPXETAS    EXPAND OR CONTRACT ETA GRIDS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM TRANSFORMS BETWEEN THE STAGGERED ETA GRIDS
C           AS USED IN THE ETA MODEL AND FOR NATIVE GRID TRANSMISSION
C           AND THEIR FULL EXPANSION AS USED FOR GENERAL INTERPOLATION
C           AND GRAPHICS.  THE ETA GRIDS ARE ROTATED LATITUDE-LONGITUDE
C           GRIDS STAGGERED AS DEFINED BY THE ARAKAWA E-GRID, THAT IS
C           WITH MASS DATA POINTS ALTERNATING WITH WIND DATA POINTS.
C           THE EXPANSION OF THE FIELDS IS DONE BY 4-POINT AVERAGING.
C
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C
C USAGE:    CALL IPXETAS(IDIR,M1,M2,KM,KGDS1,F1,KGDS2,F2,IRET)
C
C   INPUT ARGUMENT LIST:
C     IDIR     - INTEGER TRANSFORM OPTION
C                (+1 TO EXPAND STAGGERED MASS FIELDS TO FULL FIELDS)
C                (+2 TO EXPAND STAGGERED WIND FIELDS TO FULL FIELDS)
C                (-1 TO CONTRACT FULL MASS FIELDS TO STAGGERED FIELDS)
C                (-2 TO CONTRACT FULL WIND FIELDS TO STAGGERED FIELDS)
C     M1       - INTEGER SKIP NUMBER BETWEEN STAGGERED GRID FIELDS
C     M2       - INTEGER SKIP NUMBER BETWEEN FULL GRID FIELDS
C     KM       - INTEGER NUMBER OF FIELDS TO TRANSFORM
C     KGDS1    - INTEGER (200) GDS PARMS OF STAGGERED GRID IF IDIR>0
C     F1       - REAL (M1,KM) STAGGERED GRID FIELDS IF IDIR>0
C     KGDS2    - INTEGER (200) GDS PARMS OF FULL GRID IF IDIR<0
C     F2       - REAL (M2,KM) FULL GRID FIELDS IF IDIR<0
C
C   OUTPUT ARGUMENT LIST:
C     KGDS1    - INTEGER (200) GDS PARMS OF STAGGERED GRID IF IDIR<0
C     F1       - REAL (M1,KM) STAGGERED GRID FIELDS IF IDIR<0
C     KGDS2    - INTEGER (200) GDS PARMS OF FULL GRID IF IDIR>0
C     F2       - REAL (M2,KM) FULL GRID FIELDS IF IDIR>0
C     IRET     - INTEGER RETURN CODE
C                0    SUCCESSFUL TRANSFORMATION
C                1    IMPROPER GRID SPECIFICATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      INTEGER KGDS1(200),KGDS2(200)
      REAL F1(M1,KM),F2(M2,KM)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM GDS
      IRET=0
C  EXPAND STAGGERED GDS TO FULL GDS
      IF(IDIR.GT.0.AND.KGDS1(1).EQ.201) THEN
        KGDS2(1:22)=KGDS1(1:22)
        KGDS2(1)=202
        KGDS2(7)=KGDS1(7)*2-1
        KGDS2(2)=KGDS2(7)*KGDS2(8)
C  CONTRACT FULL GDS TO STAGGERED GDS
      ELSEIF(IDIR.LT.0.AND.KGDS2(1).EQ.202) THEN
        KGDS1(1:22)=KGDS2(1:22)
        KGDS1(1)=201
        KGDS1(7)=KGDS2(7)/2+1
        KGDS1(2)=KGDS1(7)*KGDS1(8)-KGDS1(8)/2
      ELSE
        IRET=1
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM FIELDS
      IF(IRET.EQ.0) THEN
        IM=KGDS2(7)
        JM=KGDS2(8)
        NM=(IM*JM+1)/2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  EXPAND STAGGERED MASS FIELDS TO FULL MASS FIELDS
        IF(IDIR.EQ.1) THEN
          DO K=1,KM
            DO N=1,NM
              F2(N*2-1,K)=F1(N,K)
            ENDDO
            DO N=1,IM*JM-NM
              F2(N*2,K)=0
              W=0
C  COLLECT DATA POINT TO THE SOUTH OF VACANT POINT
              IF(N-IM/2.GE.1) THEN
                F2(N*2,K)=F2(N*2,K)+F1(N-IM/2,K)
                W=W+1
              ENDIF
C  COLLECT DATA POINT TO THE WEST OF VACANT POINT
              IF(MOD(N,IM).NE.IM/2+1) THEN
                F2(N*2,K)=F2(N*2,K)+F1(N,K)
                W=W+1
              ENDIF
C  COLLECT DATA POINT TO THE EAST OF VACANT POINT
              IF(MOD(N,IM).NE.0) THEN
                F2(N*2,K)=F2(N*2,K)+F1(N+1,K)
                W=W+1
              ENDIF
C  COLLECT DATA POINT TO THE NORTH OF VACANT POINT
              IF(N+1+IM/2.LE.NM) THEN
                F2(N*2,K)=F2(N*2,K)+F1(N+1+IM/2,K)
                W=W+1
              ENDIF
              F2(N*2,K)=F2(N*2,K)/W
            ENDDO
          ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  EXPAND STAGGERED WIND FIELDS TO FULL WIND FIELDS
        ELSEIF(IDIR.EQ.2) THEN
          DO K=1,KM
            DO N=1,IM*JM-NM
              F2(N*2,K)=F1(N,K)
            ENDDO
            DO N=1,NM
              F2(N*2-1,K)=0
              W=0
C  COLLECT DATA POINT TO THE SOUTH OF VACANT POINT
              IF(N-1-IM/2.GE.1) THEN
                F2(N*2-1,K)=F2(N*2-1,K)+F1(N-1-IM/2,K)
                W=W+1
              ENDIF
C  COLLECT DATA POINT TO THE WEST OF VACANT POINT
              IF(MOD(N,IM).NE.1) THEN
                F2(N*2-1,K)=F2(N*2-1,K)+F1(N-1,K)
                W=W+1
              ENDIF
C  COLLECT DATA POINT TO THE EAST OF VACANT POINT
              IF(MOD(N,IM).NE.IM/2+1) THEN
                F2(N*2-1,K)=F2(N*2-1,K)+F1(N,K)
                W=W+1
              ENDIF
C  COLLECT DATA POINT TO THE NORTH OF VACANT POINT
              IF(N+IM/2.LE.IM*JM-NM) THEN
                F2(N*2-1,K)=F2(N*2-1,K)+F1(N+IM/2,K)
                W=W+1
              ENDIF
              F2(N*2-1,K)=F2(N*2-1,K)/W
            ENDDO
          ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CONTRACT FULL MASS FIELDS TO STAGGERED MASS FIELDS
        ELSEIF(IDIR.EQ.-1) THEN
          DO K=1,KM
            DO N=1,NM
              F1(N,K)=F2(N*2-1,K)
            ENDDO
          ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CONTRACT FULL WIND FIELDS TO STAGGERED WIND FIELDS
        ELSEIF(IDIR.EQ.-2) THEN
          DO K=1,KM
            DO N=1,IM*JM-NM
              F1(N,K)=F2(N*2,K)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
