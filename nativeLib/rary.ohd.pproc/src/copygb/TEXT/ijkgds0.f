C-----------------------------------------------------------------------
      SUBROUTINE IJKGDS0(KGDS,IJKGDSA)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  IJKGDS0    SET UP PARAMETERS FOR FUNCTION IJKGDS1
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM DECODES THE GRIB GRID DESCRIPTION SECTION
C           AND RETURNS A NAVIGATION PARAMETER ARRAY TO ALLOW FUNCTION
C           IJKGDS1 TO DECODE THE FIELD POSITION FOR A GIVEN GRID POINT.
C
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   97-03-11  IREDELL  ALLOWED HEMISPHERIC GRIDS TO WRAP OVER ONE POLE
C   98-07-13  BALDWIN  ADD 2D STAGGERED ETA GRID INDEXING (203)
C 1999-04-08  IREDELL  SPLIT IJKGDS INTO TWO
C
C USAGE:    CALL IJKGDS0(KGDS,IJKGDSA)
C
C   INPUT ARGUMENT LIST:
C     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
C
C   OUTPUT ARGUMENT LIST:
C     IJKGDSA  - INTEGER (20) NAVIGATION PARAMETER ARRAY
C                IJKGDSA(1) IS NUMBER OF X POINTS
C                IJKGDSA(2) IS NUMBER OF Y POINTS
C                IJKGDSA(3) IS X WRAPAROUND INCREMENT
C                           (0 IF NO WRAPAROUND)
C                IJKGDSA(4) IS Y WRAPAROUND LOWER PIVOT POINT
C                           (0 IF NO WRAPAROUND)
C                IJKGDSA(5) IS Y WRAPAROUND UPPER PIVOT POINT
C                           (0 IF NO WRAPAROUND)
C                IJKGDSA(6) IS SCANNING MODE
C                           (0 IF X FIRST THEN Y; 1 IF Y FIRST THEN X;
C                            2 IF STAGGERED DIAGONAL LIKE PROJECTION 201;
C                            3 IF STAGGERED DIAGONAL LIKE PROJECTION 203)
C                IJKGDSA(7) IS MASS/WIND FLAG FOR STAGGERED DIAGONAL
C                           (0 IF MASS; 1 IF WIND)
C                IJKGDSA(8:20) ARE UNUSED AT THE MOMENT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      INTEGER KGDS(200)
      INTEGER IJKGDSA(20)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET USUAL VALUES
      IM=KGDS(2)
      JM=KGDS(3)
      IWRAP=0
      JWRAP1=0
      JWRAP2=0
      NSCAN=MOD(KGDS(11)/32,2)
      KSCAN=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET EXCEPTIONAL VALUES FOR PROJECTION 000
      IF(KGDS(1).EQ.0) THEN
        RLON1=KGDS(5)*1.E-3
        RLON2=KGDS(8)*1.E-3
        ISCAN=MOD(KGDS(11)/128,2)
        IF(ISCAN.EQ.0) THEN
          DLON=(MOD(RLON2-RLON1-1+3600,360.)+1)/(IM-1)
        ELSE
          DLON=-(MOD(RLON1-RLON2-1+3600,360.)+1)/(IM-1)
        ENDIF
        IWRAP=NINT(360/ABS(DLON))
        IF(IM.LT.IWRAP) IWRAP=0
        IF(IWRAP.GT.0.AND.MOD(IWRAP,2).EQ.0) THEN
          RLAT1=KGDS(4)*1.E-3
          RLAT2=KGDS(7)*1.E-3
          DLAT=ABS(RLAT2-RLAT1)/(JM-1)
          IF(ABS(RLAT1).GT.90-0.25*DLAT) THEN
            JWRAP1=2
          ELSEIF(ABS(RLAT1).GT.90-0.75*DLAT) THEN
            JWRAP1=1
          ENDIF
          IF(ABS(RLAT2).GT.90-0.25*DLAT) THEN
            JWRAP2=2*JM
          ELSEIF(ABS(RLAT2).GT.90-0.75*DLAT) THEN
            JWRAP2=2*JM+1
          ENDIF
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET EXCEPTIONAL VALUES FOR PROJECTION 001
      ELSEIF(KGDS(1).EQ.1) THEN
        RLON1=KGDS(5)*1.E-3
        RLON2=KGDS(8)*1.E-3
        ISCAN=MOD(KGDS(11)/128,2)
        IF(ISCAN.EQ.0) THEN
          DLON=(MOD(RLON2-RLON1-1+3600,360.)+1)/(IM-1)
        ELSE
          DLON=-(MOD(RLON1-RLON2-1+3600,360.)+1)/(IM-1)
        ENDIF
        IWRAP=NINT(360/ABS(DLON))
        IF(IM.LT.IWRAP) IWRAP=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET EXCEPTIONAL VALUES FOR PROJECTION 004
      ELSEIF(KGDS(1).EQ.4) THEN
        RLON1=KGDS(5)*1.E-3
        RLON2=KGDS(8)*1.E-3
        ISCAN=MOD(KGDS(11)/128,2)
        IF(ISCAN.EQ.0) THEN
          DLON=(MOD(RLON2-RLON1-1+3600,360.)+1)/(IM-1)
        ELSE
          DLON=-(MOD(RLON1-RLON2-1+3600,360.)+1)/(IM-1)
        ENDIF
        IWRAP=NINT(360/ABS(DLON))
        IF(IM.LT.IWRAP) IWRAP=0
        IF(IWRAP.GT.0.AND.MOD(IWRAP,2).EQ.0) THEN
          JG=KGDS(10)*2
          IF(JM.EQ.JG) THEN
            JWRAP1=1
            JWRAP2=2*JM+1
          ENDIF
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET EXCEPTIONAL VALUES FOR PROJECTION 201
      ELSEIF(KGDS(1).EQ.201) THEN
        IM=KGDS(7)
        JM=KGDS(8)
        NSCAN=2
        KSCAN=MOD(KGDS(11)/256,2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET EXCEPTIONAL VALUES FOR PROJECTION 202
      ELSEIF(KGDS(1).EQ.202) THEN
        IM=KGDS(7)
        JM=KGDS(8)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET EXCEPTIONAL VALUES FOR PROJECTION 203
      ELSEIF(KGDS(1).EQ.203) THEN
        NSCAN=3
        KSCAN=MOD(KGDS(11)/256,2) 
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FILL NAVIGATION PARAMETER ARRAY
      IJKGDSA(1)=IM
      IJKGDSA(2)=JM
      IJKGDSA(3)=IWRAP
      IJKGDSA(4)=JWRAP1
      IJKGDSA(5)=JWRAP2
      IJKGDSA(6)=NSCAN
      IJKGDSA(7)=KSCAN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
