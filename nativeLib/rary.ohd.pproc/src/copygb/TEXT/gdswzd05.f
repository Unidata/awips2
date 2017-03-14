C-----------------------------------------------------------------------
      SUBROUTINE GDSWZD05(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                    LROT,CROT,SROT,LMAP,XLON,XLAT,YLON,YLAT,AREA)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  GDSWZD05   GDS WIZARD FOR POLAR STEREOGRAPHIC AZIMUTHAL
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM DECODES THE GRIB GRID DESCRIPTION SECTION
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63)
C           AND RETURNS ONE OF THE FOLLOWING:
C             (IOPT=+1) EARTH COORDINATES OF SELECTED GRID COORDINATES
C             (IOPT=-1) GRID COORDINATES OF SELECTED EARTH COORDINATES
C           FOR POLAR STEREOGRAPHIC AZIMUTHAL PROJECTIONS.
C           IF THE SELECTED COORDINATES ARE MORE THAN ONE GRIDPOINT
C           BEYOND THE THE EDGES OF THE GRID DOMAIN, THEN THE RELEVANT
C           OUTPUT ELEMENTS ARE SET TO FILL VALUES.
C           THE ACTUAL NUMBER OF VALID POINTS COMPUTED IS RETURNED TOO.
C           OPTIONALLY, THE VECTOR ROTATIONS AND THE MAP JACOBIANS
C           FOR THIS GRID MAY BE RETURNED AS WELL.
C
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   97-10-20  IREDELL  INCLUDE MAP OPTIONS
C
C USAGE:    CALL GDSWZD05(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
C    &                    LROT,CROT,SROT,LMAP,XLON,XLAT,YLON,YLAT,AREA)
C
C   INPUT ARGUMENT LIST:
C     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
C     IOPT     - INTEGER OPTION FLAG
C                (+1 TO COMPUTE EARTH COORDS OF SELECTED GRID COORDS)
C                (-1 TO COMPUTE GRID COORDS OF SELECTED EARTH COORDS)
C     NPTS     - INTEGER MAXIMUM NUMBER OF COORDINATES
C     FILL     - REAL FILL VALUE TO SET INVALID OUTPUT DATA
C                (MUST BE IMPOSSIBLE VALUE; SUGGESTED VALUE: -9999.)
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT>0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT>0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT<0
C                (ACCEPTABLE RANGE: -360. TO 360.)
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT<0
C                (ACCEPTABLE RANGE: -90. TO 90.)
C     LROT     - INTEGER FLAG TO RETURN VECTOR ROTATIONS IF 1
C     LMAP     - INTEGER FLAG TO RETURN MAP JACOBIANS IF 1
C
C   OUTPUT ARGUMENT LIST:
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT<0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT<0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT>0
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT>0
C     NRET     - INTEGER NUMBER OF VALID POINTS COMPUTED
C     CROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION COSINES IF LROT=1
C     SROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION SINES IF LROT=1
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C     XLON     - REAL (NPTS) DX/DLON IN 1/DEGREES IF LMAP=1
C     XLAT     - REAL (NPTS) DX/DLAT IN 1/DEGREES IF LMAP=1
C     YLON     - REAL (NPTS) DY/DLON IN 1/DEGREES IF LMAP=1
C     YLAT     - REAL (NPTS) DY/DLAT IN 1/DEGREES IF LMAP=1
C     AREA     - REAL (NPTS) AREA WEIGHTS IN M**2 IF LMAP=1
C                (PROPORTIONAL TO THE SQUARE OF THE MAP FACTOR)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      INTEGER KGDS(200)
      REAL XPTS(NPTS),YPTS(NPTS),RLON(NPTS),RLAT(NPTS)
      REAL CROT(NPTS),SROT(NPTS)
      REAL XLON(NPTS),XLAT(NPTS),YLON(NPTS),YLAT(NPTS),AREA(NPTS)
      PARAMETER(RERTH=6.3712E6)
      PARAMETER(PI=3.14159265358979,DPR=180./PI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(KGDS(1).EQ.005) THEN
        IM=KGDS(2)
        JM=KGDS(3)
        RLAT1=KGDS(4)*1.E-3
        RLON1=KGDS(5)*1.E-3
        IROT=MOD(KGDS(6)/8,2)
        ORIENT=KGDS(7)*1.E-3
        DX=KGDS(8)
        DY=KGDS(9)
        IPROJ=MOD(KGDS(10)/128,2)
        ISCAN=MOD(KGDS(11)/128,2)
        JSCAN=MOD(KGDS(11)/64,2)
        NSCAN=MOD(KGDS(11)/32,2)
        H=(-1.)**IPROJ
        HI=(-1.)**ISCAN
        HJ=(-1.)**(1-JSCAN)
        DXS=DX*HI
        DYS=DY*HJ
        DE=(1.+SIN(60./DPR))*RERTH
        DR=DE*COS(RLAT1/DPR)/(1+H*SIN(RLAT1/DPR))
        XP=1-H*SIN((RLON1-ORIENT)/DPR)*DR/DXS
        YP=1+COS((RLON1-ORIENT)/DPR)*DR/DYS
        DE2=DE**2
        XMIN=0
        XMAX=IM+1
        YMIN=0
        YMAX=JM+1
        NRET=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSLATE GRID COORDINATES TO EARTH COORDINATES
        IF(IOPT.EQ.0.OR.IOPT.EQ.1) THEN
          DO N=1,NPTS
            IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.
     &         YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
              DI=(XPTS(N)-XP)*DXS
              DJ=(YPTS(N)-YP)*DYS
              DR2=DI**2+DJ**2
              IF(DR2.LT.DE2*1.E-6) THEN
                RLON(N)=0.
                RLAT(N)=H*90.
              ELSE
                RLON(N)=MOD(ORIENT+H*DPR*ATAN2(DI,-DJ)+3600,360.)
                RLAT(N)=H*DPR*ASIN((DE2-DR2)/(DE2+DR2))
              ENDIF
              NRET=NRET+1
              IF(LROT.EQ.1) THEN
                IF(IROT.EQ.1) THEN
                  CROT(N)=H*COS((RLON(N)-ORIENT)/DPR)
                  SROT(N)=SIN((RLON(N)-ORIENT)/DPR)
                ELSE
                  CROT(N)=1
                  SROT(N)=0
                ENDIF
              ENDIF
              IF(LMAP.EQ.1) THEN
                IF(DR2.LT.DE2*1.E-6) THEN
                  XLON(N)=0.
                  XLAT(N)=-SIN((RLON(N)-ORIENT)/DPR)/DPR*DE/DXS/2
                  YLON(N)=0.
                  YLAT(N)=H*COS((RLON(N)-ORIENT)/DPR)/DPR*DE/DYS/2
                  AREA(N)=RERTH**2*DXS*DYS*4/DE2
                ELSE
                  DR=SQRT(DR2)
                  CLAT=COS(RLAT(N)/DPR)
                  XLON(N)=H*COS((RLON(N)-ORIENT)/DPR)/DPR*DR/DXS
                  XLAT(N)=-SIN((RLON(N)-ORIENT)/DPR)/DPR*DR/DXS/CLAT
                  YLON(N)=SIN((RLON(N)-ORIENT)/DPR)/DPR*DR/DYS
                  YLAT(N)=H*COS((RLON(N)-ORIENT)/DPR)/DPR*DR/DYS/CLAT
                  AREA(N)=RERTH**2*CLAT**2*DXS*DYS/DR2
                ENDIF
              ENDIF
            ELSE
              RLON(N)=FILL
              RLAT(N)=FILL
            ENDIF
          ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSLATE EARTH COORDINATES TO GRID COORDINATES
        ELSEIF(IOPT.EQ.-1) THEN
          DO N=1,NPTS
            IF(ABS(RLON(N)).LE.360.AND.ABS(RLAT(N)).LE.90.AND.
     &                                 H*RLAT(N).NE.-90) THEN
              DR=DE*TAN((90-H*RLAT(N))/2/DPR)
              XPTS(N)=XP+H*SIN((RLON(N)-ORIENT)/DPR)*DR/DXS
              YPTS(N)=YP-COS((RLON(N)-ORIENT)/DPR)*DR/DYS
              IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.
     &           YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
                NRET=NRET+1
                IF(LROT.EQ.1) THEN
                  IF(IROT.EQ.1) THEN
                    CROT(N)=H*COS((RLON(N)-ORIENT)/DPR)
                    SROT(N)=SIN((RLON(N)-ORIENT)/DPR)
                  ELSE
                    CROT(N)=1
                    SROT(N)=0
                  ENDIF
                ENDIF
                IF(LMAP.EQ.1) THEN
                  DR2=DR**2
                  IF(DR2.LT.DE2*1.E-6) THEN
                    XLON(N)=0.
                    XLAT(N)=-SIN((RLON(N)-ORIENT)/DPR)/DPR*DE/DXS/2
                    YLON(N)=0.
                    YLAT(N)=H*COS((RLON(N)-ORIENT)/DPR)/DPR*DE/DYS/2
                    AREA(N)=RERTH**2*DXS*DYS*4/DE2
                  ELSE
                    CLAT=COS(RLAT(N)/DPR)
                    XLON(N)=H*COS((RLON(N)-ORIENT)/DPR)/DPR*DR/DXS
                    XLAT(N)=-SIN((RLON(N)-ORIENT)/DPR)/DPR*DR/DXS/CLAT
                    YLON(N)=SIN((RLON(N)-ORIENT)/DPR)/DPR*DR/DYS
                    YLAT(N)=H*COS((RLON(N)-ORIENT)/DPR)/DPR*DR/DYS/CLAT
                    AREA(N)=RERTH**2*CLAT**2*DXS*DYS/DR2
                  ENDIF
                ENDIF
              ELSE
                XPTS(N)=FILL
                YPTS(N)=FILL
              ENDIF
            ELSE
              XPTS(N)=FILL
              YPTS(N)=FILL
            ENDIF
          ENDDO
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PROJECTION UNRECOGNIZED
      ELSE
        IRET=-1
        IF(IOPT.GE.0) THEN
          DO N=1,NPTS
            RLON(N)=FILL
            RLAT(N)=FILL
          ENDDO
        ENDIF
        IF(IOPT.LE.0) THEN
          DO N=1,NPTS
            XPTS(N)=FILL
            YPTS(N)=FILL
          ENDDO
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
