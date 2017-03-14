      SUBROUTINE GDSWIZ05(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                    LROT,CROT,SROT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  GDSWIZ05   GDS WIZARD FOR POLAR STEREOGRAPHIC AZIMUTHAL
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
C           CALCULATIONS FOR A SPHERICAL OR ELLIPTICAL EARTH
C           ARE INVOKED ACCORDING TO THE INFORMATION IN 
C           SECTION 1, OCTET 17, BIT 2.  NOTE: ELLIPTICAL EARTH 
C           CALCULATIONS ARE BASED ON THE WGS84 DATUM.
C
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   06-01-10  GAYNO    CALCULATIONS FOR ELLIPTICAL EARTH.
C
C USAGE:    CALL GDSWIZ05(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
C     &                   LROT,CROT,SROT)
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
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      IMPLICIT NONE
      INTEGER         :: IM, JM, IROT, KGDS(200), IPROJ
      INTEGER         :: ISCAN, JSCAN, NSCAN, NRET
      INTEGER         :: ITER, NPTS, IOPT, N, LROT
      REAL            :: XPTS(NPTS),YPTS(NPTS),RLON(NPTS),RLAT(NPTS)
      REAL            :: CROT(NPTS),SROT(NPTS)
      REAL, PARAMETER :: RERTH=6.3712E6
      REAL, PARAMETER :: RERTH_WGS84=6.378137E6
      REAL, PARAMETER :: PI=3.14159265358979
      REAL, PARAMETER :: DPR=180./PI
      REAL, PARAMETER :: PI2=PI/2.0
      REAL, PARAMETER :: PI4=PI/4.0
      REAL, PARAMETER :: E2=.00669437999013  ! wgs84 datum
      REAL            :: E
      REAL            :: E_OVER_2
      REAL, PARAMETER :: SLAT=60.0  ! standard latitude according
                                    ! to grib standard
      REAL, PARAMETER :: SLATR=SLAT/DPR
      REAL            :: MC, RLAT1, RLON1, ORIENT, DX, DY, H, HI, HJ
      REAL            :: DXS, DYS, DE, DR, XP, YP, DE2, ALAT, ALONG
      REAL            :: T, RHO, TC, XMIN, XMAX, YMIN, YMAX, DI, DJ
      REAL            :: ALAT1, DR2, DIFF, FILL
      LOGICAL         :: ELLIPTICAL
      
      E=SQRT(E2)
      E_OVER_2=E*0.5
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(KGDS(1).EQ.005) THEN
        ELLIPTICAL=MOD(KGDS(6)/64,2).EQ.1
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
        IF(H.EQ.-1)ORIENT=ORIENT+180.
        HI=(-1.)**ISCAN
        HJ=(-1.)**(1-JSCAN)
        DXS=DX*HI
        DYS=DY*HJ
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C FIND X/Y OF POLE
        IF (.NOT.ELLIPTICAL) THEN
          DE=(1.+SIN(SLAT/DPR))*RERTH
          DR=DE*COS(RLAT1/DPR)/(1+H*SIN(RLAT1/DPR))
          XP=1-H*SIN((RLON1-ORIENT)/DPR)*DR/DXS
          YP=1+COS((RLON1-ORIENT)/DPR)*DR/DYS
          DE2=DE**2
        ELSE
          ALAT=H*RLAT1/DPR
          ALONG = (RLON1-ORIENT)/DPR
          T=TAN(PI4-ALAT/2.)/((1.-E*SIN(ALAT))/
     &      (1.+E*SIN(ALAT)))**(E_OVER_2)
          TC=TAN(PI4-SLATR/2.)/((1.-E*SIN(SLATR))/
     &      (1.+E*SIN(SLATR)))**(E_OVER_2)
          MC=COS(SLATR)/SQRT(1.0-E2*(SIN(SLATR)**2))
          RHO=RERTH_WGS84*MC*T/TC
          YP = 1.0 + RHO*COS(H*ALONG)/DYS
          XP = 1.0 - RHO*SIN(H*ALONG)/DXS
        ENDIF ! ELLIPTICAL
        XMIN=0
        XMAX=IM+1
        YMIN=0
        YMAX=JM+1
        NRET=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSLATE GRID COORDINATES TO EARTH COORDINATES
        IF(IOPT.EQ.0.OR.IOPT.EQ.1) THEN
          IF(.NOT.ELLIPTICAL)THEN
            DO N=1,NPTS
              IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.
     &           YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
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
              ELSE
                RLON(N)=FILL
                RLAT(N)=FILL
              ENDIF
            ENDDO
          ELSE ! ELLIPTICAL
            DO N=1,NPTS
              IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.
     &           YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
                DI=(XPTS(N)-XP)*DXS
                DJ=(YPTS(N)-YP)*DYS
                RHO=SQRT(DI*DI+DJ*DJ)
                T=(RHO*TC)/(RERTH_WGS84*MC)
                IF(ABS(YPTS(N)-YP)<0.01)THEN
                  IF(DI>0.0) ALONG=ORIENT+H*90.0
                  IF(DI<=0.0) ALONG=ORIENT-H*90.0
                ELSE
                  ALONG=ORIENT+H*ATAN(DI/(-DJ))*DPR
                  IF(DJ>0) ALONG=ALONG+180.
                END IF
                ALAT1=PI2-2.0*ATAN(T)
                DO ITER=1,10
                  ALAT = PI2 - 2.0*ATAN(T*(((1.0-E*SIN(ALAT1))/
     &                  (1.0+E*SIN(ALAT1)))**(E_OVER_2)))
                  DIFF = ABS(ALAT-ALAT1)*DPR
                  IF (DIFF < 0.000001) EXIT
                  ALAT1=ALAT
                ENDDO
                RLAT(N)=H*ALAT*DPR
                RLON(N)=ALONG
                IF(RLON(N)<0.0) RLON(N)=RLON(N)+360.
                IF(RLON(N)>360.0) RLON(N)=RLON(N)-360.0
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
              ELSE
                RLON(N)=FILL
                RLAT(N)=FILL
              ENDIF
            ENDDO
          ENDIF ! ELLIPTICAL
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSLATE EARTH COORDINATES TO GRID COORDINATES
        ELSEIF(IOPT.EQ.-1) THEN
          IF(.NOT.ELLIPTICAL)THEN
            DO N=1,NPTS
              IF(ABS(RLON(N)).LE.360.AND.ABS(RLAT(N)).LE.90.AND.
     &                                   H*RLAT(N).NE.-90) THEN
                DR=DE*TAN((90-H*RLAT(N))/2/DPR)
                XPTS(N)=XP+H*SIN((RLON(N)-ORIENT)/DPR)*DR/DXS
                YPTS(N)=YP-COS((RLON(N)-ORIENT)/DPR)*DR/DYS
                IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.
     &             YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
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
                ELSE
                  XPTS(N)=FILL
                  YPTS(N)=FILL
                ENDIF
              ELSE
                XPTS(N)=FILL
                YPTS(N)=FILL
              ENDIF
            ENDDO
          ELSE ! ELLIPTICAL CALCS
            DO N=1,NPTS
              IF(ABS(RLON(N)).LE.360.AND.ABS(RLAT(N)).LE.90.AND.
     &                                   H*RLAT(N).NE.-90) THEN
                ALAT = H*RLAT(N)/DPR
                ALONG = (RLON(N)-ORIENT)/DPR
                T=TAN(PI4-ALAT*0.5)/((1.-E*SIN(ALAT))/
     &            (1.+E*SIN(ALAT)))**(E_OVER_2)
                RHO=RERTH_WGS84*MC*T/TC
                XPTS(N)= XP + RHO*SIN(H*ALONG) / DXS
                YPTS(N)= YP - RHO*COS(H*ALONG) / DYS
                IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.
     &             YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
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
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PROJECTION UNRECOGNIZED
      ELSE
        NRET=0
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
