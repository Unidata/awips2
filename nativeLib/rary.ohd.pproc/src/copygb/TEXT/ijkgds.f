C-----------------------------------------------------------------------
      FUNCTION IJKGDS(I,J,KGDS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  IJKGDS     RETURN FIELD POSITION FOR A GIVEN GRID POINT
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM DECODES THE GRIB GRID DESCRIPTION SECTION
C           AND RETURNS THE FIELD POSITION FOR A GIVEN GRID POINT.
C
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   97-03-11  IREDELL  ALLOWED HEMISPHERIC GRIDS TO WRAP OVER ONE POLE
C   98-07-13  BALDWIN  ADD 2D STAGGERED ETA GRID INDEXING (203)
C
C USAGE:    ...IJKGDS(I,J,KGDS)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER X GRID POINT
C     J        - INTEGER Y GRID POINT
C     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
C
C   OUTPUT ARGUMENT LIST:
C     IJKGDS   - INTEGER POSITION IN GRIB FIELD TO LOCATE GRID POINT
C                (0 IF OUT OF BOUNDS)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      INTEGER KGDS(200)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET GRID DIMENSIONS
      IF(KGDS(1).EQ.201) THEN
        IM=KGDS(7)*2-1
        JM=KGDS(8)
        KSCAN=MOD(KGDS(11)/256,2)
        IF(KSCAN.EQ.0) THEN
          IS1=(JM+1)/2
        ELSE
          IS1=JM/2
        ENDIF
      ELSEIF(KGDS(1).EQ.202) THEN
        IM=KGDS(7)
        JM=KGDS(8)
        KSCAN=0
        IS1=0
      ELSEIF(KGDS(1).EQ.203) THEN
        IM=KGDS(2)
        JM=KGDS(3)
        KSCAN=MOD(KGDS(11)/256,2) 
        IF(KSCAN.EQ.0) THEN
          IS1=(JM+1)/2
        ELSE
          IS1=JM/2
        ENDIF
      ELSE
        IM=KGDS(2)
        JM=KGDS(3)
        KSCAN=0
        IS1=0
      ENDIF
      NSCAN=MOD(KGDS(11)/32,2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ACCOUNT FOR WRAPAROUNDS IN EITHER DIRECTION
      II=I
      JJ=J
      IF(KGDS(1).EQ.0.OR.KGDS(1).EQ.1.OR.KGDS(1).EQ.4) THEN
        RLON1=KGDS(5)*1.E-3
        RLON2=KGDS(8)*1.E-3
        ISCAN=MOD(KGDS(11)/128,2)
        IF(ISCAN.EQ.0) THEN
          DLON=(MOD(RLON2-RLON1-1+3600,360.)+1)/(IM-1)
        ELSE
          DLON=-(MOD(RLON1-RLON2-1+3600,360.)+1)/(IM-1)
        ENDIF
        IG=NINT(360/ABS(DLON))
        IF(IM.GE.IG) THEN
          II=MOD(I-1+IG,IG)+1
          IF((J.LE.0.OR.J.GE.JM+1).AND.MOD(IG,2).EQ.0) THEN
            IF(KGDS(1).EQ.0) THEN
              RLAT1=KGDS(4)*1.E-3
              RLAT2=KGDS(7)*1.E-3
              DLAT=ABS(RLAT2-RLAT1)/(JM-1)
              IF(J.LE.0.AND.ABS(RLAT1).GT.90-0.25*DLAT) THEN
                JJ=2-J
                II=MOD(II-1+IG/2,IG)+1
              ELSEIF(J.LE.0.AND.ABS(RLAT1).GT.90-0.75*DLAT) THEN
                JJ=1-J
                II=MOD(II-1+IG/2,IG)+1
              ELSEIF(J.GE.JM+1.AND.ABS(RLAT2).GT.90-0.25*DLAT) THEN
                JJ=2*JM-J
                II=MOD(II-1+IG/2,IG)+1
              ELSEIF(J.GE.JM+1.AND.ABS(RLAT2).GT.90-0.75*DLAT) THEN
                JJ=2*JM+1-J
                II=MOD(II-1+IG/2,IG)+1
              ENDIF
            ELSEIF(KGDS(1).EQ.4) THEN
              JG=KGDS(10)*2
              IF(J.LE.0.AND.JM.EQ.JG) THEN
                JJ=1-J
                II=MOD(II-1+IG/2,IG)+1
              ELSEIF(J.GE.JM+1.AND.JM.EQ.JG) THEN
                JJ=2*JM+1-J
                II=MOD(II-1+IG/2,IG)+1
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  RECKON FIELD POSITION
      IF(KGDS(1).EQ.201) THEN
        IIF=JJ+(II-IS1)
        JJF=JJ-(II-IS1)+KSCAN
        II=IIF
        JJ=JJF
      ELSEIF(KGDS(1).EQ.203) THEN
        IIF=JJ+(II-IS1)
        JJF=JJ-(II-IS1)+KSCAN
        II=(IIF+MOD(JJF+KSCAN,2))/2
        JJ=JJF
      ENDIF
      IF(II.GE.1.AND.II.LE.IM.AND.JJ.GE.1.AND.JJ.LE.JM) THEN
        IF(NSCAN.EQ.0) THEN
          IJKGDS=II+(JJ-1)*IM
        ELSE
          IJKGDS=JJ+(II-1)*JM
        ENDIF
      ELSE
        IJKGDS=0
      ENDIF
      IF(KGDS(1).EQ.201) THEN
        IJKGDS=(IJKGDS+1-KSCAN)/2
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
