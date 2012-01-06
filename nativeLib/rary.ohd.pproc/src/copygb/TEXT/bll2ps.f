C-----------------------------------------------------------------------
      SUBROUTINE BLL2PS(IBM,IM,JM,KM,NPS,KB,TRUE,XMESH,ORIENT,
     &                  LB,B,LBN,BN,LBS,BS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  BLL2PS     INTERP. LATLON BUDGET TO POLAR STEREOGRAPHIC
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-20
C
C ABSTRACT: INTERPOLATES A BUDGET-TYPE SCALAR FIELD FROM A GLOBAL
C           LATITUDE-LONGITUDE CYLINDRICAL GRID (INCLUDING THE POLES)
C           TO A MATCHED PAIR OF POLAR STEREOGRAPHIC GRIDS CENTERED
C           ON THE RESPECTIVE POLES, WHERE THE ORIENTATION LONGITUDE
C           OF THE SOUTHERN HEMISPHERE GRID IS 180 DEGREES OPPOSITE
C           THAT OF THE NORTHERN HEMISPHERE GRID.  THIS INTERPOLATION
C           IS DESIGNED TO APPROXIMATE AN AREA-AVERAGE CONSERVING
C           INTERPOLATION FROM FINE TO COARSE RESOLUTION BUT TO BECOME
C           A BILINEAR INTERPOLATION FROM COARSE TO FINE RESOLUTION.
C           THIS IS ACCOMPLISHED BY BILINEARLY INTERPOLATING TO A MUCH
C           FINER POLAR STEREOGRAPHIC GRID AND THEN AREA-AVERAGING TO
C           THE OUTPUT GRID.  THE CURRENT CONFIGURATION INTERPOLATES
C           TO AN EXTRAVAGANT 121 POINTS IN EVERY OUTPUT GRID BOX.
C           THIS ROUTINE ALSO WOULD INTERPOLATE ASSOCIATED BITMAPS.
C           THIS ROUTINE IS CONFIGURED ONLY FOR (I,J,K) ORDER GRIDS.
C           THIS ROUTINE IS FULLY VECTORIZED AND MULTITASKED.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C 1998-12-15  IREDELL  OBSOLESCENCE WARNING MESSAGE ISSUED
C
C USAGE:    CALL BLL2PS(IBM,IM,JM,KM,NPS,KB,TRUE,XMESH,ORIENT,
C    &                  LB,B,LBN,BN,LBS,BS)
C
C   INPUT ARGUMENT LIST:
C     IBM      - INTEGER BITMAP IDENTIFIER
C                (0 FOR NO BITMAP, 1 TO INTERPOLATE BITMAP)
C     IM       - INTEGER NUMBER OF INPUT LONGITUDES
C     JM       - INTEGER NUMBER OF INPUT LATITUDES
C     KM       - INTEGER NUMBER OF FIELDS TO INTERPOLATE
C     NPS      - INTEGER ODD ORDER OF THE POLAR STEREOGRAPHIC GRIDS
C                (CENTER POINTS ARE THE POLE POINTS)
C     KB       - INTEGER SKIP NUMBER BETWEEN INPUT FIELDS
C     TRUE     - REAL LATITUDE AT WHICH PS GRID IS TRUE (USUALLY 60.)
C     XMESH    - REAL GRID LENGTH AT TRUE LATITUDE (M)
C     ORIENT   - REAL LONGITUDE AT BOTTOM OF NORTHERN PS GRID
C                (SOUTHERN PS GRID WILL HAVE OPPOSITE ORIENTATION.)
C     LB       - LOGICAL (IM,JM,KM) BITMAP IF IBM=1
C     B        - REAL (IM,JM,KM) SCALAR FIELD TO INTERPOLATE
C
C   OUTPUT ARGUMENT LIST:
C     LBN      - LOGICAL (NPS,NPS,KM) NORTHERN PS BITMAP IF IBM=1
C     BN       - REAL (IM,JM,KM) INTERPOLATED NORTHERN PS FIELD
C     LBN      - LOGICAL (NPS,NPS,KM) SOUTHERN PS BITMAP IF IBM=1
C     BN       - REAL (IM,JM,KM) INTERPOLATED SOUTHERN PS FIELD
C
C REMARKS: FORTRAN 90 EXTENSIONS ARE USED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      LOGICAL LB(KB,KM),LBN(NPS,NPS,KM),LBS(NPS,NPS,KM)
      REAL B(KB,KM),BN(NPS,NPS,KM),BS(NPS,NPS,KM)
      PARAMETER(NXH=5,NX=2*NXH+1,NXQ=NX*NX)
      PARAMETER(RERTH=6.3712E6)
      PARAMETER(PI=3.14159265358979,DPR=180./PI)
      REAL WN(NPS,NPS,KM),WS(NPS,NPS,KM)
      INTEGER IN1(NXQ),IN2(NXQ),JN1(NXQ),JN2(NXQ)
      REAL WIN1(NXQ),WIN2(NXQ),WJN1(NXQ),WJN2(NXQ)
      INTEGER IS1(NXQ),IS2(NXQ),JS1(NXQ),JS2(NXQ)
      REAL WIS1(NXQ),WIS2(NXQ),WJS1(NXQ),WJS2(NXQ)
      INTEGER IJN11(NXQ),IJN21(NXQ),IJN12(NXQ),IJN22(NXQ)
      INTEGER IJS11(NXQ),IJS21(NXQ),IJS12(NXQ),IJS22(NXQ)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL ERRMSG('BLL2PS will no longer be supported.')
      CALL ERRMSG('Please call IPOLATES(3,...) rather than BLL2PS.')
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      XPX=(NPS-1)/2*NX+NXH+1
      XMESHX=XMESH/NX
      G2=((1.+SIN(TRUE/DPR))*RERTH/XMESHX)**2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(KM.EQ.1) THEN
CDIR$ IVDEP
        DO IJ=1,NPS*NPS
          J=(IJ-1)/NPS+1
          I=IJ-(J-1)*NPS
          BN(I,J,1)=0.
          WN(I,J,1)=0.
          BS(I,J,1)=0.
          WS(I,J,1)=0.
        ENDDO
      ELSE
        DO IJ=1,NPS*NPS
          J=(IJ-1)/NPS+1
          I=IJ-(J-1)*NPS
          DO K=1,KM
            BN(I,J,K)=0.
            WN(I,J,K)=0.
            BS(I,J,K)=0.
            WS(I,J,K)=0.
          ENDDO
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CMIC$ DO ALL PRIVATE(K,IJ,J,I,N,JXI,IXI,JX,IX,DJX,DIX,R2)
CMIC$&       PRIVATE(RLATN,RLONN,RLATS,RLONS)
CMIC$&       PRIVATE(YN,XN,IN1,IN2,WIN2,WIN1,JN1,JN2,WJN2,WJN1)
CMIC$&       PRIVATE(YS,XS,IS1,IS2,WIS2,WIS1,JS1,JS2,WJS2,WJS1)
CMIC$&       PRIVATE(IJN11,IJN21,IJN12,IJN22,IJS11,IJS21,IJS12,IJS22)
CMIC$&       SHARED(KM,NPS,NXQ,NX,XPX,DPR,G2,ORIENT)
CMIC$&       SHARED(IM,JM,IBM,B,LB,BN,BS,WN,WS)
      DO IJ=1,NPS*NPS
        J=(IJ-1)/NPS+1
        I=IJ-(J-1)*NPS
CDIR$ IVDEP
        DO N=1,NXQ
          JXI=(N-1)/NX+1
          IXI=N-(JXI-1)*NX
          JX=(J-1)*NX+JXI
          IX=(I-1)*NX+IXI
          DJX=JX-XPX
          DIX=IX-XPX
          R2=DJX**2+DIX**2
          IF(R2.GT.0.) THEN
            RLATN=DPR*ASIN((G2-R2)/(G2+R2))
            RLONN=MOD(720+ORIENT+(DPR*ATAN2(DIX,-DJX)),360.)
            RLATS=-DPR*ASIN((G2-R2)/(G2+R2))
            RLONS=MOD(720+180+ORIENT-(DPR*ATAN2(DIX,-DJX)),360.)
          ELSE
            RLATN=90
            RLONN=0
            RLATS=-90
            RLONS=0
          ENDIF
          YN=(90-RLATN)/180*(JM-1)+1
          XN=RLONN/360*IM+1
          IN1(N)=XN
          IN2(N)=MOD(IN1(N),IM)+1
          WIN2(N)=XN-IN1(N)
          WIN1(N)=1-WIN2(N)
          JN1(N)=YN
          JN2(N)=MIN(JN1(N)+1,JM)
          WJN2(N)=YN-JN1(N)
          WJN1(N)=1-WJN2(N)
          YS=(90-RLATS)/180*(JM-1)+1
          XS=RLONS/360*IM+1
          IS1(N)=XS
          IS2(N)=MOD(IS1(N),IM)+1
          WIS2(N)=XS-IS1(N)
          WIS1(N)=1-WIS2(N)
          JS1(N)=YS
          JS2(N)=MIN(JS1(N)+1,JM)
          WJS2(N)=YS-JS1(N)
          WJS1(N)=1-WJS2(N)
          IJN11(N)=IN1(N)+(JN1(N)-1)*IM
          IJN21(N)=IN2(N)+(JN1(N)-1)*IM
          IJN12(N)=IN1(N)+(JN2(N)-1)*IM
          IJN22(N)=IN2(N)+(JN2(N)-1)*IM
          IJS11(N)=IS1(N)+(JS1(N)-1)*IM
          IJS21(N)=IS2(N)+(JS1(N)-1)*IM
          IJS12(N)=IS1(N)+(JS2(N)-1)*IM
          IJS22(N)=IS2(N)+(JS2(N)-1)*IM
        ENDDO
        IF(KM.EQ.1) THEN
CDIR$ IVDEP
          DO N=1,NXQ
            IF(IBM.EQ.0) THEN
              BN(I,J,1)=BN(I,J,1)+WJN1(N)*WIN1(N)*B(IJN11(N),1)
     &                           +WJN1(N)*WIN2(N)*B(IJN21(N),1)
     &                           +WJN2(N)*WIN1(N)*B(IJN12(N),1)
     &                           +WJN2(N)*WIN2(N)*B(IJN22(N),1)
              BS(I,J,1)=BS(I,J,1)+WJS1(N)*WIS1(N)*B(IJS11(N),1)
     &                           +WJS1(N)*WIS2(N)*B(IJS21(N),1)
     &                           +WJS2(N)*WIS1(N)*B(IJS12(N),1)
     &                           +WJS2(N)*WIS2(N)*B(IJS22(N),1)
            ELSE
              IF(LB(IJN11(N),1)) THEN
                BN(I,J,1)=BN(I,J,1)+WIN1(N)*WJN1(N)*B(IJN11(N),1)
                WN(I,J,1)=WN(I,J,1)+WIN1(N)*WJN1(N)
              ENDIF
              IF(LB(IJN21(N),1)) THEN
                BN(I,J,1)=BN(I,J,1)+WIN2(N)*WJN1(N)*B(IJN21(N),1)
                WN(I,J,1)=WN(I,J,1)+WIN2(N)*WJN1(N)
              ENDIF
              IF(LB(IJN12(N),1)) THEN
                BN(I,J,1)=BN(I,J,1)+WIN1(N)*WJN2(N)*B(IJN12(N),1)
                WN(I,J,1)=WN(I,J,1)+WIN1(N)*WJN2(N)
              ENDIF
              IF(LB(IJN22(N),1)) THEN
                BN(I,J,1)=BN(I,J,1)+WIN2(N)*WJN2(N)*B(IJN22(N),1)
                WN(I,J,1)=WN(I,J,1)+WIN2(N)*WJN2(N)
              ENDIF
              IF(LB(IJS11(N),1)) THEN
                BS(I,J,1)=BS(I,J,1)+WIS1(N)*WJS1(N)*B(IJS11(N),1)
                WS(I,J,1)=WS(I,J,1)+WIS1(N)*WJS1(N)
              ENDIF
              IF(LB(IJS21(N),1)) THEN
                BS(I,J,1)=BS(I,J,1)+WIS2(N)*WJS1(N)*B(IJS21(N),1)
                WS(I,J,1)=WS(I,J,1)+WIS2(N)*WJS1(N)
              ENDIF
              IF(LB(IJS12(N),1)) THEN
                BS(I,J,1)=BS(I,J,1)+WIS1(N)*WJS2(N)*B(IJS12(N),1)
                WS(I,J,1)=WS(I,J,1)+WIS1(N)*WJS2(N)
              ENDIF
              IF(LB(IJS22(N),1)) THEN
                BS(I,J,1)=BS(I,J,1)+WIS2(N)*WJS2(N)*B(IJS22(N),1)
                WS(I,J,1)=WS(I,J,1)+WIS2(N)*WJS2(N)
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO N=1,NXQ
            IF(IBM.EQ.0) THEN
              DO K=1,KM
                BN(I,J,K)=BN(I,J,K)+WJN1(N)*WIN1(N)*B(IJN11(N),K)
     &                             +WJN1(N)*WIN2(N)*B(IJN21(N),K)
     &                             +WJN2(N)*WIN1(N)*B(IJN12(N),K)
     &                             +WJN2(N)*WIN2(N)*B(IJN22(N),K)
                BS(I,J,K)=BS(I,J,K)+WJS1(N)*WIS1(N)*B(IJS11(N),K)
     &                             +WJS1(N)*WIS2(N)*B(IJS21(N),K)
     &                             +WJS2(N)*WIS1(N)*B(IJS12(N),K)
     &                             +WJS2(N)*WIS2(N)*B(IJS22(N),K)
              ENDDO
            ELSE
              DO K=1,KM
                IF(LB(IJN11(N),K)) THEN
                  BN(I,J,K)=BN(I,J,K)+WIN1(N)*WJN1(N)*B(IJN11(N),K)
                  WN(I,J,K)=WN(I,J,K)+WIN1(N)*WJN1(N)
                ENDIF
                IF(LB(IJN21(N),K)) THEN
                  BN(I,J,K)=BN(I,J,K)+WIN2(N)*WJN1(N)*B(IJN21(N),K)
                  WN(I,J,K)=WN(I,J,K)+WIN2(N)*WJN1(N)
                ENDIF
                IF(LB(IJN12(N),K)) THEN
                  BN(I,J,K)=BN(I,J,K)+WIN1(N)*WJN2(N)*B(IJN12(N),K)
                  WN(I,J,K)=WN(I,J,K)+WIN1(N)*WJN2(N)
                ENDIF
                IF(LB(IJN22(N),K)) THEN
                  BN(I,J,K)=BN(I,J,K)+WIN2(N)*WJN2(N)*B(IJN22(N),K)
                  WN(I,J,K)=WN(I,J,K)+WIN2(N)*WJN2(N)
                ENDIF
                IF(LB(IJS11(N),K)) THEN
                  BS(I,J,K)=BS(I,J,K)+WIS1(N)*WJS1(N)*B(IJS11(N),K)
                  WS(I,J,K)=WS(I,J,K)+WIS1(N)*WJS1(N)
                ENDIF
                IF(LB(IJS21(N),K)) THEN
                  BS(I,J,K)=BS(I,J,K)+WIS2(N)*WJS1(N)*B(IJS21(N),K)
                  WS(I,J,K)=WS(I,J,K)+WIS2(N)*WJS1(N)
                ENDIF
                IF(LB(IJS12(N),K)) THEN
                  BS(I,J,K)=BS(I,J,K)+WIS1(N)*WJS2(N)*B(IJS12(N),K)
                  WS(I,J,K)=WS(I,J,K)+WIS1(N)*WJS2(N)
                ENDIF
                IF(LB(IJS22(N),K)) THEN
                  BS(I,J,K)=BS(I,J,K)+WIS2(N)*WJS2(N)*B(IJS22(N),K)
                  WS(I,J,K)=WS(I,J,K)+WIS2(N)*WJS2(N)
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CDIR$ IVDEP
      IF(KM.EQ.1) THEN
CDIR$ IVDEP
        DO IJ=1,NPS*NPS
          J=(IJ-1)/NPS+1
          I=IJ-(J-1)*NPS
          IF(IBM.EQ.0) THEN
            BN(I,J,1)=BN(I,J,1)/NXQ
            BS(I,J,1)=BS(I,J,1)/NXQ
          ELSE
            LBN(I,J,1)=WN(I,J,1).GE.0.5*NXQ
            IF(LBN(I,J,1)) THEN
              BN(I,J,1)=BN(I,J,1)/WN(I,J,1)
            ELSE
              BN(I,J,1)=0.
            ENDIF
            LBS(I,J,1)=WS(I,J,1).GE.0.5*NXQ
            IF(LBS(I,J,1)) THEN
              BS(I,J,1)=BS(I,J,1)/WS(I,J,1)
            ELSE
              BS(I,J,1)=0.
            ENDIF
          ENDIF
        ENDDO
      ELSE
        DO IJ=1,NPS*NPS
          J=(IJ-1)/NPS+1
          I=IJ-(J-1)*NPS
          IF(IBM.EQ.0) THEN
            DO K=1,KM
              BN(I,J,K)=BN(I,J,K)/NXQ
              BS(I,J,K)=BS(I,J,K)/NXQ
            ENDDO
          ELSE
            DO K=1,KM
              LBN(I,J,K)=WN(I,J,K).GE.0.5*NXQ
              IF(LBN(I,J,K)) THEN
                BN(I,J,K)=BN(I,J,K)/WN(I,J,K)
              ELSE
                BN(I,J,K)=0.
              ENDIF
              LBS(I,J,K)=WS(I,J,K).GE.0.5*NXQ
              IF(LBS(I,J,K)) THEN
                BS(I,J,K)=BS(I,J,K)/WS(I,J,K)
              ELSE
                BS(I,J,K)=0.
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
