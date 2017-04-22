C MEMBER SWB54
C
      SUBROUTINE SWB54(RM,ETDM,ET,RS,RG,R,TA,PP,TWE,FEI,SNO,IFRZE,LSN,
     +                 ITP,IDAY,IHOUR,IPRINT,IOUT,IBUG)
C
C     THIS SUBROUTINE CALL THE NILE54 SUBROUTINE
      REAL KG,KDT,KIMP
      COMMON/PM54/DMAX,KG,ALPSM,ALPRT,KDT
      COMMON/FZPM54/KIMP,DSOIL,POROS,WWP,CVICE
      COMMON/CO54/SU,SB,FDP(2),TDP(2),SDP,SDN,WICE(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_swbnile/RCS/swb54.f,v $
     . $',                                                             '
     .$Id: swb54.f,v 1.2 1998/04/07 12:15:57 page Exp $
     . $' /
C    ===================================================================
C
C
C     ... CONVERT TIME INTERVAL FROM HOURS TO DAYS ...
C
      DT=ITP/24.0
C
C     ... CONVERT RAIN+SNOWMELT AND POT-ET FROM MM/DT TO MM/DAY ...
C
      P=RM/DT
      EP=ETDM/DT
C
C     ...GET THE STATE VARIABLES...
C
      DMRT=DMAX*ALPRT
      DU=DMRT-SU
      DB=DMAX-SB
C
      IF (IFRZE.NE.1) GO TO 100
      UDZ=FDP(1)
      BDZ=FDP(2)
      DZ=UDZ+BDZ
      UTHAW=TDP(1)
      BTHAW=TDP(2)
      SNDP=SDP
      SNDN=SDN
      UWICE=WICE(1)
      BWICE=WICE(2)
C
C     ...RUN SNOWPACK SUBROUTINE IF IFRZE IS EQUAL TO 1...
C
      DTH = DT*24.
      SMAVG=(SU+SB)/(DMRT+DMAX)
      CALL SNWPCK54(RM,TA,PP,TWE,SNO,SNDP,SNDN,DZ,SMAVG,DSOIL,DTH,LSN)
      SDP=SNDP
      SDN=SNDN
C
C     ...CALL MODEL NILE54 TO COMPUTE RUNOFF...
C
  100 CALL NILE54 (DT,P,EP,TA,
     &              R,RS,RG,ET,EU,EB,FEI,DU,DB,
     &              DMAX,KG,ALPSM,ALPRT,KDT,
     &              KIMP,DSOIL,POROS,WWP,CVICE,
     &              UDZ,BDZ,UTHAW,BTHAW,
     &              UWICE,BWICE,SNDP,SNDN,IFRZE,
     &              IDAY,IHOUR,IOUT,IBUG)
C
C     RECORD OUTPUTS
C
      SU = DMRT-DU
      SB = DMAX-DB
      IF (IFRZE.EQ.0) GO TO 300
      FDP(1) = UDZ
      FDP(2) = BDZ
      TDP(1) = UTHAW
      TDP(2) = BTHAW
      WICE(1) = UWICE
      WICE(2) = BWICE
C.......................................
C     PRINT DETAILED ACCOUNTING VALUES IF REQUESTED.
C
  200 IF (IPRINT.EQ.1) WRITE (IOUT,905) IDAY,IHOUR,RM,ETDM,ET,EU,EB,
     &                 R,RS,RG,DU,DB,FDP,TDP,WICE
  905 FORMAT (1H ,2I3,16F8.3)
      RETURN
C
  300 IF (IPRINT.EQ.1) WRITE (IOUT,905) IDAY,IHOUR,RM,ETDM,ET,EU,EB,
     &                 R,RS,RG,DU,DB
      RETURN
C
C     ...END OF SUBROUTINE SWB54...
C
      END
