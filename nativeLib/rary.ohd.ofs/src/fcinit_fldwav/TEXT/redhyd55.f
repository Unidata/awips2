      SUBROUTINE REDHYD55(PO,IUSEP,LEFTP,Z,IUSEZ,LEFTZ,KU,KD,NGAGE,
     . NGS,GZ,STTNAM,NQL,LQ1,NB,GZ1,STM,GZN,SLFI,NUMLAD,XFACT,NODESC,
     . IERR,K1,K2,K3,K4,K10,K23)

      CHARACTER*4 STNAME(5)
      CHARACTER*80 DESC

      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M655/KTIME,DTHYD,J1
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/MXVAL55/MXNB,MXNGAG,MXNCM1,MXNCML,MXNQL,MXINBD,MXRCH,
     .               MXMGAT,MXNXLV,MXROUT,MXNBT,MXNSTR,MXSLC
      COMMON/XNGZ55/NGZ,NGZN
      COMMON/NPC55/NP,NPST,NPEND
      COMMON/NYQDC55/NYQD
      COMMON/TKEP55/DTHII,MDT,NDT,DTHS,TFH1
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
        COMMON/NETWK55/NET

      INCLUDE 'common/ofs55'

      DIMENSION PO(1),Z(1),NB(K1),KU(K1),KD(K1),NGAGE(K1),NQL(K1)
      DIMENSION NGS(K4,K1),GZ(K4,K1),STTNAM(5,K4,K1),LQ1(K10,K1)
      DIMENSION SLFI(K2,K1),STM(K1),GZ1(K1),GZN(K1),NUMLAD(K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/redhyd55.f,v $
     . $',                                                             '
     .$Id: redhyd55.f,v 1.4 2004/02/02 20:41:36 jgofus Exp $
     . $' /
C    ===================================================================
C

      IF(NODESC.EQ.0)THEN
      IF(IBUG.EQ.1) WRITE(IODBUG,96)
   96 FORMAT(//
     .10X,'LQ1   =  SECTION NO.IMMEDIATELY UPSTREAM OF LATERAL FLOW'/
     .10X,'QL    =  LATERAL INFLOW AT CROSS SECTION'/
     .10X,'NGS   =  SEQUENCE NO. OF GAGING OR PLOTTING STATION'/
     .10X,'GZ    =  GAGE CORRECT TO CONVERT OBS STAGES TO MEAN SEA LEV'/
     .10X,'STTNAM=  NAME OF GAGING OR PLOTTING STATION'/
     .10X,'STT   =  GAGING STATION NAME'/
     .10X,'STQ   =  OBS. DISCHARGE HYDROGRAPH AT GAGING STATION'/
     .10X,'J     =  RIVER NO.'/
     .10X,'TPG   =  TIME FROM INITIAL STEADY FLOW TO PEAK IN MATH.HYDR'/
     .10X,'RHO   =  RATIO OF PEAK TO INITIAL FLOW IN MATH.HYDROGRAPH'/
     .10X,'GAMA  =  RATIO OF TG TO TPG'/
     .10X,'YQI   =  INITIAL DISCH. OR WATER ELEV. IN MATH. HYDROGRAPH'/
     .10X,'ST1   =  OBS. (KNOWN) WATER ELEV. OR DISCH. OF 1ST SECT.'/
     .10X,'T1    =  TIME ORDINATE FOR HYDRO AT U/S AND D/S BOUNDARY'/
     .10X,'GZ1   =  GAGE ZERO CORRECTION FOR NON-JUNCTION U/S BOUNDARY'/
     .10X,'STN   =  OBSERVED HYDROGRAPH AT DOWNSTREAM BOUNDARY'/
     .10X,'GZN   =  GAGE ZERO CORRECTION FOR NON-JUNCTION D/S BOUNDARY'/
     .10X,'YQD   =  W/S ELEV. ASS. WITH DISCH. IN D/S BDY RATING'/
     .10X,'QYQD  =  DISCHARGE ASS. WITH STAGE IN DOWNSTREAM RATING'/)
      ENDIF

      IF(IBUG.EQ.1) WRITE(IODBUG,*)

C.......................................................................
C 132    LQ1    --  NO. OF SECTION IMMEDIATELY UPSTREAM OF LATERAL FLOW
C.......................................................................
      IERR=0
      NTQL=PO(313)
      NTGAG=PO(323)
      NLOCK=PO(321)

      IF(NTQL.GT.0) THEN
        LOQL=IUSEZ+1
        IUSEZ=LOQL+NTQL*K3-1
      ENDIF
      IF(NTGAG.GT.0.AND.IOBS.NE.0) THEN
        LOSTT=IUSEZ+1
        IUSEZ=IUSEZ+NTGAG*K3-1
        IF(KPL.EQ.3) THEN
          LOSTQ=IUSEZ+1
          IUSEZ=LOSTQ+NTGAG*K3-1
        ENDIF
      ENDIF
      LOST1=IUSEZ+1
      LOT1=LOST1+JN*K3
      IUSEZ=LOT1+JN*K3-1
      IF(KD(1).LE.2.OR.NP.LE.-1) THEN
        LOSTN=IUSEZ+1
        IUSEZ=LOSTN+JN*K3-1
      ENDIF
      IF(NYQD.GT.0) THEN
        LOYQD=IUSEZ+1
        LOQYQD=LOYQD+NYQD*JN
        IUSEZ=LOQYQD+NYQD*JN-1
      ENDIF
      IF(NLOCK.GT.0) THEN
        LOPLT=IUSEZ+1
        LOIWT=LOPLT+NLOCK*K3
        IUSEZ=LOIWT+NLOCK*K3-1
      ENDIF

      CALL CHECK55(IUSEZ,LEFTZ,NERR)
      IF(NERR.NE.0) THEN
        IUSEZ=0
        GO TO 5000
      ENDIF

      Z(1)=LOQL+1.01
      Z(2)=LOSTT+1.01
      Z(3)=LOSTQ+1.01
      Z(4)=LOST1+1.01
      Z(5)=LOSTN+1.01
      Z(6)=LOPLT+1.01
      Z(7)=LOIWT+1.01
      Z(8)=LOT1+1.01
      Z(9)=LOYQD+1.01
      Z(10)=LOQYQD+1.01

      IF(NTQL.EQ.0) GO TO 200
      IMSG=0
      KUSE=LOQL
      DO 190 J=1,JN
      IF(NQL(J)) 190,190,110
  110 NQLJ=NQL(J)
      DO 180 L=1,NQLJ
      READ(IN,'(A)',END=1000) DESC
      READ(IN,*) LQ1(L,J)
      IF(IBUG.EQ.1) WRITE(IODBUG,120) L,J,LQ1(L,J)
  120 FORMAT (/5H LQ1(,I2,1H,,I2,1H),I5)
      IF(LQ1(L,J).LE.0) THEN
        IMSG=IMSG+1
        GO TO 180
      ENDIF
C.......................................................................
C     QL    --  LATERAL INFLOW AT CROSS SECTION
C.......................................................................
  140 READ(IN,'(A)',END=1000) DESC
      READ(IN,*) (Z(KUSE+K),K=1,NU)
  150 IF(IBUG.EQ.1) WRITE(IODBUG,160) L,J
  160 FORMAT(/6H QL(K,,I1,1H,,I1,12H), K = 1, NU)
      IF(IBUG.EQ.1) WRITE(IODBUG,2041) (Z(KUSE+K),K=1,NU)
 2041 FORMAT (8F10.0)
      KUSE=KUSE+K3
  180 CONTINUE
  190 CONTINUE

      IF(IMSG.GT.0) THEN
        IF(IBUG.EQ.1) WRITE(IODBUG,104)
  104   FORMAT(/' **ERROR** ROUTED LATERAL FLOW OPTION IS NOT AVAILABLE'
     .,         ' IN THE CURRENT MODEL ... PROGRAM TERMINATED.'/)
        CALL ERROR
      ENDIF

  200 IF(NTGAG.EQ.0) GO TO 360
C.......................................................................
C     NGAG   --  NO. OF GAGES ON EACH RIVER
C     KPL    --  PARAMETER FOR TYPE OF HYDROGRAPH TO BE PLOTTED
C     IOBS   --  PARAMETER INDICATING IF OBSERVED DATA ARE AVAILABLE
C.......................................................................
C     NGS    --  SEQUENCE NO. OF GAGING OR PLOTTING STATION
C     STTNAM --  NAME OF GAGING OR PLOTTING STATION
C     STT    --  OBS. WATER ELEV. OR DISCH. T/S AT GAGING STATION
C     GZ     --  GAGE CORRECTION TO CONVERT OBS STAGES TO MEAN SEA
C                LEVEL DATUM
C     STQ    --  OBS. DISCHARGE HYDROGRAPH AT GAGING STATION
C.......................................................................
      IF(IOBS.GT.0) THEN
        LOSTTN=IUSEP+1
        IUSEP=LOSTTN+5*NTGAG-1

        CALL CHECKP(IUSEP,LEFTP,NERR)
        IF(NERR.NE.0) THEN
          IUSEP=0
          GO TO 5000
        ENDIF
      ENDIF

      PO(352)=LOSTTN+1.01

      LSTT=LOSTT
      LSTQ=LOSTQ
      DO 350 J=1,JN
      NGAG=NGAGE(J)
      IF(NGAG.LE.0) GO TO 350
      IF(IBUG.EQ.1) WRITE(IODBUG,204) J
  204 FORMAT(/,32H PLOTTING T.S. INFO FOR RIVER J=,1X,I2/10X,
     . 11HSTATION (I),6X,2HID,10X,4HTYPE,4X,8HNGS(I,J),4X,7HGZ(I,J))

      DO 340 I=1,NGAG
      READ(IN,'(A)',END=1000) DESC
      IF(KPL.EQ.2) THEN
        READ(IN,321) STNAME,NGS(I,J)
  321   FORMAT(5A4,I10,F10.2)
        IF(IBUG.EQ.1) WRITE(IODBUG,330) I,STNAME,NGS(I,J)
  330   FORMAT (/10X,I10,5A4,I10,F10.2)
      ELSE
        READ(IN,321) STNAME,NGS(I,J),GZ(I,J)
        IF(IBUG.EQ.1) WRITE(IODBUG,330) I,STNAME,NGS(I,J),GZ(I,J)
      ENDIF
      IF(IOBS.LE.0) GO TO 340
      READ(IN,'(A)',END=1000) DESC
      READ(IN,*) (Z(LSTT+K),K=1,NU)
      IF(IBUG.EQ.1) WRITE(IODBUG,2070) (Z(LSTT+K),K=1,NU)
      LSTT=LSTT+K3
      IF(NP.EQ.0.AND.KPL.EQ.3) THEN
        READ(IN,'(A)',END=1000) DESC
        READ(IN,*) (Z(LSTQ+K),K=1,NU)
        IF(IBUG.EQ.1) WRITE(IODBUG,2070) (Z(LSTQ+K),K=1,NU)
        WSTQ=WSTQ+K3
      ENDIF
  340 CONTINUE
  350 CONTINUE

C.......................................................................
C     OUTPUT TIME SERIES NOT NEEDED IN STAND-ALONE MODEL
C.......................................................................
C     TPG    --  TIME FROM INITIAL STEADY FLOW TO PEAK IN MATH.HYDR
C     RHO    --  RATIO OF PEAK TO INITIAL FLOW IN MATH.HYDROGRAPH
C     GAMA   --  RATIO OF TG TO TPG; TG IS TIME FROM INITIAL FLOW
C                CENTROID OF MATH. HYDROGRAPH
C     YQI    --  INITIAL DISCH. OR WATER ELEV. IN MATH. HYDROGRAPH
C.......................................................................
  360 IF(IOBS.GE.0) GO TO 390
      LOTPG=IUSE+1
      LORHO=LOTPG+JN
      LOGAMA=LORHO+JN
      LOYQI=LOGAMA+JN
      IUSE=LOYQI+JN-1

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

  365 PO(239)=LOTPG+0.01
      PO(240)=LORHO+0.01
      PO(241)=LOGAMA+0.01
      PO(242)=LOYQI+0.01
      IF(IBUG.EQ.1) WRITE(IODBUG,380)
  380 FORMAT (/5X,'J       TPG       RHO      GAMA       YQI')
      DO 370 J=1,JN
      READ(IN,'(A)',END=1000) DESC
      READ(IN,*) PO(LOTPG+J-1),PO(LORHO+J-1),PO(LOGAMA+J-1),
     1        PO(LOYQI+J-1)
      IF(IBUG.EQ.1) WRITE(IODBUG,2043) J,PO(LOTPG+J-1),PO(LORHO+J-1),
     1        PO(LOGAMA+J-1),PO(LOYQI+J-1)    
  370 CONTINUE
      GO TO 450

C.......................................................................
C         ST1   --  OBS. (KNOWN) WATER ELEV. OR DISCH. OF 1ST SECT.
C         T1    --  TIME ORDINATE GOR HYDROGRAPH AT U/S AND D/S BOUNDARY
C         GZ1   --  GAGE ZERO CORRECTION FOR NON-JUNCTION U/S BOUNDARY
C.......................................................................

      LST1=LOST1-1
      LT1=LOT1-1
C U/S
  390 DO 440 J=1,JN-NET
        READ(IN,'(A)',END=1000) DESC
        IF(KU(J).EQ.1) THEN
          READ(IN,*) STM(J),GZ1(J)
          IF(IBUG.EQ.1) WRITE(IODBUG,392) J,J
  392     FORMAT (/5X,'STM(',I1,')',5X,'GZ1(',I1,')')
          IF(IBUG.EQ.1) WRITE(IODBUG,2070) STM(J),GZ1(J)
        ELSEIF(KU(J).EQ.2) THEN
          READ(IN,*) STM(J)
          IF(IBUG.EQ.1) WRITE(IODBUG,394) J
  394     FORMAT (/5X,'STM(',I1,')')
          IF(IBUG.EQ.1) WRITE(IODBUG,2070) STM(J)
        ENDIF

        READ(IN,'(A)',END=1000) DESC
        READ(IN,*) (Z(LST1+K),K=1,NU)
        IF(IBUG.EQ.1) WRITE(IODBUG,400) J
  400   FORMAT (/7H ST1(K,,I1,12H), K = 1, NU)
        IF(IBUG.EQ.1) WRITE(IODBUG,2060) (Z(LST1+K),K=1,NU)

        IF(DTHYD.GT.0.) THEN
          DO 402 K=1,NU
            Z(LT1+K)=(K-1)*DTHYD
  402     CONTINUE
        ELSE
          IF(DTHYD.LE.0.) THEN
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*) (Z(LT1+K),K=1,NU)
            IF(IBUG.EQ.1) WRITE(IODBUG,405) J
  405       FORMAT (/6H T1(K,,I1,12H), K = 1, NU)
            IF(IBUG.EQ.1) WRITE(IODBUG,2060) (Z(LT1+K),K=1,NU)
          ENDIF
        ENDIF
        LST1=LST1+NU
        LT1=LT1+NU
  440 CONTINUE

C.......................................................................
C         STN   --  OBSERVED HYDROGRAPH AT DOWNSTREAM BOUNDARY
C         GZN   --  GAGE ZERO CORRECTION FOR NON-JUNCTION D/S BOUNDARY
C.......................................................................
C         YQD   --  W/S ELEV. ASSOCIATED WITH DISCH. IN D/S BDY RATING
C         QYQD  --  DISCHARGE ASSOCIATED WITH STAGE IN DOWNSTREAM RATING
C.......................................................................
C         SLFI    --  BED/INITIAL WATER SURFACE SLOPE OF CHANNEL
C.......................................................................
  450 LSTN=LOSTN
      LYQD=LOYQD
      LQYQD=LOQYQD
C D/S
      DO 550 J=1,JN-NET
        IF(KD(J).EQ.1) THEN
          READ(IN,'(A)',END=1000) DESC
          READ(IN,*) GZN(J)
          IF(IBUG.EQ.1) WRITE(IODBUG,490) J
  490     FORMAT (/5H GZN(,I1,1H))
          IF(IBUG.EQ.1) WRITE(IODBUG,2070) GZN(J)
          READ(IN,'(A)',END=1000) DESC
          READ(IN,*) (Z(LSTN+K),K=1,NU)
          IF(IBUG.EQ.1) WRITE(IODBUG,470) J
  470     FORMAT (/' STN(K,',I2,'), K = 1, NU')
          IF(IBUG.EQ.1) WRITE(IODBUG,2070) (Z(LSTN+K),K=1,NU)
        ELSEIF(KD(J).EQ.2) THEN
          READ(IN,'(A)',END=1000) DESC
          READ(IN,*) (Z(LSTN+K),K=1,NU)
          IF(IBUG.EQ.1) WRITE(IODBUG,472) J
  472     FORMAT (/' STN(K,',I2,'), K = 1, NU')
          IF(IBUG.EQ.1) WRITE(IODBUG,2070) (Z(LSTN+K),K=1,NU)
        ELSEIF(KD(J).EQ.3) THEN
          READ(IN,'(A)',END=1000) DESC
          READ(IN,*) GZN(J)
          IF(IBUG.EQ.1) WRITE(IODBUG,482) J
  482     FORMAT (/5H GZN(,I1,1H))
          IF(IBUG.EQ.1) WRITE(IODBUG,2070) GZN(J)

          IF(NYQD.LE.0) THEN
            WRITE(IPR,480)
            STOP
          ELSE
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*) (Z(LYQD+K),K=1,NYQD)
            IF(IBUG.EQ.1) WRITE(IODBUG,452) J
  452       FORMAT (/' YQD(K,',I2,'), K = 1, NYQD')
            IF(IBUG.EQ.1) WRITE(IODBUG,2070) (Z(LYQD+K),K=1,NYQD)
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*) (Z(LQYQD+K), K = 1, NYQD)
            IF(IBUG.EQ.1) WRITE(IODBUG,460) J
  460       FORMAT (/ ' QYQD(K,',I2,'), K = 1, NYQD')
            IF(IBUG.EQ.1) WRITE(IODBUG,2040) (Z(LQYQD+K), K = 1, NYQD)
          ENDIF
        ELSEIF(KD(J).EQ.5) THEN
          READ(IN,'(A)',END=1000) DESC
          N=NB(J)
          READ(IN,*) SLFI(N,J)
          IF(NODESC.EQ.0) THEN
            IF(IBUG.EQ.1) WRITE(IODBUG,530) SLFI(N,J)
  530       FORMAT(/5X,'BED/INITIAL WATER SURFACE SLOPE AT D/S BOUNDARY'
     .      ,4X,'SLFI',4X,F10.5)
          ELSE
            IF(IBUG.EQ.1) WRITE(IODBUG,540) SLFI(N,J)
  540       FORMAT(/'      SLFI'/F10.5)
          ENDIF
        ENDIF
        LSTN=LSTN+NU
        LYQD=LYQD+NYQD
        LQYQD=LQYQD+NYQD
  550 CONTINUE

C.......................................................................
C   READ IN LOCK AND DAM TIME SERIES INFO
C.......................................................................
      IF(NLOCK.EQ.0) GO TO 9000
      LPLT=LOPLT
      LIWT=LOIWT
      DO 600 J=1,JN
        IF(NUMLAD(J).EQ.0) GO TO 600
        NUML=NUMLAD(J)
        IF(IBUG.EQ.1) WRITE(IODBUG,560) J
  560   FORMAT(/10X,'TIME SERIES FOR POOL ELEVATIONS ON RIVER NO.',I3)
        DO 570 I=1,NUML
          READ(IN,'(A)',END=1000) DESC
          READ(IN,*) (Z(LPLT+K),K=1,NU)
          IF(IBUG.EQ.1) WRITE(IODBUG,565) J
  565     FORMAT (/' POOLT(K,',I2,'), K = 1, NU')
          IF(IBUG.EQ.1) WRITE(IODBUG,2070) (Z(LPLT+K),K=1,NU)
  570   CONTINUE
        NUML=NUMLAD(J)
        IF(IBUG.EQ.1) WRITE(IODBUG,580) J
  580   FORMAT(/10X,'TIME SERIES FOR GATE CONTROL SWITCHES ON RIVER NO.'
     .             ,I3)
        DO 590 I=1,NUML
          READ(IN,'(A)',END=1000) DESC
          READ(IN,*) (Z(LIWT+K),K=1,NU)
          IF(IBUG.EQ.1) WRITE(IODBUG,585) J
  585     FORMAT (/' ITWT(K,',I2,'), K = 1, NU')
          IF(IBUG.EQ.1) WRITE(IODBUG,2070) (Z(LIWT+K),K=1,NU)
  590   CONTINUE
        LPLT=LPLT+NU
        LIWT=LIWT+NU
  600 CONTINUE

 9010 FORMAT(//2X,'**ERROR** AMOUNT OF STORAGE EXCEEDED ... PROGRAM TERM
     *INATED.')

 2040 FORMAT(8F10.0)
 2043 FORMAT(7X,I3,8F10.2)
 2060 FORMAT( 8F11.2)
 2070 FORMAT(8F10.2)

      GO TO 9000
 5000 IERR=1      
      WRITE(IPR,8000)
      GO TO 9000
 1000 WRITE(IPR,1010)
  480 FORMAT(//'  ***ERROR*** YOU MUST SPECIFY THE NUMBER OF POINTS IN T
     .HE RATING CURVE.'/14X,'  PROGRAM TERMINATED IN SUBROUTINE REDHYD')
 1010 FORMAT(/5X,'**ERROR** END OF FILE ENCOUNTERED WHILE READING INPUT
     *FOR TIME SERIES INFO.'/)
 8000 FORMAT(//'  **ERROR**  WORK SPACE AVAILABLE FOR TIME SERIES DATA E
     *XCEEDED ... PROGRAM TERMINATED.')
 9000 RETURN
      END
