C MODULE FHQS1
C
      SUBROUTINE FHQS1(H,Q,ICONV,IBUG,NEEDEX,LOWEXT,IUPEXT,
     $      NW,NRANGE,MISING,CARRYO)
C
C................................
C      CONVERTS A SINGLE STAGE VALUE TO DISCHARGE(ICONV=1) OR
C      DISCHARGE VALUE TO STAGE(ICONV=2).
C      ARGUMENTS
C         H     STAGE (INPUT ICONV=1,OUTPUT ICONV=2)
C         Q     DISCHARGE (OUTPUT ICONV=1,INPUT ICONV=2)
C      ICONV    CONVERSION DIRECTION FLAG(INPUT)
C      IBUG     DEBUG FLAG(INPUT)
C      NEEDEX   EXTENSION INDICATOR (UPDATE)
C                 =0 NO EXTENSION
C                 =1 LOG-LOG
C                 =2 HYDRAULIC UPPER
C                 =3 LOG-LOG LOWER,HYDRAULIC UPPER
C                 =4 LINEAR
C                 =5 LINEAR LOWER,HYDRAULIC UPPER
C      LOWEXT  =1 EXTENSION LOWER USED, ELSE=0 (UPDATE)
C      IUPEXT  =1 EXTENSION UPPER USED, ELSE=0 (UPDATE)
C       NW     NUMBER OF TIMES H.LT.STGMIN (UPDATE)
C      NRANGE  =1 MANNINGS N OUTSIDE RANGE,ELSE=0 (UPDATE)
C      MISING  =1 H AND Q ARE MISSING,ELSE=0 (OUTPUT)
C      CARRYO  CARRYOVER FROM PREVIOUS PERIOD (INPUT)
C................................
C      WRITTEN BY ERIC ANDERSON,HRL  MAY 1988
C................................
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fratng'
      include 'common/rcnew'
       DIMENSION  CARRYO(*),AH(2),AQ(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rc/RCS/fhqs1.f,v $
     . $',                                                             '
     .$Id: fhqs1.f,v 1.3 2002/02/11 13:30:46 michaelo Exp $
     . $' /
C    ===================================================================
C
C................................
C      CHECK IF TRACE IS ON.
       IF (ITRACE.GE.3) WRITE(IODBUG,900)
900    FORMAT(1H0,17H*** FHQS1 ENTERED)
C................................
C      INITIAL VALUES
c AV debug
cc       ibug = 1
c AV debug
       IONE = 1
       ITWO = 2
       HESTIM=0.0
       MISING=0
       LOFF=EMPTY(2)
       NOFF=0

       if(nrcpmd .eq. 0) then 
         iflg = 0
       else 
         iflg = 1
       end if
       IF (LOFF.GT.0)NOFF=XRC(LOFF)
     
       IF (ICONV.EQ.2)  GO TO 200
C................................
C      CONVERT STAGE TO DISCHARGE
       IF(IFMSNG(H).EQ.0) GO TO 100
       Q=-999.
       MISING=1
       GO TO 995
C      CHECK IF H.LT. STGMIN
 100   IF(H.GE.STGMIN) GO TO 110
       H=STGMIN
       NW=NW+1
       
C      FIND THE TWO RATING POINTS TO BE USED.
 110   if (iflg .eq. 0)then   
          DO 120 I=2,NRCPTS
          IF(H.GE.XRC(LOCH-1+I)) GO TO 120
          I2=I
          I1=I2-1
          LL=I1
          GO TO 130
 120      CONTINUE
       else
          DO 125 I=2,nrcpmd      
          IF(H.GE.hmod(I)) GO TO 125
          I2=I
          I1=I2-1
          LL=I1
          GO TO 130
 125      CONTINUE
       end if
 
       if (iflg.eq.0) then
         I2=NRCPTS
       else
         I2=nrcpmd
       end if 
       I1=I2-1
       LL=I2
       
      
C      DETERMINE ADJUSTMENT(SHIFT OR OFFSET)
 130   ASHFT=0.0
       IF(H.GE.EMPTY(3)) GO TO 131
       ASHFT=SHIFT
       
       GO TO 140
 131   IF(NOFF.EQ.0) GO TO 140
       DO 135 I=1,NOFF

       IF (H.LT.XRC(LOFF+I)) GO TO 135

       
       ASHFT=-XRC(LOFF+I+NOFF)

 135   CONTINUE
C      CREATE TWO POINT RATING WITH ADJUSTED STAGES.
 140   HA=H+ASHFT

       
Cav if there is no data in hmod and qmod  
       if(iflg .eq. 0) then
          
          AH(1)=XRC(LOCH-1+I1)+ASHFT
          AH(2)=XRC(LOCH-1+I2)+ASHFT
          AQ(1)=XRC(LOCQ-1+I1)
          AQ(2)=XRC(LOCQ-1+I2)
       else
          AH(1)=hmod(I1)+ASHFT
          AH(2)=hmod(I2)+ASHFT
          AQ(1)=qmod(I1)
          AQ(2)=qmod(I2)
       end if
C      COMPUTE DISCHARGE
       if(iflg .ne. 0) then
          IF (H.NE.hmod(LL)) GO TO 150
          Q=qmod(LL)
          GO TO 990
       else
          IF (H.NE.XRC(LOCH-1+LL)) GO TO 150
C      DEFINED POINT ON CURVE
          Q=XRC(LOCQ-1+LL)
       
          GO TO 990
       end if
C      CHECK IF A ZERO DISCHARGE IS INVOLVED.
 150   IF (AQ(1).LT.0.0001) AQ(1)=0.0001
       if(iflg .eq. 0) then  
         IF (H.LE.XRC(LOCH-1+NRCPTS)) GO TO 160
       else 
         IF (H.LE.hmod(nrcpmd)) GO TO 160
       end if
C      UPPER END EXTRAPOLATION
       IUPEXT=1
       IF (NCROSS.EQ.0) GO TO 155
       CALL FHYDEX(H,Q,HESTIM,ICONV,NRANGE,IBUG)
       IF(NEEDEX.EQ.0) NEEDEX=2
       IF(NEEDEX.EQ.1) NEEDEX=3
       IF(NEEDEX.EQ.4) NEEDEX=5
       GO TO 990
 155   CALL FXTRPL(HA,AH,ITWO,AQ,Q,IBUG)
      IF(NEEDEX.EQ.0) THEN
       NEEDEX=1
       IF(EMPTY(4).GE.1.0) NEEDEX=4
      END IF
       GO TO 990
 160   if(iflg .eq. 0) then 
         IF(H.GE.XRC(LOCH)) GO TO 170
       else
         IF(H.GE.hmod(1)) GO TO 170
       end if
C      LOW END EXTRAPOLATION
       LOWEXT=1
       CALL FXTRPL(HA,AH,IONE,AQ,Q,IBUG)
      IF(NEEDEX.EQ.0) THEN
       NEEDEX=1
       IF(EMPTY(4) .GE. 1.0) NEEDEX=4
      END IF
      IF(NEEDEX.EQ.2) THEN
       NEEDEX=3
       IF(EMPTY(4) .GE. 1.0) NEEDEX=5
      END IF
      IF(NEEDEX.EQ.6) THEN
       NEEDEX=7
       IF(EMPTY(4) .GE. 1.0) NEEDEX=8
      END IF
       GO TO 990
C      NEED TO INTERPOLATE
 170   CALL FTERPL(HA,AH,ITWO,AQ,Q,IBUG)
       GO TO 990
C................................
C      CONVERT DISCHARGE TO STAGE
 200   IF(IFMSNG(Q).EQ.0) GO TO 210
       H=-999.
       MISING=1
       GO TO 995
C      FIND THE TWO RATING POINTS TO BE USED.
 210   if(iflg .eq. 0)then
         DO 220 I=2,NRCPTS
         IF(Q.GE.XRC(LOCQ-1+I)) GO TO 220
         
         I2=I
         I1=I2-1
         LL=I1
         GO TO 230
 220     CONTINUE
       else 
         DO 225 I=2,nrcpmd
         IF(Q.GE.qmod(I)) GO TO 225
       
         I2=I
         I1=I2-1
         LL=I1
         GO TO 230
 225   CONTINUE
       end if
      if(iflg.eq.0)then   
         I2=NRCPTS
       else 
         I2=nrcpmd
       end if
       I1=I2-1
       LL=I2
C      DETERMINE ADJUSTMENT (SHIFT OR OFFSET)
 230   ASHFT=0.0
       IF (LL.GT.1) GO TO 233
       if(iflg.eq.0)then
         IF (Q.GE.XRC(LOCQ))GO TO 233
       else
         IF (Q.GE.qmod(1))GO TO 233
       end if
       ASHFT=SHIFT
       GO TO 240
 233  if(iflg .eq. 0) then
         HLL=XRC(LOCH-1+LL)
      else
         HLL=hmod(LL)
      end if
       IF (HLL.GE.EMPTY(3)) GO TO 234
       ASHFT=SHIFT
       GO TO 240
 234   IF (NOFF.EQ.0) GO TO 240
       DO 235 I=1,NOFF
       IF (HLL.LT.XRC(LOFF+I)) GO TO 235
       ASHFT=-XRC(LOFF+I+NOFF)
 235   CONTINUE
C      CREATE TWO POINT RATING WITH ADJUSTED STAGES
 240   if(iflg.eq.0)then
          
          AH(1)=XRC(LOCH-1+I1)+ASHFT
          AH(2)=XRC(LOCH-1+I2)+ASHFT
          AQ(1)=XRC(LOCQ-1+I1)
          AQ(2)=XRC(LOCQ-1+I2)
       else
          AH(1)=hmod(I1)+ASHFT
          AH(2)=hmod(I2)+ASHFT
          AQ(1)=qmod(I1)
          AQ(2)=qmod(I2)
       end if
C      COMPUTE STAGE
       if(iflg.eq.0) then
        IF (Q.NE.XRC(LOCQ-1+LL)) GO TO 250
       else
         IF (Q.NE.qmod(LL)) GO TO 250
       end if
C      DEFINED POINT ON CURVE
       if(iflg.eq.0)then
          H=XRC(LOCH-1+LL)
       else
          H=hmod(LL)
       end if
       GO TO 990
C      CHECK IF A ZERO DISCHARGE IS INVOLVED.
 250   QA=Q
       IF(Q.LT.0.0001) QA=0.0001
       IF (AQ(1).LT.0.0001) AQ(1)=0.0001
       if(iflg.eq. 0)then
         IF (Q.LE.XRC(LOCQ-1+NRCPTS)) GO TO 260
       else
         IF (Q.LE.qmod(nrcpmd)) GO TO 260
       end if
C      UPPER END EXTRAPOLATION
       IUPEXT=1
       IF (NCROSS.EQ.0) GO TO 255
       HESTIM=CARRYO(1)+CARRYO(3)
       CALL FHYDEX(Q,H,HESTIM,ICONV,NRANGE,IBUG)
       IF(NEEDEX.EQ.0) NEEDEX=2
       IF(NEEDEX.EQ.1) NEEDEX=3
       IF(NEEDEX.EQ.4) NEEDEX=5
       GO TO 990
 255   CALL FXTRPL(QA,AQ,ITWO,AH,HA,IBUG)
      IF(NEEDEX.EQ.0) THEN
       NEEDEX=1
       IF(EMPTY(4) .GE. 1.0) NEEDEX=4
      END IF
       GO TO 280
       
 260   if(iflg .eq.0)then
         IF (Q.GE.XRC(LOCQ)) GO  TO 270
       else 
         IF (Q.GE.qmod(1)) GO  TO 270
       end if
C      LOW END EXTRAPOLATION
       LOWEXT=1
 265   CALL FXTRPL(QA,AQ,IONE,AH,HA,IBUG)
      IF(NEEDEX.EQ.0) THEN
       NEEDEX=1
       IF(EMPTY(4) .GE. 1.0) NEEDEX=4
      END IF
      IF(NEEDEX.EQ.2) THEN
       NEEDEX=3
       IF(EMPTY(4) .GE. 1.0) NEEDEX=5
      END IF
      IF(NEEDEX.EQ.6) THEN
        NEEDEX=7
        IF(EMPTY(4) .GE. 1.0) NEEDEX=8
      END IF
       GO TO 280
C      NEED TO INTERPOLATE
 270   CALL FTERPL(QA,AQ,ITWO,AH,HA,IBUG)
C      REMOVE ADJUSTMENT TO GET STAGE
 280   H=HA-ASHFT
C      CHECK IF RESULTING H.LT.STGMIN
 290   IF (H.GE.STGMIN) GO TO 990
       H=STGMIN
       NW=NW+1
C................................
C      DEBUG OUTPUT
 990   IF (IBUG.EQ.0) GO TO 995
       WRITE(IODBUG,901) ICONV,H,HA,ASHFT,Q,I1,I2,LL,
     $     AH,AQ,NEEDEX,IUPEXT,LOWEXT,NW
 901   FORMAT (1H0,' ICONV,H,HA,ASHFT,Q,I1,I2,LL,',
     &  'AH,AQ,NEEDEX,IUPEXT,LOWEXT,NW',
     &  /I5,3F7.2,F10.0,3I5,2F10.2,2F10.0,4I5)
       IF (NCROSS.EQ.0) GO TO 995
       WRITE(IODBUG,902) NCROSS,NRANGE,ABELOW,HESTIM,
     $ (CARRYO(I),I=1,4)
 902   FORMAT(1H ,' NCROSS,NRANGE,ABELOW,HESTIM,',
     $ '(CARRYO(I),I=1,4)',2I5,6F10.2)
C................................
 995   IF(ITRACE.GE.3) WRITE(IODBUG,903)
 903   FORMAT(1H0,14H*** EXIT FHQS1)
       RETURN
       END
