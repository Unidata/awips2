C*********************************************************************
C  SUBROUTINE GET_ICING_FM_SND USED FOR NSHARP'S ICEMAP 
C    USING ROUTINES...FREEZL...THETAE2...AND OUTPUT (PORTED FROM MCIDAS)
C
C  MCIDAS CODE WRITTEN BY RON OLSON, AWC
C  NSHARP/GET_ICING_FM_SND INTERFACE WRITTEN BY LARRY J. HINSON, AWC
C
C*********************************************************************
      SUBROUTINE GET_ICING_FM_SND(SNDG,NUMLEV,ELEV,
     *L1BOT,L1TOP,L1INTS,L1TYPE,L1PROB,
     *L2BOT,L2TOP,L2INTS,L2TYPE,L2PROB,NFLAG)
C*********************************************************************
      IMPLICIT INTEGER (A-Z)
      REAL SNDG(*)
      INTEGER NUMLEV
      INTEGER ELEV
      INTEGER ITHAE(200)
      REAL Z(200),T(200),TD(200),P(200)
C      REAL PF1,TF1,TDF1,ZF1,PF2,TF2,TDF2,ZF2,ZF3,PF3,TF3,TDF3
      REAL PF(3),TF(3),TDF(3),ZF(3)
      NLEV=1
      DO 50 I=1,NUMLEV
C         WRITE (*,*) 'SNDG[I]=',SNDG(I)       
         IF (SNDG((I-1)*7+4) .GT. -900.0) THEN
           P(NLEV)=SNDG((I-1)*7+2)
           Z(NLEV)=SNDG((I-1)*7+3)
           T(NLEV)=SNDG((I-1)*7+4)+273.16
           TD(NLEV)=SNDG((I-1)*7+5)+273.16
           NLEV=NLEV+1
         ENDIF
50    CONTINUE
      NLEV=NLEV-1
      IZS=ELEV 
      CALL FREEZL(NLEV,IZS,P,T,TD,Z,PF,TF,TDF,ZF)                           
                                                                                
C-------- Compute equivalent potential temperature --------------------         
                                                                                
      CALL THETAE2(NLEV,P,T,TD,ITHAE)                                            
                                                                                
C-------- Determine type, intensity, and probability -------------------        
                                                                                
      CALL OUTPUT(NLEV,IZS,P,T,TD,Z,PF(1),TF(1),TDF(1),ZF(1),ITHAE,                     
     *                              PF(2),TF(2),TDF(2),ZF(2),                           
     *ILEV,L1BOT,L1TOP,L1INTS,L1TYPE,L1PROB,                                    
     *     L2BOT,L2TOP,L2INTS,L2TYPE,L2PROB,NFLAG)
      RETURN
      END                
        
C*********************************************************************          
      SUBROUTINE FREEZL(NLEV,IZS,PO,TO,TDO,ZO,PF,TF,TDF,ZF)                     
C*********************************************************************          
C     Locate Freezing Levels.                                                   
C     If freezing level falls between reported levels, use linear               
C     interpolation to find approximate height, dew point, and pressure.                                                    
      IMPLICIT INTEGER (A-Z)                                                    
      REAL PO(200),TO(200),TDO(200),ZO(200)
      REAL PF(3),TF(3),TDF(3),ZF(3)
      REAL P(200),T(200),TD(200),Z(200)                                       
      REAL T1,T2,P1,P2,Z1,Z2,TD1,TD2                                            
      DATA MSG/-99999./ 
                                                              
C-----Initialize variables
      DO 10 I=1,3
        PF(I)=MSG
        TF(I)=MSG
        TDF(I)=MSG
        ZF(I)=MSG
10    CONTINUE
      N=0
      DO 20 I=1,NLEV
        IF ((TO(I) .GT. 230 .AND. TO(I) .LT. 325) .AND. 
     *      (TDO(I) .GT. 230 .AND. TDO(I) .LT. 305) .AND.
     *      (ZO(I) .GE. IZS .AND. PO(I) .GE. 0)) THEN
          N=N+1
          P(N)=PO(I)
          T(N)=TO(I)
          TD(N)=TDO(I)
          Z(N)=ZO(I)
        ENDIF
20    CONTINUE
      IF (T(1) .LE. 273.16) THEN
        TF(1)=INT(T(1))
        TDF(1)=INT(TD(1))
        PF(1)=P(1)
        ZF(1)=Z(1)
      ENDIF
      DO 30 I=2,N
        P1=P(I-1)
        P2=P(I)
        T1=INT(T(I-1))
        T2=INT(T(I))
        TD1=INT(TD(I-1))
        TD2=INT(TD(I))
        Z1=Z(I-1)
        Z2=Z(I)
        IF (T1.GT.273.16 .AND. T2.LE.273.16 .AND. ZF(1).EQ.MSG) THEN 
          IF ((T2-T1) .NE. 0) THEN
             ZF(1)=(Z2-Z1)/(T2-T1)*(273.16-T1)+Z1
             PF(1)=(P2-P1)/(T2-T1)*(273.16-T1)+P1
             TF(1)=273.16
             TDF(1)=(TD2-TD1)/(P2-P1)*(PF(1)-P1)+TD1
          ELSE
            ZF(1)=Z2
            PF(1)=P2
            TF(1)=273.16
            TDF(1)=T2
          ENDIF
        ENDIF
        IF (T1.LE.273.16 .AND. T2.GT.273.16 .AND. ZF(1).NE.MSG) THEN
          IF ((T2-T1) .NE. 0) THEN
            ZF(2)=(Z2-Z1)/(T2-T1)*(273.16-T1)+Z1
            PF(2)=(P2-P1)/(T2-T1)*(273.16-T1)+P1
            TF(2)=273.16
            TDF(2)=(TD2-TD1)/(P2-P1)*(PF(1)-P1)+TD1
          ELSE
            ZF(2)=Z2
            PF(2)=P2
            TF(2)=273.16
            TDF(2)=T2
          ENDIF
        ENDIF
        IF (T1.GT.273.16 .AND. T2.LT.273.16 .AND. ZF(2).NE.MSG
     *    .AND. ZF(3).EQ.MSG) THEN
          IF ((T2-T1) .NE. 0) THEN
            ZF(3)=(Z2-Z1)/(T2-T1)*(273.16-T1)+Z1
            PF(3)=(P2-P1)/(T2-T1)*(273.16-T1)+P1
            TF(3)=273.16
            TDF(3)=(TD2-TD1)/(P2-P1)*(PF(1)-P1)+TD1
          ELSE
            ZF(3)=Z2
            PF(3)=P2
            TF(3)=273.16
            TDF(3)=T2
          ENDIF
        ENDIF
30    CONTINUE
      RETURN                                                                  
      END                                                                       
C*********************************************************************          
      SUBROUTINE THETAE2(NLEV,P,T,TD,ITHAE)                                      
C*********************************************************************          
C     Calculate equivalent potential temperatures                               
                                                                                
      IMPLICIT INTEGER (A-Z)                                                    
      REAL TT,TDT,PT,TLCL,THA,E,W,THAE                                          
      REAL P(200),T(200),TD(200)
      REAL MSG                                    
      DIMENSION ITHAE(200)                                                      
      DATA MISS/Z'80808080'/
      DATA MSG/-99999./                                                         
                                                                                
C-----Initialize variables                                                      
      TT=MSG                                                                    
      TDT=MSG                                                                   
      PT=MSG                                                                    
      TLCL=MSG                                                                  
      THA=MSG                                                                   
      THAE=MSG                                                                  
      E=MSG                                                                     
      W=MSG                                                                     
                                                                                
C-----Loop through levels                                                       
      DO 100 I=1,NLEV                                                           
                                                                                
C-----Assign values and convert to celcius                                      
      IF(T(I).EQ.MSG) GOTO 100                                                  
      IF (TD(I) .LT. 0) GOTO 100
      IF(TD(I).EQ.MSG) GOTO 100                                                 
      TT=T(I)-273.16                                                            
      TDT=TD(I)-273.16                                                          
      PT=P(I)                                                                   
C     IF(T(I).EQ.T(IFL)) TT=273.16                                              
                                                                                
C-----Find temperature of lifting condensation level                            
      TLCL=TDT-(.001296*TDT+.1963)*(TT-TDT)                                     
                                                                                
C-----Convert temperatures back kelvin                                          
      TLCL=TLCL+273.16                                                          
      TT=TT+273.16                                                              
      TDT=TDT+273.16                                                            
                                                                                
C-----Compute potential temperature                                             
      THA=TT*(1000./PT)**.286                                                   
C-----Saturation vapor pressure                                                 
      E=EXP(21.6271-5414.06/TDT)                                                
C-----Mixing ratio                                                              
      W=622.*E/(PT-E)                                                                                
C-----Calculate equivalent potential temperature                                
      THAE=THA*EXP(2.4815*W/TLCL)                                               
      ITHAE(I)=NINT(THAE)
      
                                                            
C------------------------------------------------------                         
                                                                                
100   CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
C*********************************************************************          
      SUBROUTINE OUTPUT(NLEV,IZS,P,T,TD,Z,PF1,TF1,TDF1,ZF1,ITHAE,               
     *                                    PF2,TF2,TDF2,ZF2,                     
     *ILEV,         L1BOT,L1TOP,L1INTS,L1TYPE,L1PROB,                           
     *              L2BOT,L2TOP,L2INTS,L2TYPE,L2PROB,      NFLAG)               
C*********************************************************************          
C     Determine type, intensity and probability of icing, and                   
C     send send results back to main program.                                   
                                                                                
      IMPLICIT INTEGER (A-Z)                                                    
      REAL TQ,TDQ,PQ,ZQ,DT,DD,TBOUND,XDD,MSG                                    
      REAL TF1,TDF1,PF1,ZF1,TF2,TDF2,PF2,ZF2,RZS                                
      REAL Z(200),T(200),TD(200),P(200)                                         
      DIMENSION IPROB(2,7),ITHAE(200)                                           
      DIMENSION NOUT(1601)                                                       
      DATA IPROB/17,51,41,19,17,10,9,16,33,23,11,10,9,0/                        
      DATA MAX/200/                                                             
      DATA MISS/Z'80808080'/
      DATA MSG/-99999./                                                         
                                                                                
C-----Initialize variables to be used-----------------------------------        
                                                                                
      NFLAG=1                                                                   
                                                                                
      TQ=MSG                                                                    
      TDQ=MSG                                                                   
      PQ=MSG                                                                    
      ZQ=MSG                                                                    
      DD=MSG                                                                    
      DT=MSG                                                                    
      XDD=MSG                                                                   
      IX=0                                                                   
                                                                                
      L1BOT=MISS                                                                
      L1TOP=MISS                                                                
      L1INTS=MISS                                                               
      L1TYPE=MISS                                                               
      L1PROB=MISS                                                               
                                                                                
      L2BOT=MISS                                                                
      L2TOP=MISS                                                                
      L2INTS=MISS                                                               
      L2TYPE=MISS                                                               
      L2PROB=MISS                                                               
      RZS=FLOAT(IZS)                                                            
                                                                                
      DO 1 I=2,1601                                                              
       NOUT(I)=MISS                                                             
1     CONTINUE                                                                  
                                                                                
      NOUT(1)=0                                                                 
      J=0                                                                       
      ICOUNT=0                                                                  
                                                                                
C-----Loop through all levels-------------------------------------------        
                                                                                
      DO 100 J=1,NLEV                                                           
                                                                                
      ICOUNT=ICOUNT+1                                                           
                                                                                
      IF(T(J) .EQ. MISS .OR. T(J) .EQ. MSG) GOTO 100                            
      IF(TD(J) .EQ. MISS .OR. TD(J) .EQ. MSG) GOTO 100                          
      IF(P(J) .EQ. MISS .OR. P(J) .EQ. MSG) GOTO 100                            
      IF(Z(J) .EQ. MISS .OR. Z(J) .EQ. MSG) GOTO 100                            
      IF(Z(J) .LT. 0) GOTO 100                                                  
      IF(P(J) .EQ. 0) GOTO 100                                                  
                                                                                
C-----If freezing level at surface                                              
                                                                                
      IF(ICOUNT.EQ.1) THEN                                                      
       IF(T(J).LE.273.16) THEN                                                  
         TQ=TF1                                                                 
         TDQ=TDF1                                                               
         PQ=PF1                                                                 
         ZQ=ZF1                                                                 
       ENDIF                                                                    
      ENDIF                                                                     
                                                                                
C-----If freezing level is not at surface use interpolated parameters           
                                                                                
      IF(ICOUNT .GE. 2) THEN                                                    
       IF(T(J) .LE. 273.16 .AND. T(J-1) .GT. 273.16) THEN                       
        IF(Z(J) .GT. RZS) THEN                                                  
          TQ=TF1                                                                
          TDQ=TDF1                                                              
          PQ=PF1                                                                
          ZQ=ZF1                                                                
        ENDIF                                                                   
       ENDIF                                                                    
      ENDIF                                                                     
                                                                                
C-----If both level and level are below freezing use info for level             
                                                                                
      IF(ICOUNT .GE. 2) THEN                                                    
       IF(T(J) .LE. 273.16 .AND. T(J-1) .LE. 273.16) THEN                       
        IF(Z(J) .GT. RZS) THEN                                                  
          TQ=T(J)                                                               
          TDQ=TD(J)                                                             
          PQ=P(J)                                                               
          ZQ=Z(J)                                                               
        ENDIF                                                                   
       ENDIF                                                                    
      ENDIF                                                                     
                                                                                
C-----Calculate DD (dew point depression) and DT (Appleman Line)                
      DD=TQ-TDQ                                                                 
      DT=-0.2*(TDQ-273.16)                                                      
                                                                                
C-----Never any icing if dd exceeds 4.00                                        
      IF(DD .GT. 4.00) GOTO 99                                                  
                                                                                
C-----Never any icing if T is above 0c or below -22C                            
      IF (TQ .GT. 273.16 .OR. TQ .LT. 251.16) GO TO 99                          
                                                                                
C-----Check for sufficient moisture content at temp                             
C..   If temp between 0c and -12c use Appleman criteria                         
      IF (TQ .GE. 261.16 .AND. DD .GT. DT) GOTO 99                              
                                                                                
C..   If temp colder then -12c use 2 deg dd criteria                            
      IF (TQ .LT. 261.16 .AND. TQ .GE. 256.16) THEN                             
       IF (DD .GT. 2.5) GOTO 99                                                 
      ENDIF                                                                     
C..   If temp colder then -17c use 3 deg dd criteria                            
      IF (TQ .LT. 256.16 .AND. DD .GT. 3.5) GOTO 99                             
                                                                                
C-----Increment counter if level meets icing criteria                           
      NOUT(1)=NOUT(1)+1                                                         
      ILEV=NOUT(1)                                                              
      IF (NOUT(1).GT.MAX) THEN                                                  
       NOUT(1)=MAX                                                              
       GOTO 150                                                                 
      ENDIF                                                                     
                                                                                
C-----Defining the 'IX' pointer for valid icing level                           
      IX=(NOUT(1)-1)*8+1                                                        
                                                                                
                                                                                
C-----Flag = 0, if not using interpolated freezing level                        
      NOUT(IX+5)=0                                                              
C     Flag = 1, if using interpolated freezing level                            
      IF (TQ.EQ.273.16) NOUT(IX+5)=1                                            
                                                                                
C-----Estimate probability of icing (from AWS/TR-80)--------------------        
5     M=1                                                                       
      TBOUND=271.16                                                             
      DO 10 N=1,7                                                               
      IF (TQ.GE.TBOUND) GO TO 15                                                
      TBOUND=TBOUND-5.                                                          
10    CONTINUE                                                                  
15    XDD=FLOAT(N)-1.                                                           
      IF (DD.GT.XDD) M=2                                                        
      NOUT(IX+4)=IPROB(M,N)                                                     
                                                                                
C-----Estimate Intensity based on moisture content----------------------        
C     Initialize icing intensity to none                                        
      NOUT(IX+8)=1                                                              
C     Trace icing intensity                                                     
      IF (DD .LE. 4.) NOUT(IX+8)=2                                              
C     Light icing intensity                                                     
      IF (DD .LE. 3.) NOUT(IX+8)=3                                              
C     Moderate icing intensity                                                  
      IF (DD .LE. 2. .AND. TQ .GE. -12.) NOUT(IX+8)=4                           
                                                                                
C-----Estimate Type of icing--------------------------------------------        
C     Use the temperature, and stability of the atmosphere below.               
C     At warm temperatures (above -8c), droplets spread before freezing,        
C     resulting in clear or glaze ice.  At cold temperatures (below -15         
C     c), droplets freeze on contact forming rime ice.  With potentially        
C     stable airmass, stratiform clouds are likely, indicating rime at          
C     any temperature.  Between -8 and -32 c with potentially unstable          
C     air, a mixture of clear and rime ice is the most likely.                  
                                                                                
      IF(ITHAE(J).EQ.MISS) THEN                                                 
       NOUT(IX+7)=2                                                             
       GOTO 11                                                                  
      ENDIF                                                                     
      ISTA=ITHAE(J)-LTHAE                                                       
      NOUT(IX+7)=1                                                              
      IF(TQ .LT. 258.16) NOUT(IX+7)=2                                           
      IF(TQ .LE. 272.16 .AND. TQ .GE. 258.16) THEN                              
        IF(ISTA .GT. 0.5) NOUT(IX+7)=2                                          
      ENDIF                                                                     
      IF(NOUT(IX+7).EQ.1) THEN                                                  
       IF(TQ .LE. 264.16 .AND. TQ .GE. 258.16) NOUT(IX+7)=3                     
      ENDIF                                                                     
11    CONTINUE                                                                  
                                                                                
C-----Final calculations------------------------------------------------        
                                                                                
      NOUT(IX+1)=NINT(TQ*100)                                                   
      NOUT(IX+6)=NINT(TDQ*100)                                                  
      NOUT(IX+2)=NINT(PQ)                                                       
      NOUT(IX+3)=NINT(ZQ)                                                       
      LTHAE=ITHAE(J)                                                            
                                                                                
C****************** L a y e r  O n e ***********************************        
C-----Determine 1st icing base using Intensity as criteria--------------        
      IF(L1BOT.EQ.MISS) THEN                                                    
       IF(NOUT(1).GE.2) THEN                                                    
        IF(NOUT(IX+8).GE.1) THEN                                                
         IF(NOUT(IX+8).EQ.NOUT(IX+8-8)) THEN                                    
          IF(NOUT(IX+3-8).GE.IZS) THEN                                          
           L1BOT=NOUT(IX+3-8)                                                   
           L1PROB=NOUT(IX+4-8)                                                  
           L1TYPE=NOUT(IX+7)                                                    
           L1INTS=NOUT(IX+8-8)                                                  
          ENDIF                                                                 
         ENDIF                                                                  
        ENDIF                                                                   
       ENDIF                                                                    
      ENDIF                                                                     
C-----Assign 1st icing top using Intensity as criteria------------------        
      IF(L1BOT.NE.MISS) THEN                                                    
       IF(L1TOP.EQ.MISS) THEN                                                   
        IF(NOUT(IX+8).NE.NOUT(IX+8-8)) THEN                                     
         IF(NOUT(IX+8-8).EQ.NOUT(IX+8-16)) THEN                                 
          IF(NOUT(IX+8-8).GE.1) THEN                                            
           IF(NOUT(IX+3-8).GT.L1BOT+100) THEN                                   
            L1TOP=NOUT(IX+3-8)                                                  
           ENDIF                                                                
          ENDIF                                                                 
         ENDIF                                                                  
        ENDIF                                                                   
       ENDIF                                                                    
      ENDIF                                                                     
C****************** L a y e r  T w o ***********************************        
C-----If first icing layer has been found ------------------------------        
C-----Determine 2nd icing base using Intensity as criteria--------------        
      IF(L1TOP.NE.MISS) THEN                                                    
       IF(L2BOT.EQ.MISS) THEN                                                   
        IF(NOUT(IX+8).EQ.NOUT(IX+8-8)) THEN                                     
         IF(NOUT(IX+8-8).GE.1) THEN                                             
          IF(NOUT(IX+3-8).GE.L1TOP) THEN                                        
           L2BOT=NOUT(IX+3-8)                                                   
           L2PROB=NOUT(IX+4-8)                                                  
           L2TYPE=NOUT(IX+7)                                                    
           L2INTS=NOUT(IX+8-8)                                                  
          ENDIF                                                                 
         ENDIF                                                                  
        ENDIF                                                                   
       ENDIF                                                                    
      ENDIF                                                                     
C-----Determine 2nd icing top using Intensity as criteria--------------         
      IF(L2BOT.NE.MISS) THEN                                                    
       IF(L2TOP.EQ.MISS) THEN                                                   
        IF(NOUT(IX+8).NE.NOUT(IX+8-8)) THEN                                     
         IF(NOUT(IX+8-8).NE.NOUT(IX+8-16)) THEN                                 
          IF(NOUT(IX+8-8).GE.1) THEN                                            
           IF(NOUT(IX+3-8).GT.L2BOT+100) THEN                                   
            L2TOP=NOUT(IX+3-8)                                                  
           ENDIF                                                                
          ENDIF                                                                 
         ENDIF                                                                  
        ENDIF                                                                   
       ENDIF                                                                    
      ENDIF                                                                     
      GOTO 100                                                                  
C-----------------------------------------------------------------------        
99    CONTINUE                                                                  
C-----------------------------------------------------------------------        
C-----Assign a top1 if jumped out of the loop for the following reasons         
      IF(L1BOT.NE.MISS) THEN                                                    
      IF(L1TOP.EQ.MISS) THEN                                                    
      IF(NOUT(IX+8).GE.1) THEN                                                  
      IF(NOUT(IX+3).GT.L1BOT) THEN                                              
C     If dd exeeds 4.00                                                         
        IF(DD .GT. 4.00) THEN                                                   
         L1TOP=NOUT(IX+3)                                                       
         GOTO 100                                                               
        ENDIF                                                                   
C     If temp warmer than 0c or colder than -22c                                
        IF(TQ.GT.273.16 .OR. TQ.LT.251.16) THEN                                 
         L1TOP=NOUT(IX+3)                                                       
         GOTO 100                                                               
        ENDIF                                                                   
C     If temp between 0c and -12c and Appleman criteria exceeded                
       IF (TQ .GE. 261.16 .AND. DD .GT. DT) THEN                                
        L1TOP=NOUT(IX+3)                                                        
        GOTO 100                                                                
       ENDIF                                                                    
C     If temp between -12c and -17c use 2 deg dd criteria                       
       IF (TQ .LT. 261.16 .AND. TQ .GE. 256.16) THEN                            
        IF (DD .GT. 2.5) THEN                                                   
         L1TOP=NOUT(IX+3)                                                       
         GOTO 100                                                               
        ENDIF                                                                   
       ENDIF                                                                    
C     If temp colder then -17c use 3 deg dd criteria                            
       IF (TQ .LT. 256.16 .AND. DD .GT. 3.5) THEN                               
        L1TOP=NOUT(IX+3)                                                        
        GOTO 100                                                                
       ENDIF                                                                    
      ENDIF                                                                     
      ENDIF                                                                     
      ENDIF                                                                     
      ENDIF                                                                     
C-----Assign a top2 if jumped out of the loop for the following reasons         
      IF(L2BOT.NE.MISS) THEN                                                    
      IF(L2TOP.EQ.MISS) THEN                                                    
      IF(NOUT(IX+8).GE.1) THEN                                                  
      IF(NOUT(IX+3).GE.L1BOT) THEN                                              
C     If dd exeeds 4.00                                                         
        IF(DD .GT. 4.00) THEN                                                   
         L2TOP=NOUT(IX+3)                                                       
         GOTO 100                                                               
        ENDIF                                                                   
C     If temp warmer than 0c or colder than -22c                                
        IF(TQ.GT.273.16 .OR. TQ.LT.251.16) THEN                                 
         L2TOP=NOUT(IX+3)                                                       
         GOTO 100                                                               
        ENDIF                                                                   
C     If temp between 0c and -12c and Appleman criteria exceeded                
        IF (TQ .GE. 261.16 .AND. DD .GT. DT) THEN                               
         L2TOP=NOUT(IX+3)                                                       
         GOTO 100                                                               
        ENDIF                                                                   
C     If temp between -12c and -17c use 2 deg dd criteria                       
       IF (TQ .LT. 261.16 .AND. TQ .GE. 256.16) THEN                            
        IF (DD .GT. 2.5) THEN                                                   
         L2TOP=NOUT(IX+3)                                                       
         GOTO 100                                                               
        ENDIF                                                                   
       ENDIF                                                                    
C     If temp colder then -17c use 3 deg dd criteria                            
       IF (TQ .LT. 256.16 .AND. DD .GT. 3.5) THEN                               
        L2TOP=NOUT(IX+3)                                                        
        GOTO 100                                                                
       ENDIF                                                                    
      ENDIF                                                                     
      ENDIF                                                                     
      ENDIF                                                                     
      ENDIF                                                                     
C-----------------------------------------------------------------------        
100   CONTINUE                                                                  
150   CONTINUE                                                                  
C..If we make it here without a base send back a nil flag                       
      IF(L1BOT.EQ.MISS) NFLAG=0                                                 
C-----------------------------------------------------------------------        
      RETURN                                                                    
      END                                                                       
