C MODULE PIN7
C-----------------------------------------------------------------------
C
      SUBROUTINE PIN7 (P,LP,IP,C,LC,IC)
C.......................................................................
C
C     THIS IS THE INPUT ROUTINE FOR THE LAG/K OPERATION.
C     THIS SUBROUTINE READS ALL CARDS FOR THIS OPERATION
C     AND FILLS THE P AND C ARRAYS.
C.......................................................................
C
C   ROUTINE ORIGINALLY WRITTEN BY
C      GEORGE F. SMITH - HRL   OCTOBER 1979   VERSION 1.0
C   UPDATED MARCH 1982 TO ALLOW INPUT IN ENGLISH OR METRIC UNITS
C   UPDATED JANUARY 1984 FOR USE WITH RESERVOIR OPERATION
C   UPDATED JUNE 1989 TO ALLOW FORT WORTH FLOW TRANSMISSION
C           LOSS COMPUTATIONS
C   UPDATED FEB 1990 TO SET IATL VARIABLE BASED ON FT. WORTH TRANS
C           LOSS COMPUTATIONS
C   UPDATED OCT 1993 TO MOVE WHERE IATL VARIABLE IS SET SO WE GO
C           THROUGH THIS CODE EVEN WHEN PIN7 IS CALLED FROM THE RES-SNGL
C   UPDATED JAN 1996 TO PROPERLY CONVERT FORT WORTH TRANSMISSION
C           LOSS COEFFICIENT FROM CMS TO CFS
C.......................................................................
C
C     THIS SUBROUTINE CAN HANDLE TWO DIFFERENT METHODS OF COMPUTING
C     VARIABLE K.  THESE TWO METHODS ARE:
C      1. THE METHOD USED IN THE MCP2 PROGRAM AND DESCRIBED IN SECTION
C         II.4.2.1 OFTHE NWSRFS USERS MANUAL.
C      2. THE METHOD USED BY THE ATLANTA RFC.  THIS METHOD SOLVES THE
C         STORAGE EQUATION BY USING THE RELATIONSHIP BETWEEN 2*S/DT+O
C         AND O. THIS RELATIONSHIP IS CONSTRUCTED FROM THE VARIABLE K
C         CURVE USING THE EQUATION DELTA S = K * DELTA O.
C
C     COMMON BLOCK FATLGK DETERMINES WHICH OF THE TWO METHODS IS USED.
C     IF IATL=0, THE MCP2 METHOD IS USED.
C     IF IATL=1, THE ATLANTA RFC METHOD IS USED.
C       IF THE ATLANTA RFC METHOD IS USED TWO ADDITIONAL VALUES ARE
C       REQUIRED TO SPECIFY THE DELTA O INCREMENTS USED IN COMPUTING
C       THE 2*S/DT+O VS O TABLE.  THESE VALUES ARE C1 AND C2.
C
C     IATL,C1,AND C2 ARE SET IN BLOCK DATA ROUTINES FOR PROGRAMS
C       MCP3, OPT3, ESP3, AND FCAT
C     IATL IS RESET BASED ON WHETHER OR NOT FT. WORTH
C       TRANSMISSION LOSS COMPUTATIONS ARE DONE FOR
C       THE CURRENT LAG/K OPERATION
C       IATL = 1 IF TRANS LOSS COMP ARE OFF (I.E., P(11).EQ.0.0)
C       IATL = 0 IF TRANS LOSS COMP ARE ON  (I.E., P(11).GT.0.0)
C.......................................................................
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      COMMON /FRES7/ MENNST,LAGDT,NCD7,NLAGK
      COMMON /FATLGK/ IATL,C1,C2
C
      DIMENSION P(LP),C(LC)
      DIMENSION TSIDA(2),TSIDB(2)
      CHARACTER*8 SNAME/'PIN7'/
      LOGICAL FOP7,MEANQ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pin7.f,v $
     . $',                                                             '
     .$Id: pin7.f,v 1.5 2006/03/16 16:35:26 xfan Exp $
     . $' /
C    ===================================================================
C
C
      DATA L3OT/4HL3/T/,L3/4HL3  /
      DATA RSTYPI/4HQINE/,RSTYPM/4HQME /
      DATA IBLANK/4H    /,METR/4HMETR/,IENG/4HENGL/
      DATA LAGWRD/4HLAG /,KWRD/4HK   /
      DATA RES,SNGL/4HRES-,4HSNGL/
      DATA BLANK/4H    /
C
C
      LTRACE=1
      NOP=7
      CALL FPRBUG (SNAME,LTRACE,NOP,IBUG)
C
      VER=1.0
      IC=1
      ZERO=0.0
      IS0=0
      IS1=1
      NCD7=0
C
      READ (IN,10) TSIDA,DTA,ITA,TSIDB,DTB,ITB,JLAG,JK,METENG,TLRC,
     *  QBNTL
10    FORMAT (2A4,1X,A4,1X,I2,1X,2A4,1X,A4,1X,I2,1X,I5,1X,I5,1X,A4,
     1 F5.0,F10.0)
C
      IF (IBUG.GT.0) WRITE (IODBUG,20) TSIDA,DTA,ITA,TSIDB,DTB,ITB,
     1 JLAG,JK,METENG,TLRC,QBNTL
20    FORMAT (' TSIDA=',2A4,' DTA=',A4,' ITA=',I2,' TSIDB=',2A4,
     *  ' DTB=',A4,' ITB=',I2,' JLAG=',I4,' JK=',I4,
     *  ' METENG=',A4,' TLRC=',F6.2,' QBNTL=',F11.1)
C
      MENNST=1
      IF (NLAGK.EQ.1) THEN
         IF (DTA.EQ.BLANK.OR.DTA.EQ.RSTYPM) MENNST=0
         ENDIF
      NCD7=NCD7+1
C
C  STORE TWO PARAMETERS FOR FT. WORTH TRANSMISSION COMPUTATIONS
C
      IF (TLRC.LT.0.0 .OR. TLRC.GT.1.0) GO TO 30
      P(11)=TLRC
      GO TO 50
C
C  TLRC OUT OF VALID RANGE - SET TO ZERO.
30    WRITE (IPR,40) TLRC
40    FORMAT ('0**WARNING** VALUE OF THE RECESSION ',
     1 'COEFFICIENT FOR THE FT. WORTH TRANSMISSION LOSS ',
     2 'COMPUTATIONS (',F12.1,') CANNOT BE ',
     3 'LESS THAN ZERO OR GREATER THAN ONE. ',
     4 'VALUE WILL BE SET TO ZERO.')
      P(11)=0.0
C
50    IF (QBNTL.LT.0.0) GO TO 60
      IF (METENG.EQ.IENG) THEN
         CALL FCONVT('CMS ','L3/T',KENGUN,CFSM1,CFSA,IER)
         QBNTL=QBNTL/CFSM1
         ENDIF
      P(12)=QBNTL
      GO TO 80
C
C  QBNTL LESS THAN ZERO - SET TO ZERO
60    WRITE (IPR,70)QBNTL
70    FORMAT ('0**WARNING** MINIMUM FLOW ENTERED FOR THE ',
     1 'FT. WORTH TRANSMISSION LOSS COMPUTATIONS (',F12.1,
     2 ') CANNOT BE LESS THAN ZERO. VALUE WILL BE SET TO ZERO.')
      P(12)=0.0
C
C  NEEDED TO FILL FT. WORTH TRANSMISSION LOSS PARAMETERS
C  AT THIS POINT IN PROGRAM SO THAT WE KNOW HOW TO SET IATL
80    IATL = 1
      IF (P(11).GT.0.0) IATL = 0
C
      LMETR=-1
      IF (METENG.EQ.METR.OR.METENG.EQ.IBLANK) LMETR=1
      IF (METENG.EQ.IENG) LMETR=0
      IF (LMETR.EQ.-1) THEN
         WRITE (IPR,90) METENG
90    FORMAT ('0**WARNING** ENGLISH/METRIC INDICATOR (',A4,
     *   ' MUST BE ''METR'', ''ENGL'' OR BLANK. METRIC IS ASSUMED.')
         CALL WARN
         LMETR=1
         ENDIF
C
      IF (LMETR.EQ.0) THEN
         CALL FCONVT ('CMS ','L3/T',IENGUN,CFSM,CFSA,IER)
         CALL FCONVT ('CMSD','L3  ',JENGUN,CFDM,CMDA,IER)
         ENDIF
C
      IF (TSIDA(1).EQ.RES.AND.TSIDA(2).EQ.SNGL) GO TO 190
C
      CALL CHEKTS (TSIDA,DTA,ITA,IS0,IDIMI,IS0,IS1,IER)
C
      IF (IDIMI.EQ.L3OT.OR.IDIMI.EQ.L3) GO TO 110
      WRITE (IPR,100)IDIMI
100   FORMAT ('0**ERROR** INVALID DIMENSIONS (',A4,') FOR INFLOW TIME ',
     1   'SERIES. VALID DIMENSIONS ARE L3/T AND L3.')
      CALL ERROR
C
110   MEANQ=.FALSE.
      IF (IDIMI.EQ.L3) MEANQ=.TRUE.
C
      IF (.NOT.MEANQ.OR.IATL.EQ.1) GO TO 130
         WRITE (IPR,120)
120   FORMAT ('0**ERROR** CANNOT USE PERIOD AVERAGE INFLOW WITH ',
     1  'MCP2 ATTENUATION (K) TECHNIQUE.')
         CALL ERROR
C
130   IF (ITB.GT.0) CALL CHEKTS (TSIDB,DTB,ITB,IS0,IDIMO,IS1,IS1,IER)
C
      IF (ITB.GT.0.OR..NOT.MEANQ) GO TO 150
         WRITE (IPR,140)
140   FORMAT ('0**ERROR** CANNOT WRITE OUTFLOW TIME SERIES OVER ',
     1   'INFLOW TIME SERIES WHEN INFLOW IS PERIOD AVERAGED.')
         CALL ERROR
C
150   IF (ITB.EQ.0) GO TO 170
      IF (IDIMO.EQ.L3OT) GO TO 170
         WRITE (IPR,160)IDIMO
160   FORMAT ('**ERROR** DIMENSIONS FOR OUTFLOW TIME SERIES (',A4,
     1   ') MUST BE L3/T.')
         CALL ERROR
C
170   IF (ITA.LE.ITB.OR.ITB.EQ.0) GO TO 200
         WRITE (IPR,180)ITA,ITB
180   FORMAT ('0**ERROR** INFLOW TIME STEP (',I2,
     1  ') MUST NOT BE GREATER THAN THE OUTFLOW TIME STEP (',I2,'.')
         CALL ERROR
         GO TO 200
C
190   ITA=LAGDT
      ITB=ITA
      MEANQ=.FALSE.
      IF (MENNST.EQ.0)MEANQ=.TRUE.
      IDIMI=L3OT
      DTA = RSTYPI
      IF (MEANQ) DTA = RSTYPM
      IF (MEANQ)IDIMI=L3
C
200   IF (JLAG.LT.0.OR.JK.LT.0) THEN
         WRITE (IPR,210) JLAG,JK
210   FORMAT ('0**ERROR** NUMBER OF PAIRS OF VALUES IN THE ',
     1  '(LAG,Q) TABLE (',I5,') OR (K,Q) TABLE (',I5,
     *  ') CANNOT BE LESS THAN ZERO.')
         CALL ERROR
         GO TO 560
         ENDIF
C
      IP=19
      CALL CHECKP (IP,LP,IER)
      IF (IER.EQ.1) GO TO 560
C.......................................................................
C
C     STORE VERSION IDENTIFIER, TIME SERIES ID'S, AND CONSTANT OR
C     VARIABLE LAG VALUES IN P ARRAY.
C.......................................................................
C
      P(1)=VER
      CALL UMEMOV (TSIDA,P(2),2)
      P(4)=DTA
      P(5)=ITA+.01
      CALL UMEMOV (TSIDB,P(6),2)
      P(8)=DTB
      P(9)=ITB+.01
      P(10)=LMETR+.01
C
C  HAVE FILLED P(11) AND P(12) EARLIER WITH FT. WORTH
C  TRANSMISSION LOSS PARAMETERS
C
C  SET 3 UNUSED SPACES ON P ARRAY
      CALL UMEMST (ZERO,P(13),3)
      P(19)=JLAG+.01
      IF (JLAG.GT.0) GO TO 230
         IP=IP+1
         CALL CHECKP (IP,LP,IER)
         IF (IER.EQ.1) GO TO 560
         IX=1
         CALL FFRDRL (P(IP),IX,IBUG,IER,NCD7)
         IF (IER.EQ.1) GO TO 560
         IF (P(IP).EQ.0.0.OR.IDIMI.EQ.L3OT) GO TO 330
            WRITE (IPR,220) IDIMI
220   FORMAT ('0**ERROR** DIMENSIONS FOR INFLOW TIME SERIES (',A4,
     *   ' MUST BE L3/T.')
            CALL ERROR
            GO TO 330
230   IF (IDIMI.EQ.L3OT) GO TO 240
         WRITE (IPR,220) IDIMI
         CALL ERROR
C
240   IPT=IP+2*JLAG
      CALL CHECKP (IPT,LP,IER)
      IF (IER.EQ.1) GO TO 560
      IX=2*JLAG
      CALL FFRDRL (P(IP+1),IX,IBUG,IER,NCD7)
      IF (IER.EQ.1) GO TO 560
      IF (IBUG.GT.1) WRITE (IODBUG,250) (P(IP+NP),NP=1,IX)
250   FORMAT (' P=',10F10.2)
      JLG=JLAG-1
      IF (JLG.EQ.0) GO TO 290
      DO 280 NP=1,JLG
         L1=IP+NP*2
         L2=L1+2
         IF (IBUG.GT.1) WRITE (IPR,260) L1,L2,P(L1),P(L2)
260   FORMAT (' L1=',I5,' L2=',I5,' P(L1)=',F10.2,' P(L2)=',F10.2)
         IF (P(L1).LE.P(L2)) GO TO 280
            WRITE (IPR,270)
270   FORMAT ('0**ERROR** DISCHARGES IN THE LAG VS Q TABLE MUST ',
     1   'BE IN ASCENDING ORDER.')
            CALL ERROR
            GO TO 290
280      CONTINUE
C
290   IF (LMETR.EQ.0) THEN
         DO 300 I=2,IX,2
            P(IP+I)=P(IP+I)/CFSM
300         CONTINUE
         ENDIF
C
      IBGNG=IP+2
      NPAIR=0
      DO 320 I=IBGNG,IPT,2
         NPAIR=NPAIR+1
         IF (P(I).LE.1.E6) GO TO 320
            WRITE (IPR,310) NPAIR,LAGWRD,P(I)
310   FORMAT ('0**WARNING** FLOW FOR PAIR ',I4,
     1 ' IN THE ',A4,'VS Q TABLE WAS ENTERED AS ',G10.4,' CMS.'/
     2 11X,'THIS VALUE HAS BEEN RESET TO 1000000 CMS.'/
     3 11X,'IF YOU INTENDED TO ENTER FLOW VALUES IN CFS PUT ''ENGL''',
     4 ' IN COLUMNS 47-50 ON CARD 1 OF THE LAG/K INPUT.')
            P(I)=1.E6
            CALL WARN
320      CONTINUE
C
      IP=IPT
C.......................................................................
C
C     STORE CONSTANT OR VARIABLE K VALUES IN P ARRAY.
C.......................................................................
C
330   IP=IP+1
      CALL CHECKP (IP,LP,IER)
      IF (IER.EQ.1) GO TO 560
      P(IP)=JK+.01
      P(18)=IP+.01
      XITA=ITA
      IF (JK.GT.0) GO TO 350
      IP=IP+1
      CALL CHECKP (IP,LP,IER)
      IF (IER.EQ.1) GO TO 560
      IX=1
      CALL FFRDRL (P(IP),IX,IBUG,IER,NCD7)
      IF (IER.EQ.1) GO TO 560
C.......................................................................
C
C     CHECK CONSTANT K - IF LT COMPUTATIONAL INT / 4 SET TO ZERO
C                      - IF LT COMPUTATIONAL INT / 2 SET TO
C                          COMPUTATIONAL INT / 2
C.......................................................................
C
      IF (P(IP).EQ.0.0) GO TO 440
      IF (IATL.EQ.0)XITA=XITA/2.
      IF (P(IP).GE.XITA/2.) GO TO 440
      SETK=0.
      IF (MEANQ)SETK=XITA/2.
      IF (P(IP).GT.XITA/4.)SETK=XITA/2.
      WRITE (IPR,340) P(IP),SETK
340   FORMAT ('0**WARNING** CONSTANT K VALUE READ IN (',G10.4,
     1   ') IS LESS THAN ONE HALF OF THE COMPUTATIONAL INTERVAL. '
     1   'VALUE HAS BEEN SET TO ',G10.4,'.')
      CALL WARN
      P(IP)=SETK
      GO TO 440
350   IPT=IP+2*JK
      CALL CHECKP (IPT,LP,IER)
      IF (IER.EQ.1) GO TO 560
      IX=2*JK
      CALL FFRDRL (P(IP+1),IX,IBUG,IER,NCD7)
      IF (IER.EQ.1) GO TO 560
      JKK=JK-1
      IF (JKK.EQ.0) GO TO 380
      DO 370 NP=1,JKK
         L1=IP+NP*2
         L2=L1+2
         IF (IBUG.GT.1) WRITE (IPR,260) L1,L2,P(L1),P(L2)
         IF (P(L1).LE.P(L2)) GO TO 380
            WRITE (IPR,360)
360   FORMAT ('0**ERROR** DISCHARGES IN THE K VS Q TABLE MUST BE ',
     1 'IN ASCENDING ORDER.')
            CALL ERROR
            GO TO 380
370      CONTINUE
C
380   IF (LMETR.EQ.0) THEN
         DO 390 I=2,IX,2
            P(IP+I)=P(IP+I)/CFSM
390         CONTINUE
         ENDIF
C
      NPAIR=0
      DO 400 I=2,IX,2
         NPAIR=NPAIR+1
         IF (P(IP+I).LE.1.E6) GO TO 400
            WRITE (IPR,310) NPAIR,KWRD,P(IP+I)
            P(IP+I)=1.E6
            CALL WARN
400      CONTINUE
C
      IF (.NOT.MEANQ) GO TO 430
      IPS=IP+1
      XITAO2=XITA/2.
      KNT=0
      DO 420 I=IPS,IPT,2
         KNT=KNT+1
         IF (P(I).GE.XITAO2) GO TO 420
            WRITE (IPR,410) KNT,XITAO2
410   FORMAT ('0**WARNING** VARIABLE K FOR PAIR ',I4,
     1   ' IN THE K VS Q TABLE HAS BEEN RESET TO ',G10.4,'.'/
     2 11X,'THIS VALUE CANNOT BE LESS THAN ONE HALF THE ',
     3     'ROUTING INTERVAL WHEN PERIOD AVERAGE FLOW IS INPUT.')
            CALL WARN
         P(I)=XITAO2
420      CONTINUE
C
430   IP=IPT
C.......................................................................
C
C      READ IN CARRYOVER DEFAULT FLAG
C.......................................................................
C
440   P(16)=IP+.01
      IX=1
      CALL FFRDIN (NDFLT,IX,IBUG,IER,NCD7)
      IF (IER.EQ.1) GO TO 560
C
      IF (IBUG.GT.0) WRITE (IODBUG,*) 'NDFLT=',NDFLT
C
      IF (NDFLT.LT.0) THEN
         WRITE (IPR,450) NDFLT
450   FORMAT ('0**ERROR** DEFAULT CARRYOVER FLAG (',I2,
     *   ') CANNOT BE LESS THAN ZERO.')
         CALL ERROR
         GO TO 560
         ENDIF
C
      P(17)=NDFLT + .01
      IC=4
      IOPTL=0
      IF (FOP7(P(19),P(20)))IOPTL=1
      IF (IOPTL.NE.1) GO TO 530
      IF (IBUG.GT.0) WRITE (IODBUG,*) 'LAG OPTION ENABLED'
C.......................................................................
C
C     DETERMINE MAXIMUM LAG CARRYOVER
C.......................................................................
C
      IF (JLAG.EQ.0) GO TO 470
      X=-1.0
      DO 460 I=1,JLAG
         K=19+2*I-1
         IF (P(K).GT.X)X=P(K)
460      CONTINUE
      MXCOL=X/ITA + 2
      GO TO 480
470   MXCOL=P(20)/ITA + 2
480   IC=5+2*MXCOL
      CALL CHECKC(IC,LC,IER)
      IF (IER.EQ.1) GO TO 560
      C(5)=MXCOL + .01
      IF (NDFLT.GT.0) GO TO 490
C.......................................................................
C
C     FILL IN DEFAULT LAG CARRYOVER VALUES
C.......................................................................
C
      IX=2*C(5)
      CALL UMEMST (ZERO,C(6),IX)
      GO TO 530
C.......................................................................
C
C     DETERMINE IF NUMBER OF LAG CARRYOVER VALUES TO BE READ IN (NDFLT)
C     IS LEGAL
C.......................................................................
C
490   IF (NDFLT.GT.0.AND.NDFLT.LE.2*MXCOL) GO TO 510
C
      WRITE (IPR,500) NDFLT,MXCOL
500   FORMAT ('0**ERROR** NUMBER OF LAG CARRYOVER VALUES (',I5,
     *   ') MUST BE GREATER THAN ZERO AND LESS THAN OR EQUAL TO ',I5,
     *   '.')
      CALL ERROR
      GO TO 560
C.......................................................................
C
C     READ IN LAG CARRYOVER VALUES AND STORE IN C ARRAY
C.......................................................................
C
510   IX=NDFLT*2
      CALL FFRDRL (C(6),IX,IBUG,IER,NCD7)
      IF (IER.EQ.1) GO TO 560
C
      IF (LMETR.EQ.0) THEN
         DO 520 I=1,IX,2
            C(5+I)=C(5+I)/CFSM
520         CONTINUE
         ENDIF
C
C.......................................................................
C
C     DEFAULT LAG PORTION OF C ARRAY NOT READ IN ABOVE
C.......................................................................
C
      IFIL=2*C(5)-IX
      CALL UMEMST (ZERO,C(6+IX),IFIL)
C.......................................................................
C
C     DEFAULT K PORTION OF C ARRAY
C.......................................................................
C
530   IX=3
      CALL UMEMST (ZERO,C(2),IX)
      IOPTK=0

C dws    P(18) was placed into an integer to replace it in the next
C dws     statement to avoid compiler warnings ... 2006-01-23
 
         NUMP18 = P(18)

      IF (FOP7(P(NUMP18),P(NUMP18+1))) IOPTK=1
C
C  CHECK IF TO READ CARRYOVER VALUES
      IF (NDFLT.EQ.0) GO TO 550
C.......................................................................
C
C     IF K NOT ENABLED, READ ONLY ONE VALUE (BEGINNING OUTFLOW)
C.......................................................................
C
      IY=3
      IX=1
C.......................................................................
C
C     IF K ENABLED, READ 3 VALUES (BEGINNING INFLOW, BEGINNING OUTFLOW,
C     AND PREVIOUS OUTFLOW)
C.......................................................................
C
      IF (IOPTK.NE.1) GO TO 540
      IF (IBUG.GT.0) WRITE (IODBUG,*) 'K OPTION ENABLED'
      IY=2
      IX=3
      IF (.NOT.MEANQ) GO TO 540
C
C     IF PERIOD AVERAGE INFLOW ONLY READ IN TWO CARRYOVER VALUES
C     FOR LAG - (OUTFLOW FOR CURRENT PERIOD AND STORAGE)
C
      IY=3
      IX=2
540   CALL FFRDRL (C(IY),IX,IBUG,IER,NCD7)
      IF (IER.EQ.1) GO TO 560
C
      IF (LMETR.EQ.0) THEN
         C(2)=C(2)/CFSM
         C(3)=C(3)/CFSM
         C(4)=C(4)/CFDM
         ENDIF
C
550   C(1)=IC+.01
      IF (IATL.EQ.1.AND.IOPTK.EQ.1) THEN
         CALL PINA7 (P,LP,IP,C,LC,IC,IBUG,IER)
         ENDIF
      IF (IBUG.GT.0) CALL FPRPC7 (IP,P,IC,C)
      IF (IER.EQ.1) GO TO 560
      GO TO 580
C
C  GET HERE IF ERROR ENCOUNTERED PROCESSING INPUT
560   IP=0
      IC=0
C
      IF (IBUG.GT.0) WRITE (IPR,570)
570   FORMAT (' ERRORS ENCOUNTERED PROCESSING LAG/K ',
     1  'INPUT - IP AND IC SET TO ZERO')
C
580   RETURN
C
C.......................................................................
C
C     CONTENTS OF THE P ARRAY - LAG/K OPERATION
C
C     STARTING
C     POSITION                   CONTENTS
C     --------                   --------
C
C         1           VERSION NUMBER FOR THE OPERATION
C
C         2           8-CHARACTER NAME ID FOR INFLOW TIME SERIES
C
C         4           4-CHARACTER DATA TYPE FOR INFLOW TIME SERIES
C
C         5           TIME STEP IN HOURS FOR INFLOW TIME SERIES
C
C         6           8-CHARACTER NAME ID FOR OUTFLOW TIME SERIES
C
C         8           4-CHARACTER DATA TYPE FOR OUTFLOW TIME SERIES
C
C         9           TIME STEP IN HOURS FOR OUTFLOW TIME SERIES
C
C        10           FLAG TELLING IN WHICH UNITS (ENGLISH OR METRIC)
C                     DATA WERE ENTERED
C                       0 = DATA WERE ENTERED IN ENGLISH UNITS
C                       1 = DATA WERE ENTERED IN METRIC UNITS
C
C        11           TRANSMISSION LOSS RECESSION COEFFICIENT FOR
C                     FT. WORTH TRANSMISSION LOSS COMPUTATIONS.
C                       VALID RANGE GT 0 AND LT 1.
C                       TECHNIQUE IS OFF IF TLRC = 0.
C
C        12           FOR FT. WORTH TRANSMISSION LOSS COMPUTATIONS -
C                     FLOW BELOW WHICH THERE IS NO TRANSMISSION LOSS.
C                     (STORED IN P ARRAY AS CMS.)
C
C      13-15          RESERVED FOR FUTURE USE
C                      (PRESENTLY SET TO ZERO)
C
C        16           NUMBER OF VALUES IN THE P ARRAY
C                      (EXCLUDING THE SPACE NEEDED FOR ATLANTA K METHOD)
C
C        17           FLAG FOR DEFAULT CARRYOVER
C                      0 = DEFAULT CARRYOVER
C                      1 = READ SOME OR ALL OF CARRYOVER
C
C        18           LOCATION IN P ARRAY OF BEGINNING OF K INFORMATION
C
C        19           NUMBER OF PAIRS (LAG,Q) IN VARIABLE LAG TABLE
C                      0   = CONSTANT LAG
C                      > 0 = NUMBER OF PAIRS
C
C        20           IF P(19) 0 = CONSTANT LAG
C                     IF P(19) > 0, P(19) PAIRS (LAG,Q) IN VARIABLE
C                       LAG TABLE
C
C     P(18)           NUMBER OF PAIRS (K,Q) IN VARIABLE K TABLE
C                      0   = CONSTANT K
C                      > 0 = NUMBER OF PAIRS
C
C   P(18)+1           IF P(P(18)) 0 = CONSTANT K
C                     IF P(P(18)) GT 0, P(P(18)) PAIRS (K,Q) IN VARIABLE
C                       K TABLE
C
C      THE FOLLOWING VALUES ARE STORED IN THE P ARRAY ONLY FOR
C      THE ATLANTA METHOD OF COMPUTING K.  THAT IS ONLY IF IATL=1,
C      AND K IS ENABLED.
C
C   P(16)+1           THE NUMBER OF PAIRS OF VALUES IN THE
C                       O VS 2*S/DT+O TABLE
C
C   P(16)+2           P(P(16)+1) PAIRS (O,2*S/DT+O) IN THE TABLE
C
C P(16)+2 +
C 2*P(P(16)+1)        THE NUMBER OF PAIRS OF VALUES IN THE
C                       O VS 2*S/(DT/4)+O TABLE
C
C P(16)+2 +
C 2*P(P(16)+1) + 1    P(P(16)+2+2*P(P(16)+1)) PAIRS
C                       (O,2*S/(DT/4)+O) IN THE TABLE
C.......................................................................
C
C.......................................................................
C
C     CONTENTS OF C ARRAY - LAG/K OPERATION
C
C     THE FIRST LOCATION IN THE C ARRAY CONTAINS A SINGLE VALUE
C     SPECIFYING THE LENGTH OF THE C ARRAY.
C
C     THE NEXT THREE POSITIONS OF THE C ARRAY CONTAIN:
C
C     1. THE CURRENT LAGGED INFLOW.
C     2. THE CURRENT OUTFLOW.
C     3. THE CURRENT STORAGE.
C
C     IF K IS NOT ENABLED, THE INFLOW AND STORAGE
C     WILL BE SET TO ZERO, AND WILL NOT BE USED IN ANY CALCULATIONS.
C
C     IF K IS ENABLED AND PERIOD AVERAGE INFLOWS ARE USED
C     INFLOW IS SET TO ZERO, AND IS NOT USED IN ANY CALCULATIONS.
C
C     THE LAG PORTION OF THE CARRYOVER ARRAY BEGINS IN
C     POSITION 5 OF THE C ARRAY .
C     THE LAG PORTION OF THE C ARRAY CONTAINS:
C
C     1. A SINGLE VALUE WHICH SPECIFIES HOW MANY PAIRS (Q,T) OF LAG
C        CARRYOVER VALUES WILL FOLLOW.
C     2. THE LAG CARRYOVER VALUES.
C.......................................................................
C
      END
