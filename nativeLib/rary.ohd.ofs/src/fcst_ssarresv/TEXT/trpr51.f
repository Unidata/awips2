C MEMBER TRPR51
C DESC DRIVER FOR 3-VARIABLE TABLE LOOKUP
C
C@PROCESS LVL(77)
C
      SUBROUTINE TRPR51 (IP,FLIP,FLIS,FLIE,WK,NPA,IS,FA,FB)
CC
CC  KSH CHANGE START
CC  NOTE: THIS IS FROM SSARR ROUTINE TRPRT (IP,FLIP,FLIS,FLIE,IS,FA,FB)
CC    THE NAME OF ROUTINE, FEXTR, HAS BEEN CHANGED TO EXTR51
CC    THE NAME OF ROUTINE, STLU2, HAS BEEN CHANGED TO TLU251
CC    THE NAME OF ROUTINE, TLU3F, HAS BEEN CHANGED TO LU3F51
CC  KSH CHANGE END
CC
C Calls FEXAVE to return QON as start-of-period flow to give correct
C volume downstream from specified flows which contain values in the
C middle of a compute period. 9/13/90
C        TRANSFER POINT TYPE STATION- SUMMING POINT, INPUT POINT, LOCAL
C    CALCULATION, ADJACENT STATION, 3-DIMENSIONAL TABLE LOOKUP, AND/OR
C    SIGN REVERSAL.
C        GIVEN STATION INTERNAL NUMBER (IP), INSTANTANEOUS INFLOW AT
C    END OF PREVIOUS PERIOD (FLIP), START OF THIS PERIOD (FLIS), AND
C    PERIOD END (FLIE), CHARACTERISTIC RECORD (IS ARRAY), FILE IOTA/IOTB
C    RECORD AT START OF COMPUTE PERIOD (FA ARRAY), AND END OF PERIOD
C    (FB ARRAY), COMPUTE STATION DISCHARGE AND ELEVATION AT PERIOD-END
C    AND ENTER THEM IN FB ARRAY AND CHARACTERISTIC RECORD.
CC
CC  KSH CHANGE START
CC      INCLUDE 'C1.CM'
CC      INCLUDE 'STREAM.CM'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/sarr51'
C
CC      DIMENSION NPA (1)
CC      DIMENSION IS(*), FA(*), FB(*), R(5), XPA(1)
CC      EQUIVALENCE (V,IV),(ICS(1),NPA(1),XPA(1)),(Q,IQ),(E,IE)
      DIMENSION WK(*),NPA(*)
      DIMENSION IS(*), FA(*), FB(*), R(5)
      EQUIVALENCE (V,IV),(Q,IQ),(E,IE)
CC  KSH CHANGE END
CC
      EQUIVALENCE (ITO,XITO),(IT2,XIT2),(V2,IV2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ssarresv/RCS/trpr51.f,v $
     . $',                                                             '
     .$Id: trpr51.f,v 1.1 1996/03/21 13:43:41 page Exp $
     . $' /
C    ===================================================================
C
CC
CC  KSH CHANGE START
      NP=NRES
      IS(25)=0
CC  KSH CHANGE END
CC
      NC=0
      Q = FLIE
      QON=FA(1+2*NP+IP)
      isw1st=0
      XITO=FA(1)
      XIT2=FB(1)
      IH=IT2-ITO
C        TEST FOR REGULATION - SPECIFIED OUTFLOWS
 10   IF(IS(25)) 190,100,190
C        TEST FOR ADJACENT STATION
 100  IF(IS(2).NE.4) GO TO 500
      IF(IS(51)) 300, 110, 300
C        TEST FOR LOCAL CALCULATION
 110  IF(IS(50)) 400, 500, 400
C
C        GET VALUE OUT OF RECORD CODE 100 OR 101
 190  I=IS(25)
      IF(IH.EQ.0) THEN
C       EXTRACT REGULATION AT RUN START TIME (CALLED BY ICADJ)
          CALL EXTR51 (ITO,IH,NPA(I),V,NC,ITR)
      ELSE
C       CALLED BY ROUTER TO GET PERIOD-END REGULATION
          QO=QON
CC
CC  KSH CHANGE START
CC          CALL XAVE51(ITO,IH,NPA(I),QO,V,NC,ITR,QON)
CC  KSH CHANGE END
CC
      ENDIF
C        GO PROCESS BY DATA CODE OF REGULATION
      IF(NC-1) 100,200,196
C         DATA CODE NOT FREEFLOW OR FLOW; IS IT ELEVATION
 196  IF(NC.EQ.2) GO TO 210
C        NOT A VALID REGULATION FOR TRANSFER POINT.
C        USE INFLOW AS FLOW SPECIFICATION
      GO TO 500
C        FLOW SPECIFIED
 200  Q = V
      GO TO 500


C        ELEVATION SPECIFIED - TEST FOR Q VS E TABLE
 210  E=V
      IF(IS(28)) 230,600,230
C        LOOKUP Q
 230  I = IS(28)
      J = 2
 250  CALL TLU251 (IS(I),J,V,R)
      Q = R(1)
      E = V
      GO TO 600
C
C        TEST FOR C2 TABLE AVAILABLE.
 300  IF(IS(32).NE.0) GO TO 440
C        IS LOCAL STATION AVAIL. (IMPLYING LU3F51 PROCESS)
 302  IF(IS(50)) 405, 310, 405
C        ADJACENT STATION
 310  I = IS(51)
C        IF ADJ. STA. IS DOWNSTREAM, USE START-OF-PERIOD VALUE
      IF(IP.LT.IS(51).or.isw1st.ne.0) THEN
C        USE FLOW OR ELEVATION AS CONTROL
          IF(IS(57).EQ.2) THEN
C        ELEVATION
              E = FA(2*I+1)
              GO TO 320
          ELSE
              Q = FA(2*I)
              GO TO 330
          ENDIF
      ELSE
          IF(IS(57).EQ.2) THEN
              E = FB(2*I+1)
              GO TO 320
          ELSE
              Q = FB(2*I)
              GO TO 330
          ENDIF
      ENDIF
C
C        ELEVATION SPECIFIED - TEST FOR Q VS E TABLE
 320  IF(IS(28).EQ.0) GO TO 600
C        LOOKUP Q FOR PROCESSING WITH RELATION AND WEIGHT.
C        E WILL BE RECOMPUTED BASED ON ADJUSTED Q (AT LABEL 500)
          I = IS(28)
          J = 2
          CALL TLU251 (IS(I),J,E,R)
          Q = R(1)
C       IS A RELATION SPECIFIED?
 330  IF(IS(49)) 340, 350, 340
 340  I = IS(49)
      J = 1
      CALL TLU251 (NPA(I),J,Q,R)
      Q  = R(2)
C       MULTIPLY Q BY WEIGHT FROM CC02 CARD
 350  IV2=IS(47)
      IF(V2.NE.0.) Q = Q * V2/100.0
      GO TO 500
C
C        LOCAL CALCULATION
 400  I = IS(50)
CC
CC  KSH CHANGE START
CC  NOT USED!
CC      CALL INFLOW (I,fa,FB,qip, QUS, QU)
CC KSH CHANGE END
CC
      if(isw1st.eq.0) then
         Q = FB(2*I)
         Q = Q - QU
      else
c          calculate local at start of period for instantaneous
c          flow change.
         q = fa(2*i)
         q = q - qus
      endif
      GO TO 500
C         SETUP SCALING FOR LU3F51
C         PICKUP 1ST INDEPENDENT VARIABLE
 405  IF(IS(56)-2) 412, 414, 412
 412  SCALE = 1.
      J = 0
      GO TO 416
 414  SCALE = 100.0
      J = 1
 416  J = IS(50)*2 + J
C        IF LOCAL STA. IS DOWNSTREAM, USE START-OF-PERIOD VALUE
      IF(IP.LT.IS(50).or.isw1st.ne.0) THEN
         E = FA(J)*SCALE
      ELSE
         E = FB(J) * SCALE
      ENDIF
C         PICKUP 2ND INDEPENDENT VARIABLE
      IF(IS(57)-2) 418, 420, 418
 418  SCALE = 1.
      J = 0
      GO TO 422
 420  SCALE = 100.0
      J = 1
 422  J = IS(51)*2 + J
C        IF ADJ. STA. IS DOWNSTREAM, USE START-OF-PERIOD VALUE
      IF(IP.LT.IS(51).or.isw1st.ne.0) THEN
         E2 = FA(J)*SCALE
      ELSE
         E2 = FB(J) * SCALE
      ENDIF
      I = IS(49)
      CALL LU3F51 (E,E2,NPA(I+6),Q)
C         SCALE RESULT
      IF(IS(55)-2) 500,435,500
 435  V=Q/100.0
      GO TO 210
C        3-DIMENSION TLU
C        SETUP SCALING FOR TLU3V (C2 TABLE)
C         PICKUP 1ST INDEPENDENT VARIABLE
 440  IF(IS(56)-2) 442, 444, 442
 442  SCALE = 100.0
      J = 0
      GO TO 446
 444  SCALE = 1.
      J = 1
 446  J = IS(50)*2 + J
C        IF LOCAL STA. IS DOWNSTREAM, USE START-OF-PERIOD VALUE
      IF(IP.LT.IS(50).or.isw1st.ne.0) THEN
         E = FA(J)/SCALE
      ELSE
         E = FB(J)/SCALE
      ENDIF
C        PICKUP 2ND INDEPENDENT VARIABLE
      IF(IS(57)-2) 448, 450, 448
 448  SCALE = 100.0
      J = 0
      GO TO 452
 450  SCALE = 1.
      J = 1
 452  J = IS(51)*2 + J
C        IF ADJ. STA. IS DOWNSTREAM, USE START-OF-PERIOD VALUE
      IF(IP.LT.IS(51).or.isw1st.ne.0) THEN
         E2 = FA(J)/SCALE
      ELSE
         E2 = FB(J)/SCALE
      ENDIF
CC
CC  KSH CHANGE START
CC  NOT USED!
CC 4525 CALL TLU3V (E, E2, IS(IS(32)), Q)
CC KSH CHANGE END
CC
C        SCALE RESULT
 453  IF(IS(55)-2) 500, 454, 500
 454  V=Q/100.0
      GO TO 210
C
C        If calculating flow at start of next period, E not needed.
 500  if(isw1st.ne.0) go to 600
C        GET CORRESPONDING ELEVATION IF TABLE AVAILABLE
      IF(IS(28)) 520, 510, 520
 510  E = 0.
      GO TO 600
 520  I = IS(28)
      J = 1
      CALL TLU251  (IS(I),J,Q,R)
      E = R(2)
C
 600  CONTINUE
      IF(IS(2).eq.1.or.IS(2).eq.5) GO TO 607
 602  IF(IS(43)) 603, 607, 603
C        REVERSE SIGN OF OUTFLOW
 603  Q = -Q
 607  if(isw1st.eq.0) then
          FB(2*IP)   = Q
          FB(2*IP+1) = E
          FB(1+2*NP+IP) = Q
           IF(IH.EQ.0) RETURN
C      go back to top to compute Q at start of period if
C      instantaneous flow change and there is no 100 record.
          if(flis.ne.FLIP.and.is(25).eq.0) then
              isw1st=1
              Q = flis
              go to 10
          ELSE
              FA(1+2*NP+IP) = QON
          endif
      ELSE
          FA(1+2*NP+IP) = Q
      endif
 620  RETURN
      END
