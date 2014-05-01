C----- THIS SUBPROGRAM DEALS WITH LEVEL POOL IN MUTIPLE ROUTINGS -------
      SUBROUTINE POOL55(PO,CO,Z,J,QU,QD,YU,YD,KRCH,IRT1,ST1,LTST1,
     . T1,LTT1,K1,K2,K7,K8,K9,K15,K16,K19,K20,K21)

      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'
      INCLUDE 'common/ionum'

      DIMENSION PO(*),CO(*),Z(*),ST1(*),LTST1(*),T1(*),LTT1(*)
      DIMENSION QU(K2,K1),QD(K2,K1),YU(K2,K1),YD(K2,K1),KRCH(K2,K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/pool55.f,v $
     . $',                                                             '
     .$Id: pool55.f,v 1.3 2004/02/02 21:50:15 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/'POOL55  '/
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      IF(IRT1.GT.1) GOTO 170
      TB=TT
      TA=TT-DT/3600.0
      IF (TA.LT.0.0) TA=0.0
      JA1=1
      JB1=1
      LT1=LTT1(J)-1
      DO 100 I=1,NU-1
      IF (T1(I+LT1).LE.TA) JA1=I
100   IF (T1(I+LT1).LE.TB) JB1=I 
      LST1=LTST1(J)-1
      JA2=JA1+1
      JB2=JB1+1
      TEMPA=(TA-T1(JA1+LT1))/(T1(JA2+LT1)-T1(JA1+LT1))
      TEMPB=(TB-T1(JB1+LT1))/(T1(JB2+LT1)-T1(JB1+LT1))
      QA=ST1(JA1+LST1)+TEMPA*(ST1(JA2+LST1)-ST1(JA1+LST1))
      QB=ST1(JB1+LST1)+TEMPB*(ST1(JB2+LST1)-ST1(JB1+LST1))
      QI=QA+QB           
      GOTO 180
170   QI=QU(IRT1,J)+QD(IRT1,J)
180   I=IRT1+1 
      IF (IRT1.EQ.1) I=1 
      YU(I+1,J)=YD(I+1,J)

      CALL IDDB55(I,J,PO(LONLAD),PO(LOLAD),KL,K1,K16)
      AC=1.0
190   YU(I,J)=YD(I,J)-0.02
      ITRS=1
      K=0
C------------------  SAR AND QDAM AT TD  -------------------------------
200   K=K+1

      IF (K.GE.10 .AND. AC.GE.0.99) THEN
      AC=0.1
      GOTO 190
      ELSE IF (K.GE.100) THEN
      WRITE(IPR,*) 'PROBLEM IN LEVEE POOL ITERATION'
      STOP
      ENDIF

      CALL DAM55(PO,CO,Z,ST1,LTST1,T1,LTT1,POLH,LTPOLH,ITWT,LTITWT,
     . Z(LZD),Z(LZC),Z(LZQU),Z(LZQD),Z(LZYU),Z(LZYD),PO(LOHDD),
     1 PO(LOHSPD),PO(LOHGTD),PO(LOCSD),PO(LOCGD),PO(LOZBCH),PO(LOCDOD),
     2 PO(LCNFLD),Z(LZBBP),Z(LZYBP),Z(LZSQS1),Z(LZSQS2),Z(LZSQO),
     3 Z(LZQBCH),Z(LZQOTP),Z(LZQOTR),KRCH,PO(LOQGH),PO(LOCLL),PO(LOSPL),
     4 PO(LORHI),PO(LORQI),PO(LOTIQH),CO(LXQDI),QN,DQNYU,DT,IFL,ITRS,
     5 II,II2,1,2,3,4,ITQ,I,J,K1,K2,K7,K8,K9,K15,K16,K19,K20,K21)
      IF (K.EQ.1) Q1=QN
      CALL RESSAR55(J,KL,YU(I,J),PO(LOSAR),PO(LOHSAR),SAU,DSAU,K1,K16)
      IF (K.EQ.1) SA1=SAU
C------------------  NEWTON ITERATION FOR YU(I,J) ---------------------- 
      YUK1=YU(I,J)
      DH=YU(I,J)-YD(I,J)
      F=QI-(Q1+QN)-(SA1+SAU)*43560.*DH/DT
      DF=-DQNYU-43560.0/DT*(SA1+SAU+DH*DSAU)
      YU(I,J)=YUK1-AC*F/DF
      ERO=ABS(YU(I,J)-YUK1)
      IF (ERO.GT.0.03) GOTO 200
      QU(I,J)=QN
      QU(I+1,J)=QN
      RETURN
      END
