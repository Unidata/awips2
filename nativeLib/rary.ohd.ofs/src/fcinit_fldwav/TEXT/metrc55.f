      SUBROUTINE METRC55(Z,KU,KD,NQCM,EPQJ,KFLP,KCG,NCG,NLEV,NPOND,
     . NP,NCS,IOBS,K1)

C  THIS SUBROUTINE CONVERTS ALL METRIC UNITS TO ENGLISH

      COMMON/M3055/EPSY,EPSQ,EPSQJ,THETA,XFACT
      COMMON/UNTS55/DSTNCE,XLNGTH,FLOW,SAREA,VOLUME,VELCTY,BSLOPE
      COMMON/MXVAL55/MXNB,MXNGAG,MXNCM1,MXNCML,MXNQL,MXINBD,MXRCH,
     $               MXMGAT,MXNXLV,MXROUT,MXNBT,MXNSTR,MXSLC

      INCLUDE 'common/ofs55'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'

      DIMENSION Z(*),NQCM(K1),KU(K1),KD(K1),EPQJ(K1)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/metrc55.f,v $
     . $',                                                             '
     .$Id: metrc55.f,v 1.3 2000/09/27 10:58:59 page Exp $
     . $' /
C    ===================================================================
C
      DATA  SNAME / 'METRC55 ' /

      CALL FPRBUG(SNAME,1,55,IBUG)

CC      WRITE(IPR,1)
CC    1 FORMAT(1X,'** ENTER METRC **')

      UYQCM=1.

C CONVERT TOLERANCES

      EPSY=EPSY*3.281
      EPSQ=EPSQ*35.32
      XFACT=XFACT/1609.265
CC      XFACT=(XFACT/1.6093)*5280.
CC      XFACT=5280.
      XICD=ICD/100*3.281
      ICD=XICD+0.5

CC      DO 50 J=1,JN
CC      EPQJ(J)=EPQJ(J)*35.32
CC   50 CONTINUE
CC      IF(JN.EQ.1) THEN
CC        EPSQJ=EPSQ
CC      ELSE
CC        EPSQJ=EPQJ(2)
CC      ENDIF

C
C  CONVERT RIVER LOCATIONS AND DISTANCE INTERVALS

      L1=LOXT
      L2=LORCHT-1
      DO 100 I=L1,L2
       Z(I)=Z(I)*0.6214
  100 CONTINUE

C CONVERT DAM/BRIDGE INFO

      IF(MXINBD.GT.0) THEN
        LEND=JN*MXINBD
        DO 150 L=1,LEND
        Z(LOHDD+L-1)=Z(LOHDD+L-1)*3.281
        Z(LOCLL+L-1)=Z(LOCLL+L-1)*3.281
        Z(LOCDOD+L-1)=Z(LOCDOD+L-1)*1.811
        Z(LOQTD+L-1)=Z(LOQTD+L-1)*35.32
        Z(LOHSPD+L-1)=Z(LOHSPD+L-1)*3.281
        Z(LOSPL+L-1)=Z(LOSPL+L-1)*3.281
        Z(LOCSD+L-1)=Z(LOCSD+L-1)*1.811
        Z(LOHGTD+L-1)=Z(LOHGTD+L-1)*3.281
        Z(LOCGD+L-1)=Z(LOCGD+L-1)*10.764
        DO 120 K=1,8
        ICNT=(L-1)*8
        Z(LOSAR+K+ICNT-1)=Z(LOSAR+K+ICNT-1)*247.099
        Z(LOHSAR+K+ICNT-1)=Z(LOHSAR+K+ICNT-1)*3.281
        Z(LOHCRL+K+ICNT-1)=Z(LOHCRL+L-1)*3.281
        Z(LOCRL+K+ICNT-1)=Z(LOCRL+K+ICNT-1)*3.281
        Z(LORHI+K+ICNT-1)=Z(LORHI+K+ICNT-1)*3.281
        Z(LORQI+K+ICNT-1)=Z(LORQI+K+ICNT-1)*35.32
        Z(LOBRBS+K+ICNT-1)=Z(LOBRBS+K+ICNT-1)*3.281
        Z(LOBRHS+K+ICNT-1)=Z(LOBRHS+K+ICNT-1)*3.281
        Z(LOEMBE+K+ICNT-1)=Z(LOEMBE+K+ICNT-1)*3.281
        Z(LOEMBW+K+ICNT-1)=Z(LOEMBW+K+ICNT-1)*3.281
  120   CONTINUE
        DO 130 K=1,KCG
        ICNT=(L-1)*8
        Z(LOQGH+K+ICNT-1)=Z(LOQGH+K+ICNT-1)*35.32
        Z(LOCGCG+K+ICNT-1)=Z(LOCGCG+K+ICNT-1)*3.281
  130   CONTINUE
        DO 140 K=1,64
        ICNT=(L-1)*64
        Z(LOQHT+K+ICNT-1)=Z(LOQHT+K+ICNT-1)*35.32
  140   CONTINUE
        Z(LOHFDD+L-1)=Z(LOHFDD+L-1)*3.281
        Z(LOBBD+L-1)=Z(LOBBD+L-1)*3.281
        Z(LOYMIN+L-1)=Z(LOYMIN+L-1)*3.281
        Z(LOCPIP+L-1)=Z(LOCPIP+L-1)*3.281
        Z(LOEBE1+L-1)=Z(LOEBE1+L-1)*3.281
        Z(LOEBE2+L-1)=Z(LOEBE2+L-1)*3.281
        Z(LOEBW1+L-1)=Z(LOEBW1+L-1)*3.281
        Z(LOEBW2+L-1)=Z(LOEBW2+L-1)*3.281
        Z(LOBRGW+L-1)=Z(LOBRGW+L-1)*3.281
        Z(LOCDBR+L-1)=Z(LOCDBR+L-1)*1.811
        Z(LCTOPN+L-1)=Z(LCTOPN+L-1)*3.281
  150   CONTINUE
      ENDIF
      IF(MXMGAT.GT.0) THEN
        L1=LOGSIL
        L2=LOTGHT-1
        DO 152 L=L1,L2
          Z(L)=Z(L)*3.281
  152   CONTINUE
        L1=LOGHT
        L2=LCNFLD-1
        DO 154 L=L1,L2
          Z(L)=Z(L)*3.281
  154   CONTINUE
      ENDIF
C
      IF(NLEV.GT.0) THEN
        DO 200 L=1,MXNXLV
        Z(LOHWLV+L-1)=Z(LOHWLV+L-1)*3.281
        Z(LOWCLV+L-1)=Z(LOWCLV+L-1)*1.811
        Z(LOBLMX+L-1)=Z(LOBLMX+L-1)*3.281
        Z(LOHFLV+L-1)=Z(LOHFLV+L-1)*3.281
        Z(LOHLMN+L-1)=Z(LOHLMN+L-1)*3.281
        Z(LOHPLV+L-1)=Z(LOHPLV+L-1)*3.281
        Z(LODPLV+L-1)=Z(LODPLV+L-1)*3.281
  200  CONTINUE

       IF(NPOND.GT.0) THEN
         DO 205 L=1,NPOND
           Z(LOHPND+L-1)=Z(LOHPND+L-1)*3.281
           LSAP=LOSAP+(L-1)*8-1
           LHSAP=LOHSAP+(L-1)*8-1
           DO 202 KK=1,8
             Z(LHSAP+KK)=Z(LHSAP+KK)*3.281
             Z(LSAP+KK)=Z(LSAP+KK)*247.099
  202      CONTINUE
  205    CONTINUE
       ENDIF
      ENDIF

C  CONVERT CROSS SECTION PROPERTIES

      L1=LOFLST
      L2=LXQDI-1
      DO 210 I=L1,L2
      Z(I)=Z(I)*3.281
  210 CONTINUE

      L1=LXQDI
      L2=LOAS-1
      DO 220 I=L1,L2
      Z(I)=Z(I)*35.32
  220 CONTINUE

      L1=LOAS
      L2=LOHS-1
      DO 230 I=L1,L2
      Z(I)=Z(I)*10.765
  230 CONTINUE

      L1=LOHS
      L2=LOBSS+JN*MXNB*NCS-1
      DO 240 I=L1,L2
      Z(I)=Z(I)*3.281
  240 CONTINUE

      IF(KFLP.EQ.1) THEN
        L1=LOBSL
        L2=LOASL-1
        DO 250 I=L1,L2
        Z(I)=Z(I)*3.281
  250   CONTINUE

        L1=LOASL
        L2=LOASR+JN*MXNB*NCS-1
        DO 260 I=L1,L2
        Z(I)=Z(I)*10.765
  260   CONTINUE
      ENDIF

      IF(KFLP.GE.1) THEN
        N=30*MXNB*JN
        DO 270 I=1,N
        Z(LOHKC+I-1)=Z(LOHKC+I-1)*3.281
        Z(LOQKC+I-1)=Z(LOQKC+I-1)*35.32
  270   CONTINUE
      ENDIF

      IF(NP.EQ.-3.OR.NP.EQ.-4) THEN
        N=MXNGAG*JN
        DO 280 I=1,N
        Z(LOFKC+I-1)=Z(LOFKC+I-1)*3.281
        Z(LOFKF+I-1)=Z(LOFKF+I-1)*3.281
        Z(LOFKO+I-1)=Z(LOFKO+I-1)*3.281
  280   CONTINUE
      ENDIF

      DO 290 J=1,JN
      Z(LOVWND+J-1)=Z(LOVWND+J-1)*3.281
  290 CONTINUE


      N=MXNCM1*MXNCML
      DO 310 J=1,JN
        FAC=3.281
        IF(NQCM(J).LT.0) FAC=35.32
        ICNT=(J-1)*N
        DO 300 I=1,N
        Z(LOYQCM+ICNT+I-1)=Z(LOYQCM+ICNT+I-1)*FAC
  300   CONTINUE
        Z(LOVWND+J-1)=Z(LOVWND+J-1)*3.281
  310 CONTINUE

C  CONVERT GENERATED INFLOW PARAMETERS
      IF(IOBS.LT.0) THEN
        L1=LOYQI
        L2=LOYQI+JN-1
        DO 312 L=L1,L2
          Z(L)=Z(L)*35.32
  312   CONTINUE
      ENDIF

      IF(ITRACE.EQ.1) WRITE(IODBUG,9010) SNAME
 9010 FORMAT(1H0,'** ',A8,' EXITED.')
      RETURN
      END




