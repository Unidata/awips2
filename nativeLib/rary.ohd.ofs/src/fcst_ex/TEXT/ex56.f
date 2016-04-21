      SUBROUTINE EX56(PO,CO,RM,GO,AF)
C...................................................
C     THIS IS THE EXCUTION ROUTINE FOR THE GLACIER
C     RUNOFF MODEL
C...................................................
C
      DIMENSION PO(*),CO(*),RM(*),GO(*),CT(2),AF(*)
      CHARACTER*8  SNAME
      REAL STORAGE,DSTORE,INFLOW,AFI,NUM,KG1,KG2,KG
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON /FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     &LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON /FCARY/ IFILLC,NCSTOR,ICDAY(20),ICHOUR(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex56.f,v $
     . $',                                                             '
     .$Id: ex56.f,v 1.3 2003/08/15 13:21:13 gzhou Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA  SNAME / 'EX56    ' /
C....................................................
      IBUG=1
C     TRACE LEVEL=1,DEBUG FLAG=IBUG
      CALL FPRBUG(SNAME,1,56,IBUG)
C....................................................
C     CONTROL STATEMENTS
      IVER=PO(1)
      IDT = PO(8)
      RIDT=IDT
      IJH=(IDA-1)*24+IHR
      LJH=(LDA-1)*24+LHR
      NPD=((LJH-IJH)/IDT)+1
      IHD=(IDADAT-1)*24+IDT
      IOFF=(IJH-IHD)/IDT
      CG1=PO(9)
      CG2=PO(10)
      CG3=PO(11)
      KG1=PO(12)
      KG2=PO(13)
      IF(IVER.EQ.1)IAF=O
      IF(IVER.EQ.2)IAF=1
      STORAGE=CO(1)
      AFI=CO(2)
      IC=1
      LCO=2
C....................................................
C     ADJUST PARAMETERS FOR TIME STEP
      KG1=1-((1.0-KG1)**(RIDT/24))
      KG2=1-((1.0-KG2)**(RIDT/24))
      CG3=CG3**(RIDT/24)
C....................................................
C     BEGIN MAIN COMPUTATION LOOP
      DO 200 N=1,NPD
        INFLOW=RM(IOFF+N)
        AFI=CG3*AFI+RM(IOFF+N)
        NUM=EXP(CG1+CG2*AFI)
        FAFI=NUM/(1+NUM)
        KG=KG1+(KG2-KG1)*FAFI
        IF(IAF.EQ.1)AF(IOFF+N)=FAFI
        GO(IOFF+N)=KG*STORAGE
        DSTORE=INFLOW-GO(IOFF+N)
        STORAGE=STORAGE+DSTORE
        CT(1)=STORAGE
        CT(2)=AFI
C       SAVE CARRYOVER IF REQUESTED
        IF(IFILLC.EQ.0)GOTO 200
        IF(NCSTOR.EQ.0) GOTO 200
        IF (IC.GT.NCSTOR) GOTO 200
        KJH=IJH+(N-1)*IDT
        KDA=((KJH-1)/24)+1
        KHR=KJH-((KDA-1)*24)
        IF ((KDA.EQ.ICDAY(IC)).AND.(KHR.EQ.ICHOUR(IC))) GOTO 170
        GOTO 200
 170    CALL FCWTCO(KDA,KHR,CT,LCO)
        IC=IC+1
 200  CONTINUE
C     END OF COMPUTATIONAL LOOP
C...................................
C     UPDATE CO ARRAY IF REQUESTED
      IF (IFILLC.EQ.0) GOTO 290
      CO(1)=STORAGE
      CO(2)=AFI
C...................................
C     DEBUG OUTPUT
 290  IF (IBUG.EQ.0) GOTO 295
      I1=IOFF+1
      I2=IOFF+NPD
      WRITE(IODBUG,901)
 901  FORMAT(1H0,'RAIN MELT OUTPUT TIME SERIES')
      WRITE(IODBUG,903) (RM(I),I=I1,I2)
 903  FORMAT(1H0,15F8.2)
      WRITE(IODBUG,905)
 905  FORMAT(1H0,'GLACIER OUTPUT TIME SERIES')
      WRITE(IODBUG,907) (GO(I),I=I1,I2)
 907  FORMAT(1H0,15F8.2)
      WRITE(IODBUG,906)
 906  FORMAT(1H0,'FAFI OUTPUT TIME SERIES')
      WRITE(IODBUG,908) (AF(I),I=I1,I2)
 908  FORMAT(1H0,15F8.2)

C................................................
 295  IF (ITRACE.GE.1) WRITE(IODBUG,909)
 909  FORMAT(1H0,'**EXIT EX56')
      RETURN
      END
