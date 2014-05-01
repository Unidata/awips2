C MODULE EX55
C-----------------------------------------------------------------------
C
C  THIS IS THE MAIN ROUTINE FOR THE FLDWAV OPERATION WHICH IS TO
C  DYNAMICALLY ROUTE AN INFLOW HYDROGRAPH DOWNSTREAM BY USING THE
C  UNSTEADY FLOW EQATIONS.
C
      SUBROUTINE EX55(PO,IPO,CO,FFS,FS,QA,TII,YA,T1,LTT1,STN,LTSTN,QLJ,
     2 LTQLJ,ST1,LTST1,XITWT,LTXIWT,POLHS,LTPOLHS,QL,LTQL,QUSJ,LTQUSJ,
     3 QTC,LTQTC,STC,LTSTC,STT,LTSTT,STQ,LTSTQ,QSTR,LTQSR,ITWT,LTITWT,
     4 POLH,LTPOLH,TO,PLTIM,XNOS,LTNOS,TIDE,LTTID,STE,LTSTE,IRF,NB,
     5 Z,LEFTZ)
C
C  ROUTINE WAS WRITTEN ORIGINALLY BY: JANICE LEWIS - HRL - 12/1998
C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C     add call to a copy routine (cpymap55) to copy output flood map
C     data files to all necessary directories
C
      CHARACTER*4 JUNK(7),FGRF(3),FMAP(2)
      CHARACTER*20 FILNAM
      CHARACTER*150 FILANIM,PATHMP
      COMMON /FCTIME/ IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     1               LOCAL,NOUTZ,NOUTDS,NLISTZ,IDA,IHR,LDA,LHR,IDADAT
C jgg added to fix problem writing dates in esp mode - 12/2/04
      COMMON /ETIME/IJDFC,IHFC,IJDLST,IHLST,LJDLST,LHLST,ISHIFT,IHYR,
     1 LHYR,IWJD(5),IWH(5),LWJD(5),LWH(5),IBHYR,LBHYR,FLOCAL,NCM
C jgg    
      INCLUDE 'common/fratng'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
      INCLUDE 'common/ofs55'
      INCLUDE 'common/opfil55'
      INCLUDE 'common/fldmap55'
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FNOPR/NOPROT
      COMMON/FPLTAB/IPLHY,IPRHY
      COMMON/FCARY/IFILLC,NCSTOR,ICDAY(20),ICHOUR(20)
      COMMON/ERR55/IERR
      COMMON/IDOS55/IDOS,IFCST
      COMMON/FLP55/KFLP
      COMMON/GT55/KCG,NCG
      COMMON/KREV55/KREVRS
      COMMON/LEV55/NLEV,DHLV,NPOND,DTHLV,IDTHLV
      COMMON/METR55/METRIC
      COMMON/MIXX55/MIXFLO,DFR,FRC
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M655/KTIME,DTHYD,J1
      COMMON/M3055/EPSY,EPSQ,EPSQJ,THETA,XFACT
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/M3455/KXP,ICD,ITMAX,KWARM
      COMMON/NPC55/NP,NPST,NPEND
      COMMON/NYQDC55/NYQD
      COMMON/PRES55/KPRES
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/TDBG55/TDBG1,TDBG2,JNKDBG,JDBG1,JDBG2,LDBG1,LDBG2,MCMDBG
      COMMON/TPL55/DTHPLT
      COMMON/VS55/MUD,IWF,SHR,VIS,UW,PB,SIMUD
      COMMON/XNGZ55/NGZ,NGZN
      COMMON/TKEP55/DTHII,MDT,NDT,DTHS,TFH1
      COMMON/DTBRK55/DTFMN
      COMMON/NETWK55/NET
cc      COMMON/FLDMAP55/NMAP,FILANIM,FILNAM,MPTIM

      DIMENSION PO(*),IPO(*),CO(*),FFS(*),FS(*),TII(*),QA(*),TO(*)
      DIMENSION YA(*),STN(*),QLJ(*),LTQLJ(*),ST1(*),LTST1(*)
      DIMENSION ITWT(*),LTITWT(*),POLH(*),LTPOLH(*),QL(*),LTQL(*)
      DIMENSION QTC(*),LTQTC(*),STC(*),LTSTC(*),STT(*),LTSTT(*)
      DIMENSION QSTR(*),LTQSR(*),XITWT(*),LTXIWT(*),QUSJ(*),LTQUSJ(*)
      DIMENSION POLHS(*),LTPOLHS(*),STQ(*),LTSTQ(*),T1(*),LTT1(*)
      DIMENSION XNOS(*),TIDE(*),STE(*),LTSTE(*),IRF(*)
      DIMENSION Z(*),PLTIM(*),NB(*)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/ex55.f,v $
     . $',                                                             '
     .$Id: ex55.f,v 1.12 2005/01/24 21:21:09 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/ 'EX55    '    /
      DATA JUNK/'JNK0','JNK1','JNK2','JNK4','JNK5','JNK9','JNKX'/
      DATA FGRF/'FGON','FGOF','FAON'/
      DATA FMAP/'FVON','FVOF'/
C
C
      CALL FPRBUG(SNAME,1,55,IBUG)
      IERR=0
C
C          DUMP THE PO ARRAY
C

      G=32.2
      IVER  =PO(1)
      IUSEP =PO(2)
      IUSED =PO(4)
      IDOS  =PO(9)
      EPSY  =PO(11)
      THETA =PO(12)
      F1    =PO(13)
      XFACT =PO(14)
      DTHYD =PO(15)
      IDHF  =DTHYD+0.01
      METRIC=PO(16)
      JN    =PO(17)
      NU    =PO(18)
      ITMAX =PO(19)
      KWARM =PO(20)
      KFLP  =PO(21)
      NET   =PO(22)
      DTOUT =PO(23)
      NDHF  =DTOUT+0.01
      KSTG  =PO(24)
      ICOND =PO(25)
      NODESC=PO(26)
      NYQD  =PO(27)
      NRC=0
      IF(NYQD.NE.0) NRC=NYQD
      KCG   =PO(28)
      NCG   =PO(29)
      KPRES =PO(30)
      NCS   =PO(31)
      KPL   =PO(32)
      KPL=IABS(KPL)
      JNK   =PO(33)
      IF(IFBUG(JUNK(1)).EQ.1) JNK=0
      IF(IFBUG(JUNK(2)).EQ.1) JNK=1
      IF(IFBUG(JUNK(3)).EQ.1) JNK=2
      IF(IFBUG(JUNK(4)).EQ.1) JNK=4
      IF(IFBUG(JUNK(5)).EQ.1) JNK=5
      IF(IFBUG(JUNK(6)).EQ.1) JNK=9
      IF(IFBUG(JUNK(7)).EQ.1) JNK=119
      KREVRS=PO(34)

C  CHECK IF FLOOD ANIMATION DATA IS TO BE STORED
      NMAP=PO(375)
      K30=NMAP
      IF(K30.EQ.0) K30=1
      MPFRST=0
      MPTIM=0
      LODTMP=PO(377)
      IF(NMAP.GT.0) MPTIM=PO(LODTMP)+0.1
      LOSPTH=PO(379)
      LOTPTH=PO(380)
C
C  CHECK IF TO OUTPUT TO FLDGRF FILES
      NFGRF =PO(35)
      IF(IFBUG(FGRF(1)).EQ.1) NFGRF=0
      IF(IFBUG(FGRF(2)).EQ.1) NFGRF=1
C
C  CHECK IF TO OUTPUT TO FLDAT FILES
      IF(IFBUG(FGRF(3)).EQ.1) NFGRF=2


      IF(MAINUM.EQ.2.AND.NFGRF.NE.1) THEN
        NFGRF=1
        WRITE(IPR,9984)
 9984   FORMAT(//'**NOTE** RUNNING ESP ... FLDGRF/FLDAT OPTIONS TURNED',
     .           ' OFF.')
      ENDIF

      IOPNERR=0
      IF(NFGRF.NE.1) THEN
        IF(IOPFIL55.EQ.1) THEN
          IF(NFGRF.EQ.0) WRITE (IPR,9987)
 9987 FORMAT ('0**WARNING** IN EX55 - FLDGRF FILES ALREADY OPENED. ',
     * 'ADDITIONAL OUTPUT WILL NOT BE WRITTEN.')
          IF(NFGRF.EQ.2) WRITE (IPR,9983)
 9983 FORMAT ('0**WARNING** IN EX55 - FLDAT FILES ALREADY OPENED. ',
     * 'ADDITIONAL OUTPUT WILL NOT BE WRITTEN.')
          CALL WARN
          IOPNERR=1
        ELSE
          CALL OPNFIL55(IOPNERR)
          IF(IOPNERR.EQ.0) THEN
            IOPFIL55=1
          ELSE
            NFGRF=1
            WRITE (IPR,9986) NFGRF
 9986 FORMAT ('0**WARNING** IN EX55 - FLDGRF/FLDAT FILES NOT',
     * ' SUCCESSFULLY OPENED. NFGRF OPTION SET TO ',I2,'.')
            CALL WARN
            IOPNERR=1
          ENDIF
        ENDIF
      ENDIF
C
C  CHECK IF TO OUTPUT TO FLDVIEW FILES
      NFMAP =PO(375)
      IF(NFMAP.GT.1) NFMAP=1
      IF(IFBUG(FMAP(1)).EQ.1) NFMAP=1
      IF(IFBUG(FMAP(2)).EQ.1) NFMAP=0
      IOPNMAP=0
C jgg added the following for MR 1954 to prevent writing flood map files 
C     during an IFP run.
      IF(IFP55.EQ.1) THEN
        NFMAP=0
	IOPNMAP=1
      ENDIF
C end of jgg changes      
      IF(NFMAP.EQ.1) THEN
        IF(IOPFMP55.EQ.1) THEN
          WRITE (IPR,9985)
 9985     FORMAT (/'**WARNING** IN EX55 - FLDVIEW FILES ALREADY ',
     *             'OPENED. ADDITIONAL OUTPUT WILL NOT BE WRITTEN.')
          CALL WARN
          IOPNMAP=1
        ELSE
          CALL OPNMAPF55(IOPNMAP,PO(LOSPTH),PO(LOTPTH),K30)

          IF(MAINUM.EQ.2) THEN 
C jgg modified to fix problem writing dates in esp mode - 12/2/04
C jgg            CALL MDYH1(IDA,IHR,K1MO,K1DA,K1YR,K1HR,NOUTZ,NOUTDS,
C jgg     .                 ZONE)
            CALL MDYH1(IJDLST,IHLST,K1MO,K1DA,K1YR,K1HR,NOUTZ,NOUTDS,
     .                 ZONE)
            CALL MDYH1(LJDLST,LHLST,K2MO,K2DA,K2YR,K2HR,NOUTZ,NOUTDS,
     .      	    ZONE)
          ELSE
            CALL MDYH1(LDACPD,LHRCPD,K1MO,K1DA,K1YR,K1HR,NOUTZ,NOUTDS,
     .                 ZONE)
            CALL MDYH1(LDA,LHR,K2MO,K2DA,K2YR,K2HR,NOUTZ,NOUTDS,ZONE)
          ENDIF
C jgg          CALL MDYH1(LDA,LHR,K2MO,K2DA,K2YR,K2HR,NOUTZ,NOUTDS,ZONE)
          WRITE(JFDAT,10) K1MO,K1DA,K1YR,K1HR,K2MO,K2DA,K2YR,K2HR
C jgg end of fix for problem writing dates in esp mode
 10       FORMAT(2(2I2.2,I4,I4.4,1X))
          IF(MPTIM.GT.0) THEN
            CALL MDYH1(IDA,IHR,K1MO,K1DA,K1YR,K1HR,NOUTZ,NOUTDS,
     .                 ZONE)
            WRITE(JFDAT2,10) K1MO,K1DA,K1YR,K1HR,K2MO,K2DA,K2YR,K2HR
          ENDIF
          IF(IOPNMAP.EQ.0) THEN
            IOPFMP55=1
          ELSE
            NFMAP=1
            WRITE (IPR,9989) NFMAP
 9989 FORMAT ('0**WARNING** IN EX55 - FLDVIEW FILES NOT SUCCESSFULLY ',
     *        'OPENED. NFMAP OPTION SET TO ',I2,'.')
            CALL WARN
            IOPNMAP=1
          ENDIF
        ENDIF
      ENDIF

      IOBS  =PO(36)
      KTERM =PO(37)
      NP    =PO(38)
      NPST  =PO(39)
      NPEND =PO(40)
      TDBG1 =PO(41)
      TDBG2 =PO(42)
      JNKDBG=PO(43)
      JDBG1 =PO(44)
      JDBG2 =PO(45)
      LDBG1 =PO(46)
      LDBG2 =PO(47)
      MCMDBG=PO(48)
      NMSG  =PO(49)
      TEH   =PO(50)
      DTHII =PO(51)
      DTHPLT=PO(52)
      FRDFR =PO(53)
      DTEXP =PO(54)
      MDT   =PO(55)
      NDT   =PO(56)
      NNLEV =PO(59)
      NLEV  =PO(60)
      DHLV  =PO(61)
      DTHLV =PO(62)
      NLPI  =PO(97)
      NLOS  =PO(98)
      K29   =PO(100)
      K1    =PO(101)
      K2    =PO(102)
      K3    =PO(103)
      K4    =PO(104)
      K5    =PO(105)
      K6    =PO(106)
      K7    =PO(107)
      K8    =PO(108)
      K9    =PO(109)
      K10   =PO(110)
      K11   =PO(111)
      K12   =PO(112)
      K13   =PO(113)
      K14   =PO(114)
      K15   =PO(115)
      K16   =PO(116)
      K17   =PO(117)
      K18   =PO(118)
      K19   =PO(119)
      K20   =PO(120)
      K21   =PO(121)
      K22   =PO(122)
      K23   =PO(123)
      K24   =PO(124)
      K25   =PO(125)
      DTI   =PO(133)
      NPOND =PO(148)
cc      GZN   =PO(222)
      DSTNCE=PO(300)
      XLNGTH=PO(301)
      FLOW  =PO(302)
      SAREA =PO(303)
      VELCTY=PO(304)
      VOLUME=PO(305)
      BSLOPE=PO(306)
      DTHS=PO(308)
      TFH1=PO(309)
      NTQL  =PO(313)
      NSECT =PO(315)
      LOOPKD=PO(320)
      NLOCK =PO(321)
      NTGAG =PO(323)
      NTOUT =PO(331)
      MXINBD=PO(378)
      K26=NLOCK
C jgg added 7/04 for HSD r23-48
      IF(K26.EQ.0) K26=1
      K27=NTOUT
      IF(K27.EQ.0)K27=1
      K28=NTGAG
      IF(K28.EQ.0)K28=1
      K30=NMAP
      IF(K30.EQ.0)K30=1
C
C          DUMP THE CO ARRAY
C
      LXYDI =PO(316)
      LXQDI =PO(317)
      LXQLI =PO(318)
      LXPLTI=PO(319)
      LXIWTI=PO(364)
      JSIZE=PO(3)

C
C          STORE IN THE D ARRAY THE FOLLOWING PARAMETERS:
C
C             1 ---  LZC            51 ---  LZTFDO      101 --- LZYQR
C             2 ---  LZD            52 ---  LZQDSN      102 --- LZFYPK
C             3 ---  LZFHT          53 ---  LZHDSN      103 --- LZFQPK
C             4 ---  LZICT          54 ---  LZSQW       104 --- LZFTYPK
C             5 ---  LZIPSV         55 ---  LZSQS1      105 --- LZFTQPK
C             6 ---  LZIRSV         56 ---  LZSQS2
C             7 ---  LZITRX         57 ---  LZSQO
C             8 ---  LZMINT         58 ---  LZLRMX
C             9 ---  LZNDPS         59 ---  LZICTR
C            10 ---  LZNSTR         60 ---  LZYUMN
C            11 ---  LZQC           61 ---  LZIFR
C            12 ---  LZQD           62 ---  LZKSP
C            13 ---  LZQI           63 ---  LZKS1
C            14 ---  LZQII          64 ---  LZKSN
C            15 ---  LZQJ           65 ---  LZYN
C            16 ---  LZQU           66 ---  LZYCR
C            17 ---  LZSTYP         67 ---  LZFLAG
C            18 ---  LZVC           68 ---  LZWDSN
C            19 ---  LZVD           69 ---  LZTYPK
C            20 ---  LZVU           70 ---  LZYPK
C            21 ---  LZXX           71 ---  LZTQPK
C            22 ---  LZYC           72 ---  LZQPK
C            23 ---  LZYD           73 ---  LZQLV
C            24 ---  LZYII          74 ---  LZQPND
C            25 ---  LZYJ           75 ---  LZPLTM
C            26 ---  LZYU           76 ---  LZQMX
C            27 ---  LZQINT         77 ---  LZYMX
C            28 ---  LZYINT         78 ---  LZRMS
C      x     29 ---  LZFFS       x  79 ---  LZAVD
C      x     30 ---  LZFS           80 ---  LZNFLO
C      x     31 ---  LZITSV         81 ---  LZVPK
C      x     32 ---  LZQA           82 ---  LZBKT
C      x     33 ---  LZQLJ          83 ---  LZQKT
C      x     34 ---  LZQTC          84 ---  LZERQX
C      x     35 ---  LZSTC          85 ---  LZSNMT
C      x     36 ---  LZTII          86 ---  LCTFT
C      x     37 ---  LZVLSV         87 ---  LCY1FT
C      x     38 ---  LZYA           88 ---  LCY2FT
C            39 ---  LZQLLT         89 ---  LCPFT
C            40 ---  LZQLSM         90 ---  LCQFT
C            41 ---  LZDHEQ         91 ---  LCHFT
C            42 ---  LZNQRT         92 ---  LCRFT
C       x    43 ---  LZQLRT         93 ---  LCZFT
C            44 ---  LZBBP          94 ---  LCCFT
C            45 ---  LZYBP          95 ---  LCDFT
C            46 ---  LZQBCH         96 ---  LCEFT
C            47 ---  LZQOTP         97 ---  LCVFT
C            48 ---  LZQOTR         98 ---  LCAKFT
C            49 ---  LZTFDB         99 ---  LCEXPP
C            50 ---  LZIORF        100 ---  LCEXPR
C
C
cc      DO 25 I=1,LEFTZ
cc        Z(I)=0.
cc 25   CONTINUE

      CALL SIZE55(PO,IUSEZ,LEFTZ,LIMIT,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,
     * K11,K12,K13,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K28,K29,
     * K30)


      IF(IUSEZ.GT.LEFTZ) THEN
        WRITE(IPR,9988) IUSEZ,LEFTZ
 9988   FORMAT('0**ERROR** SPACE NEEDED:',I8,' SPACE AVAILABLE:',
     . I8,5X,'** NOT ENOUGH SPACE TO FILL ARRAYS..CONTACT YOUR FOCAL ',
     . 'POINT AT HRL'/25X,'THE FLDWAV OPERATION WILL BE SKIPPED.'/)
       CALL ERROR
       GO TO 9500
      ENDIF

      EPSQ=PO(LOEPQJ)
C
C
C    SET EXCEEDED TABLE COUNTERS TO ZERO
C
      NBDXCD=0
      NCMXCD=0
      NICXCD=0
      NFRXCD=0
      NINXCD=0
C.......................................................................
C  compute the starting, current, and ending times
C
      CALL MDYH1(IDA,IHR,KMO,KDA,KYR,KHR,NOUTZ,NOUTDS,ZONE)

      INOW=(LDACPD-1)*24+LHRCPD
      ISTRT=(IDA-1)*24
c=========
c  compute offset for mcp and esp runs - all of these offsets must be undone
      IOFFSET=(IDA-IDADAT)*24/IDHF+(IHR-1)/IDHF+1
c=========
c  adjust the locations in the d array for the input data.

      LTSTN=LTSTN+IOFFSET-1
      LTNOS=LTNOS+IOFFSET-1
      LTTID=LTTID+IOFFSET-1

      IF(JN.GT.0)THEN
      DO 400 IE=1,JN
       LTST1(IE)=LTST1(IE)+IOFFSET-1
       LTQLJ(IE)=LTQLJ(IE)+IOFFSET-1
       LTQUSJ(IE)=LTQUSJ(IE)+IOFFSET-1
400   CONTINUE
      ENDIF

      IF(NLOCK.GT.0)THEN
      DO 401 IE=1,NLOCK
       LTXIWT(IE)=LTXIWT(IE)+IOFFSET-1
       LTPOLHS(IE)=LTPOLHS(IE)+IOFFSET-1
       LTITWT(IE)=LTITWT(IE)+IOFFSET-1
       LTPOLH(IE)=LTPOLH(IE)+IOFFSET-1
401   CONTINUE
      ENDIF

      IF(NTQL.GT.0)THEN
      DO 402 IE=1,NTQL
       LTQL(IE)=LTQL(IE)+IOFFSET-1
cc       LOQLT(IE)=LOQLT(IE)+IOFFSET-1
402   CONTINUE
      ENDIF

      IF(NTGAG.GT.0)THEN
      DO 403 IE=1,NTGAG
       LTQTC(IE)=LTQTC(IE)+IOFFSET-1
       LTSTC(IE)=LTSTC(IE)+IOFFSET-1
       IF(IOBS.GT.0)LTSTT(IE)=LTSTT(IE)+IOFFSET-1
       IF(IOBS.GT.0.AND.KPL.EQ.3)LTSTQ(IE)=LTSTQ(IE)+IOFFSET-1
       IF(IOBS.GT.1)LTSTE(IE)=LTSTE(IE)+IOFFSET-1
403   CONTINUE
      ENDIF

      IF(NTOUT.GT.0)THEN
      DO 404 IE=1,NTOUT
       LTQSR(IE)=LTQSR(IE)+IOFFSET-1
404   CONTINUE
      ENDIF
c==========
cc      WRITE(6,9900) IDA,IDADAT,IDHF,IHR,ISTRT,ldacpd,inow
cc 9900 FORMAT(3X,'=-=-=-=       IDA    IDADAT IDHF  IHR     ISTRT    ldac
cc     . pd      inow ='/10X,2I10,2I5,4I10,' =-=-=-=-=')
      IF(IOBS.EQ.0) GO TO 30
      IF(INOW.GT.ISTRT) GO TO 30
      WRITE(IPR,7000) LDACPD,LHRCPD,IDA,IHR
 7000 FORMAT(1H0,10X,68H**WARNING** THERE IS NO OBSERVED DATA FOR THIS R
     .UN PERIOD.  (LDACPD=,I10,10H   LHRCPD=,I2,1H),5X,5H(IDA=,I10,7H
     .IHR=,I2,31H).   NO STATISTICS CAN BE DONE.)
      CALL WARN
      IOBS=0
C
c==========
C         COMPUTE TOTAL TRAVEL PERIOD
C
   30 IF(NU.GT.0) NU=(LDA-IDA)*24/IDHF+(LHR-IHR)/IDHF+1
      NPD=(LDA-IDA)*24/DTHPLT+(LHR-IHR)/DTHPLT+1
      NPO=(LDA-IDA)*24/NDHF+(LHR-IHR)/NDHF+1
      NPP=2*((LDA-IDA)*24/DTI+(LHR-IHR)/DTI)

      K3=MAX(NU,NPD)
      K3=MAX(K3,NPO)
      K24=MAX(1,NPP)
      TEH=DTHPLT*NPD
      IF(IBUG.NE.0) WRITE(IODBUG,999) DTHYD,DTHPLT,DTI,NU,NPD,NPO,NPP,
     . IDA,LDA,TEH
  999 FORMAT(5X,' DTHYD DTHPLT  DTI  NU NPD NPO NPP     IDA     LDA',
     1       '    TEH ='/5X,2F6.0,F6.3,4I4,2I8,F6.1)
C.......................................................................
C
C        PRINT THE ENTIRE PARAMETER AND CARRYOVER ARRAYS IN REAL,INTEGER
C        AND ALPHANUMERIC FORMATS
C
      IF(IBUG.LE.1) GO TO 40

C
      IF(IBUG.NE.0) WRITE(IODBUG,8000)
 8000 FORMAT(1H0,10X,47HDEBUG EXECUTION SUBROUTINE FOR FLDWAV OPERATION
     *//)
      IF(IBUG.LE.1) GO TO 40
      WRITE(IODBUG,8010)
 8010 FORMAT(1H0,10X,31HCONTENTS OF THE PARAMETER ARRAY)
      CALL ARRY21(PO,PO,IUSEP)
      WRITE(IODBUG,8020)
 8020 FORMAT(//1H0,10X,31HCONTENTS OF THE CARRYOVER ARRAY)
      CALL ARRY21(CO,CO,JSIZE)
C.......................................................................
C
C
C          STORE RATING CURVE IN THE Z ARRAY
C
C  40 WRITE(IODBUG,666) NYQD,NRC
C 666 FORMAT(//3X,25(1H=),'NYQD=',I3,3X,'NRC=',I3,1X,25(1H=))
   40 NYQD=0
      LRAT=LCRC
      IF(IPO(LOKD).NE.3) GO TO 45
      CALL STRC21(PO,LRAT,PO(LOYQD),PO(LOQYQD),NYQD,PO(LOKD),KDD,1)
      GZN=0.
      LRAT=LRAT+2
cc      CALL FGETRC(PO(LCRC),ISW)
cc      NYQD=NRCPTS
cc      GZN=GZERO
cc      IF(GZN.LE.-999.) GZN=0.
C.......................................................................
C         store dam rating curve in rhi,qhi space

   45 CALL STRC55(PO,LRAT,PO(LORHI),PO(LORQI),PO(LCRCP),PO(LOKRCH),
     . PO(LOLAD),PO(LONLAD),JN,K1,K2,K16)



C.......................................................................
C
C      STORE TARGET POOL ELEVATIONS AND GATE CONTROL SWITCHES INTO
C      TEMPORARY ARRAY SPACE.  CHECK FOR MISSING VALUES AND SET TO ZERO.
C
      IF(NLOCK.EQ.0) GO TO 50
C
      IF(IBUG.EQ.0) GO TO 47
      WRITE(IODBUG,8030) (LTXIWT(K),K=1,NLOCK)
 8030 FORMAT(/10X,8HLTXIWT =,20I5)
      WRITE(IODBUG,8032) (LTITWT(K),K=1,NLOCK)
 8032 FORMAT(/10X,8HLTITWT =,20I5)
      WRITE(IODBUG,8034) (LTPOLHS(K),K=1,NLOCK)
 8034 FORMAT(/10X,8HLTPOLHS=,20I5)
      WRITE(IODBUG,8036) (LTPOLH(K),K=1,NLOCK)
 8036 FORMAT(/10X,8HLTPOLH =,20I5)
C
   47 IF(NLOCK.GT.0)THEN
      DO 49 K=1,NLOCK
      LXIWT=LTXIWT(K)-1
      LIWT=LTITWT(K)-1
      LPLTS=LTPOLHS(K)-1
      LPLT=LTPOLH(K)-1
      IF(IBUG.NE.0) WRITE(IODBUG,8038) (XITWT(LXIWT+I),I=1,NU)
 8038 FORMAT(1H ,10X,7H XITWT=,20F5.0)
      IF(IBUG.NE.0) WRITE(IODBUG,8040) (POLHS(LPLTS+I),I=1,NU)
 8040 FORMAT(1H ,10X,'POLHS= ',10F10.2)
      DO 48 I=1,NU
      POLH(LPLT+I)=0.
      IF(IFMSNG(POLHS(LPLTS+I)).EQ.0) POLH(LPLT+I)=POLHS(LPLTS+I)
      ITWT(LIWT+I)=0
      IF(IFMSNG(XITWT(LXIWT+I)).EQ.0) ITWT(LIWT+I)=XITWT(LXIWT+I)+0.5
   48 CONTINUE
      IF(IBUG.NE.0) WRITE(IODBUG,8048) (ITWT(LIWT+I),I=1,NU)
 8048 FORMAT(1H ,10X,6H ITWT=,20I5)
      IF(IBUG.NE.0) WRITE(IODBUG,8050) (POLH(LPLT+I),I=1,NU)
 8050 FORMAT(1H ,10X,5HPOLH=,10F10.2)
   49 CONTINUE
      ENDIF
C.......................................................................
C
C          CONVERT TIME SERIES TO ENGLISH UNITS
C
   50 ICHK=1


      CALL ENGL55(ICHK,STN,LTSTN,ST1,LTST1,POLH,LTPOLH,QL,LTQL,
     . STT,LTSTT,STQ,LTSTQ,QSTR,LTQSR,PO(LOKD),PO(LOKU),PO(LONQL),
     . PO(LONLAD),PO(LOLAD),PO(LOCHTW),PO(LONGAG),PO(LONSTR),
     . PO(LCKTYP),PO(LCRCP),PO(LORHI),PO(LORQI),PO(LOYQD),PO(LOQYQD),
     . TIDE(LTTID),XNOS(LTNOS),STE,LTSTE,NB,NYQD,NRC,IOBS,KPL,JN,NU,
     . NPO,NLOCK,PO(LOKRCH),K1,K2,K14,K16)



C.......................................................................
C
c         RESET MIXF IF LPI OPTIONIS USED

      DO 51 J=1,JN
        IF(IPO(LOMIXF+J-1).EQ.5) IPO(LOMIXF+J-1)=2
 51   CONTINUE
C.......................................................................
C
c         Compute the initial Manning n values if zeroes read in

      IF(KFLP.LE.1) CALL INCM55(PO,JN,NU,XFACT,PO(LONQL),PO(LOX),
     * PO(LOLQ1),PO(LCLQN),PO(LOCM),PO(LOCML),PO(LOCMR),STT,LTSTT,
     * QL,LTQL,ST1,LTST1,PO(LOHS),PO(LOSLFI),PO(LONLAD),PO(LOLAD),
     * PO(LONQCM),PO(LONJUN),PO(LOYQCM),PO(LONCM1),PO(LONCM),NB,
     * QUSJ,LTQUSJ,Z(LZYQR),PO(LOMRV),PO(LCORDR),K1,K2,K4,K7,K8,K9,K10)
C.......................................................................

C          COMPUTE EXTRA GAGING STATIONS FOR AUTOMATIC CALIBRATION RUN

      IF(NP.LE.-1) CALL PRECAL55(NB,PO(LONGAG),PO(LONGS),PO(LOX),
     *  STT,LTSTT,PO(LONJUN),PO(LOGZ),PO(LOKD),STN,LTSTN,PO(LOGZN),
     *  PO(LOMRV),PO(LCORDR),K1,K2,K4)
C.......................................................................

C          PREPARE DATA TO BE USED IN THE FINITE DIFFERENCE SCHEME
C
      CALL IDPREP55(PO,PO(LOX),NB,PO(LCDDX),PO(LOAS),PO(LOBS),
     * PO(LOBSL),PO(LOBSR),
     * PO(LOGZ),PO(LOHS),PO(LOKD),PO(LOKU),PO(LCWF),PO(LOASS),PO(LOATF),
     * PO(LOBSS),STN,LTSTN,STT,LTSTT,ST1,LTST1,POLH,LTPOLH,T1,LTT1,QL,
     * LTQL,PO(LONGAG),PO(LOGZ1),PO(LOGZN),PO(LOYQD),PO(LOQYQD),TO,
     * PO(LOCOFW),PO(LOVWND),PO(LOWAGL),PO(LCHCAV),PO(LCIFCV),
     * PO(LOSTM),PO(LOKRCH),PO(LONQL),PO(LOLQ1),PO(LCLQN),PO(LOGZPL),
     * PO(LONLAD),PO(LOLAD),PO(LOPTAR),CO(LXQLI),CO(LXPLTI),XNOS(LTNOS),
     * TIDE(LTTID),DTHPLT,KSTG,NLOCK,NPO,DTOUT,
     * K1,K2,K4,K6,K9,K10,K16)



C.......................................................................

C  INITIAL CONDITION COMPUTATION
      JNN=JN
      JN=JN-NET
      CALL INITC55(PO,CO,Z,QL,LTQL,ST1,LTST1,STT,LTSTT,T1,LTT1,POLH,
     .  LTPOLH,ITWT,LTITWT,JNK,PO(LOMIXF),PO(LONJUN),NB,
     .  PO(LONQCM),Z(LZIFR),CO(LXYDI),Z(LZYD),CO(LXQDI),Z(LZQD),PO(LOX),
     .  PO(LCDDX),PO(LOHS),PO(LOAS),PO(LOBS),Z(LZYN),Z(LZYCR),
     .  PO(LOKRCH),PO(LOKU),PO(LOKD),PO(LOSLFI),PO(LONLAD),Z(LZSQW),
     .  Z(LZSQS1),Z(LZSQS2),Z(LZSQO),PO(LOQGH),PO(LOMUD),PO(LOUW),
     .  PO(LOVIS),PO(LOSHR),PO(LOPOWR),PO(LOIWF),PO(LOMRV),PO(LCORDR),
     .  PO(LOMRU),PO(LONJUM),K1,K2,K4,K6,K7,K8,K9,K10,K15,K16,K19,
     .  K20,K21)
C.......................................................................

C  LOW FLOW FILTER COMPUTATION
       CALL YFILTR55(PO,CO,Z,STN,LTSTN,ST1,LTST1,T1,LTT1,POLH,LTPOLH,
     .  ITWT,LTITWT,JNK,PO(LOMIXF),PO(LONJUN),NB,PO(LONQCM),
     .  Z(LZIFR),CO(LXYDI),Z(LZYUMN),CO(LXQDI),PO(LOX),PO(LCDDX),
     .  PO(LOHS),PO(LOAS),PO(LOBS),Z(LZYN),Z(LZYCR),PO(LOKRCH),PO(LOKU),
     .  PO(LOKD),PO(LOSLFI),PO(LOMRV),PO(LCORDR),K1,K2,K6,K7,K8,K9,
     .  K15,K16,K19,K20,K21)



C.......................................................................
C
C  DYNAMIC ROUTING

      IF(NP.GE.0) CALL DWAVSM55(PO,CO,Z,ST1,LTST1,T1,LTT1,STT,LTSTT,QTC,
     . LTQTC,STC,LTSTC,QL,LTQL,QLJ,LTQLJ,POLH,LTPOLH,ITWT,LTITWT,
     . STN,LTSTN,Z(LZST0),Z(LZQT0),CO(LXQLI),PO(LOMIXF),PO(LOKU),
     1 NB,Z(LZQJ),Z(LZYJ),PO(LONJUN),PO(LONQCM),Z(LZITRX),
     2 Z(LZNSIR),Z(LZMINT),Z(LZSTYP),Z(LZICT),Z(LZQU),Z(LZYD),Z(LZYU),
     3 Z(LZQC),Z(LZYC),CO(LXQDI),CO(LXYDI),Z(LZVU),Z(LZVC),Z(LZVD),
     4 PO(LONGS),PO(LOLROT),PO(LCKRTP),PO(LCKRT1),PO(LCKRTN),PO(LOGZ),
     5 Z(LZYII),TII,Z(LZQD),PO(LONGAG),PO(LONQL),Z(LZQLLT),PO(LOKD),
     7 PO(LOKRCH),PO(LCNFLD),Z(LZSQW),Z(LZSQS1),Z(LZSQS2),Z(LZSQO),
     9 PO(LODTHF),PO(LONLAD),PO(LOLAD),PO(LOHFDD),Z(LZTFDO),Z(LZKSP),
     * Z(LZKS1),Z(LZKSN),Z(LZLRMX),Z(LZTYPK),Z(LZYPK),Z(LZTQPK),
     * Z(LZQPK),PLTIM,Z(LZQMX),Z(LZYMX),PO(LCDDX),PO(LOLQ1),
     * PO(LCLQN),PO(LOKFTR),PO(LOKLPI),PO(LOMUD),PO(LOUW),PO(LOVIS),
     * PO(LOSHR),PO(LOPOWR),PO(LOIWF),PO(LOX),PO(LOHS),PO(LOMRV),
     * PO(LOEPQJ),Z(LZNFLO),Z(LZVPK),Z(LZJT),Z(LZCCO),ISTRT,TO,QSTR,
     * LTQSR,TIDE(LTTID),Z(LZWTLV),Z(LZLVPK),PO(LODTMP),Z(LZFYPK),
     . Z(LZFQPK),Z(LZFTYPK),Z(LZFTQPK),Z(LZFVPK),Z(LZFLVPK),NPO,NPD,
     . JSIZE,IHRCO,IOPNMAP,INOW,MXINBD,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,
     . K11,K12,K14,K15,K16,K19,K20,K21,K22,K23,K24,K28,K30)



C.......................................................................
CC
CC
CC  AUTOMATIC CALIBRATION
CC       IF(NP.LE.-1) CALL DWAVAC55(PO,Z,T1,LTT1,TII,STT,LTSTT,QTC,LTQTC,
CC     . STC,LTSTC,POLH,LTPOLH,ITWT,LTITWT,QL,LTQL,QLJ,LTQLJ,YA,QA,FS,FFS,
CC     . PO(LONQL),PO(LOKD),PO(LOKU),NB,PO(LONJUN),Z(LZITRX),PO(LOLROT),
CC     . PO(LCKRT1),PO(LCKRTN),PO(LOMIXF),Z(LZLRMX),Z(LZKSP),Z(LZKS1),
CC     . Z(LZKSN),Z(LZSQW),Z(LZSQS1),Z(LZSQS2),Z(LZSQO),PO(LONLAD),
CC     . Z(LZICT),PO(LONQCM),PO(LOLQ1),Z(LZQD),Z(LZQU),Z(LZYD),Z(LZYU),
CC     . PO(LCDDX),Z(LZQC),Z(LZYC),CO(LXQDI),CO(LXYDI),Z(LZQII),Z(LZYII),
CC     . PO(LOX),PO(LOHS),PO(LOCM),PO(LOYQCM),PO(LOCHMN),PO(LOCHMX),PO(LOFKC),
CC     . PO(LOFKF),PO(LOFMC),PO(LOFMF),PO(LOKRCH),PO(LOIFXC),PO(LOBS),PO(LOSTTN),
CC     . PO(LOMRV),PO(LCNFLD),Z(LZNFLO),TIDE(LTTID),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K15,
CC     . K16,K19,K20,K21,K22)
CCC.......................................................................

C          PLOT DESIRED HYDROGRAPHS

      JN=JNN
      IF(NP.LE.-1.OR.NP.EQ.-3.OR.NP.EQ.-4) GO TO 200
      IF(KPL.NE.0) CALL WAPLOT55(PO,Z,T1,LTT1,TII,QTC,LTQTC,STC,LTSTC,
     . STT,LTSTT,STQ,LTSTQ,FS,FFS,PO(LONGAG),PO(LOSTTN),PO(LONGS),
     . PO(LOFLST),Z(LZTYPK),Z(LZYPK),Z(LZTQPK),Z(LZQPK),PO(LOGZ),
     . KHR,KDA,KMO,KYR,ISTRT,INOW,KSTG,K1,K2,K4,K28)
C.......................................................................

C  STATISTICS
      IF(IOBS.GT.0) CALL STAT55(PO,Z,STT,LTSTT,STC,LTSTC,QTC,LTQTC,TII,
     . PO(LONGAG),PO(LOSTTN),PO(LONGS),PO(LOGZ),Z(LZST0),Z(LZQT0),
     . CO(LXYDI),CO(LXQDI),Z(LZRMS),Z(LZAVD),STE,LTSTE,IRF,PO(LONSLC),
     . NB,ISTRT,INOW,KSTG,STQ,LTSTQ,K1,K2,K4,K28,K29)




C.......................................................................

C EXPORT FLDGRF DATA
      IF (NFGRF.NE.1.AND.IOPNERR.EQ.0) THEN
         CALL WRTITL55 (NB,PO(LONBT),
     . PO(LOX),PO(LOXT),PO(LOFLST),Z(LZTYPK),Z(LZYPK),
     . Z(LZTQPK),Z(LZQPK),PO(LONGAG),PO(LONGS),PO(LOGZ),
     . PO(LOSTTN),STT,LTSTT,STQ,LTSTQ,T1,LTT1,Z(LZRMS),Z(LZAVD),
     . PO(LCNN),PO(LOMRV),PO(LONJUN),PO(LOKRCH),PO(LOHS),PO(LOBS),
     . PO(LOBSS),PO(LOBSL),PO(LOBSR),PO(LORIVR),PO(LORIVR+20),KMO,
     . KDA,KYR,KHR,ZONE,K1,K2,K4,K9,K23,K28)
        ENDIF
C.......................................................................

C EXPORT FLOOD MAP DATA
      IF(NMAP.GT.0.AND.IOPNMAP.EQ.0) THEN
        MPFRST=2
        CALL WRTMAP55(PO,
     .   PO(LONB),PO(LOMPRV),PO(LOXLAT),PO(LOXLON),Z(LZFYPK),PO(LOHS),
     .   PO(LOBS),PO(LOBSS),PO(LOX),PO(LZFLVPK),PO(LONJFM),PO(LONIFM),
     .   PO(LONJTO),PO(LONITO),PO(LONQL),PO(LOLQ1),PO(LCLQN),PO(LOMPLC),
     .   Z(LZJCK),Z(LZIMAP),PO(LOMRV),PO(LONJUN),PO(LONSTR),
     .   PO(LONST),PO(LOSTON),PO(LOGZO),PO(LODTMP),
     .   DTHYD,IOPNMAP,K1,K2,K9,K10,K14,K22,K27,K30)
      ENDIF
C.......................................................................
C COPY FLOOD MAP DATA TO OTHER FOLDERS
      IF(NMAP.GT.1) THEN
        CALL CPYMAP55(IOPNMAP,PO(LOSPTH),PO(LOTPTH),K30) 
      ENDIF
C
      IF(NMAP.GT.0.AND.MAINUM.EQ.2) NMAP=0
C.......................................................................

C COMPARE INFLOW HYDROGRAPH WITH MINIMUM VALUE HYDROGRAPH
C
CC      ITRACE=1
      CALL LOWQ21(ST1,LTST1,PO(LOSTM),PO(LOKU),JN,NU)

C.......................................................................

C         PRINT THE OUTPUT TIME SERIES ARRAYS

ccc      IF(IBUG.GT.0.AND.JNK.GE.1) CALL PRTS55(NPO,TO,QSTR,LTQSR,
ccc     . PO(LONSTR),PO(LONST),PO(LOSNM),JN)

C.......................................................................

C CONVERT TIME SERIES HYDROGRAPHS BACK TO ORIGINAL FORMS
C
  200 CALL UNDO55(STT,LTSTT,ST1,LTST1,STN,LTSTN,QL,LTQL,PO(LOGZ),
     . PO(LOGZ1),PO(LOGZN),PO(LONGAG),PO(LOLQ1),PO(LCLQN),PO(LONQL),
     . PO(LOKD),PO(LOKU),NB,PO(LOX),JN,KPL,NU,IOBS,CO(LXQLI),
     . KSTG,XNOS(LTNOS),TIDE(LTTID),STE,LTSTE,K1,K2,K4,K10)
C



C.......................................................................

C CONVERT TIME SERIES BACK TO METRIC UNITS
C
      ICHK=0


      CALL ENGL55(ICHK,STN,LTSTN,ST1,LTST1,POLH,LTPOLH,QL,LTQL,
     . STT,LTSTT,STQ,LTSTQ,QSTR,LTQSR,PO(LOKD),PO(LOKU),PO(LONQL),
     . PO(LONLAD),PO(LOLAD),PO(LOCHTW),PO(LONGAG),PO(LONSTR),
     . PO(LCKTYP),PO(LCRCP),PO(LORHI),PO(LORQI),PO(LOYQD),PO(LOQYQD),
     . TIDE(LTTID),XNOS(LTNOS),STE,LTSTE,NB,NYQD,NRC,IOBS,KPL,JN,NU,
     . NPO,NLOCK,PO(LOKRCH),K1,K2,K14,K16)



C
C.......................................................................

c compute offset for mcp and esp runs
c all of these offsets must be undone
      IOFFSET=(IDA-IDADAT)*24/IDHF+(IHR-1)/IDHF+1
c adjust the locations in the d array for the input data.
      LTSTN=LTSTN-ioffset+1
      LTTID=LTTID-IOFFSET+1
      LTNOS=LTNOS-IOFFSET+1
      if(jn.gt.0)then
      do 600 ie=1,jn
       LTST1(ie)=LTST1(ie)-ioffset+1
       LTQLJ(ie)=LTQLJ(ie)-ioffset+1
       LTQUSJ(ie)=LTQUSJ(ie)-ioffset+1
600   continue
      endif
      if(nlock.gt.0)then
      do 601 ie=1,nlock
       LTXIWT(ie)=LTXIWT(ie)-ioffset+1
       LTPOLHS(ie)=LTPOLHS(ie)-ioffset+1
       LTITWT(ie)=LTITWT(ie)-ioffset+1
       LTPOLH(ie)=LTPOLH(ie)-ioffset+1
601   continue
      endif
      if(ntql.gt.0)then
      do 602 ie=1,NTQL
       LTQL(ie)=LTQL(ie)-ioffset+1
cc       loqlt(ie)=loqlt(ie)-ioffset+1
602   continue
      endif
      IF(NTGAG.GT.0)THEN
      do 603 ie=1,NTGAG
       LTQTC(ie)=LTQTC(ie)-ioffset+1
       LTSTC(ie)=LTSTC(ie)-ioffset+1
       IF(IOBS.GT.0)LTSTT(ie)=LTSTT(ie)-ioffset+1
       IF(IOBS.GT.0.AND.KPL.EQ.3)LTSTQ(ie)=LTSTQ(ie)-ioffset+1
       IF(IOBS.GT.1)LTSTE(IE)=LTSTE(IE)-IOFFSET+1
603   continue
      ENDIF
      if(ntout.gt.0)then
      do 604 ie=1,NTOUT
       LTQSR(ie)=LTQSR(ie)-ioffset+1
604   continue
      endif
C.......................................................................

 9500 RETURN
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
      END

