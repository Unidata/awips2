C MODULE PUC55
C-----------------------------------------------------------------------
C
C    THIS PROGRAM PUCHES OUT THE INPUT DATA FOR FLDWAV OPERATION
C
      SUBROUTINE PUC55(PO,IPO,CO,ICO)
C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C     add system_name and scenario_name to the punch output
C

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
cc      INCLUDE 'common/ofs55'

      CHARACTER*80 MSG
      CHARACTER*8  SNAME
      DIMENSION    PO(*),IPO(*),CO(*),ICO(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/puc55.f,v $
     . $',                                                             '
     .$Id: puc55.f,v 1.15 2004/09/24 22:46:35 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA  SNAME / 'PUC55   ' /

      CALL FPRBUG(SNAME, 1, 55, IBUG)

      LMSG=PO(10)-1
      NMSG=PO(49)
      DO 10 L=1,NMSG
        WRITE(IPU,5) (PO(LMSG+K),K=1,20)
    5   FORMAT(20A4)
        LMSG=LMSG+20
   10 CONTINUE
      WRITE(IPU,15)
   15 FORMAT(4HEOM )


   20 NODESC=IPO(26)
      IF(NODESC.EQ.0) THEN
        MSG='DESC'
      ELSE
        MSG='NO DESC'
      ENDIF
      WRITE(IPU,'(A80)') MSG

      EPSY=PO(11)
      THETA=PO(12)
      F1=PO(13)
      XFACT=PO(14)
      DTHYD=PO(15)
      METRIC=PO(16)
      DTOUT=PO(23)

      WRITE(IPU,5000)
      WRITE(IPU,4000) EPSY,THETA,F1,XFACT,DTHYD,DTOUT,METRIC

      JN=PO(17)
      NU=PO(18)
      ITMAX=PO(19)
      KWARM=PO(20)
      KFLP=PO(21)
      NET=PO(22)
      ICOND=PO(25)
      NMAP=PO(375)
      MAPI=PO(376)
      WRITE(IPU,5005)
      WRITE(IPU,4005) JN,NU,ITMAX,KWARM,KFLP,NET,ICOND,NMAP,0,0

      NYQD=PO(27)
      IF(NYQD.EQ.112) NYQD=1
      KCG=PO(28)
      NCG=PO(29)
      KPRES=PO(30)
      WRITE(IPU,5010)
      WRITE(IPU,4010) NYQD,KCG,NCG,KPRES

      NCS=PO(31)
      KPL=PO(32)
      JNK=PO(33)
      KREVRS=PO(34)
      NFGRF=PO(35)
      WRITE(IPU,5015)
      WRITE(IPU,4010) NCS,KPL,JNK,KREVRS,NFGRF

      IOBS=PO(36)
      KTERM=PO(37)
      NP=PO(38)
      NPST=PO(39)
      NPEND=PO(40)
      WRITE(IPU,5017)
      WRITE(IPU,4010) IOBS,KTERM,NP,NPST,NPEND

      IF(JNK.GE.0) GO TO 160
      TDBG1=PO(41)
      TDBG2=PO(42)
      JNKDBG=PO(43)
      JDBG1=PO(44)
      JDBG2=PO(45)
      LDBG1=PO(46)
      LDBG2=PO(47)
      MCMDBG=PO(48)
      WRITE(IPU,5020)
      WRITE(IPU,4015) TDBG1,TDBG2,JNKDBG,JDBG1,JDBG2
      IF(NP.LT.0) THEN
        WRITE(IPU,5025)
        WRITE(IPU,4010) LDBG1,LDBG2,MCMDBG
      ENDIF
  160 CONTINUE

      TEH=PO(50)
      DTHII=PO(51)
      DTHPLT=PO(52)
      FRDFR=PO(53)
      DTEXP=PO(54)
      MDT=PO(55)
      NDT=PO(56)
      WRITE(IPU,5030)
      WRITE(IPU,4020) TEH,DTHII,DTHPLT,FRDFR,DTEXP,MDT

      LODTIN=PO(57)
      LOTDTN=PO(58)
      IF(NDT.LE.0) GO TO 180
      WRITE(IPU,5035)
      WRITE(IPU,4025) (PO(LODTIN+K-1),K=1,NDT)
      WRITE(IPU,5040)
      WRITE(IPU,4030) (PO(LOTDTN+K-1),K=1,NDT)
  180 CONTINUE

      NLEV=PO(60)
      DHLV=PO(61)
      DTHLV=PO(62)
      IF(DTHLV.GE.9999.99) DTHLV=0.0
      WRITE(IPU,5045)
      WRITE(IPU,4035) NLEV,DHLV,DTHLV

      LONJFT  =PO(63)
      LONIFT  =PO(64)
      LONJTT  =PO(65)
      LONITT  =PO(66)
      LONXLV  =PO(67)
      LONBT   =PO(68)
      LONQL   =PO(69)
      LONJUN  =PO(70)
      LONPT   =PO(71)
      LOKU    =PO(72)
      LOKD    =PO(73)
      LONCM1  =PO(74)
      LONGAG  =PO(75)
      LOMIXF  =PO(76)
      LONQCM  =PO(77)
      LOMUD   =PO(78)
      LOUW    =PO(79)
      LOVIS   =PO(80)
      LOSHR   =PO(81)
      LOPOWR  =PO(82)
      LOIWF   =PO(83)
      LOKFTR  =PO(84)
      LOKLPI  =PO(85)
      LOXLOS  =PO(86)
      LOQLOS  =PO(87)
      LOALOS  =PO(88)
      LOMRV   =PO(89)
      LOKLOS  =PO(90)
      LCORDR  =PO(91)
      LOCOFW  =PO(92)
      LOVWND  =PO(93)
      LOWAGL  =PO(94)
      LOEPQJ  =PO(95)
      LOATF   =PO(96)
      LONSTR  =PO(99)
      LOMRU   =PO(369)
      LONJUM  =PO(370)
      LOMPRV  =PO(371)
      LOMPLC  =PO(372)-1
      LOXLAT  =PO(373)
      LOXLON  =PO(374)
      LODTMP  =PO(377)
      LOSPTH  =PO(379)
      LOTPTH  =PO(380)
      LCNN    =PO(281)
      K23     =PO(123)

      IF(NLEV.EQ.0) GOTO 200
      WRITE(IPU,5050)
      DO 190 L=1,NLEV
      WRITE(IPU,4040) IPO(LONJFT+L-1),IPO(LONIFT+L-1),IPO(LONJTT+L-1),
     $                 IPO(LONITT+L-1)
      IF(L.NE.NLEV) WRITE(IPU,*)
  190 CONTINUE

  200 IF(NMAP.EQ.0) GO TO 240
      WRITE(IPU,5052)
      lspth=lospth-1
      ltpth=lotpth-1
      DO 205 L=1,NMAP
        MR=IPO(LOMPRV+L-1)
        N=IPO(LONBT+MR-1)
        LNN=LCNN+K23*(MR-1)
        CALL OLDVAL55(IPO(LOMPLC+2*L-1),MPLC1,1,IPO(LNN),N)
        CALL OLDVAL55(IPO(LOMPLC+2*L),MPLC2,1,IPO(LNN),N)
C jgg changes for MR1954	
cc        SYSPTH=PO(LOSPTH+L)  system name
cc        TWNPTH=PO(LOTPTH+L)  scenario name
        IF(L.EQ.1) THEN
	   WRITE(IPU,4006) IPO(LOMPRV+L-1),MPLC1,MPLC2,PO(LODTMP+L-1),
     .        (PO(LSPTH+LL),LL=1,6),(PO(LTPTH+LL),LL=1,6)
        ELSE
	   WRITE(IPU,*)
	   WRITE(IPU,4007) IPO(LOMPRV+L-1),MPLC1,MPLC2,
     .        (PO(LSPTH+LL),LL=1,6),(PO(LTPTH+LL),LL=1,6) 
        ENDIF         
        LSPTH=LSPTH+6
        LTPTH=LTPTH+6
  205 CONTINUE

  240 IF(JN.EQ.1) THEN
        WRITE(IPU,5055)
        WRITE(IPU,4045) IPO(LONBT),IPO(LONPT),IPO(LONPT+1),
     $     PO(LOEPQJ),PO(LOCOFW),PO(LOVWND),PO(LOWAGL)

      ELSE
        WRITE(IPU,5060)
        WRITE(IPU,4050) IPO(LONBT),IPO(LONPT),IPO(LONPT+1),
     $     PO(LOEPQJ),PO(LOCOFW),PO(LOVWND),PO(LOWAGL)
      ENDIF

      IF(JN.LE.1) GOTO 260

      DO 250 J=2,JN
        NPT2=IPO(LONPT+J*2-1)
        NPT1=IPO(LONPT+J*2-2)
CC        NJN=IPO(LONJUN+J-1)
        MR=IPO(LOMRV+J-1)
        N=IPO(LONBT+MR-1)
        LNN=LCNN+K23*(MR-1)
        CALL OLDVAL55(IPO(LONJUN+J-1),NJUN,1,IPO(LNN),N)
        ATF=PO(LOATF+J-1)
CC        ATF=90.0

        WRITE(IPU,*)
        IF(NET.EQ.0) THEN
        WRITE(IPU,4052) IPO(LONBT+J-1),NPT1,NPT2,IPO(LOMRV+J-1),NJUN,
     $     ATF,PO(LOEPQJ+J-1),PO(LOCOFW+J-1),
     $     PO(LOVWND+J-1),PO(LOWAGL+J-1)
        ELSE
        WRITE(IPU,4055) IPO(LONBT+J-1),NPT1,NPT2,IPO(LOMRV+J-1),NJUN,
     $     IPO(LOMRU+J-1), IPO(LONJUM+J-1),
     &     ATF,PO(LOEPQJ+J-1),PO(LOCOFW+J-1),
     $     PO(LOVWND+J-1),PO(LOWAGL+J-1)
       ENDIF
  250 CONTINUE

  260 WRITE(IPU,5065)
      DO 280 J=1,JN
        IF(J.GT.1) WRITE(IPU,*)
        WRITE(IPU,4060) IPO(LOKU+J-1),IPO(LOKD+J-1),IPO(LONQL+J-1),
     .    IPO(LONGAG+J-1),IPO(LONCM1+J-1),IPO(LONQCM+J-1),
     .    IPO(LONSTR+J-1),0,0,0
  280 CONTINUE

      WRITE(IPU,5070)
      DO 295 J=1,JN
        IF(J.GT.1) WRITE(IPU,*)
        WRITE(IPU,4065) IPO(LOMIXF+J-1),IPO(LOMUD+J-1),IPO(LOKFTR+J-1),
     $     IPO(LOKLOS+J-1),0,0,0,0,0,0
  295 CONTINUE

      NLPI=PO(97)
      IF(NLPI.EQ.0) GOTO 320
      WRITE(IPU,5075)
      WRITE(IPU,4070) (IPO(LOKLPI+K-1),K=1,NLPI)

  320 IF(IPO(LOMUD).EQ.0) GOTO 340
      WRITE(IPU,5080)
      DO 330 J=1,JN
        IF(IPO(LOMUD+J-1).EQ.0) GOTO 330
        IF(J.GT.1) WRITE(IPU,*)
        WRITE(IPU,4075) PO(LOUW+J-1),PO(LOVIS+J-1),PO(LOSHR+J-1),
     $     PO(LOPOWR+J-1),IPO(LOIWF+J-1)
  330 CONTINUE

  340 NLOS=PO(98)
      IF(NLOS.EQ.0) GOTO 360
      WRITE(IPU,5085)
      DO 350 J=1,JN
        IF(IPO(LOKLOS+J-1).EQ.0) GOTO 350
        WRITE(IPU,4080) PO(LOXLOS+J-1),PO(LOXLOS+J),PO(LOQLOS),
     $                   PO(LOALOS)
  350 CONTINUE

C---------------  XT,  DXM,  KRCHT  ------------------------
  360 K1  =PO(101)
      K2  =PO(102)
      K3  =PO(103)
      K4  =PO(104)
      K5  =PO(105)
      K6  =PO(106)
      K7  =PO(107)
      K8  =PO(108)
      K9  =PO(109)
      K10 =PO(110)
      K11 =PO(111)
      K12 =PO(112)
      K13 =PO(113)
      K14 =PO(114)
      K15 =PO(115)
      K16 =PO(116)
      K17 =PO(117)
      K18 =PO(118)
      K19 =PO(119)
      K20 =PO(120)
      K21 =PO(121)
      K22 =PO(122)
      K23 =PO(123)
      K24 =PO(124)
      LONB=PO(126)
      LOXT=PO(127)
      LODXM=PO(128)
      LORCHT=PO(129)
      LONLAD=PO(130)
      LOLROT=PO(131)
      LCXYZ=PO(307)

      DO 450 J=1,JN
      N=IPO(LONBT+J-1)
      NJJ=K23*(J-1)-1
      WRITE(IPU,5095) J
      WRITE(IPU,4091) (PO(LOXT+NJJ+KK),KK=1,N)
      WRITE(IPU,5100) J
      WRITE(IPU,4090) (PO(LODXM+NJJ+KK),KK=1,N-1)
      WRITE(IPU,5105) J
      WRITE(IPU,4095) (IPO(LORCHT+NJJ+KK),KK=1,N-1)
  450 CONTINUE
C-------------  LEVEE DATA  ----------------------------------------
      IF (NLEV.EQ.0) GOTO 490
      LOHWLV  =PO(135)
      LOSLV   =PO(136)
      LOWCLV  =PO(137)
      LCTFL0  =PO(138)
      LOTFLV  =PO(139)
      LOBLMX  =PO(140)
      LOHFLV  =PO(141)
      LOHLMN  =PO(142)
      LOSAP   =PO(143)
      LOHSAP  =PO(144)
      LOHPND  =PO(145)
      LOHPLV  =PO(146)
      LODPLV  =PO(147)
      WRITE(IPU,5110)
      DO 480 L=1,NLEV
      JFM=IPO(LONJFT+L-1)
      IFM=IPO(LONIFT+L-1)
      XLEV=PO(LOXT+(JFM-1)*K23+IFM-1)
      HPL=PO(LOHPLV+L-1)
      IF(HPL.GT.9999.) HPL=0.
      BLM=PO(LOBLMX+L-1)
      IF(BLM.GT.9999.) BLM=0.
      HFL=PO(LOHFLV+L-1)
      IF(HFL.GT.9999.) HFL=0.
cc      print 488, hpl,blm,hfl
 488  format('hpl=',f12.2,'  blm=',f12.2,'  hfl=',f12.2)
      WRITE(IPU,4100) PO(LOHWLV+L-1),PO(LOWCLV+L-1),PO(LOTFLV+L-1),BLM,
     * HFL,PO(LOHLMN+L-1),PO(LOSLV+L-1),HPL,PO(LODPLV+L-1)
       IF(L.NE.NLEV) WRITE(IPU,*)
  480 CONTINUE

  490 NPOND=PO(148)
      IF(NPOND.EQ.0) GOTO 530
cc      WRITE(IPU,*)
      DO 520 L=1,NPOND
      WRITE(IPU,5115)
      WRITE(IPU,4105) PO(LOHPND+L-1)
      LSAP=LOSAP+(L-1)*8-1
      LHSAP=LOHSAP+(L-1)*8-1
      WRITE(IPU,5120)
      WRITE(IPU,4110) (PO(LSAP+KK),KK=1,8)
      WRITE(IPU,5125)
      WRITE(IPU,4115) (PO(LHSAP+KK),KK=1,8)
  520 CONTINUE

C------------------  INTERNAL BOUNDARY  ----------------------------
  530 LOLAD   =PO(151)
      LORHI   =PO(152)
      LORQI   =PO(153)
      LODTHF  =PO(154)
      LOBEXP  =PO(155)
      LOGSIL  =PO(156)
      LOSAR   =PO(157)
      LOHSAR  =PO(158)
      LOQGH   =PO(159)
      LOHFDD  =PO(160)
      LOTFH   =PO(161)
      LOBBD   =PO(162)
      LOZBCH  =PO(163)
      LOYMIN  =PO(164)
      LOHDD   =PO(165)
      LOHSPD  =PO(166)
      LOHGTD  =PO(167)
      LOCSD   =PO(168)
      LOCGD   =PO(169)
      LOCDOD  =PO(170)
      LOQTD   =PO(171)
      LCNFLD  =PO(172)
      LOTIQH  =PO(173)
      LOCPIP  =PO(174)
      LOCGCG  =PO(175)
      LOGWID  =PO(176)
      LONG    =PO(177)
      LOGHT   =PO(178)
      LOQHT   =PO(179)
      LCTOPN  =PO(180)
      LOGZPL  =PO(181)
      LOQTT   =PO(185)
      LOTQT   =PO(186)
      LOICHN  =PO(187)
      LOPTAR  =PO(188)
      LOCHTW  =PO(189)
      LOPOLH  =PO(190)
      LOITWT  =PO(191)
      LOCLL   =PO(192)
      LOSPL   =PO(193)
      LOCRL   =PO(194)
      LOHCRL  =PO(195)
      LCTCG   =PO(196)
      LOICG   =PO(197)
      LOTGHT  =PO(198)
      LOBRBS  =PO(199)
      LOBRHS  =PO(200)
      LOEBE2  =PO(201)
      LOEBE1  =PO(202)
      LOBRGW  =PO(203)
      LOCDBR  =PO(204)
      LOEBW2  =PO(205)
      LOEBW1  =PO(206)
      LOEMBW  =PO(207)
      LOEMBE  =PO(208)
      LOTS1   =PO(322)
      LCNN    =PO(281)

      KLOCK=0
      ILOCK=1
      DO 650 J=1,JN
      NJJ=K23*(J-1)-1
      KRB=0
      K=0
      NDAM=0
      NBRG=0
      DO 650 I=1,IPO(LONBT+J-1)-1
      KRA=IABS(IPO(LORCHT+NJJ+I))
      IF(KRA.LT.10 .OR. KRA.GT.40) GO TO 650
      IF(I.GT.1) KRB=IABS(IPO(LORCHT+NJJ+I))
      K=K+1
            IF (KRA.EQ.28) THEN
            KLOCK=KLOCK+1
            ILOCK=LOTS1+(KLOCK-1)*10
            ENDIF
        IF(KRA.EQ.35) THEN
        NBRG=NBRG+1
        GO TO 630
        ELSE
        NDAM=NDAM+1
        ENDIF
C DAM AND RESERVOIR
cc      WRITE(IPU,*)
      IRES=0
      IF (I.EQ.1 .OR. KRB.EQ.4) IRES=1
      CALL PUDAM55(K,J,KRA,PO(LOHDD),PO(LOCLL),PO(LOHSPD),PO(LOSPL),
     1 PO(LOCRL),PO(LOHCRL),PO(LOCSD),PO(LOHGTD),PO(LOCGD),PO(LOCDOD),
     2 PO(LOQTD),PO(LOHFDD),PO(LOTFH),PO(LOBBD),PO(LOZBCH),PO(LOYMIN),
     3 PO(LORHI),PO(LORQI),PO(LCTCG),PO(LOQGH),PO(LOCGCG),PO(LOQHT),
     4 PO(LONG),PO(LOLAD),PO(LOICG),PO(LODTHF),PO(LOBEXP),PO(LOCPIP),
     5 PO(LOTIQH),PO(LONLAD),PO(LOGSIL),PO(LOGWID),PO(LOTGHT),
     6 PO(LOGHT),PO(LOICHN),PO(LOPTAR),PO(LOCHTW),
     7 PO(LOGZPL),PO(LOSAR),PO(LOHSAR),PO(ILOCK),
     8 IRES,NODESC,K1,K16,K19,K20,K21)
      GOTO 650
C   BRIDGE
cc  630 WRITE(IPU,*)
 630  CALL PPBRG55(K,J,PO(LOEBE1),PO(LOBRGW),PO(LOHFDD),PO(LOTFH),
     2  PO(LOBBD),PO(LOZBCH),PO(LOYMIN),PO(LOLAD),PO(LODTHF),PO(LOBEXP),
     3  PO(LOEBE2),PO(LOCDBR),PO(LOBRBS),PO(LOBRHS),PO(LOCPIP),
     4  PO(LOEBW2),PO(LOEBW1),NODESC,K1,K16,1)
  650 CONTINUE

C......................................................................
C--------------  LATERAL FLOWS T.S. INFORMATION  -------------------------
C......................................................................
      LLQ1=PO(132)
      LQL=PO(350)-1
      LNN=LCNN
      DO 800 J=1,JN
      N=IPO(LONBT+J-1)
      NQLJ=IPO(LONQL+J-1)
      IF(NQLJ.EQ.0) GOTO 795
      CALL OLDVAL55(IPO(LLQ1),IPO(LCXYZ),NQLJ,IPO(LNN),N)
      WRITE(IPU,5130) J
      DO 790 L=1,NQLJ
      IF(L.GT.1) WRITE(IPU,*)
CC      WRITE(IPU,4120) IPO(LLQ1+L),(PO(LQL+LL),LL=1,3)
      WRITE(IPU,4120) IPO(LCXYZ+L-1),(PO(LQL+LL),LL=1,3)
      LQL=LQL+3
  790 CONTINUE
  795   LLQ1=LLQ1+K10
        LNN=LNN+K23
  800 CONTINUE
C.......................................................................
C     PLOTTING GAGE STATION T.S. INFORMATION
C.......................................................................
      LONGS=PO(211)
      LOGZ=PO(212)
      LOSTT=PO(352)
      LOSTQ=PO(354)
      LSTT=LOSTT-1
      LSTQ=LOSTQ-1
      LNGS=LONGS
      LGZ=LOGZ-1
      LNN=LCNN

      DO 900 J=1,JN
      N=IPO(LONBT+J-1)
      NG=IPO(LONGAG+J-1)
      IF(NG.LE.0) GO TO 895
      CALL OLDVAL55(IPO(LNGS),IPO(LCXYZ),NG,IPO(LNN),N)
      WRITE(IPU,5135) J
      DO 890 I=1,NG
        NGS=IPO(LCXYZ+I-1)
        IF(I.GT.1) WRITE(IPU,*)
        IF(IOBS.LE.0.OR.KPL.EQ.2) THEN
          WRITE(IPU,4120) NGS,(PO(LSTT+LN),LN=1,3)
CC          WRITE(IPU,4120) IPO(LNGS+I),(PO(LSTT+LN),LN=1,3)
        ELSE
          WRITE(IPU,4125) NGS,PO(LGZ+I),(PO(LSTT+LN),LN=1,3)
CC          WRITE(IPU,4125) IPO(LNGS+I),PO(LGZ+I),(PO(LSTT+LN),LN=1,3)
        ENDIF
        LSTT=LSTT+5
        IF(KPL.EQ.3) THEN
          WRITE(IPU,4125) NGS,(PO(LSTQ+LN),LN=1,3)
CC          WRITE(IPU,4125) IPO(LNGS+I),(PO(LSTQ+LN),LN=1,3)
          LSTQ=LSTQ+5
        ENDIF
  890 CONTINUE
  895 LNGS=LNGS+K4
      LGZ=LGZ+K4
      LNN=LNN+K23
  900 CONTINUE
C.......................................................................
C     OUTPUT T.S. INFO
C.......................................................................
      NTOUT=PO(331)
      IF(NTOUT.LE.0) GOTO 950
      LONST=PO(358)-1
      LOGZO=PO(360)-1
      LOKTYP=PO(359)-1
      LOQLST=PO(356)-1
      LNST=LONST
      LGZO=LOGZO
      LKTYP=LOKTYP
      LQLST=LOQLST
      LNN=LCNN

      DO 920 J=1,JN
      N=IPO(LONBT+J-1)
      NOUT=IPO(LONSTR+J-1)
      IF(NOUT.LE.0) GO TO 915
cc      CALL OLDVAL55(IPO(LNST),IPO(LCXYZ),NOUT,IPO(LNN),N)
      WRITE(IPU,5137) J
      DO 910 I=1,NOUT
cc      IF(MAPI.EQ.0) THEN
cc        NST=IPO(LCXYZ+I-1)
cc      ELSE
        NST=IPO(LNST+I)
cc      ENDIF
      IF(I.GT.1) WRITE(IPU,*)
      KTYP=IPO(LKTYP+I)
cc      IF(KTYP.EQ.1) WRITE(IPU,4085) NST,(PO(LQLST+LL),LL=1,3),PO(LGZO+I)
cc      IF(KTYP.GE.2) WRITE(IPU,4120) NST,(PO(LQLST+LL),LL=1,3)
C Change made by jls 1/27/04
C      IF(KTYP.EQ.1) WRITE(IPU,4085) IPO(LNST+I),(PO(LQLST+LL),LL=1,3),
      WRITE(IPU,4085) IPO(LNST+I),(PO(LQLST+LL),LL=1,3),
     .   PO(LGZO+I)
C      IF(KTYP.EQ.2) WRITE(IPU,4085) IPO(LNST+I),(PO(LQLST+LL),LL=1,3)
C  End of change

      LQLST=LQLST+3
  910 CONTINUE
  915 LNST=LNST+K14
      LGZO=LGZO+K14
      LKTYP=LKTYP+K14
      LNN=LNN+K23
  920 CONTINUE
C.........................................................................
C   TPG  RHO GAMA YQI
  950 IF (IOBS.GE.0) GOTO 980
      LOTPG =PO(217)
      LORHO =PO(218)
      LOGAMA=PO(219)
      LOYQI =PO(220)
      WRITE(IPU,*)
      DO 970 J=1,JN
      WRITE(IPU,5140)
      WRITE(IPU,4130) J,PO(LOTPG+J-1),PO(LORHO+J-1),PO(LOGAMA+J-1),
     * PO(LOYQI+J-1)
  970 CONTINUE
      GO TO 1000
C.......................................................................
C
C    BOUNDARY T.S. INFORMATION
C
C------------------------------------------------------------------------
  980 LOGZ1   =PO(221)
      LOGZN   =PO(222)
      LOST1=PO(361)-1
      LOSTM=PO(363)-1
      LST1=LOST1

      WRITE(IPU,5138)
      DO 990 J=1,JN-NET
      IF(J.GT.1) WRITE(IPU,*)
        IF(IPO(LOKU+J-1).EQ.2) WRITE(IPU,4190) PO(LOSTM+J),
     .       (PO(LST1+JJ),JJ=1,3)
        IF(IPO(LOKU+J-1).EQ.1) WRITE(IPU,4170) PO(LOSTM+J),
     .      PO(LOGZ1+J),(PO(LST1+JJ),JJ=1,3)
        LST1=LST1+3
  990 CONTINUE
 1000 CONTINUE

C  DOWNSTREAM BOUNDARY INFORMATION
CC      WRITE(IPU,*)
         KD1=IPO(LOKD)
         LORC=PO(366)
         LOSTNN=PO(362)-1
         LONOSN=PO(348)-1
         LORIVN=PO(349)
         LOTIDN=PO(351)-1
         IF(KD1.EQ.0) THEN
           WRITE(IPU,5275) PO(LOGZN),(PO(LOSTNN+K),K=1,3)
           WRITE(IPU,5280) (PO(LONOSN+K),K=1,3),PO(LORIVN),PO(LORIVN+1)
           WRITE(IPU,5280) (PO(LOTIDN+K),K=1,3)
         ENDIF
         IF(KD1.EQ.1) WRITE(IPU,4165) PO(LOGZN),(PO(LOSTNN+K),K=1,3)
         IF(KD1.EQ.2) WRITE(IPU,4180) (PO(LOSTNN+K),K=1,3)
         IF(KD1.EQ.3) THEN
           WRITE(IPU,4185) (PO(LORC+K-1),K=1,2)
           LORC=LORC+2
         ENDIF
         IF(KD1.EQ.5) THEN
           LOSLFI=PO(229)
           WRITE(IPU,4175) PO(LOSLFI+K2-1)
         ENDIF

C.......................................................................
C     ADJUSTED STAGE TIME SERIES INFORMATION
C.......................................................................
      IF(IOBS.GT.1) THEN
        LSTE=PO(353)
        DO 5458 J=1,JN
          NGAG=IPO(LONGAG+J-1)
          N=IPO(LONBT+J-1)
          IF(J.EQ.1.AND.NGAG.EQ.N) NGAG=NGAG-1
          DO 5456 I=1,NGAG
            WRITE(IPU,5280) (PO(LSTE+L-1),L=1,3)
            LSTE=LSTE+3
 5456     CONTINUE
 5458   CONTINUE
      ENDIF
C.......................................................................
C     DAM INFORMATION
C.......................................................................
      LPLNM=PO(367)
      LIGNM=PO(368)
      LRC=LORC
      LCKO=0
      DO 440 J=1,JN
        NUML=IPO(LONLAD+J-1)
        IF(NUML.EQ.0) GO TO 440
        NDX=K13*(J-1)
        LLD=K16*(J-1)
C -- POOLS ---------------------------------------------------------------
        LCK=LCKO
        DO 420 I=1,NUML
          LL=IPO(LOLAD+LLD+I-1)
          KRCH=IPO(LORCHT+NDX+LL-1)
          IF(KRCH.NE.28) GO TO 420
          LCK=LCK+2
          WRITE(IPU,412) J,LCK,LL,(PO(LPLNM+L-1),L=1,3)
 412      FORMAT(' ID       TYPE   RIVER NO',I3,'  DAM NO.',I3,
     .           '  LAD=',I5/2A4,1X,A4)
          LPLNM=LPLNM+3
  420   CONTINUE

C -- GATE CONTROL SWITCHES ---------------------------------------- 
        LCK=LCKO
        DO 430 I=1,NUML
          LL=IPO(LOLAD+LLD+I-1)
          KRCH=IPO(LORCHT+NDX+LL-1)
          IF(KRCH.NE.28) GO TO 430
          LCK=LCK+1
          WRITE(IPU,412) J,LCK,LL,(PO(LIGNM+L-1),L=1,3)          
          LIGNM=LIGNM+3
  430   CONTINUE
        LCKO=LCK

C -- RATING CURVE AT DAMS ----------------------------------------
        IF((NYQD.EQ.0).OR.(NYQD.EQ.1.AND.KD1.EQ.3)) GO TO 440
        
        DO 435 I=1,NUML
          LL=IPO(LOLAD+LLD+I-1)
          KRCH=IPO(LORCHT+NDX+LL-1)
          IF(KRCH.NE.11) GO TO 435
          WRITE(IPU,445) J,I,(PO(LRC+L-1),L=1,2)
  445     FORMAT(10X,'RATING CURVE ID ON RIVER NO.',I3,' AT DAM NO',I3,
     .        /2A4 )
          LRC=LRC+2
  435   CONTINUE
  440 CONTINUE
C----------------  CROSS-SECTIONAL DATA  -------------------------------
 955  LOX     =PO(235)
      LOFLST  =PO(236)
      LXYDI   =PO(237)
      LXQDI   =PO(238)
      LOAS    =PO(239)
      LOHS    =PO(240)
      LOBS    =PO(241)
      LOBSS   =PO(242)
      LOBSL   =PO(243)
      LOBSR   =PO(244)
      LOASL   =PO(245)
      LOASR   =PO(246)
      LOHKC   =PO(247)
      LOQKC   =PO(248)
      LOFKC   =PO(249)
      LOFMC   =PO(250)
      LOFKF   =PO(251)
      LOFMF   =PO(252)
      LOFKO   =PO(253)
      LOFMO   =PO(254)
      LOCHMN  =PO(255)
      LOCHMX  =PO(256)
      LNN=LCNN
      DO 1300 J=1,JN
      LFLST=LOFLST+(J-1)*K2-1
      LYDI=LXYDI+(J-1)*K2-1
      LQDI=LXQDI+(J-1)*K2-1
      LLAT=LOXLAT+(J-1)*K2-1
      LLON=LOXLON+(J-1)*K2-1

      N=IPO(LONBT+J-1)
      DO 1270 II=1,N
      I=IPO(LNN+II-1)
      IM=I-1
      LAS=LOAS+(J-1)*K2*NCS+(I-1)*NCS-1
      LHS=LOHS+(J-1)*K2*NCS+(I-1)*NCS-1
      LMHS=LOHS+(J-1)*K2*NCS+(IM-1)*NCS-1
      LBS=LOBS+(J-1)*K2*NCS+(I-1)*NCS-1
      LBSS=LOBSS+(J-1)*K2*NCS+(I-1)*NCS-1
      WRITE(IPU,5145) II
      IF(NMAP.EQ.0) THEN
        WRITE(IPU,4135) PO(LFLST+I),PO(LAS+1)
      ELSE
        WRITE(IPU,4135) PO(LFLST+I),PO(LAS+1),PO(LLAT+I),PO(LLON+I)
      ENDIF
      WRITE(IPU,4140) (PO(LHS+K),K=1,NCS)
      WRITE(IPU,4140) (PO(LBS+K),K=1,NCS)
      IF(KFLP.NE.1) GO TO 1180
      LASL=LOASL+(J-1)*K2*NCS+(I-1)*NCS-1
      LASR=LOASR+(J-1)*K2*NCS+(I-1)*NCS-1
      LBSL=LOBSL+(J-1)*K2*NCS+(I-1)*NCS-1
      LBSR=LOBSR+(J-1)*K2*NCS+(I-1)*NCS-1
      WRITE(IPU,4140)(PO(LBSL+K),K=1,NCS)
      WRITE(IPU,4140)(PO(LBSR+K),K=1,NCS)
 1180 WRITE(IPU,'(/8F10.1)') (PO(LBSS+K),K=1,NCS)
      IF(KFLP.LE.1) GO TO 1270
      LHKC=LOHKC+(J-1)*K2*30+(I-1)*30-1
      LQKC=LOQKC+(J-1)*K2*30+(I-1)*30-1
      WRITE(IPU,4140)(PO(LHKC+K),K=1,KFLP)
      WRITE(IPU,4140)(PO(LQKC+K),K=1,KFLP)
 1270 CONTINUE
      LNN=LNN+K23
 1300 CONTINUE

C---------------  MANNING'S N DATA,  CONTRACTION/EXPANSION COE. -------------------------
      LOFKEC =PO(260)
      LOSNC  =PO(261)
      LOSNM  =PO(262)
      LONCM  =PO(263)
      LOYQCM =PO(264)
      LOCM   =PO(265)
      LOCML  =PO(266)
      LOCMR  =PO(267)
CC      LCXYZ  =PO(307)

      LNN=LCNN
      DO 1500 J=1,JN
      N=IPO(LONBT+J-1)
      NM=N-1
      IF(KFLP.NE.1) GO TO 1400
      DO 1350 II=1,NM
      I=IPO(LCNN+II-1)
      WRITE(IPU,*)
      LSNC=LOSNC+(J-1)*K2*NCS+(I-1)*NCS-1
      LSNM=LOSNM+(J-1)*K2*NCS+(I-1)*NCS-1
      LBSL=LOBSL+(J-1)*K2*NCS+(I-1)*NCS-1
      LBSR=LOBSR+(J-1)*K2*NCS+(I-1)*NCS-1
      WRITE(IPU,5150)
      WRITE(IPU,4145)(PO(LSNM+K),K=1,NCS)
      WRITE(IPU,5155)
      WRITE(IPU,4145)(PO(LSNC+K),K=1,NCS)
 1350 CONTINUE

 1400 CONTINUE
      LFKEC=LOFKEC+(J-1)*K2-1
      WRITE(IPU,5160)
      WRITE(IPU,4150) (PO(LFKEC+I),I=1,NM)

      IF(KFLP.GE.2) GO TO 1500
cc      WRITE(IPU,*)

      NRCM=IPO(LONCM1+J-1)
      LNCM=LONCM+(J-1)*K7
      I=IPO(LNCM+K-1)


      NRCM=IPO(LONCM1+J-1)
      CALL OLDVAL55(IPO(LNCM),IPO(LCXYZ),NRCM,IPO(LNN),N)
      WRITE(IPU,5165)
      WRITE(IPU,4155) (IPO(LCXYZ+K-1),K=1,NRCM)
CC      WRITE(IPU,4155) (IPO(LNCM+K-1),K=1,NRCM)
C    KFLP>0, USE CONVEYANCE; IMPLY NQCM=0
C    NQCM=>0 MANNING N IS FCN OF H; <0 MANNING N IS FCN OF Q
      NQCMJ=IPO(LONQCM+J-1)
      NCML=IABS(NQCMJ)
      IF(NCML.EQ.0) NCML=NCS
      DO 1480 II=1,NRCM
cc      WRITE(IPU,*)
      II2=IPO(LONCM+(J-1)*K7+II-1)
      LCM=LOCM+(J-1)*K7*K8+(II2-1)*K8-1
      WRITE(IPU,5170)
      WRITE(IPU,4160) (PO(LCM+K),K=1,NCML)
 1436 FORMAT(8F10.0)
 1440 FORMAT(8F10.2)
      IF(NQCMJ.NE.0) GO TO 1460
C    NQCM<>0, USE MANNING EQUATION, IMPLY KFLP=0
      IF(KFLP.EQ.0) GO TO 1480
      LCML=LOCML+(J-1)*K7*K8+(II2-1)*K8-1
      LCMR=LOCMR+(J-1)*K7*K8+(II2-1)*K8-1
      WRITE(IPU,*)
      WRITE(IPU,4160) (PO(LCML+K),K=1,NCML)
      WRITE(IPU,*)
      WRITE(IPU,4160) (PO(LCMR+K),K=1,NCML)
      GO TO 1480
 1460 LYQCM=LOYQCM+(J-1)*K7*K8+(II2-1)*K8-1
      WRITE(IPU,*)
      IF(NQCMJ.LT.0) WRITE(IPU,1436) (PO(LYQCM+K),K=1,NCML)
      IF(NQCMJ.GE.0) WRITE(IPU,1440) (PO(LYQCM+K),K=1,NCML)
 1480 CONTINUE
      LNN=LNN+K23
 1500 CONTINUE

C.......................................................................
C    STATISTICS FOR ADJUSTING T.S. AT GAGES
C.......................................................................
      IF(IOBS.LE.1) GO TO 1490

      MXSLC=PO(100)
      LONSLC=PO(324)
      LONQSL=PO(325)
      LOSLIC=PO(326)
      LOFRMO=PO(327)
      LOFBIO=PO(328)
      LORRMO=PO(329)
      LORBIO=PO(330)

      IF(IOBS.EQ.3) THEN
        WRITE(IPU,5410) (IPO(LONSLC+J-1),J=1,JN)
        WRITE(IPU,5420) (IPO(LONQSL+J-1),J=1,JN)
      ENDIF

      LSLIC=LOSLIC-1
      LFRMO=LOFRMO-1
      LFBIO=LOFBIO-1
      LRRMO=LORRMO-1
      LRBIO=LORBIO-1

      LNGS=LONGS
      DO 2016 J=1,JN
        NGAG=IPO(LONGAG+J-1)
        LNGS=LNGS+NGAG-1
        IF(IPO(LNGS).EQ.IPO(LONBT+J-1)) NGAG=NGAG-1
        DO 2014 I=1,NGAG
          NSLICE=IPO(LONSLC+J-1)
          IF(IOBS.EQ.3) THEN
            WRITE(IPU,2002) J,I,(PO(LSLIC+K),K=1,NSLICE)
 2002       FORMAT(10X,'SLICES FOR RIVER NO.',I5,5X,'GAGE NO.',I5,
     .             500(/8F10.1))
          ENDIF
C Following Lines changed by jls on 1/27/04	  
C          WRITE(IPU,2004) (PO(LFRMO+K),K=1,NSLICE)
C 2004     FORMAT(5X,'FRMSO:  ',500(/8F10.3))
C          WRITE(IPU,2006) (PO(LFBIO+K),K=1,NSLICE)
C 2006     FORMAT(5X,'FBIASO: ',500(/8F10.3))
C          WRITE(IPU,2008) (PO(LRRMO+K),K=1,NSLICE)
C 2008     FORMAT(5X,'RRMSO:  ',500(/8F10.3))
C          WRITE(IPU,2012) (PO(LRBIO+K),K=1,NSLICE)
C 2012     FORMAT(5X,'RBIASO: ',500(/8F10.3))
C
          WRITE(IPU,2004)
 2004     FORMAT(5X,'FRMSO:  ')
          WRITE(IPU,4145) (PO(LFRMO+K),K=1,NSLICE)
          WRITE(IPU,2006) 
 2006     FORMAT(5X,'FBIASO: ')
          WRITE(IPU,4145) (PO(LFBIO+K),K=1,NSLICE)
          WRITE(IPU,2008) 
 2008     FORMAT(5X,'RRMSO:  ')
          WRITE(IPU,4145) (PO(LRRMO+K),K=1,NSLICE)
          WRITE(IPU,2012) 
 2012     FORMAT(5X,'RBIASO: ')
          WRITE(IPU,4145) (PO(LRBIO+K),K=1,NSLICE)
C
C  End of change
C
 2014   CONTINUE
        LSLIC=LSLIC+MXSLC
        LFRMO=LFRMO+MXSLC
        LFBIO=LFBIO+MXSLC
        LRRMO=LRRMO+MXSLC
        LRBIO=LRBIO+MXSLC
 2016 CONTINUE

C ------------------------------- INITIAL CONDITIONS -----------------
 1490 IF(ICOND.EQ.0) GO TO 1555
      LXYDI =PO(316)
      LXQDI =PO(317)
      LXQLI =PO(318)
      LXPLTI=PO(319)
      LXIWTI=PO(364)

      DO 1510 J=1,JN
        N=IPO(LONB+J-1)
        LYDI=LXYDI+(J-1)*K2-1
        WRITE(IPU,5175) J
        WRITE(IPU,4115) (CO(I+LYDI),I=1,N)
 1510 CONTINUE

      DO 1520 J=1,JN
        N=IPO(LONB+J-1)
        LQDI=LXQDI+(J-1)*K2-1
        WRITE(IPU,5180) J
        WRITE(IPU,4110) (CO(I+LQDI),I=1,N)
 1520 CONTINUE

      LQLI=LXQLI-1
      DO 1530 J=1,JN
        N=IPO(LONQL+J-1)
        IF(N.EQ.0) GO TO 1530
        WRITE(IPU,5185) J
        WRITE(IPU,4110) (CO(I+LQLI),I=1,N)
        LQLI=LQLI+N
 1530 CONTINUE

      NLOCK=PO(321)
      IF(NLOCK.EQ.0) GO TO 1555
      LXPLTI=PO(319)-1
      LONLAD=PO(130)-1
      LOLAD=PO(151)-1
      LORCHT=PO(129)-1
      K16=PO(116)
      K13=PO(113)
      IP1=1
      DO 1540 J=1,JN
      NUMLAD=IPO(LONLAD+J)
      IF(NUMLAD.EQ.0) GO TO 1540
      NPL=0
      I1=K16*(J-1)
      KR=K13*(J-1)
      DO 72 I=1,NUMLAD
        LD=IPO(LOLAD+I+I1)
        KRCH=IPO(LORCHT+LD+KR)
        IF(KRCH.EQ.28) NPL=NPL+1
 72   CONTINUE
      IF(NPL.EQ.0) GO TO 1540
      IP2=IP1+NPL-1
      WRITE(IPU,5190) J
      WRITE(IPU,'(8F10.2)') (CO(LXPLTI+KK),KK=IP1,IP2)
      LXPLTI=LXPLTI+NPL
C      IP1=IP2+1
      IP1=1
 1540 CONTINUE
      LPLTI=0
   
      LXIWTI=PO(364)-1
      IP1=1
      DO 1550 J=1,JN
      NUMLAD=IPO(LONLAD+J)
      IF(NUMLAD.EQ.0) GO TO 1550
      NPL=0
      I1=K16*(J-1)
      KR=K13*(J-1)
      DO 74 I=1,NUMLAD
        LD=IPO(LOLAD+I+I1)
        KRCH=IPO(LORCHT+LD+KR)
        IF(KRCH.EQ.28) NPL=NPL+1
 74   CONTINUE
      IF(NPL.EQ.0) GO TO 1550
      IP2=IP1+NPL-1
      WRITE(IPU,5195) J
      WRITE(IPU,'(20I4)') (ICO(LXIWTI+KK),KK=IP1,IP2)
      LXIWTI=LXIWTI+NPL
C      IP1=IP2+1
      IP1=1
 1550 CONTINUE

 1555 IF(NFGRF.NE.1) THEN
        LORIVR=PO(365)-1
        WRITE(IPU,'(20A4)') (PO(LORIVR+I),I=1,20)
        DO 1560 J=1,JN
          LORIVR=LORIVR+20
          WRITE(IPU,'(20A4)') (PO(LORIVR+I),I=1,20)
 1560   CONTINUE
      ENDIF
 
C--------------- END MESSAGE  ----------------------------------------------
cc      WRITE(IPU,*)
cc      WRITE(IPU,'(18HEND OF FLDWAV DATA)')
 4000 FORMAT(3F8.3,F10.2,2F10.3,4I8)
 4005 FORMAT(8I8,3X,2I2)
C jgg modified formats for scenario lines MR 1954
 4006 FORMAT(3I5,F5.0,1X,6A4,1X,6A4)
 4007 FORMAT(3I5,6X,6A4,1X,6A4)
C
 4010 FORMAT(8I8)
 4015 FORMAT(2F8.3,3I8)
 4020 FORMAT(F8.3,F8.4,F8.4,F8.2,F8.4,2I8)
 4025 FORMAT(10F8.5)
 4030 FORMAT(10F8.3)
 4035 FORMAT(I8,2F8.5)
 4040 FORMAT(4I8)
 4045 FORMAT(I5,2I8,F10.0,3F8.2)
 4050 FORMAT(I5,2I5,26X,F10.0,3F8.2)
 4052 FORMAT(5I5,10X,F8.2,F8.0,3F8.2)
 4055 FORMAT(7I5,F8.2,F8.0,3F8.2)
 4060 FORMAT(7I8,5X,4I2)
 4065 FORMAT(4I8,5X,6I2)
 4070 FORMAT(10I8)
 4075 FORMAT(4F8.2,I8)
 4080 FORMAT(4F12.2)
 4085 FORMAT(I10,2X,2A4,1X,A4,F8.2)
 4090 FORMAT(8F8.3)
C jgg  4091 FORMAT(8F9.3) changed 3/9/04 for fldview beta bug
 4091 FORMAT(8F10.4)
 4095 FORMAT(5X,8I5)
 4100 FORMAT(F8.2,5F8.2,F8.5,2F8.2)
 4105 FORMAT(F10.2)
 4110 FORMAT(8F10.0)
 4115 FORMAT(8F10.2)
 4120 FORMAT(I10,2X,2A4,1X,A4)
 4125 FORMAT(I10,F10.2,2X,2A4,1X,A4)
 4130 FORMAT(7X,I3,10F8.2)
 4135 FORMAT(2F12.2,2f12.4)
 4140 FORMAT(/8F10.2)
 4145 FORMAT(8F10.3)
 4150 FORMAT(8F10.2)
 4155 FORMAT(15I5)
 4160 FORMAT(8F10.5)
 4165 FORMAT('     GZN  ID       TYPE'/F10.2,2X,2A4,1X,A4)
 4170 FORMAT(2F10.2,2X,2A4,1X,A4)
 4175 FORMAT('    SLFI'/F8.6)
 4180 FORMAT('  ID       TYPE'/2X,2A4,2X,A4)
 4185 FORMAT('RATING CURVE ID'/2A4)
 4190 FORMAT(F10.2,2X,2A4,1X,A4)

 5000 FORMAT('    EPSY   THETA      F1     XFACT     DTHYD     DTOUT  ME
     .TRIC')
 5005 FORMAT('      JN      NU   ITMAX   KWARM    KFLP     NET   ICOND     
     .  NMAP   FUTURE DATA')
 5010 FORMAT('    NYQD     KCG     NCG   KPRES')
 5015 FORMAT('     NCS     KPL     JNK  KREVRS   NFGRF')
 5017 FORMAT('    IOBS   KTERM      NP    NPST   NPEND')
 5020 FORMAT('   TDBG1   TDBG2  JNKDBG   JDBG1   JDBG2')
 5025 FORMAT('   LDBG1   LDBG2  MCMDBG')
 5030 FORMAT('     TEH   DTHII  DTHPLT   FRDFR   DTEXP     MDT')
 5035 FORMAT('    DTIN')
 5040 FORMAT('     DTN')
 5045 FORMAT('    NLEV    DHLV   DTHLV')
 5050 FORMAT('    NJFT    NIFT    NJTT    NITT')
 5052 FORMAT('    MPRV  MPLOC1  MPLOC2   DTMAP')
 5055 FORMAT('     NBT    NPT1    NPT2    EPQJ    COFW   VWIND  WINAGL')
 5060 FORMAT('  NBT NPT1 NPT2  MRV NJUN  MRU NJUM     ATF     EPQJ    COFW
     . VIWND  WINAGL')
 5065 FORMAT('      KU      KD     NQL    NGAG    NCM1     NQCM    NSTR    
     . FUTURE DATA')
 5070 FORMAT('    MIXF     MUD    KFTR    KLOS   FUTURE DATA')
 5075 FORMAT('    KLPI      NLPI')
 5080 FORMAT('      UW       VIS       SHR      POWR       IWF')
 5085 FORMAT('    XLOS     XLOS2      QLOS      ALOS')
 5095 FORMAT('      XT FOR RIVER NO.',I2)
 5100 FORMAT('     DXM FOR RIVER NO.',I2)
 5105 FORMAT('   KRCHT FOR RIVER NO.',I2)
 5110 FORMAT('    HWLV    WCLV    TFLV    HFLV   BLVMX   HLVMN     SLV
     .  HPLV    DPLV')
 5115 FORMAT('    HPND')
 5120 FORMAT('     SAP')
 5125 FORMAT('    HSAP')
 5130 FORMAT('       LQ1  ID       TYPE   FOR RIVER NO.',I2)
 5135 FORMAT('       NGS      GZ     ID      TYPE    FOR RIVER NO.',I2)
 5137 FORMAT('       NST  ID       TYPE      GZ    FOR RIVER NO.',I2)
 5138 FORMAT('       STM  ID       TYPE      GZ1')
 5140 FORMAT('       TPG     RHO      GAMA       YQI FOR RIVER NO.',
     .        I2)
 5145 FORMAT('        FLST          AS    SECT=',I6)
 5150 FORMAT('     SNM')
 5155 FORMAT('     SNC')
 5160 FORMAT('    FKEC')
 5165 FORMAT('     NCM')
 5170 FORMAT(' CM/YQCM')
 5175 FORMAT('    YDI FOR RIVER NO.',I2)
 5180 FORMAT('    QDI FOR RIVER NO.',I2)
 5185 FORMAT('    QLI FOR RIVER NO.',I2)
 5190 FORMAT('   PLTI FOR RIVER NO.',I2)
 5195 FORMAT('   IWTI FOR RIVER NO.',I2)
 5275 FORMAT(' GAGE ZERO  ID       TYPE'/F10.2,2X,2A4,1X,A4)
 5280 FORMAT('  ID       TYPE'/2X,2A4,2X,A4,2X,2A4)
 5410 FORMAT(8X,5HNSLC=,5X,8I10,500(/10I10))
 5420 FORMAT(8X,5HNQSL=,5X,8I10,500(/10I10))
      RETURN
      END



