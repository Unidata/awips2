      SUBROUTINE PRP55(PO,IPO,MSG)

C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C     add system_name and scenario_name to the print output
C

      INCLUDE 'updaio'

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'

      CHARACTER RTYP*8,STRUC*6,RSON(3)*20,RIVERJ*16
      CHARACTER*80 MSG
cc    CHARACTER*4 MSG(20,20)
      INTEGER IPO(*)
      DIMENSION PO(*),IFUT(6)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/prp55.f,v $
     . $',                                                             '
     .$Id: prp55.f,v 1.12 2004/09/24 22:46:32 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA  SNAME / 'PRP55   ' /

cjms  add fldview capability

      CALL FPRBUG(SNAME, 1, 55, IBUG)

      DO 5 L=1,6
        IFUT(L)=0
 5    CONTINUE

      WRITE(IPR,5000)
 5000 FORMAT(
cc     .       /10X,'            PROGRAM FLDWAV -  VERSION 1.0  9/30/98'
cc     .     ///10X,'                HYDROLOGIC RESEACH LABORATORY'
cc     .       /10X,'                  W/OH1 OFFICE OF HYDROLOGY'
cc     .       /10X,'                NOAA, NATIONAL WEATHER SERVICE'
cc     .       /10X,'                SILVER SPRING, MARYLAND  20910'
     .    ////10X,'                *******************************'
     .       /10X,'                *******************************'
     .       /10X,'                ***                         ***'
     .       /10X,'                ***  SUMMARY OF INPUT DATA  ***'
     .       /10X,'                ***                         ***'
     .       /10X,'                *******************************'
     .       /10X,'                *******************************'///)

      LMSG=PO(10)-1
        NMSG=PO(49)
        DO 10 L=1,NMSG

        WRITE(IPR,6) (PO(LMSG+K),K=1,20)
 6      FORMAT(20A4)
        LMSG=LMSG+20
   10 CONTINUE

      EPSY   =PO(11)
      THETA  =PO(12)
      F1     =PO(13)
      XFACT  =PO(14)
      DTHYD  =PO(15)
      METRIC =PO(16)
      DTOUT  =PO(23)
      NODESC =PO(26)

      IF(NODESC.EQ.1) THEN
        WRITE(IPR,5005)
        WRITE(IPR,4000) EPSY,THETA,F1,XFACT,DTHYD,DTOUT,METRIC
      ELSE
        WRITE(IPR,5010) EPSY,THETA,F1,XFACT,DTHYD,DTOUT,METRIC
      ENDIF

      JN=PO(17)
      NU=PO(18)
      ITMAX=PO(19)
      KWARM=PO(20)
      KFLP=PO(21)
      NET=PO(22)
      ICOND=PO(25)
      NMAP=PO(375)
      IF(NODESC.EQ.1) THEN
        WRITE(IPR,5015)
        WRITE(IPR,4005) JN,NU,ITMAX,KWARM,KFLP,NET,ICOND,NMAP,
     .     (IFUT(L),L=1,2)
      ELSE
        WRITE(IPR,5020) JN,NU,ITMAX,KWARM,KFLP,NET,ICOND,NMAP,2
      ENDIF

      NYQD=PO(27)
      KCG=PO(28)
      NCG=PO(29)
      KPRES=PO(30)
      IF(NODESC.EQ.1) THEN
        WRITE(IPR,5025)
        WRITE(IPR,4010) NYQD,KCG,NCG,KPRES
      ELSE
        WRITE(IPR,5030) NYQD,KCG,NCG,KPRES
      ENDIF

      NCS=PO(31)
      KPL=PO(32)
      JNK=PO(33)
      KREVRS=PO(34)
      NFGRF=PO(35)
      IF(NODESC.EQ.1) THEN
        WRITE(IPR,5035)
        WRITE(IPR,4010) NCS,KPL,JNK,KREVRS,NFGRF
      ELSE
        WRITE(IPR,5040) NCS,KPL,JNK,KREVRS,NFGRF
      ENDIF

      IOBS=PO(36)
      KTERM=PO(37)
      NP=PO(38)
      NPST=PO(39)
      NPEND=PO(40)

      IF(NODESC.EQ.1) THEN
        WRITE(IPR,5045)
        WRITE(IPR,4010) IOBS,KTERM,NP,NPST,NPEND
      ELSE
        WRITE(IPR,5050) IOBS,KTERM,NP,NPST,NPEND
      ENDIF

      IF(JNK.GE.0) GO TO 160
      TDBG1=PO(41)
      TDBG2=PO(42)
      JNKDBG=PO(43)
      JDBG1=PO(44)
      JDBG2=PO(45)
      LDBG1=PO(46)
      LDBG2=PO(47)
      MCMDBG=PO(48)
      WRITE(IPR,5055) TDBG1,TDBG2,JNKDBG,JDBG1,JDBG2
      IF(NP.LT.0) WRITE(IPR,5060) LDBG1,LDBG2,MCMDBG
  160 CONTINUE

      TEH=PO(50)
      DTHII=PO(51)
      DTHPLT=PO(52)
      FRDFR=PO(53)
      DTEXP=PO(54)
      MDT=PO(55)
      NDT=PO(56)
      IF(NODESC.EQ.1) THEN
        WRITE(IPR,5065)TEH,DTHII,DTHPLT,FRDFR,DTEXP,MDT
      ELSE
        WRITE(IPR,5070) TEH,DTHII,DTHPLT,FRDFR,DTEXP,MDT
      ENDIF

      LODTIN=PO(57)
      LOTDTN=PO(58)
      IF(NDT.LE.0) GO TO 180
      WRITE(IPR,5075) (PO(LODTIN+K-1),K=1,NDT)
      WRITE(IPR,5080) (PO(LOTDTN+K-1),K=1,NDT)
  180 CONTINUE

CC      NNLEV=0
CC      NLEV=PO(60)
CC      IF(NLEV.GT.0) NNLEV=PO(59)
      NNLEV=PO(59)
      DHLV=PO(61)
      DTHLV=PO(62)
      IF (DTHLV.GE.9999.99) DTHLV=0.0
      IF(NODESC.EQ.1) THEN
        WRITE(IPR,5085) NNLEV,DHLV,DTHLV
      ELSE
        WRITE(IPR,5090)  NNLEV,DHLV,DTHLV
      ENDIF

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
      LONB   =PO(126)
      LONJFM=PO(274)
      LONIFM=PO(275)
      LONJTO=PO(276)
      LONITO=PO(277)
      LOMRU=PO(369)
      LONJUM=PO(370)
      LOMPRV=PO(371)
      LOMPLC=PO(372)-1
cc      LOXLAT=PO(373)
cc      LOXLON=PO(374)
      LODTMP=PO(377)
      LOSPTH=PO(379)
      LOTPTH=PO(380)

      IF(NNLEV.EQ.0) GOTO 200
      IF(NODESC.EQ.0) WRITE(IPR,5095)
      WRITE(IPR,5100)
      DO 190 L=1,NNLEV
      WRITE(IPR,4015) L,IPO(LONJFM+L-1),IPO(LONIFM+L-1),IPO(LONJTO+L-1),
     .                 IPO(LONITO+L-1)
  190 CONTINUE

  200 IF(NMAP.EQ.0) GO TO 240
      IF(NODESC.EQ.0) WRITE(IPR,5097)
      WRITE(IPR,5102)
      DO 205 L=1,NMAP
C jgg        WRITE(IPR,4016) L,IPO(LOMPRV+L-1),IPO(LOMPLC+L-1),IPO(LOMPLC+L),
C jgg     .                  PO(LODTMP)
C jgg added following to only write DTMAP for first scenario
        IF(L.EQ.1) THEN
          WRITE(IPR,4016) L,IPO(LOMPRV+L-1),IPO(LOMPLC+2*L-1),
     .      IPO(LOMPLC+2*L),PO(LODTMP+L-1),(PO(LOSPTH+L1-1),L1=1,6),
     .                   (PO(LOTPTH+L2-1),L2=1,6)
        ELSE
          WRITE(IPR,4017) L,IPO(LOMPRV+L-1),IPO(LOMPLC+2*L-1),
     .      IPO(LOMPLC+2*L),(PO(LOSPTH+L1-1),L1=1,6),
     .                   (PO(LOTPTH+L2-1),L2=1,6)
	
	ENDIF
        LOSPTH=LOSPTH+6
        LOTPTH=LOTPTH+6

  205 CONTINUE

  240 IF(NODESC.EQ.0) WRITE(IPR,5105)
      IF(JN.EQ.1) THEN
        WRITE(IPR,5110)
        WRITE(IPR,4020) 1,IPO(LONB),IPO(LONPT),IPO(LONPT+1),
     .     PO(LOEPQJ),PO(LOCOFW),PO(LOVWND),PO(LOWAGL)
      ELSE
        WRITE(IPR,5115)
        WRITE(IPR,4025) 1,IPO(LONB),IPO(LONPT),IPO(LONPT+1),
     .     PO(LOEPQJ),PO(LOCOFW),PO(LOVWND),PO(LOWAGL)
      ENDIF

      IF(JN.LE.1) GOTO 260
      DO 250 J=2,JN
        NPT1=IPO(LONPT+(J-1)*2)
        NPT2=IPO(LONPT+(J-1)*2+1)
        WRITE(IPR,4030) J,IPO(LONB+J-1),NPT1,NPT2,IPO(LOMRV+J-1),
     .    IPO(LONJUN+J-1), IPO(LOMRU+J-1), IPO(LONJUM+J-1),
     .    PO(LOATF+J-1),PO(LOEPQJ+J-1),PO(LOCOFW+J-1),
     .    PO(LOVWND+J-1),PO(LOWAGL+J-1)
  250 CONTINUE

  260 IF(NODESC.EQ.0) WRITE(IPR,5120)
      WRITE(IPR,5125)
      DO 280 J=1,JN
      WRITE(IPR,4035) J,IPO(LOKU+J-1),IPO(LOKD+J-1),IPO(LONQL+J-1),
     .  IPO(LONGAG+J-1),IPO(LONCM1+J-1),IPO(LONQCM+J-1),IPO(LONSTR+J-1),
     .  (IFUT(L),L=1,3)
  280 CONTINUE

      IF(NODESC.EQ.0) WRITE(IPR,5130)
      WRITE(IPR,5135)
      DO 295 J=1,JN
      WRITE(IPR,4040) J,IPO(LOMIXF+J-1),IPO(LOMUD+J-1),IPO(LOKFTR+J-1),
     .   IPO(LOKLOS+J-1),(IFUT(L),L=1,6)
  295 CONTINUE

      NLPI=PO(97)
      IF (NLPI.EQ.0) GOTO 320
      WRITE(IPR,5140)
      WRITE(IPR,4045) (IPO(LOKLPI+K-1),K=1,NLPI)

  320 IF(IPO(LOMUD).EQ.0) GOTO 340
      MUD1=0
      MUD2=0
      DO 325 J=1,JN
        IF(IPO(LOMUD+J-1).EQ.1) MUD1=1
        IF(IPO(LOMUD+J-1).EQ.1) MUD2=1
  325 CONTINUE
      IF(NODESC.EQ.0) THEN
        IF(MUD1.EQ.1) WRITE(IPR,5145)
        IF(MUD2.EQ.1) WRITE(IPR,5150)
      END IF
      WRITE(IPR,5155)
      DO 330 J=1,JN
      IF (IPO(LOMUD+J-1).EQ.0) GOTO 330
      WRITE(IPR,4050) J,PO(LOUW+J-1),PO(LOVIS+J-1),PO(LOSHR+J-1),
     .  PO(LOPOWR+J-1),IPO(LOIWF+J-1)
  330 CONTINUE

  340 NLOS=PO(98)
      IF(NLOS.EQ.0) GOTO 360
      IF(NODESC.EQ.0) WRITE(IPR,5160)
      WRITE(IPR,5165)
      DO 350 J=1,JN
        IF(IPO(LOKLOS+J-1).EQ.0) GOTO 350
        WRITE(IPR,4055) J,PO(LOXLOS+J-1),PO(LOXLOS+J),PO(LOQLOS+J-1),
     .                   PO(LOALOS+J-1)
  350 CONTINUE

C       XT   DXM   KRCHT
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
      k25 =PO(125)
      LOXT   =PO(127)
      LODXM  =PO(128)
      LORCHT =PO(129)
      LONLAD =PO(130)
      LOLROT =PO(131)
      LOLQ1  =PO(132)
      LOX    =PO(235)
      LOQL   =PO(350)

      IF(NODESC.EQ.0) WRITE(IPR,5170)
      DO 450 J=1,JN
      N=IPO(LONBT+J-1)
      ICKVAL=999999
      IF (N.GT.ICKVAL) THEN
         WRITE (IPR,997) 'N',N,ICKVAL
997   FORMAT ('0**WARNING** IN PRP55 - VALUE OF VARIABLE ',A,' (',I11,
     *   ') IS GREATER THAN ',I7,'.')
         CALL WARN
         GO TO 999
      ENDIF
      NXT=K23*(J-1)-1
      NDX=K13*(J-1)-1
      WRITE(IPR,5175) J,J
      WRITE(IPR,4060) (PO(LOXT+NXT+KK),KK=1,N)

      WRITE(IPR,5180) J,J
      WRITE(IPR,4060) (PO(LODXM+NDX+KK),KK=1,N-1)

      WRITE(IPR,5185) J
      WRITE(IPR,4065) (IPO(LORCHT+NDX+KK),KK=1,N-1)
  450 CONTINUE

      IF(NNLEV.EQ.0) GOTO 490
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
      IF(NODESC.EQ.0) WRITE(IPR,5190)
      WRITE(IPR,5195)
      DO 480 L=1,NNLEV
      JFM=IPO(LONJFM+L-1)
      IFM=IPO(LONIFM+L-1)
      XLEV=PO(LOX+(JFM-1)*K2+IFM-1)
      HPL=PO(LOHPLV+L-1)
      IF(HPL.GT.9999.) HPL=10000.
      WRITE(IPR,4070) L,IPO(LONJFM+L-1),IPO(LONIFM+L-1),IPO(LONJTO+L-1),
     * IPO(LONITO+L-1),XLEV,
     * PO(LOHWLV+L-1),PO(LOWCLV+L-1),PO(LOTFLV+L-1),PO(LOBLMX+L-1),
     * PO(LOHFLV+L-1),PO(LOHLMN+L-1),PO(LOSLV+L-1),HPL,
     * PO(LODPLV+L-1)
  480 CONTINUE

  490 NPOND   =PO(148)
      IF (NPOND.EQ.0) GOTO 530
      IF(NODESC.EQ.0) WRITE(IPR,5200)
      ICKVAL=9999
cc      IF (IBUG.EQ.1) WRITE (UE,*) 'IN PRP55 - NPOND=',NPOND
      IF (IBUG.EQ.1) WRITE (IODBUG,*) 'IN PRP55 - NPOND=',NPOND
      IF (NPOND.GT.ICKVAL) THEN
         WRITE (IPR,997) 'NPOND',NPOND,ICKVAL
         CALL WARN
         GO TO 530
         ENDIF
      ICKVAL=999999
cc      IF (IBUG.EQ.1) WRITE (UE,*) 'IN PRP55 - LOHPND=',LOHPND
      IF (IBUG.EQ.1) WRITE (IODBUG,*) 'IN PRP55 - LOHPND=',LOHPND
      IF (LOHPND.GT.ICKVAL) THEN
         WRITE (IPR,997) 'LOHPND',LOHPND,ICKVAL
         CALL WARN
         GO TO 530
      ENDIF
cc      IF (IBUG.EQ.1) WRITE (UE,*) 'IN PRP55 - LSAP=',LSAP
      IF (IBUG.EQ.1) WRITE (IODBUG,*) 'IN PRP55 - LSAP=',LSAP
      IF (LSAP.GT.ICKVAL) THEN
         WRITE (IPR,997) 'LSAP',LSAP,ICKVAL
         CALL WARN
         GO TO 530
      ENDIF
cckwz      IF (IBUG.EQ.1) WRITE (UE,*) 'IN PRP55 - LHSAP=',LHSAP
cckwz      IF (LHSAP.GT.ICKVAL) THEN
cckwz         WRITE (IPR,997) 'LHSAP',LHSAP,ICKVAL
cckwz         CALL WARN
cckwz         GO TO 530
cckwz         ENDIF
cckwz LHSAP was not initialized before this line, but it does 4 lines below.
      DO 520 L=1,NPOND
      WRITE(IPR,5205) L,PO(LOHPND+L-1)
      LSAP=LOSAP+(L-1)*8-1
      LHSAP=LOHSAP+(L-1)*8-1
      WRITE(IPR,5210) (PO(LSAP+KK),KK=1,8)
      WRITE(IPR,5215) (PO(LHSAP+KK),KK=1,8)
  520 CONTINUE

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
cc      LOTS1   =PO(322)
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
cc            IF (KRA.EQ.28) THEN
cc            KLOCK=KLOCK+1
cc            ILOCK=LOTS1+(KLOCK-1)*10
cc            ENDIF
      IF(KRA.EQ.35) THEN
        NBRG=NBRG+1
        GO TO 630
      ELSE
        NDAM=NDAM+1
      ENDIF
C DAM AND RESERVOIR
      WRITE(IPR,5220) J,NDAM
      IRES=0
      IF(I.EQ.1.OR.KRB.EQ.4) IRES=1
      CALL PPDAM55(K,J,KRA,PO(LOHDD),PO(LOCLL),PO(LOHSPD),PO(LOSPL),
     1 PO(LOCRL),PO(LOHCRL),PO(LOCSD),PO(LOHGTD),PO(LOCGD),PO(LOCDOD),
     2 PO(LOQTD),PO(LOHFDD),PO(LOTFH),PO(LOBBD),PO(LOZBCH),PO(LOYMIN),
     3 PO(LORHI),PO(LORQI),PO(LCTCG),PO(LOQGH),PO(LOCGCG),PO(LOQHT),
     4 PO(LONG),PO(LOLAD),PO(LOICG),PO(LODTHF),PO(LOBEXP),
     5 PO(LOCPIP),PO(LOTIQH),PO(LONLAD),PO(LOGSIL),PO(LOGWID),
     6 PO(LOTGHT),PO(LOGHT),PO(LOICHN),PO(LOPTAR),PO(LOCHTW),
     7 PO(LOGZPL),PO(LOSAR),PO(LOHSAR),
     8 IRES,NODESC,K1,K16,K19,K20,K21)
      GOTO 650
C   BRIDGE
  630 WRITE(IPR,5225) J,NBRG
      CALL PPBRG55(K,J,PO(LOEBE1),PO(LOBRGW),PO(LOHFDD),PO(LOTFH),
     2  PO(LOBBD),PO(LOZBCH),PO(LOYMIN),PO(LOLAD),PO(LODTHF),
     3  PO(LOBEXP),PO(LOEBE2),PO(LOCDBR),PO(LOBRBS),PO(LOBRHS),
     4  PO(LOCPIP),PO(LOEBW2),PO(LOEBW1),NODESC,K1,K16,0)
  650 CONTINUE

C......................................................................
C    LATERAL FLOWS T.S. INFORMATION
C......................................................................
      LLQ1=LOLQ1-1
      LQL=LOQL-1
      DO 800 J=1,JN
        NQLJ=IPO(LONQL+J-1)
        IF(NQLJ.EQ.0) GOTO 795
        WRITE(IPR,5230) J
        DO 790 L=1,NQLJ
          IF(L.EQ.1) WRITE(IPR,5235)
          WRITE(IPR,4075) L,IPO(LLQ1+L),(PO(LQL+LL),LL=1,3)
          LQL=LQL+3
  790   CONTINUE
  795   LLQ1=LLQ1+K10
  800 CONTINUE
C.......................................................................
C     PLOTTING GAGE STATION T.S. INFORMATION
C.......................................................................
      LONGS  =PO(211)
      LOGZ   =PO(212)
      LOSTT  =PO(352)
      LOSTQ  =PO(354)

      LSTT=LOSTT-1
      LSTQ=LOSTQ-1
      LNGS=LONGS-1
      LGZ=LOGZ-1

      DO 900 J=1,JN
        NG=IPO(LONGAG+J-1)
        IF(NG.LE.0) GO TO 895
CC        NOB=K4*(J-1)-1
        WRITE(IPR,5240) J
        DO 890 I=1,NG
          IF(I.EQ.1) WRITE(IPR,5245)
          IF(IOBS.LE.0.OR.KPL.EQ.2) THEN
            WRITE(IPR,4082) I,IPO(LNGS+I),(PO(LSTT+LN),LN=1,3)
          ELSE
            WRITE(IPR,4080) I,IPO(LNGS+I),PO(LGZ+I),(PO(LSTT+LN),LN=1,3)
          ENDIF
          LSTT=LSTT+5
          IF(KPL.EQ.3) THEN
            WRITE(IPR,4082) I,IPO(LNGS+I),(PO(LSTQ+LN),LN=1,3)
            LSTQ=LSTQ+3
          ENDIF
  890   CONTINUE
  895   LNGS=LNGS+K4
        LGZ=LGZ+K4
  900 CONTINUE
C.......................................................................
C     OUTPUT STATION T.S. INFORMATION
C.......................................................................
      NTOUT=IPO(331)
      IF(NTOUT.EQ.0) GO TO 941
      LONST=PO(358)-1
      LOGZO=PO(360)-1
      LOKTYP=PO(359)-1
      LOQLST=PO(356)-1
      LNST=LONST
      LGZO=LOGZO
      LKTYP=LOKTYP
      LQLST=LOQLST

      DO 920 J=1,JN
      NOUT=IPO(LONSTR+J-1)
      IF(NOUT.LE.0) GO TO 915
      WRITE(IPR,5250) J
      DO 910 I=1,NOUT
      IF(I.EQ.1) WRITE(IPR,5255)
      KTYP=IPO(LKTYP+I)
      IF(KTYP.EQ.1) WRITE(IPR,4085) I,IPO(LNST+I),
     .  (PO(LQLST+LL),LL=1,3),PO(LGZO+I)
C  Next line changed by jls on 1/27/04
C      IF(KTYP.EQ.2) WRITE(IPR,4085) I,IPO(LNST+I),    
      IF(KTYP.EQ.2.OR.KTYP.EQ.3) WRITE(IPR,4085) I,IPO(LNST+I),
     .  (PO(LQLST+LL),LL=1,3)
      LQLST=LQLST+3
  910 CONTINUE
  915 LNST=LNST+K14
      LGZO=LGZO+K14
      LKTYP=LKTYP+K14
  920 CONTINUE
C.......................................................................
C     TPG    --  TIME FROM INITIAL STEADY FLOW TO PEAK IN MATH.HYDR
C     RHO    --  RATIO OF PEAK TO INITIAL FLOW IN MATH.HYDROGRAPH
C     GAMA   --  RATIO OF TG TO TPG; TG IS TIME FROM INITIAL FLOW
C                CENTROID OF MATH. HYDROGRAPH
C     YQI    --  INITIAL DISCH. OR WATER ELEV. IN MATH. HYDROGRAPH
C.......................................................................
  941 IF(IOBS.GE.0) GOTO 950
      LOTPG =PO(217)
      LORHO =PO(218)
      LOGAMA=PO(219)
      LOYQI =PO(220)
      WRITE(IPR,5260)
      DO 946 J=1,JN
        WRITE(IPR,4090) J,PO(LOTPG+J-1),PO(LORHO+J-1),PO(LOGAMA+J-1),
     *      PO(LOYQI+J-1)
  946 CONTINUE
      GO TO 1000
C.......................................................................
C
C    UPSTREAM BOUNDARY T.S. INFORMATION
C
C------------------------------------------------------------------------
  950 LOGZ1=PO(221)-1
      LOGZN=PO(222)-1
      LOST1=PO(361)-1
      LOSTM=PO(363)-1
      LST1=LOST1

      WRITE(IPR,5265)
      DO 990 J=1,JN-NET
        IF(IPO(LOKU+J-1).EQ.2) WRITE(IPR,4095) J,PO(LOSTM+J),
     .       (PO(LST1+JJ),JJ=1,3)
        IF(IPO(LOKU+J-1).EQ.1) WRITE(IPR,4097) J,PO(LOSTM+J),
     .      PO(LOGZ1+J),(PO(LST1+JJ),JJ=1,3)
        LST1=LST1+3
  990 CONTINUE
 1000 CONTINUE
C.......................................................................
C    DOWNSTREAM BOUNDARY T.S. INFORMATION
C.......................................................................
      WRITE(IPR,5270)
      KD1=IPO(LOKD)
cc      IF(KD1.LE.2) WRITE(IPR,*) '   RIVER (J)           ID            TYPE
cc     #(GAGE ZERO)'
      LOGZN=PO(222)
      LOSTN=PO(362)
      LONOS=PO(348)
      LORVN=PO(349)
      LOTDE=PO(351)
      LORC=PO(366)
      N=IPO(LONB)
      LOSLFI=PO(229+N-1)
      IF(KD1.EQ.0) THEN
        WRITE(IPR,5275) (PO(LOSTN+L-1),L=1,3),PO(LOGZN)
        WRITE(IPR,5280) (PO(LONOS+L-1),L=1,3),PO(LORVN)
        WRITE(IPR,5280) (PO(LOTDE+L-1),L=1,3)
      ENDIF
      IF(KD1.EQ.1) WRITE(IPR,5275) (PO(LOSTN+L-1),L=1,3),PO(LOGZN)
      IF(KD1.EQ.2) WRITE(IPR,5280) (PO(LOSTN+L-1),L=1,3)
      IF(KD1.EQ.3) THEN
        WRITE(IPR,5285) (PO(LORC+L-1),L=1,2)
        LORC=LORC+2
      ENDIF
      IF(KD1.EQ.5) WRITE(IPR,5290) PO(LOSLFI)

C.......................................................................
C     ADJUSTED STAGE TIME SERIES INFORMATION
C.......................................................................
      IF(IOBS.GT.1) THEN
        LSTE=PO(353)
        DO 5458 J=1,JN
          NGAG=IPO(LONGAG+J-1)
          N=IPO(LONBT+J-1)
          IF(J.EQ.1.AND.NGAG.EQ.N) NGAG=NGAG-1
          WRITE(IPR,5452) 
 5452     FORMAT(/1X,'STATION (I)',4X,'NGS(I,J)',5X,'ID',10X,
     .             'TYPE   (ADJUSTED STAGE)')
          DO 5456 I=1,NGAG
            WRITE(IPR,5280) (PO(LSTE+L-1),L=1,3)
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
C -- POOLS --
        LCK=LCKO
        DO 420 I=1,NUML
CC          LL=I+LLD
          LL=IPO(LOLAD+LLD+I-1)
          KRCH=IPO(LORCHT+NDX+LL-1)
          IF(KRCH.NE.28) GO TO 420
          IF(LCK.EQ.LCKO) WRITE(IPR,412) J
  412     FORMAT(/10X,'TIME SERIES FOR POOL ELEVATIONS ON RIVER NO.',I3/
     .      15X,'NO',7X,'LAD',8X,'ID',8X,'TYPE')
          LCK=LCK+1
          WRITE(IPR,5400) LCK,LL,(PO(LPLNM+L-1),L=1,3)          
          LPLNM=LPLNM+3
  420   CONTINUE

C -- GATE CONTROL SWITCHES -- 
        LCK=LCKO
        DO 430 I=1,NUML
          LL=IPO(LOLAD+LLD+I-1)
          KRCH=IPO(LORCHT+NDX+LL-1)
          IF(KRCH.NE.28) GO TO 430
          IF(LCK.EQ.LCKO) WRITE(IPR,422) J
  422   FORMAT(/10X,'TIME SERIES FOR GATE CONTROL SWITCHES ON RIVER NO.'
     .      ,I3/15X,'NO',7X,'LAD',8X,'ID',8X,'TYPE')
           LCK=LCK+1
          WRITE(IPR,5400) LCK,LL,(PO(LIGNM+L-1),L=1,3)
          LIGNM=LIGNM+3
  430   CONTINUE
        LCKO=LCK

C -- RATING CURVE AT DAMS --
        IF((NYQD.EQ.0).OR.(NYQD.EQ.1.AND.KD1.EQ.3)) GO TO 440
        
        DO 435 I=1,NUML
          LL=IPO(LOLAD+LLD+I-1)
          KRCH=IPO(LORCHT+NDX+LL-1)
          IF(KRCH.NE.11) GO TO 435
          WRITE(IPR,445) J,I,(PO(LRC+L-1),L=1,2)
  445     FORMAT(/10X,'RATING CURVE ID ON RIVER NO.',I3,' AT DAM NO',I3,
     .        ': ',2A4 )
          LRC=LRC+2
  435   CONTINUE   
  440 CONTINUE
C.......................................................................
C    CROSS-SECTIONAL DATA
C.......................................................................
 955  LOFLST  =PO(236)
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
      LOFKEC  =PO(260)
      LOKRCH  =PO(273)
      LOASS   =PO(278)
      LOMPRV  =PO(371)
      LOMPLC  =PO(372)
      LOXLAT  =PO(373)
      LOXLON  =PO(374)

      IF(NODESC.EQ.0) WRITE(IODBUG,5295)
      IF((NP.EQ.-3.OR.NP.EQ.-4).AND.NODESC.EQ.0) WRITE(IODBUG,5300)

      DO 1300 J=1,JN
      WRITE(IPR,1120) J
 1120 FORMAT(//1X,'RIVER NO.',I3)
      LX=LOX+(J-1)*K2-1
      LKRCH=LOKRCH+(J-1)*K2-1
      LFKEC=LOFKEC+(J-1)*K2-1
      LFLST=LOFLST+(J-1)*K2-1
      LLAT=LOXLAT+(J-1)*K2-1
      LLON=LOXLON+(J-1)*K2-1
      N=IPO(LONB+J-1)
      DO 1270 II=1,N
      I=II
      IM=I-1
      LAS=LOAS+(J-1)*K2*NCS+(I-1)*NCS-1
      LHS=LOHS+(J-1)*K2*NCS+(I-1)*NCS-1
      LMHS=LOHS+(J-1)*K2*NCS+(IM-1)*NCS-1
      LBS=LOBS+(J-1)*K2*NCS+(I-1)*NCS-1
      LBSS=LOBSS+(J-1)*K2*NCS+(I-1)*NCS-1
      LASS=LOASS+(J-1)*K2*NCS+(I-1)*NCS-1

      IF(NMAP.EQ.0) THEN
        IF(I.LT.N) WRITE(IPR,5303) I,PO(LFLST+I),PO(LX+I),IPO(LKRCH+I),
     .        PO(LFKEC+I)
        IF(I.EQ.N) WRITE(IPR,5304) I,PO(LFLST+I),PO(LX+I)
      ELSE
        IF(I.LT.N) WRITE(IPR,5305) I,PO(LFLST+I),PO(LX+I),PO(LLAT+I),
     .        PO(LLON+I),IPO(LKRCH+I),PO(LFKEC+I)
        IF(I.EQ.N) WRITE(IPR,5306) I,PO(LFLST+I),PO(LX+I),PO(LLAT+I),
     .        PO(LLON+I)
      ENDIF

      WRITE(IPR,5310) (PO(LHS+K),K=1,NCS)
      WRITE(IPR,5315) (PO(LBS+K),K=1,NCS)
      WRITE(IPR,5331) (PO(LAS+K),K=1,NCS)
      IF(KFLP.NE.1) GO TO 1180
      LASL=LOASL+(J-1)*K2*NCS+(I-1)*NCS-1
      LASR=LOASR+(J-1)*K2*NCS+(I-1)*NCS-1
      LBSL=LOBSL+(J-1)*K2*NCS+(I-1)*NCS-1
      LBSR=LOBSR+(J-1)*K2*NCS+(I-1)*NCS-1
      WRITE(IPR,5320)(PO(LBSL+K),K=1,NCS)
      WRITE(IPR,5325)(PO(LBSR+K),K=1,NCS)
      WRITE(IPR,5332) (PO(LASL+K),K=1,NCS)
      WRITE(IPR,5333) (PO(LASR+K),K=1,NCS)
 1180 WRITE(IPR,5330) (PO(LBSS+K),K=1,NCS)
      WRITE(IPR,5334) (PO(LASS+K),K=1,NCS)

      IF(KFLP.LE.1) GO TO 1270
      LHKC=LOHKC+(J-1)*K2*30+(I-1)*30-1
      LQKC=LOQKC+(J-1)*K2*30+(I-1)*30-1
      WRITE(IPR,5335)(PO(LHKC+K),K=1,KFLP)
      WRITE(IPR,5340)(PO(LQKC+K),K=1,KFLP)
 1270 CONTINUE
 1300 CONTINUE

C.......................................................................
C    MANNING'S N DATA
C.......................................................................
      LOSNC  =PO(261)
      LOSNM  =PO(262)
      LONCM  =PO(263)
      LOYQCM =PO(264)
      LOCM   =PO(265)
      LOCML  =PO(266)
      LOCMR  =PO(267)

      IF(NODESC.EQ.0) WRITE(IODBUG,5345)
      DO 1500 J=1,JN
      N=IPO(LONBT+J-1)
      NM=N-1
      WRITE(IPR,5350) J

      IF(KFLP.NE.1) GO TO 1400
      DO 1350 I=1,NM
      LSNC=LOSNC+(J-1)*K2*NCS+(I-1)*NCS-1
      LSNM=LOSNM+(J-1)*K2*NCS+(I-1)*NCS-1
      LBSL=LOBSL+(J-1)*K2*NCS+(I-1)*NCS-1
      LBSR=LOBSR+(J-1)*K2*NCS+(I-1)*NCS-1
      WRITE(IPR,5355)(PO(LSNM+K),K=1,NCS)
      WRITE(IPR,5360)(PO(LSNC+K),K=1,NCS)
 1350 CONTINUE

 1400 CONTINUE
cc      LFKEC=LOFKEC+(J-1)*K2-1
cc      WRITE(IPR,5365) J,J
cc      WRITE(IPR,4100) (PO(LFKEC+I),I=1,NM)

      IF(KFLP.GE.2) GO TO 1500
      WRITE(IPR,5370) J,J
      NRCM=IPO(LONCM1+J-1)
      LNCM=LONCM+(J-1)*K7
      WRITE(IPR,4105) (IPO(LNCM+K-1),K=1,NRCM)
C    KFLP>0, USE CONVEYANCE; IMPLY NQCM=0
C    NQCM=>0 MANNING N IS FCN OF H; <0 MANNING N IS FCN OF Q
      NQCMJ=IPO(LONQCM+J-1)
      NCML=IABS(NQCMJ)
      IF(NCML.EQ.0) NCML=NCS
      NRCM=IPO(LONCM1+J-1)
      DO 1480 II=1,NRCM
      II2=IPO(LONCM+(J-1)*K7+II-1)
      LCM=LOCM+(J-1)*K7*K8+(II2-1)*K8-1
      WRITE(IPR,5375) II,J,(PO(LCM+K),K=1,NCML)
      IF(NQCMJ.NE.0) GO TO 1460
C    NQCM<>0, USE MANNING EQUATION, IMPLY KFLP=0
      IF(KFLP.EQ.0) GO TO 1480
      LCML=LOCML+(J-1)*K7*K8+(II2-1)*K8-1
      LCMR=LOCMR+(J-1)*K7*K8+(II2-1)*K8-1
      WRITE(IPR,5380) II,J,(PO(LCML+K),K=1,NCML)
      WRITE(IPR,5385) II,J,(PO(LCMR+K),K=1,NCML)
      GO TO 1480
 1460 LYQCM=LOYQCM+(J-1)*K7*K8+(II2-1)*K8-1
      IF(NQCMJ.LT.0) WRITE(IPR,5390) II,J,(PO(LYQCM+K),K=1,NCML)
      IF(NQCMJ.GE.0) WRITE(IPR,5395) II,J,(PO(LYQCM+K),K=1,NCML)
 1480 CONTINUE
 1500 CONTINUE

C.......................................................................
C    STATISTICS FOR ADJUSTING T.S. AT GAGES
C.......................................................................
      IF(IOBS.LE.1) GO TO 1490

      MXSLC=PO(100)
      LONSLC=PO(324)
      LONQSL=PO(325)
      LOSLCE=PO(326)
      LOFERO=PO(327)
      LOFBIO=PO(328)
      LORERO=PO(329)
      LORBIO=PO(330)

      IF(IOBS.EQ.3) THEN
        WRITE(IPR,5410) (IPO(LONSLC+J-1),J=1,JN)
        WRITE(IPR,5420) (IPO(LONQSL+J-1),J=1,JN)
      ENDIF

      LSLCE=LOSLCE-1
      LFERO=LOFERO-1
      LFBIO=LOFBIO-1
      LRERO=LORERO-1
      LRBIO=LORBIO-1

      LNGS=LONGS-1
      DO 2016 J=1,JN
        NGAG=IPO(LONGAG+J-1)
        NGS=IPO(LNGS+NGAG)
        IF(NGS.EQ.IPO(LONBT+J-1)) NGAG=NGAG-1
        DO 2014 I=1,NGAG
          NSLICE=IPO(LONSLC+J-1)
          WRITE(IPR,2001) J,I
 2001     FORMAT(/10X,'RIVER NO.',I5,5X,'GAGE NO.',I5)
          WRITE(IPR,2002) (PO(LSLCE+K),K=1,NSLICE)
 2002     FORMAT(12X,'SLICE:  ',10F12.1)
          WRITE(IPR,2004) (PO(LFERO+K),K=1,NSLICE)
 2004     FORMAT(12X,'FRMSO:  ',10F12.3)
          WRITE(IPR,2006) (PO(LFBIO+K),K=1,NSLICE)
 2006     FORMAT(12X,'FBIASO: ',10F12.3)
          WRITE(IPR,2008) (PO(LRERO+K),K=1,NSLICE)
 2008     FORMAT(12X,'RRMSO:  ',10F12.3)
          WRITE(IPR,2012) (PO(LRBIO+K),K=1,NSLICE)
 2012     FORMAT(12X,'RBIASO: ',10F12.3)
 2014   CONTINUE
        LSLCE=LSLCE+MXSLC
        LFERO=LFERO+MXSLC
        LFBIO=LFBIO+MXSLC
        LRERO=LRERO+MXSLC
        LRBIO=LRBIO+MXSLC
 2016 CONTINUE

C.......................................................................
C    RIVER INFO FOR FLDGRF
C.......................................................................

 1490 IF(NFGRF.NE.1) THEN
        LORIVR=PO(365)-1
        WRITE(IPR,'(20A4)') (PO(LORIVR+I),I=1,20)
        DO 1510 J=1,JN
          LORIVR=LORIVR+20
          WRITE(IPR,'(20A4)') (PO(LORIVR+I),I=1,20)
 1510   CONTINUE
      ENDIF

C.......................................................................
C    END MESAGE
C.......................................................................
      WRITE(IPR,5500)

      LCKRTP=PO(270)
      LCKRT1=PO(271)
      LCKRTN=PO(272)
      IF(NP.GE.0) WRITE(IPR,1550)
 1550 FORMAT(//1X,'METHOD OF ROUTING FOR THIS RIVER SYSTEM:')
      DO 1620 J=1,JN
      WRITE(IPR,1560) J
 1560 FORMAT(/5X,'RIVER NO.',I3)
      DO 1600 L=1,IPO(LOLROT+J-1)
      KK=IPO(LCKRTP+K5*(J-1)+L-1)
      KRT1=IPO(LCKRT1+K5*(J-1)+L-1)
      KRTN=IPO(LCKRTN+K5*(J-1)+L-1)
      IF (KK.EQ.0) WRITE(IPR,1622) L,KK,KRT1,KRTN
      IF (KK.EQ.1) WRITE(IPR,1624) L,KK,KRT1,KRTN
      IF (KK.EQ.2) WRITE(IPR,1626) L,KK,KRT1,KRTN
      IF (KK.EQ.3) WRITE(IPR,1628) L,KK,KRT1,KRTN
      IF (KK.EQ.4) WRITE(IPR,1630) L,KK,KRT1,KRTN
      IF (KK.EQ.5) WRITE(IPR,1632) L,KK,KRT1,KRTN
 1600 CONTINUE
 1620 CONTINUE

 1622 FORMAT(4X,2HL=,I2,5X,'KRTYP=',I2,5X,'KRT1= ',I3,3X,'KRTN= ',I3,3X,
     .24HIMPLICIT DYNAMIC ROUTING)
 1624 FORMAT(4X,2HL=,I2,5X,'KRTYP=',I2,5X,'KRT1= ',I3,3X,'KRTN= ',I3,3X,
     .26HIMPLICIT DIFFUSION ROUTING)
 1626 FORMAT(4X,2HL=,I2,5X,'KRTYP=',I2,5X,'KRT1= ',I3,3X,'KRTN= ',I3,3X,
     .30HMUSKINGUM-CUNGE ROUTING LINEAR)
 1628 FORMAT(4X,2HL=,I2,5X,'KRTYP=',I2,5X,'KRT1= ',I3,3X,'KRTN= ',I3,3X,
     .33HMUSKINGUM-CUNGE ROUTING NONLINEAR)
 1630 FORMAT(4X,2HL=,I2,5X,'KRTYP=',I2,5X,'KRT1= ',I3,3X,'KRTN= ',I3,3X,
     .18HLEVEL POOL ROUTING)
 1632 FORMAT(4X,2HL=,I2,5X,'KRTYP=',I2,5X,'KRT1= ',I3,3X,'KRTN= ',I3,3X,
     .24HEXPLICIT DYNAMIC ROUTING)

      WRITE(IPR,1650) K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K13,K15,K16,K18
      WRITE(IPR,1651) K19,K20,K21,K22,K23,K24,K25
 1650 FORMAT(///30X,'SUMMARY OF ARRAY SIZES'//10X,
     . 'NO. OF RIVERS IN THE SYSTEM ...........................',I5/10X,
     . 'MAXIMUM NO. OF CROSS SECTIONS ON ANY RIVER ............',I5/10X,
     . 'NO. OF COMPUTATIONAL TIME STEPS .......................',I5/10X,
     . 'MAXIMUM NO. OF GAGING STATIONS ON ANY RIVER ...........',I5/10X,
     . 'MAXIMUM NO. OF ROUTING TECHNIQUES IN THE SYSTEM .......',I5/10X,
     . 'NO. OF SETS OF POINTS IN THE D/S RATING CURVE TABLE ...',I5/10X,
     . 'MAXIMUM NO. OF MANNING N REACHES ON ANY RIVER .........',I5/10X,
     . 'NO. OF SETS OF POINTS IN THE MANNING N TABLE ..........',I5/10X,
     . 'NO. OF SETS OF POINTS IN THE BS VS HSS TABLE ..........',I5/10X,
     . 'MAXIMUM NO. OF LATERAL FLOW HYDROGRAPHS ON ANY RIVER ..',I5/10X,
     . 'MAXIMUM NO. OF REACHES ON ANY RIVER ...................',I5/10X,
     . 'MAXIMUM NO. OF EQUATIONS TO BE SOLVED (K2*2) ..........',I5/10X,
     . 'MAXIMUM NO. OF INTERNAL BOUNDARIES ON ANY RIVER .......',I5/10X,
     . 'TOTAL NO. OF LEVEE REACHES IN THE SYSTEM ..............',I5)
 1651 FORMAT(/10X,
     . 'MAXIMUM NO. OF MULTIPLE GATES ON ANY RIVER ............',I5/10X,
     . 'NO. OF DAMS WHICH HAVE MULTIPLE GATES .................',I5/10X,
     . 'NO. OF POINTS IN THE MOVABLE GATE TIME SERIES .........',I5/10X,
     . 'NO. OF INTERPOLATED LEVEE REACHES IN THE SYSTEM .......',I5/10X,
     . 'MAXIMUM NO. OF ACTUAL CROSS SECTIONS ON ANY RIVER .....',I5/10X,
     . 'TOTAL NO. OF HYDROGRAPH POINTS USED IN FLDGRF PROGRAM .',I5/10X,
     . 'TOTAL NO. OF GAGING STATIONS IN THE SYSTEM ............',I5)

      WRITE(IPR,1660)
 1660 FORMAT(//////30X,32(1H*)/30X,32(1H*)/30X,3(1H*),26X,3(1H*)/30X,
     1   32H***  SUMMARY OF OUTPUT DATA  ***/30X,3(1H*),26X,3(1H*)/
     2   30X,32(1H*)/30X,32(1H*)///)

C---------------------  DATA PROCESSING PART  --------------------------
      LCLQN  =PO(282)
      LCPR   =PO(285)
      LCIRGM =PO(286)
      LCIFLV =PO(287)
      LCHLV  =PO(289)
      LCBLV  =PO(290)
      LCBEV  =PO(291)
      LCNKC  =PO(292)
      LCDDX  =PO(295)
      LCWF   =PO(296)
      LCIFCV =PO(297)
      LCHCAV =PO(298) 
      LCDXR  =PO(299)
 
C    INTERPLATED MANNING'S N (JNK>=9)
cc      IF (KFLP.GT.1) GOTO 1800
cc      IF (JNK.LT.9) GOTO 1800
cc      DO 1790 J=1,JN
cc      N=IPO(LONBT+J-1)
cc      NQCMJ=IPO(LONQCM+J-1)
cc      NCML=IABS(NQCMJ)
cc      IF(NCML.EQ.0) NCML=NCS
cc      WRITE(IPR,1700) J
cc 1700 FORMAT(/2X,'MANNING TABLE AT EACH SECTION REACH ON RIVER NO.',I3)
cc      DO 1760 I=1,N
cc      LCM =LOCM +(J-1)*K7*K8+(I-1)*K8-1
cc      LCML=LOCML+(J-1)*K7*K8+(I-1)*K8-1
cc      LCMR=LOCMR+(J-1)*K7*K8+(I-1)*K8-1
cc      LYQCM=LOYQCM+(J-1)*K7*K8+(I-1)*K8-1
cc      WRITE(IPR,1762) I,J,(PO(LCM+K),K=1,NCML)
cc      IF(KFLP.NE.1) GO TO 1761
cc      WRITE(IPR,1764) I,J,(PO(LCML+K),K=1,NCML)
cc     WRITE(IPR,1766) I,J,(PO(LCMR+K),K=1,NCML)
cc 1761 IF(NQCMJ.LT.0) WRITE(IPR,1768) I,J,(PO(LYQCM+K),K=1,NCML)
cc      IF(NQCMJ.GE.0) WRITE(IPR,1770) I,J,(PO(LYQCM+K),K=1,NCML)
cc 1760 CONTINUE
 1762 FORMAT(5X,'  CM(K,',I2,1H,,I2,')= ',10F10.4,(/19X,10F10.4))
 1764 FORMAT(5X,' CML(K,',I2,1H,,I2,')= ',10F10.4,(/19X,10F10.4))
 1766 FORMAT(5X,' CMR(K,',I2,1H,,I2,')= ',10F10.4,(/19X,10F10.4))
 1768 FORMAT(5X,'YQCM(K,',I2,1H,,I2,')= ',10F10.0,(/19X,10F10.0))
 1770 FORMAT(5X,'YQCM(K,',I2,1H,,I2,')= ',10F10.2,(/19X,10F10.2))
 1790 CONTINUE

C    PRINTING SLOPE AND ROUTING TYPE INFORMATION
 1800 IF(JNK.LT.4) GOTO 2000
      DSTNCE=PO(300)
      XLNGTH=PO(301)
      BSLOPE=PO(306)
      WRITE(IPR,1810) DSTNCE,XLNGTH,DSTNCE,BSLOPE
      WRITE(IPR,1815)
 1810 FORMAT(//1X,5HRIVER,2X,4HSECT,4X,1HX,5X,9HBED ELEV.,3X,5HREACH,
     *4X,6HLENGTH,4X,5HSLOPE,3X,7HROUTING,2X,7HSTRUCT.,/3X,2HNO,4X,2HNO,
     *4X,A4,5X,A4,7X,2HNO,7X,A4,6X,A4)
 1815 FORMAT(1X,'-------------------------------------------------------
     *---------------------')
      RTYP='        '
      DO 1980 J=1,JN
      MIX=IPO(LOMIXF+J-1)
      NT=IPO(LONBT+J-1)
      LXT=LOXT+(J-1)*K23-1
      LHS=LOHS+(J-1)*K9*K2    
      LNN=LCNN+(J-1)*K23-1  
      DO 1960 I=1,NT
      II=IPO(LNN+I)
      XTI=PO(LXT+I) 
      XT2=PO(LXT+I+1)
      DXCH=ABS(XTI-XT2)
      HS1I=PO(LHS+(II-1)*K9)
      IF (I.EQ.NT) GOTO 1953
      II2=IPO(LNN+I+1) 
      DH=HS1I-PO(LHS+(II2-1)*K9)
      SL=DH/DXCH
           IF(METRIC.EQ.1) THEN
           XTI=XTI*0.6214
           HS1I=HS1I*3.281
           DXCH=DXCH*0.6214
           SL=SL/0.01894
           ENDIF
      STRUC='      '
      KRA=IABS(IPO(LORCHT+(J-1)*K23+I-1))
      KRB=IABS(IPO(LORCHT+(J-1)*K23+I))
      IF(KRA.GE.10.AND.KRA.LE.35) SL=0.0
      IF(KRA.GE.10.AND.KRA.LE.34) STRUC='DAM   '
      IF(KRA.EQ.35) STRUC='BRIDGE'
      IF(KRA.GE.10) GOTO 1950
      IF(MIX.EQ.0) RTYP='IMP(SUB)'
      IF(MIX.EQ.1) RTYP='IMP(SUP)'
      IF(MIX.EQ.5) RTYP='IMP(LPI)'
      IF(MIX.EQ.2) RTYP='IMP(MIX)'
      IF(MIX.EQ.3) RTYP='IMP(MIX)'
      IF(MIX.EQ.4) RTYP='IMP(MIX)'
      IF(KRA.EQ.1) RTYP='IMP(DIF)'
      IF(KRA.EQ.4) RTYP='POOL    '
      IF(KRA.EQ.5) RTYP='EXP     '
 1950   IF(I.EQ.1.AND.KRA.GE.10) THEN
        RTYP='POOL    '
        IF(MIX.EQ.0.AND.KRB.EQ.0) RTYP='IMP(SUB)'
        IF(MIX.GE.2.AND.KRB.EQ.0) RTYP='IMP(MIX)'
        IF(MIX.EQ.5.AND.KRB.EQ.0) RTYP='IMP(LPI)'
        IF(MIX.EQ.5.AND.KRB.EQ.6) RTYP='IMP(LPI)'
        IF(MIX.EQ.0.AND.KRB.EQ.1) RTYP='IMP(DIF)'
        ENDIF
 1953 IF(I.LT.NT) WRITE(IPR,1955) J,I,XTI,HS1I,I,DXCH,SL,RTYP,STRUC
      IF(I.EQ.NT) WRITE(IPR,1955) J,I,XTI,HS1I
 1955 FORMAT(2I5,2F10.2,3X,I5,2F10.2,3X,A8,2X,A6)
 1960 CONTINUE
      IF (J.LT.JN) WRITE(IPR,*)
 1980 CONTINUE
      WRITE(IPR,1815)

C    WARNINGS FOR POSSIBLE SUB OR SUP CONDITIONS 
 2000 IF (NP.LT.0) GOTO 2700
      IF (KFLP.GE.1 .OR. KPRES.EQ.1) GOTO 2500

      DO 2020 J=1,JN
      KKSUP=0
      N=IPO(LONBT+J-1)
      IGM=IPO(LCIRGM+(J-1)*K13+N-1)
      IF (IGM.GE.1) KKSUP=IGM
      IF (KKSUP.EQ.0) GOTO 2020
      ICKVAL=999999
      IF (KKSUP.GT.ICKVAL) THEN
         WRITE (IPR,997) 'KKSUP',KKSUP,ICKVAL
         CALL WARN
         GO TO 2020
      ENDIF
      WRITE(IPR,2010) J
      WRITE(IPR,2015) (IPO(LCIRGM+(J-1)*K13+KK-1),KK=1,KKSUP)
 2010 FORMAT(/1X,74HWARNING: SUPERCRITICAL FLOW MAY OCCUR AT FOLLOWING C 
     .ROSS SECTIONS IN RIVER,I3/10X,33HYOU MAY NEED TO RESET MIXF(J) > 0
     .)
 2015 FORMAT(6X,14I5)
 2020 CONTINUE
      DO 2050 J=1,JN
      KKSUB=0
      N=IPO(LONBT+J-1)
      IGM=IPO(LCIRGM+(J-1)*K13+N-1)
      IF (IGM.LE.-1) KKSUB=IABS(IGM)
      IF (KKSUB.EQ.0) GOTO 2050
      WRITE(IPR,2040) J
      WRITE(IPR,2015) (-1*IPO(LCIRGM+(J-1)*K13+KK-1),KK=1,KKSUB)
 2040 FORMAT(/10X,'**WARNING: SUBCRITICAL FLOW MAY OCCUR AT FOLLOWING CR
     1OSS SECTIONS IN RIVER',I3/10X,'YOU MAY NEED TO RESET MIXF(J) > OR
     2< 1')
 2050 CONTINUE                              

C    WARNINGS FOR EXPANSION/CONTRACTION DX REQUIREMENT
 2500 KKDXM=PO(309)
      IF (KKDXM.EQ.0) GOTO 2700
      WRITE(IPR,2510)
 2510 FORMAT(//2X,45HWARNING: THE FOLLOWING DXMs SHOULD BE CHANGED,//6X,
     .1HJ,4X,1HI,6X,8HDXM(I,J),3X,11HRECOMMENDED,8X,6HREASON)
 2515 FORMAT(2X,2I5,2F12.3,8X,A20)    
      RSON(1)='EXP/CON  CRITERIA   '
      RSON(2)='COURANT CONDITION   '          
      RSON(3)='ABRUPT SLOPE CHANGE ' 
      DO 2600 J=1,JN
      N=IPO(LONBT+J-1)
      LS1=LCDXR+(J-1)*K13-1
      LS2=LODXM+(J-1)*K13-1
      DO 2590 I=1,N-1   
      DXREC=PO(LS1+I)
      DXMI=PO(LS2+I)
      IF (DXREC.LE.0.0001) GOTO 2590
           IF (DXREC.GT.30000.0) THEN
           KR=3
           DXREC=DXREC-30000.0
           ELSE IF (DXREC.GT.20000.0) THEN
           KR=2
           DXREC=DXREC-20000.0
           ELSE               
           KR=1
           DXREC=DXREC-10000.0
           ENDIF
      WRITE(IPR,2515) J,I,DXMI,DXREC,RSON(KR)
 2590 CONTINUE
 2600 CONTINUE

C    INTERPLATED CROSS-SECTIONS INFORMATION  
cc 2700 IF (JNK.LT.4) GOTO 2800    
cc      WRITE(IPR,2710)
cc 2710 FORMAT(////10X,'NEW INPUT CROSS SECTION NO. AFTER INTERPOLATION')
cc 2715 FORMAT(/2X,'RIVER NO.',I5/5X,'NN=   ',20I4,100(/11X,20I4))
cc      DO 2720 J=1,JN
cc      NT=IPO(LONBT+J-1)
cc      LNN=LCNN+(J-1)*K23-1
cc      WRITE(IPR,2715) J,(IPO(LNN+I),I=1,NT)
cc 2720 CONTINUE

cc      DO 2750 J=1,JN
cc      WRITE(IPR,2751) J
cc      LL=IPO(LONLAD+J-1)
cc      LLAD=LOLAD+(J-1)*K16-1
cc      IF(LL.GT.0) WRITE(IPR,2752) (IPO(LLAD+L),L=1,LL)
cc      IF(J.GT.1) WRITE(IPR,2753) IPO(LONJUN+J-1)
cc      NGAG=IPO(LONGAG+J-1)
cc      LSGAG=LONGS+(J-1)*K4-1
cc      IF(NGAG.GT.0) WRITE(IPR,2754) (IPO(LSGAG+L),L=1,NGAG)
cc      NQLJ=IPO(LONQL+J-1)
cc      LQL1=LOLQ1+(J-1)*K10-1
cc      LQLN=LCLQN+(J-1)*K10-1
cc      IF(NQLJ.GT.0) THEN
cc        WRITE(IPR,2755) (IPO(LQL1+L),L=1,NQLJ)
cc        WRITE(IPR,2756) (IPO(LQLN+L),L=1,NQLJ)
cc      END IF
cc      LR=IPO(LOLROT+J-1)
cc      DO 2730 L=1,LR
cc      L1=LCKRTP+(J-1)*K5-1
cc      L2=LCKRT1+(J-1)*K5-1
cc      L3=LCKRTN+(J-1)*K5-1
cc 2730 WRITE(IPR,2735) L,IPO(L1+L),IPO(L2+L),IPO(L3+L)
cc 2735 FORMAT(/5X,2HL=,I5,5X,'KRTYP=',I2,5X,'KRT1= ',I3,5X,'KRTN= ',I3)
cc 2750 CONTINUE
 2751 FORMAT(/2X,'RIVER NO.',I3)
 2752 FORMAT(5X,'LAD=  ',20I4,100(/10X,20I4))
 2753 FORMAT(5X,'NJUN= ',20I4,100(/10X,20I4))
 2754 FORMAT(5X,'NGS=  ',20I4,100(/10X,20I4))
 2755 FORMAT(5X,'LQ1=  ',20I4,100(/10X,20I4))
 2756 FORMAT(5X,'LQN=  ',20I4,100(/10X,20I4))

C    CHANNEL BED SLOPE INFORMATION
 2700 LOSLFI=PO(229)
 2800 DO 2820 J=1,JN
      N=IPO(LONB+J-1)
      LS=LOSLFI+(J-1)*K2-1
      WRITE(IPR,2810) J
      WRITE(IPR,2815) (PO(LS+I),I=1,N)
 2810 FORMAT(/10X,'(SLOP(I,J),I=1,N) FOR RIVER NO. ',I3)
 2815 FORMAT(8F10.6)
 2820 CONTINUE

C    INTERPOLATED LEVEE INFORMATION (NNLEV IS NUM. OF LEVEE REACHES)
cc      IF(NNLEV.LE.0) GOTO 2900
cc      NNLEV=PO(49)
cc      WRITE(IPR,2850)
cc      DO 2830 L=1,NNLEV
cc      JFM=IPO(LONJFM+L-1)
cc      IFM=IPO(LONIFM+L-1)
cc      LX=LOX+(JFM-1)*K2-1+IFM
cc      XLEV=PO(LX)
cc      WRITE(IPR,2840) L,IPO(LONJFM+L-1),IPO(LONIFM+L-1),IPO(LONJTO+L-1),
cc     # IPO(LONITO+L-1),XLEV,PO(LOHWLV+L-1),PO(LOWCLV+L-1),
cc     # PO(LOTFLV+L-1),PO(LOBLMX+L-1),PO(LOHFLV+L-1),PO(LOHLMN+L-1),
cc     # PO(LOSLV+L-1),PO(LOHPLV+L-1),PO(LODPLV+L-1)
cc 2830 CONTINUE
cc 2840 FORMAT(5I5,7F10.2,F10.5,2F10.2)
cc 2850 FORMAT(/'    L NJFM NIFM NJTO NITO         X      HWLV      WCLV
cc     .    TFLV     BLVMX      HFLV     HLVMN       SLV      HPLV      DP
cc     .LV')

C     CONVEYANCE CURVE FOR KFLP>=1
 2900 IF (KFLP.NE.1) GOTO 3000
      IF (JNK.LT.4) GOTO 3000
      WRITE(IPR,2910)
 2910 FORMAT(/2X,'GENERATING CONVEYANCE CURVE')
      ERQK=PO(257)
      DO 2950 J=1,JN
      N=IPO(LONB+J-1)
      L=IPO(LCNKC+(J-1)*K2)
      WRITE(IPR,2914) J,1,L,ERQK,L
      WRITE(IPR,*)
      WRITE(IPR,2916) (PO(LOHKC+KK-1),KK=1,L)
      WRITE(IPR,*)
      WRITE(IPR,2918) (PO(LOQKC+KK-1),KK=1,L)
      WRITE(IPR,*)
      WRITE(IPR,2920) (PO(LCBEV+KK-1),KK=1,L)
 2914 FORMAT(/5X,2HJ=,I3,5X,2HI=,I5,5X,2HL=,I5,5X,5HERQK=,F6.2,5X,
     . 'NKC(I,J)=',I3)
 2916 FORMAT(5X,'HKC(L,I,J)=',8F13.2)
 2918 FORMAT(5X,'QKC(L,I,J)=',8F13.0)
 2920 FORMAT(5X,'BEV(L,I,J)=',8F13.3)
      WRITE(IPR,*)
      DO 2930 I=1,N
      LS=LOSNM+(J-1)*K2+(I-1)*K9-1
 2930 WRITE(IPR,2935) I,J,(PO(LOSNM+K),K=1,NCS)
 2935 FORMAT(5X,'SNM(K,',I3,1H,,I2,')=',8F10.2)
C      WRITE(IPR,*)
C      WRITE(IPR,2945) (PO(LCERQ+K-1),K=1,N)
 2945 FORMAT(5X,'ERQMX= ',10F5.2,'*',10F5.2)
 2950 CONTINUE
      WRITE(IPR,*)
      DO 2980 J=1,JN
      N=IPO(LONB+J-1)
      DO 2970 I=1,N
      LS=LOSNC+(J-1)*K2+(I-1)*K9-1
      WRITE(IPR,2965) I,J,(PO(LS+K),K=1,NCS)
 2965 FORMAT(5X,'SNC(K,',I3,1H,,I2,')=',8F10.2)
 2970 CONTINUE
 2980 CONTINUE

 3000 CONTINUE
 4000 FORMAT(6F10.3,4I10)
 4005 FORMAT(8I10,3X,2I2)
 4010 FORMAT(8I10)
 4015 FORMAT(7X,I5,3X,4I10)
 4016 FORMAT(7X,I5,3X,3I10,3X,F10.0,2(2X,6A4))
 4017 FORMAT(7X,I5,3X,3I10,13X,2(2X,6A4))
 4020 FORMAT(I5,4X,3I5,4F10.2)
 4025 FORMAT(I5,4X,3I5,30X,4F10.2)
 4030 FORMAT(I5,4X,7I5,5F10.2)
 4035 FORMAT(I5,5X,7I6,3X,3I2)
 4040 FORMAT(I5,5X,4I6,3X,6I2)
 4045 FORMAT(5X,10I5)
 4050 FORMAT(1X,I6,4F10.2,I6)
 4055 FORMAT(1X,I6,4F12.2)
C jgg  4060 FORMAT(8F10.3)  changed to fix a beta bug  3/9/04
 4060 FORMAT(8F10.4)  
 4065 FORMAT(5X,8I5)
 4070 FORMAT(5I5,7F10.2,F10.5,2F10.2)
 4075 FORMAT(3X,I4,9X,I4,7X,2A4,4X,A4)
 4080 FORMAT(3X,I4,8X,I5,6X,F8.2,7X,2A4,2X,A4)
 4082 FORMAT(3X,I4,8X,I5,21X,2A4,2X,A4)
 4085 FORMAT(3X,I4,8X,I5,7X,2A4,4X,A4,6X,F8.2)
 4090 FORMAT(7X,I3,8F10.2)
 4095 FORMAT(10X,I5,5X,F10.2,14X,2A4,2X,A4)
 4097 FORMAT(10X,I5,5X,F12.2,2X,F10.2,6X,2A4,2X,A4)
 4100 FORMAT(5X,8F10.2)
 4105 FORMAT(5X,15I5)
 5005 FORMAT (//,'      EPSY     THETA        F1     XFACT     DTHYD      
     . DTOUT    METRIC')
 5010 FORMAT(/,
     . 5X,'CONVERGENCE CRITERIA FOR STAGE                     ',
     .    'EPSY  ',F12.3,/,
     . 5X,'ACCELERATION FACTOR IN SOLVING TRIB. JUNCTION PROB ',
     .    'THETA ',F12.3,/,
     . 5X,'WEIGHTING FACTOR USED IN FINITE DIFFERENCE SCHEME  ',
     .    'F1    ',F12.3,/,
     . 5X,'FACTOR TO CONVERT LOCATIONS TO FEET                ',
     .    'XFACT ',F12.3,/,
     . 5X,'TIME INTERVAL (HRS) OF INPUT HYDROGRAPHS           ',
     .    'DTHYD ',F12.3,/,
     . 5X,'TIME INTERVAL (HRS) OF OUTPUT HYDROGRAPHS          ',
     .    'DTOUT ',F12.3,/,
     . 5X,'METRIC OPTION SWITCH                               ',
     .    'METRIC',I12)
 5015 FORMAT (/ '        JN        NU     ITMAX     KWARM      KFLP        
     .  NET     ICOND      NMAP   FUTURE DATA')
 5020 FORMAT( /
     .5X,'NUMBER OF RIVERS IN THE SYSTEM',21X,
     .'JN',6X,I10/5X,'NO. OF POINTS IN INPUT HYDROGRAPHS',17X,
     .'NU',6X,I10/5X,'MAX NO. OF ITERATIONS IN NEWTON-RAPHSON TECHNIQUE'
     .,2X,'ITMAX',3X,I10/5X,'NO. OF TIMES TO WARM UP BEFORE ROUTING FLOW
     .',8X,'KWARM',3X,I10/5X,'FLOOD PLAIN USAGE OPTION SWITCH',20X,
     .'KFLD',4X,I10/5X,'NETWORK OPTION SWITCH',30X,'NET',5X,I10/5X,
     .'INITIAL CONDITIONS SWITCH',27X,'ICOND',3X,I10/5X,
     .'NO. OF MAP SCENARIOS',31X,'NMAP',4X,I10/5X,
     .'NUMBER OF FUTURE DATA POINTS',31X,I10)
 5025 FORMAT (/ '      NYQD       KCG       NCG     KPRES')
 5030 FORMAT(/
     .5X,'NO.OF POINTS IN RATING CURVE AT D/S BOUNDARY',7X,
     .'NYQD',4X,I10/5X,'NO. OF POINTS IN SPILLWAY GATE CONTROL TIME ARRA
     .Y',2X,'KCG',5X,I10/5X,'MAX NO. OF GATES IN THE STRUCTURE',18X,
     .'NCG',
     .5X,I10/5X,'COMPUTION OF HYDRAULIC RADIUS SWITCH',15X,'KPRES',
     .3X,I10)
 5035 FORMAT(/ '       NCS       KPL       JNK    KREVRS     NFGRF')
 5040 FORMAT(/
     .5X,'NO. OF VALUES IN TABLE (BS) VS. (HS)               NCS',5X,I10
     ./5X,'PARAMETER INDICATING INFO PLOTTED                  KPL',5X,
     .I10/5X,'HYDRAULIC PRINTING SWITCH                          JNK',5X
     .,I10/5X,
     .'LOW FLOW FILTER SWITCH                             KREVRS',
     .2X,I10/5X,'FLDGRF DATA GENERATION SWITCH',22X,
     .'NFGRF',3X,I10)
 5045 FORMAT(/6X,4HIOBS,5X,5HKTERM,8X,2HNP,6X,4HNPST,5X,5HNPEND)
 5050 FORMAT( /
     .5X,'OBSERVED DATA AT GAGING STATION SWITCH             IOBS',4X,
     .I10/5X,'EQUATION OF MOTION COMPUTED & PRINTED              KTERM',
     .3X,I10/5X,'IF AUTOMATIC CALIBRATION OPTION IS USED            NP',
     .6X,I10/5X,'1ST VALUE IN HYDROGRAPH USED IN STATS              NPST
     .',4X,I10/5X,'LAST VALUE IN HYDROGRAPH USED IN STATS             NP
     .END',3X,I10/)
 5055 FORMAT(/5X,'DETAIL DEBUG OUTPUT BETWEEN (TDBG1,TDBG2)= ',2F15.5,
     & /10X,'JNKDG=',I3,5X,'BETW RIVER (JDBG1,JDBG2)= ',2I5)
 5060 FORMAT(10X,'BETW MANNING N REACHES (LDBG1,LDBG2)= ',2I5,
     & 5X,'MCM=>',I3)
 5065 FORMAT(/'       TEH     DTHII    DTHPLT     FRDFR     DTEXP           
     .MDT  '/ F10.3,2F10.5,F10.2,F10.5,2I10)
 5070 FORMAT(
     .5X,'TIME AT THE END OF THE RUN                         TEH',5X,
     .F10.3/5X,'DT (hour) FOR IMPLICIT ROUTING                     DTHII
     .',3X,F10.5/5X,'DT FOR PLOT OF HYDROGRAPH',26X,
     .'DTHPLT',2X,F10.5/5X,'FROUDE WINDOW',38X,
     .'FRDFR',3X,F10.2/5X,'DT FOR EXPLICIT ROUTING',28X,
     .'DTEXP',3X,F10.5/5X,'Tr/MDT FACTOR',38X,
     .'MDT',8X,I7/)
 5075 FORMAT(/5X,'DTIN=',10F10.5/(10X,10F10.5))
 5080 FORMAT(4X,'TDTIN=',10F10.3/(10X,10F10.3))
 5085 FORMAT(/'      NLEV      DHLV     DTHLV'/I10,2F10.5)
 5090 FORMAT(
     .5X,'TOTAL NO. OF CROSS-SECTION WITH LEVEES             NLEV',4X,
     .I10/5X,'DIFF IN MAX & MIN CREST ELEVATIONS                 DHLV',4
     .X,F10.5/5X,'STEP USED DURING LEVEE FAILURE                     DTH
     .LV',3X,F10.5)
 5095 FORMAT (//
     .5X,'L      = LEVEE NUMBER'/
     .5X,'NJFM   = RIVER NO. FROM WHICH LEVEE FLOW IS PASSED'/
     .5X,'NIFM   = REACH NO. ON RIVER NJFM PASSING FLOW TO NITO'/
     .5X,'NJTO   = RIVER NO. TO WHICH LEVEE FLOW IS PASSED'/
     .5X,'NJTO   = REACH NO. ON RIVER NJFM RECEIVING FLOW FROM NIFM'/)
 5097   FORMAT (//
     .  5X,'L      = SCENARIO NUMBER'/
     .  5X,'MPRV   = RIVER NO. OF MAPPING REACH'/
     .  5X,'MPLOC1 = SECTION NO. OF UPSTREAM END OF MAPPING REACH'/
     . 5X,'MPLOC2 = SECTION NO. OF DOWNSTREAM END OF MAPPING REACH'/
     . 5X,'DTMAP  = TIME STEP FOR ANIMATION'/
     . 5X,'SYSPTH = RIVER SYSTEM NAME USED IN FLDVIEW PATH'/
     . 5X,'TWNPTH = TOWN NAME USED IN FLDVIEW PATH (TOWN TO BE MAPPED'/)
 5100 FORMAT(/5X,'  LEVEE NO   NJFM(L)   NIFM(L)   NJTO(L)   NITO(L)')
 5102 FORMAT(/5X,'SCENARO NO   MPRV(L)  MPLOC(1,L)  MPLOC(2,L)',5X,
     .           'DTMAP',1X,'SYSPTH(L)',16X,'TWNPTH(L)')
 5105 FORMAT(//
     .15X,'NBT   = NO. OF CROSS SECTIONS'/
     .15X,'NPT1  = BEGINNING SECTION ID NO. ON RIVER J'/
     .15X,'NPT2  = FINAL SECTION ID NO. ON RIVER J'/
     .15X,'EPQJ  = DISCHARGE TOLERANCE FOR EACH RIVER'/
     .15X,'COFW  = WIND COEFFICIENT FOR J-TH RIVER'/
     .15X,'VWIND = WIND VELOCITY'/
     .15X,'ATF   = AZIMUTH ANGLE OF DYNAMIC TRIBUTARY'/
     .15X,'NJUN  = REACH NO. WHERE DYNAMIC TRIB ENTERS MAIN RIVER'/)
 5110 FORMAT(/' RIVER NO.  NBT NPT1 NPT2      EPQJ      COFW     VWIND           
     .  WINAGL')
 5115 FORMAT(/'RIVER NO.  NBT NPT1 NPT2  MRV NJUN  MRU NJUM       ATF
     .   EPQJ      COFW     VWIND    WINAGL')
 5120 FORMAT(//
     .15X,'KU    = UPSTREAM BOUNDARY SELECTION PARAMETER'/
     .15X,'KD    = DOWNSTREAM BOUNDARY SELECTION PARAMETER'/
     .15X,'NQL   = NO. OF LATERAL INFLOWS (OR OUTFLOWS)'/
     .15X,'NGAGE = NO. OF GAGING OR PLOTING STATIONS'/
     .15X,'NRCM1 = NO. OF MANNING N REACHES'/
     .15X,'NQCM  = >0, DATA POINTS FOR MANNING N VS. WATER ELEVATION'/
     .15X,'NSTR  = NO. OF OUTPUT TIME SERIES')
 5125 FORMAT(/'RIVER NO.     KU    KD   NQL NGAGE NRCM1  NQCM  NSTR  FUT
     .URE DATA')
 5130 FORMAT(//
     .15X,'MIXF  = MIXED FLOW INDICATOR; =0; SUB; =1;'/
     .15X,'MUD1  = MUD FLOW SWITCH'/
     .15X,'KFTR  = KALMAN FILTER SWITCH'/
     .15X,'KLOS  = CHANNEL FLOW LOSS SWITCH'/
     .15X,'KLPI  = BETAR IN LPI')
 5135 FORMAT(/'RIVER NO.   MIXF   MUD  KFTR  KLOS  FUTURE DATA')
 5140 FORMAT(/5X,'LPI COEFFICIENTS WHEN MIXF(J)=5')
 5145 FORMAT(//
     .10X,'UW1   = UNIT WEIGHT OF MUD/DEBRIS FLUID'/
     .10X,'VIS1  = DYNAMIC VISCOKSITY OF MUD/DEBRIS FLUID'/
     .10X,'SHR1  = INITIAL YIELD STRESS OF MUD'/
     .10X,'POWR1 = EXPONENT REPRESENT STRESS-RATE OF STRAIN RELATION'/
     .10X,'IWF1  = PARAMETER DENOTING DRY-BED ROUTING'//)
 5150 FORMAT(//10X,37HUW1   = FRICTION ANGLE OF DEBRIS FLOW)
 5155 FORMAT(/'RIVER NO.      UW       VIS       SHR      POWR   IWF')
 5160 FORMAT(/
     .20X,'XLOS1 = BEGINNING LOCATION FOR FLOW LOSS'/
     .20X,'XLOS2 = ENDING LOCATION FOR FLOW LOSS'/
     .20X,'QLOS  = PERCENTAGE OF FLOW LOSS'/
     .20X,'ALOS  = LOSS DISTRIBUTION COEFFICIENT'/)
 5165 FORMAT(/
     .'RIVER    XLOS(1,J)  XLOS(2,J)   QLOS(J) (%)   ALOS(J)')
 5170 FORMAT(//
     .10X,'XT   = LOCATION OF SECTION WHERE COMPUTATIONS ARE MADE'/
     .10X,'DXM  = MIN COMPUTATIONAL DISTANCE INTERVAL BET SECTIONS'/
     .10X,'KRCHT= PARAMETER FOR ROUTING METHOD OR INTERNAL BOUNDARY'/)
 5175 FORMAT(/' XT(I,',I2,') I=1,NB(',I2,')')
 5180 FORMAT(/' DXM(I,',I2,') I=1,NB(',I2,')')
 5185 FORMAT(/' KRCHT(I,',I2,') I=1,NRCH')
 5190 FORMAT(//
     .10X,'L    = LEVEE NUMBER'/
     .10X,'NJFM = RIVER NO. FROM WHICH LEVEE FLOW IS PASSED'/
     .10X,'NIFM = REACH NO. ON RIVER NJFM PASSING FLOW TO NITO'/
     .10X,'NJTO = RIVER NO. TO WHICH LEVEE FLOW IS PASSED'/
     .10X,'NJTO = REACH NO. ON RIVER NJFM RECEIVING FLOW FROM NIFM'/
     .10X,'HWLV = ELEVATION OF TOP LEVEE, RIDGE LINE, ETC.'/
     .10X,'WCLV = WEIR-FLOW DISCHARGE COEFFICIENT'/
     .10X,'TFLV = TIME OF LEVEE FAILURE (CREVASSE)'/
     .10X,'BLVMX= FINAL WIDTH OF LEVEE CREVASSE'/
     .10X,'HFLV = ELEVATION OF WATER SURFACE WHEN LEVEE STARTS TO FAIL'/
     .10X,'HLVMN= FINAL ELEVATION OF BOTTOM LEVEE CREVASSE'/
     .10X,'SLV  = SLOPE OF LEVEE REACH'/
     .10X,'HPLV = CENTERLINE ELEVATION OF FLOOD DRAINAGE PIPE'/
     .10X,'DPLV = DIAMETER OF FLOOD DRAINAGE PIPE'/)
 5195 FORMAT(/
     .'    L   NJFM NIFM NJTO NITO     X       HWLV       WCLV      TFLV
     .    BLVMX    HFLV      HLVMN      SLV      HPLV         DPLV'/)
 5200 FORMAT(//
     .10X,'HPND = INITIAL WSEL OF STORAGE POND LEVEE'/
     .10X,'SAP  = SURFACE AREA OF STORAGE POND CORRESPONDING TO HSAP'/
     .10X,'HSAP = ELEVATION CORRESPONDING TO SAPOND'//)
 5205 FORMAT(/4X,'POND= ',I2,5X,'HPOND=',F10.2)
 5210 FORMAT(6X,'SAPOND:',8F10.0)
 5215 FORMAT(6X,'HSAP:  ',8F10.2)
 5220 FORMAT(//1X,'RIVER NO.',I3,',  DAM NO.',I3)
 5225 FORMAT(//1X,'RIVER NO.',I3,',  BRIDGE NO.',I3)
 5230 FORMAT(/1X,38HLOCAL (LATERAL) FLOW INFO FOR RIVER J=,I3)
 5235 FORMAT(/1X,11H L-FLOW (I),4X,8HLQ1(I,J),5X,2HID,8X,4HTYPE)
 5240 FORMAT(/31H PLOTTING T.S.INFO FOR RIVER J=,1X,I2)
 5245 FORMAT(/1X,11HSTATION (I),4X,8HNGS(I,J),4X,7HGZ(I,J),6X,2HID,10X,
     .          4HTYPE)
 5250 FORMAT(/30H OUTPUT T.S. INFO FOR RIVER J=,1X,I2)
 5255 FORMAT(/1X,11HSTATION (I),6X,8HNST(I,J),5X,2HID,8X,4HTYPE,
     .          6X,8HGZO(I,J))
 5260 FORMAT (/5X,'J       TPG       RHO      GAMA       YQI')
 5265 FORMAT(/10X,'UPSTREAM BOUNDARY INFORMATION'//10X,
     .   'RIVER NO     MIN Q/H   GAGE ZERO     ID      TYPE')
 5270 FORMAT(/10X,'DOWSTREAM BOUNDARY INFORMATION')
 5275 FORMAT(/10X,'   ID      TYPE    GAGE ZERO'/10X,2A4,3X,A4,F10.2)
 5280 FORMAT(/10X,'   ID      TYPE'/10X,2A4,3X,A4,3X,2A4)
 5285 FORMAT(/10X,'RATING CURVE ID:',2A4 )
 5290 FORMAT(/10X,'INITIAL BOTTOM SLOPE:',F10.5)
 5292 FORMAT(/10X,'DAM RATING CURVE IDS:',10(2A4,2X))
 5295 FORMAT(//
     .9X,'X   = CROSS SECTION LOCATION (MILE)'/
     .9X,'FLST= FLOOD STAGE (EL. WHEN REACHED TIME WILL BE PRINTED'/
     .9X,'HS  = ELEVATION ASSOCIATED WITH BS'/
     .9X,'AS  = CROSS SECTIONAL AREA OF MAIN CHANNEL (ACTIVE)'/
     .9X,'BS  = TOP WIDTH OF MAIN CHANNEL (ACTIVE)'/
     .9X,'BSS = CROSS SECTIONAL TOP WIDTH (INACTIVE)'/
     .9X,'BSL = CROSS SECTIONAL TOP WIDTH OF LEFT FLOOD PLAIN'/
     .9X,'BSR = CROSS SECTIONAL TOP WIDTH OF RIGHT FLOOD PLAIN '/
     .9X,'ASL = CROSS SECTIONAL AREA OF LEFT FLOOD PLAIN'/
     .9X,'ASR = CROSS SECTIONAL AREA OF RIGHT FLOOD PLAIN'/
     .9X,'HKC = ELEVATION CORRESPONDING TO THE CONVEYANCE'/
     .9X,'QKC = CONVEYANCE CORRESPONDING TO ELEVATION'/)
 5300 FORMAT(//
     .13X,'INVERT ELEVATION AT THE MOST UPSTREAM SECTION'/
     .13X,'SWITCH INDICATING IF SECTION HAS SPECIAL PROPERTIES'/
     .13X,'ELEVATION AT WHICH FLOODING COMMENCES'/
     .13X,'INITIAL WATER SURFACE ELEVATIONS'/
     .13X,'INITIAL DISCHARGES'/
     .13X,'ACTIVE AREA BELOW THE LOWEST HS ELEVATION'//)
 5303 FORMAT(/2X,'I=',I5,5X,'FLDSTG=',F8.2,3X,'X=',F10.2,3X,'KRCH=',
     .       I10,3X,'FKEC=',F10.2)
 5304 FORMAT(/2X,'I=',I5,3X,'FLDSTG=',F8.2,3X,'X=',F10.2)
 5305 FORMAT(/2X,'I=',I5,3X,'FLDSTG=',F8.2,3X,'X=',F10.2,3X,'XLAT=',
     .       F10.4,3X,'XLON=',F10.4,3X,'KRCH=',I10,3X,'FKEC=',F10.2)
 5306 FORMAT(/2X,'I=',I5,3X,'FLDSTG=',F8.2,3X,'X=',F10.2,3X,'XLAT=',
     .       F10.4,3X,'XLON=',F10.4)
 5310 FORMAT(10X,3HHS=,5X,10F10.2)
 5315 FORMAT(10X,3HBS=,5X,10F10.1)
 5320 FORMAT(9X,4HBSL=,5X,10F10.1)
 5325 FORMAT(9X,4HBSR=,5X,10F10.1)
 5330 FORMAT(9X,4HBSS=,5X,10F10.1)
 5331 FORMAT(9X,4H AS=,5X,10F10.1)
 5332 FORMAT(9X,4HASL=,5X,10F10.1)
 5333 FORMAT(9X,4HASR=,5X,10F10.1)
 5334 FORMAT(9X,4HASS=,5X,10F10.1)
 5335 FORMAT(/9X,4HHKC=,5X,8F10.2/18X,10F10.2)
 5340 FORMAT(9X,4HQKC=,5X,8F10.0/18X,10F10.0)
 5345 FORMAT(//25X,'REACH INFORMATION'/
     .10X,'NCM = CROSS SECTION NUMBER JUST D/S OF A MANNING N REACH'/
     .10X,'SNC  = SINUOSITY COEFFICIENT'/
     .10X,'SNM  = SINUOSITY COEFFICIENT'/
     .10X,'FKEC = EXPANSION OR CONTRACTION COEFFICIENT'/
     .10X,'CML  = MANNING N FOR LEFT FLOODPLAIN'/
     .10X,'CMR  = MANNING N FOR RIGHT FLOODPLAIN'/
     .10X,'YQCM = WSEL OR DISCHARGES ASSOCIATED WITH MANNING N'/)
 5350 FORMAT(//1X,'REACH INFO RIVER NO.',I3)
 5355 FORMAT(/9X,4HSNM=,5X,8F10.3)
 5360 FORMAT(9X,4HSNC=,5X,8F10.3)
 5365 FORMAT(/3X,'FKEC(I,',I1,13H), I = 1, NM(,I1,1H))
 5370 FORMAT (/3X,'NCM(K,',I2,'), K=1,NRCM1(',I2,1H))
 5375 FORMAT(/5X,' CM(K,',I2,1H,,I2,')=   ',5(10F10.4/21X))
 5380 FORMAT(5X,' CML(K,',I2,1H,,I2,')=  ',5(10F10.4/21X))
 5385 FORMAT(5X,' CMR(K,',I2,1H,,I2,')=  ',5(10F10.4/21X))
 5390 FORMAT(5X,' YQCM(K,',I2,1H,,I2,')= ',5(10F10.0/21X))
 5395 FORMAT(5X,' YQCM(K,',I2,1H,,I2,')= ',5(10F10.2/21X))
 5400 FORMAT(12X,I5,I10,6X,2A4,4X,A4)
 5410 FORMAT(8X,5HNSLC=,5X,8I10/18X,10I10)
 5420 FORMAT(8X,5HNQSL=,5X,8I10/18X,10I10)
 5500 FORMAT(//1X,24HEND OF FLDWAV INPUT DATA,/) 
  999 RETURN
      END
