      SUBROUTINE SIZE55(PO,IUSEZ,LEFT,LIMIT,K1,K2,K3,K4,K5,K6,K7,K8,K9,
     * K10,K11,K12,K13,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K28,
     * K29,K30)

C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C
      COMMON/IONUM/IN,IPR,IPU
      CHARACTER*8 SNAME
      INCLUDE 'common/ofs55'

      DIMENSION PO(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/size55.f,v $
     . $',                                                             '
     .$Id: size55.f,v 1.5 2004/09/24 20:52:49 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/'SIZE55  '/
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      WRITE(IPR,525)
 525  FORMAT(//////30X,32(1H*)/30X,32(1H*)/30X,3(1H*),26X,3(1H*)/30X,
     1   32H***  SUMMARY OF OUTPUT DATA  ***/30X,3(1H*),26X,3(1H*)/
     2   30X,32(1H*)/30X,32(1H*)///)
c =====================================
C  Dump the PO array
c ======these arrays were read in
C......................................................................
C  1   LOALOS        51  LOGAMA         101  LONIFM         151  LOTFLV           1   LCAKFT
C  2   LOAS          52  LOGHT          102  LONIFT         152  LOTGHT           2   LCBEV
C  3   LOASL         53  LOGSIL         103  LONITO         153  LOTIQH           3   LCBLV
C  4   LOASR         54  LOGWID         104  LONITT         154  LOTPG            4   LCCFT
C  5   LOASS         55  LOGZ           105  LONJFM         155  LOTQT            5   LCDDX
C  6   LOATF         56  LOGZ1          106  LONJFT      x  156  LOTS1            6   LCDFT
C  7   LOBBD         57  LOGZO          107  LONJTO         157  LOUW             7   LCDXR
C  8   LOBEXP        58  LOGZN          108  LONJTT         158  LOVIS            8   LCEFT
C  9   LOBLMX        59  LOGZPL         109  LONJUN         159  LOVWND           9   LCEXPP
C 10   LOBRBS        60  LOHCRL         110  LONLAD         160  LOWAGL          10   LCHCAV
C 11   LOBRGW        61  LOHDD          111  LONPT          161  LOWCLV          11   LCHFT
C 12   LOBRHS        62  LOHFDD         112  LONQCM         162  LOX             12   LCHLV
C 13   LOBS          63  LOHFLV         113  LONQL          163  LOXLOS          13   LCIFCV
C 14   LOBSL         64  LOHGTD         114  LONST          164  LOXT            14   LCIFLV
C 15   LOBSR         65  LOHKC          115  LONSTR         165  LOYMIN          15   LCIRGM
C 16   LOBSS         66  LOHLMN         116  LONTSP         166  LOYQCM          16   LCKRT1
C 17   LOCDBR        67  LOHPLV         117  LONTST         167  LOYQD           17   LCKRTN
C 18   LOCDOD        68  LOHPND         118  LONXLV         168  LOYQI           18   LCKRTP
C 19   LOCGCG        69  LOHS           119  LOPLNM         169  LOZBCH          19   LCKTYP
C 20   LOCGD         70  LOHSAP         120  LOPOWR         170  LOFBIO          20   LCLQN
C 21   LOCHMN        71  LOHSAR         121  LOPTAR         171  LOFRMO          21   LCLQT
C 22   LOCHMX        72  LOHSPD         122  LOQGH          172  LONQSL          22   LCNFLD
C 23   LOCHTW        73  LOHWLV         123  LOQHT          173  LONSLC          23   LCNKC
C 24   LOCLL         74  LOICG          124  LOQKC          174  LORBIO          24   LCNN
C 25   LOCM          75  LOICHN         125  LOQLNM         175  LORRMO          25   LCORDR
C 26   LOCML         76  LOIFXC         126  LOQLOS         176  LOSLCE          26   LCPFT
C 27   LOCMR         77  LOIGNM         127  LOQTD          177  LORIVN          27   LCPR
C 28   LOCOFW        78  LOIWF          128  LOQTT          178  LONOSN          28   LCQDIT
C 29   LOCPIP        79  LOKD           129  LOQYQD         179  LOSTEN          29   LCQFT
C 30   LOCRL         80  LOKFTR         130  LORCHT         180  LOTIDN          30   LCQUSJ
C 31   LOCSD         81  LOKLOS         131  LORHI          181  LOMRU           31   LCRC
C 32   LODPLV        82  LOKLPI         132  LORHO          182  LONJUM          32   LCRCP
C 33   LODTHF        83  LOKRCH         133  LORIVR         183  LOMPRV          33   LCRFT
C 34   LODTIN        84  LOKU           134  LORQI          184  LOMPLC          34   LCRXPR
C 35   LODXM         85  LOLAD          135  LOSAP          185  LOXLAT          35   LCTCG
C 36   LOEBE1        86  LOLQ1          136  LOSAR          186  LOXLON          36   LCTFL0
C 37   LOEBE2        87  LOLQTT         137  LOSHR          187  LODTMP          37   LCTFT
C 38   LOEBW1        88  LOLROT         138  LOSLFI         188  LOSPTH          38   LCTOPN
C 39   LOEBW2        89  LOLTQT         139  LOSLV          189  LOTPTH          39   LCVFT
C 40   LOEMBE        90  LOMIXF         140  LOSNC                               40   LCWF
C 41   LOEMBW        91  LOMRV          141  LOSNM                               41   LCXYZ
C 42   LOEPQJ        92  LOMSG          142  LOSPL                               42   LCY1FT
C 43   LOFKC         93  LOMUD          143  LOST1N                              43   LCY2FT
C 44   LOFKEC        94  LONB           144  LOSTM                               44   LCYDIT
C 45   LOFKF         95  LONBT          145  LOSTNN                              45   LCZFT
C 46   LOFKO         96  LONCM          146  LOSTON
C 47   LOFLST        97  LONCM1         147  LOSTQN
C 48   LOFMC         98  LONG           148  LOSTTN
C 49   LOFMF         99  LONGAG         149  LOTDTN
C 50   LOFMO        100  LONGS          150  LOTFH
CC......................................................................

      LOMSG =PO(10)
      LODTIN=PO(57)
      LOTDTN=PO(58)
      LONJFT=PO(63)
      LONIFT=PO(64)
      LONJTT=PO(65)
      LONITT=PO(66)
      LONXLV=PO(67)
      LONBT =PO(68)
      LONQL =PO(69)
      LONJUN=PO(70)
      LONPT =PO(71)
      LOKU  =PO(72)
      LOKD  =PO(73)
      LONCM1=PO(74)
      LONGAG=PO(75)
      LOMIXF=PO(76)
      LONQCM=PO(77)
      LOMUD =PO(78)
      LOUW  =PO(79)
      LOVIS =PO(80)
      LOSHR =PO(81)
      LOPOWR=PO(82)
      LOIWF =PO(83)
      LOKFTR=PO(84)
      LOKLPI=PO(85)
      LOXLOS=PO(86)
      LOQLOS=PO(87)
      LOALOS=PO(88)
      LOMRV =PO(89)
      LOKLOS=PO(90)
      LOCOFW=PO(92)
      LOVWND=PO(93)
      LOWAGL=PO(94)
      LOEPQJ=PO(95)
      LOATF =PO(96)
      LONSTR=PO(99)
      LONB  =PO(126)
      LOXT  =PO(127)
      LODXM =PO(128)
      LORCHT=PO(129)
      LONLAD=PO(130)
      LOLROT=PO(131)
      LOLQ1 =PO(132)
      LOHWLV=PO(135)
      LOSLV =PO(136)
      LOWCLV=PO(137)
      LOTFLV=PO(139)
      LOBLMX=PO(140)
      LOHFLV=PO(141)
      LOHLMN=PO(142)
      LOSAP =PO(143)
      LOHSAP=PO(144)
      LOHPND=PO(145)
      LOHPLV=PO(146)
      LODPLV=PO(147)
      LOLAD =PO(151)
      LORHI =PO(152)
      LORQI =PO(153)
      LODTHF=PO(154)
      LOBEXP=PO(155)
      LOGSIL=PO(156)
      LOSAR =PO(157)
      LOHSAR=PO(158)
      LOQGH =PO(159)
      LOHFDD=PO(160)
      LOTFH =PO(161)
      LOBBD =PO(162)
      LOZBCH=PO(163)
      LOYMIN=PO(164)
      LOHDD =PO(165)
      LOHSPD=PO(166)
      LOHGTD=PO(167)
      LOCSD =PO(168)
      LOCGD =PO(169)
      LOCDOD=PO(170)
      LOQTD =PO(171)
      LOTIQH=PO(173)
      LOCPIP=PO(174)
      LOCGCG=PO(175)
      LOGWID=PO(176)
      LONG  =PO(177)
      LOGHT =PO(178)
      LOQHT =PO(179)
      LOGZPL=PO(181)
      LONTSP=PO(183)
      LONTST=PO(184)
      LOQTT =PO(185)
      LOTQT =PO(186)
      LOICHN=PO(187)
      LOPTAR=PO(188)
      LOCHTW=PO(189)
      LOCLL =PO(192)
      LOSPL =PO(193)
      LOCRL =PO(194)
      LOHCRL=PO(195)
      LOICG =PO(197)
      LOTGHT=PO(198)
      LOBRBS=PO(199)
      LOBRHS=PO(200)
      LOEBE2=PO(201)
      LOEBE1=PO(202)
      LOBRGW=PO(203)
      LOCDBR=PO(204)
      LOEBW2=PO(205)
      LOEBW1=PO(206)
      LOEMBW=PO(207)
      LOEMBE=PO(208)
      LONGS =PO(211)
      LOGZ  =PO(212)
      LOTPG =PO(217)
      LORHO =PO(218)
      LOGAMA=PO(219)
      LOYQI =PO(220)
      LOGZ1 =PO(221)
      LOGZN =PO(222)
      LOQYQD=PO(223)
      LOYQD =PO(224)
      LOSLFI=PO(229)
      LOIFXC=PO(231)
      LOX   =PO(235)
      LOFLST=PO(236)
      LOAS  =PO(239)
      LOHS  =PO(240)
      LOBS  =PO(241)
      LOBSS =PO(242)
      LOBSL =PO(243)
      LOBSR =PO(244)
      LOASL =PO(245)
      LOASR =PO(246)
      LOHKC =PO(247)
      LOQKC =PO(248)
      LOFKC =PO(249)
      LOFMC =PO(250)
      LOFKF =PO(251)
      LOFMF =PO(252)
      LOFKO =PO(253)
      LOFMO =PO(254)
      LOCHMN=PO(255)
      LOCHMX=PO(256)
      LOFKEC=PO(260)
      LOSNC =PO(261)
      LOSNM =PO(262)
      LONCM =PO(263)
      LOYQCM=PO(264)
      LOCM  =PO(265)
      LOCML =PO(266)
      LOCMR =PO(267)
      LOKRCH=PO(273)
      LONJFM=PO(274)
      LONIFM=PO(275)
      LONJTO=PO(276)
      LONITO=PO(277)
      LOASS =PO(278)
cc      LOTS1 =PO(322)
      LONSLC=PO(324)
      LONQSL=PO(325)
      LOSLIC=PO(326)
      LOFRMO=PO(327)
      LOFBIO=PO(328)
      LORRMO=PO(329)
      LORBIO=PO(330)
      LOLQTT=PO(333)
      LOLTQT=PO(334)
      LONOSN=PO(348)
      LORIVN=PO(349)
      LOQLNM=PO(350)
      LOTIDN=PO(351)
      LOSTTN=PO(352)
      LOSTEN=PO(353)
      LOSTQN=PO(354)
      LOSTON=PO(356)
      LONST =PO(358)
      LOGZO =PO(360)
      LOST1N=PO(361)
      LOSTNN=PO(362)
      LOSTM =PO(363)
      LORIVR=PO(365)
      LOPLNM=PO(367)
      LOIGNM=PO(368)
      LOMRU=PO(369)
      LONJUM=PO(370)
      LOMPRV=PO(371)
      LOMPLC=PO(372)
      LOXLAT=PO(373)
      LOXLON=PO(374)
      LODTMP=PO(377)
cc      LOSPTH=PO(379)
cc      LOTPTH=PO(380)
c ======these arrays were generated
      LCORDR=PO(91)
      LCTFL0=PO(138)
      LCEXPP=PO(149)
      LCRXPR=PO(150)
      LCNFLD=PO(172)
      LCTOPN=PO(180)
      LCTCG =PO(196)
      LCRCP =PO(225)
      LCKRTP=PO(270)
      LCKRT1=PO(271)
      LCKRTN=PO(272)
      LCNN  =PO(281)
      LCLQN =PO(282)
      LCLQT =PO(283)
      LCQUSJ=PO(284)
      LCPR  =PO(285)
      LCIRGM=PO(286)
      LCIFLV=PO(287)
      LCHLV =PO(289)
      LCBLV =PO(290)
      LCBEV =PO(291)
      LCNKC =PO(292)
      LCYDIT=PO(293)
      LCQDIT=PO(294)
      LCDDX =PO(295)
      LCWF  =PO(296)
      LCIFCV=PO(297)
      LCHCAV=PO(298)
      LCDXR =PO(299)
      LCXYZ =PO(307)
      LCTFT =PO(335)
      LCY1FT=PO(336)
      LCY2FT=PO(337)
      LCPFT =PO(338)
      LCQFT =PO(339)
      LCHFT =PO(340)
      LCRFT =PO(341)
      LCZFT =PO(342)
      LCCFT =PO(343)
      LCDFT =PO(344)
      LCEFT =PO(345)
      LCVFT =PO(346)
      LCAKFT=PO(347)
      LCKTYP=PO(359)
      LCRC  =PO(366)

C          DUMP THE CO ARRAY
C
cc      LXYDI =PO(316)
cc      LXQDI =PO(317)
cc      LXQLI =PO(318)
cc      LXPLTI=PO(319)
cc      LXIWTI=PO(364)
      JSIZE=PO(3)

C          STORE IN THE D ARRAY THE FOLLOWING PARAMETERS:
C
C             1 ---  LZAVD            51 ---  LZST0             1 ---  LTFFS
C             2 ---  LZAVDL           52 ---  LZTFDB            2 ---  LTFS
C             3 ---  LZBBP            53 ---  LZTFDO            3 ---  LTITSV
C             4 ---  LZC              54 ---  LZTQPK            4 ---  LTQA
C             5 ---  LZCSCM           55 ---  LZTYPK            5 ---  LTQLJ
C             6 ---  LZD              56 ---  LZVC              6 ---  LTTII
C             7 ---  LZDHEQ           57 ---  LZVD              6 ---  LTVLSV
C     x       8 ---  LZFHT            58 ---  LZVPK             8 ---  LTYA
C             9 ---  LZFLAG           59 ---  LZVU              9 ---  LTTO
C            10 ---  LZHDSN           60 ---  LZWDSN           10 ---  LTSTE
C            11 ---  LZICT            61 ---  LZXX             11 ---  LTTID
C            12 ---  LZICTR           62 ---  LZYBP            12 ---  LTNOS
C            13 ---  LZIFR            63 ---  LZYC
C            14 ---  LZIORF           64 ---  LZYCR
C            15 ---  LZIPSV           65 ---  LZYD
C            16 ---  LZIRSV           66 ---  LZYII
C            17 ---  LZITRX           67 ---  LZYINT
C            18 ---  LZKS1            68 ---  LZYJ
C            19 ---  LZKSN            69 ---  LZYMX
C            20 ---  LZKSP            70 ---  LZYN
C            21 ---  LZLRMX           71 ---  LZYPK
C            22 ---  LZMINT           72 ---  LZYQR
C            23 ---  LZNDPS           73 ---  LZYU
C            24 ---  LZNFLO           74 ---  LZYUMN
C            25 ---  LZNQRT           75 ---  LZJT
C            26 ---  LZNSIR           76 ---  LZFRMS
C            27 ---  LZPLTM           77 ---  LZFBIA
C            28 ---  LZQBCH           78 ---  LZRRMS
C            29 ---  LZQC             79 ---  LZRBIA
C            30 ---  LZQD             80 ---  LZJCK
C            31 ---  LZQDSN           81 ---  LZIMAP
C            32 ---  LZQII            82 ---  LZLVPK
C            33 ---  LZQINT           83 ---  LZWTLV
C            34 ---  LZQJ             84 ---  LZFYPK
C            35 ---  LZQLLT           85 ---  LZFQPK
C            36 ---  LZQLSM           86 ---  LZFTYPK
C            37 ---  LZQLV            87 ---  LZFTQPK
C            38 ---  LZQMX            88 ---  LZFVPK
C            39 ---  LZQOTP           89 ---  LZFLVPK
C            40 ---  LZQOTR
C            41 ---  LZQPK
C            42 ---  LZQPND
C            43 ---  LZQT0
C            44 ---  LZQU
C            45 ---  LZRMS
C            46 ---  LZSQO
C            47 ---  LZSQS1
C            48 ---  LZSQS2
C            49 ---  LZSQW
C            50 ---  LZSTYP
C


      IUSEZ=1
      LZC=IUSEZ
      LZD=LZC+K15
cc      LZFHT=LZD+K15*4
cc      LZICT=LZFHT+K1*K4
      LZICT=LZD+K15*4
      LZIPSV=LZICT+K2
      LZIRSV=LZIPSV+K12
      LZITRX=LZIRSV+K12
      LZMINT=LZITRX+K1
      LZNDPS=LZMINT+K11
      LZNSIR=LZNDPS+K12
      LZQC=LZNSIR+K1
      LZQD=LZQC+K1*K2
      LZQII=LZQD+K1*K2
      LZQJ=LZQII+K1*K2
      LZQU=LZQJ+K1
      LZSTYP=LZQU+K1*K2
      LZVC=LZSTYP+K12
      LZVD=LZVC+K1*K2
      LZVU=LZVD+K1*K2
      LZXX=LZVU+K1*K1
      LZYC=LZXX+K15
      LZYD=LZYC+K1*K2
      LZYII=LZYD+K1*K2
      LZYJ=LZYII+K1*K2
      LZYU=LZYJ+K1
      LZQINT=LZYU+K1*K2
      LZYINT=LZQINT+K1*K2*K11
      LZQLLT=LZYINT+K1*K2*K11
      LZQLSM=LZQLLT+K1
      LZDHEQ=LZQLSM+K2*K1
      LZNQRT=LZDHEQ+K2*K18
      LZBBP=LZNQRT+K1
      LZYBP=LZBBP+K1*K16
      LZQBCH=LZYBP+K1*K16
      LZQOTP=LZQBCH+K1*K16
      LZQOTR=LZQOTP+K1*K16
      LZTFDB=LZQOTR+K1*K16
      LZIORF=LZTFDB+K1*K16
      LZTFDO=LZIORF+K1*K16
      LZQDSN=LZTFDO+K1*K16
      LZHDSN=LZQDSN+K1*K16
      LZSQW=LZHDSN+K1*K16
      LZSQS1=LZSQW+K1*K16*2
      LZSQS2=LZSQS1+K1*K16*2
      LZSQO=LZSQS2+K1*K16*2
      LZLRMX=LZSQO+K1*K16*2
      LZICTR=LZLRMX+K1*K5
      LZYUMN=LZICTR+K1*K5*K2
      LZIFR=LZYUMN+K1*K2
      LZKSP=LZIFR+K2
      LZKS1=LZKSP+K1*K5*K2
      LZKSN=LZKS1+K1*K5*K2
      LZYN=LZKSN+K1*K5*K2
      LZYCR=LZYN+K1*K2
      LZFLAG=LZYCR+K1*K2
      LZWDSN=LZFLAG+K1*K16
      LZTYPK=LZWDSN+K1*K16
      LZYPK=LZTYPK+K1*K2
      LZTQPK=LZYPK+K1*K2
      LZQPK=LZTQPK+K1*K2
      LZQLV=LZQPK+K1*K2
      LZQPND=LZQLV+K1*K2
      LZPLTM=LZQPND+K22
      LZQMX=LZPLTM+K24
      LZYMX=LZQMX+K1*K2
      LZRMS=LZYMX+K1*K2
      LZAVD=LZRMS+K4*K1
      LZNFLO=LZAVD+K4*K1
      LZVPK=LZNFLO+K1*K16
      LZYQR=LZVPK+K1*K2
      LZAVDL=LZYQR+K8
      LZCSCM=LZAVDL+K8*2
      LZST0=LZCSCM+K8*2
      LZQT0=LZST0+K28
      LZJT=LZQT0+K28
      LZFRMS=LZJT+K1
      LZFBIA=LZFRMS+3*K29
      LZRRMS=LZFBIA+3*K29
      LZRBIA=LZRRMS+3*K29
      LZCCO=LZRBIA+3*K29
      LZJCK=LZCCO+JSIZE
      LZIMAP=LZJCK+K1
      LZWTLV=LZIMAP+K2*K1*K30
      LZLVPK=LZWTLV+K22
      LZFYPK=LZLVPK+K22
      LZFQPK=LZFYPK+K2*K1
      LZFTYPK=LZFQPK+K2*K1
      LZFTQPK=LZFTYPK+K2*K1
      LZFVPK=LZFTQPK+K2*K1
      LZFLVPK=LZFVPK+K2*K1
      IUSEZ=LZFLVPK+K22-1


C==========THESE ARE TIME ARRAYS
CC      LCFS=LCFFS+K3
CC      LCITSV=LCFS+K3
CC      LCQA=LCITSV+K3*K12
CC      LCQLJ=LCQA+K3
CC      LCQTC=LCQLJ+K1*K3
CC      LCSTC=LCQTC+K1*K4*K3
CC      LCTII=LCSTC+K1*K4*K3
CC      LCVLSV=LCTII+K3
CC      LCYA=LCVLSV+K12*K3
CC      LCQLLT=LCYA+K3
CC      LCBBP=LCQLRT+K1*K17*K3
CC      LCNFLO=LCAVD+K3*K1


      RETURN
      END
