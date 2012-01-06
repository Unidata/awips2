C MODULE EROT26
C---------------------------------------------------------------------
C
C  PRINT ERROR MESSAGES THAT OCCURRED DEFINING RES-SNGL OPERATION
C
      SUBROUTINE EROT26
C
C  ERRORS THAT HAVE OCCURRED ARE HELD IN /ERR26/. INFO HELD THERE
C  INCLUDES THE NUMBER OF THE ERROR, THE LINE NUMBER IT OCCURRED ON,
C  THE FIELD NO. WITHIN THE LINE IT OCCURRED ON, AND THE CHARACTER
C  WITHIN THE STRING WHERE THE ERROR STARTED.
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/err26'
      INCLUDE 'common/errv26'
C
C  EMSG(15,NALL): WHERE NALL IS THE NUMBER OF STATEMENTS IN ALL,
C          NALL = NMSG(1) + NMSG(2) + NMSG(3)
C  THIS SHOULD BE MODIFIED IF THERE IS ANY NEW MSG TO COME.
C
      CHARACTER*8 EMSG(15,138),FMT(15)
C
C  EACH MGXXXX(N) ALLOCATES  STORAGE FOR N/15 STATEMENTS
C
      CHARACTER*8  MG1(75),MG   6(75),MG  11(75),MG  16(75),
     .          MG  21(75),MG  26(75),MG  31(75),MG  36(75),
     .          MG  41(75),MG  46(75),MG  51(75),MG  56(75),
     .          MG  61(75),MG  66(75),MG  71(75),MG  76(75),
     .          MG  81(75),MG  86(75),MG  91(75),MG  96(75),
     .          MG 101(75),MG 106(75),MG 111(75),MG 116(75),
     .          MG 121(75),MG 126(75),
     .          MG 500(90),
     .          MG1000(30)
C
C  THE ENTRIES IN (EMSG(1, E1),MG 500) AND (EMSG(1, E2),MG1000)
C  ARE  E1 = NMSG(1) + 1 AND
C       E2 = NMSG(1) + NMSG(2) + 1 , RESPECTIVELY.
C  THESE SHOULD BE MODIFIED IF THERE IS ANY NEW MSG TO COME.
C
      EQUIVALENCE    (EMSG(1, 1),MG 1(1)),(EMSG(1, 6),MG 6(1)),
     .               (EMSG(1,11),MG11(1)),(EMSG(1,16),MG16(1)),
     .               (EMSG(1,21),MG21(1)),(EMSG(1,26),MG26(1)),
     .               (EMSG(1,31),MG31(1)),(EMSG(1,36),MG36(1)),
     .               (EMSG(1,41),MG41(1)),(EMSG(1,46),MG46(1)),
     .               (EMSG(1,51),MG51(1)),(EMSG(1,56),MG56(1)),
     .               (EMSG(1,61),MG61(1)),(EMSG(1,66),MG66(1)),
     .               (EMSG(1,71),MG71(1)),(EMSG(1,76),MG76(1)),
     .               (EMSG(1,81),MG81(1)),(EMSG(1,86),MG86(1)),
     .               (EMSG(1,91),MG91(1)),(EMSG(1,96),MG96(1)),
     .               (EMSG(1,101),MG101(1)),(EMSG(1,106),MG106(1)),
     .               (EMSG(1,111),MG111(1)),(EMSG(1,116),MG116(1)),
     .               (EMSG(1,121),MG121(1)),(EMSG(1,126),MG126(1)),
     .               (EMSG(1,131),MG500(1)),
     .               (EMSG(1,137),MG1000(1))
C
      DIMENSION ISTRT(3),IEND(3),NMSG(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/erot26.f,v $
     . $',                                                             '
     .$Id: erot26.f,v 1.6 2001/06/13 10:08:48 mgm Exp $
     . $' /
C    ===================================================================
C
C  THE MSG STATEMENTS RANGE FROM ISTRT(I) TO IEND(I), I = 1,2,3,
C  FOR THE 1ST ,2ND AND 3RD GROUP CORRESPONDINGLY.  IF THERE IS ANY
C  REARRANGE FOR THEM , THEN A SUITABLE CAHNGE SHOULD BE MADE  TO THE
C  ENTRIES OF ISTRT AND IEND.
C
      DATA ISTRT/ 1,500,1000/
      DATA IEND /130,505,1001/
C
      DATA IBLNK/4H    /
C
C
      DATA MG   1/
     .  8H('   NOT,8H A VALID,8H KEYWORD,8H, EITHER,8H NOT REC,
     .  8HOGNIZABL,8HE OR NOT,8H ALLOWED,8H IN CURR,8HENT SITU,
     1  8HATION.'),8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ''S,8HET'' FOU,8HND WHEN ,8HNOT EXPE,8HCTED.') ,
     .  8H        ,8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ''T,8HHEN, ELS,8HEIF, ELS,8HE'' FOUN,8HD WHEN N,
     .  8HOT EXPEC,8HTED.')  ,8H        ,8H        ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   REA,8HL VALUE ,8HEXPECTED,8H, NOT FO,8HUND.')  ,
     .  8H        ,8H        ,8H        ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   INT,8HEGER VAL,8HUE EXPEC,8HTED, NOT,8H FOUND.',
     .  8H)       ,8H        ,8H        ,8H        ,8H        ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  6/
     .  8H('   ERR,8HOR IN PA,8HRSING FI,8HELD.')  ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NOT,8H A VALID,8H SCHEME/,8HUTILITY ,8HID.')   ,
     .  8H        ,8H        ,8H        ,8H        ,8H        ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   SCH,8HEME/UTIL,8HITY REQU,8HESTED TO,8H BE USED,
     .  8H, HAS NO,8HT BEEN D,8HEFINED.',8H)       ,8H        ,
     8  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ILL,8HEGAL ''S,8HET'' VAR,8HIABLE NA,8HME.')   ,
     .  8H        ,8H        ,8H        ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8H''='' FO,8HLLOWING ,8H''SET'' ,8HVARIABLE,
     1  8H NAME.'),8H        ,8H        ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG 11/
     .  8H('   MIS,8HMATCHED ,8HPARENTHE,8HSES.')  ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   MIS,8HMATCHED ,8H(UNBALAN,8HCED) IF-,8HENDIF PA,
     1  8HIRS.')  ,8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   CON,8HTINUATIO,8HN EXPECT,8HED, NONE,8H FOUND.',
     1  8H)       ,8H        ,8H        ,8H        ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ''T,8HHEN'' EX,8HPECTED, ,8HNOT FOUN,8HD.')    ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ''D,8HO'' EXPE,8HCTED (AF,8HTER ''TH,8HEN''), N,
     1  8HOT FOUND,8H.')     ,8H        ,8H        ,8H        ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  16/
     .  8H('   SCH,8HEME/UTIL,8HITY IS N,8HOT ACTIV,8HATED WIT,
     1  8HH A ''DO,8H''.')   ,8H        ,8H        ,8H        ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8HDO''S OF,8H SCHEMES,8H FOUND I,8HN THIS ',
     1  8H'IF'' GR,8HOUP.')  ,8H        ,8H        ,8H        ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8HUNCONDIT,8HIONAL DO,8H''S IN P,8HRIMARY ',
     1  8H'IF'' GR,8HOUP.')  ,8H        ,8H        ,8H        ,
     8  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NUL,8HL FIELD ,8HNOT ALLO,8HWED.')  ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   FIE,8HLD LONGE,8HR THAN A,8HLLOWED.',8H)       ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  21/
     .  8H('   NO ,8HEND CARD,8H FOUND.',8H)       ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ''T,8HHEN'' NO,8HT ALLOWE,8HD UNLESS,8H FOLLOWI,
     2  8HNG'/'   ,8H''IF'' O,8HR ''ELSE,8HIF'' KEY,8HWORD.') ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ONL,8HY ''DO'',8H OR ''EN,8HDIF'' AL,8HLOWED AF,
     2  8HTER ''EL,8HSE'' STA,8HTEMENT.',8H)       ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ONL,8HY ONE '',8HELSE'' A,8HLLOWED P,8HER IF-EN,
     2  8HDIF BLOC,8HK.')    ,8H        ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ONL,8HY ''DO'',8H STATEME,8HNT CAN P,8HRECEDE ',
     2  8H'ELSE'' ,8HOR'/'   ,8H''ELSEIF,8H'' STATE,8HMENT.') ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  26/
     .  8H('   ''E,8HNDIF'' C,8HAN ONLY ,8HFOLLOW A,8H ''DO'' ,
     2  8HSTATEMEN,8HT.')    ,8H        ,8H        ,8H        ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8H''ENDRCL,8H'' CARD ,8HPROVIDED,8H.')     ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   CHA,8HRACTER N,8HOT ALLOW,8HED IN EX,8HISTING S,
     2  8HTATE OF ,8H''IF'' C,8HLAUSE.'),8H        ,8H        ,
     8  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   SYS,8HTEM COMP,8HARISON V,8HARIABLE ,8HEXPECTED,
     2  8H, NOT FO,8HUND.')  ,8H        ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   REL,8HATIONAL ,8HOPERATOR,8H EXPECTE,8HD, NOT F,
     3  8HOUND.') ,8H        ,8H        ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  31/
     .  8H('   SYS,8HTEM FUNC,8HTION NAM,8HE,SET VA,8HRIABLE N,
     3  8HAME OR R,8HEAL NUMB,8HER'/'   ,8HEXPECTED,8H,NOT FOU,
     1  8HND.')   ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ''A,8HND'' OR ,8H''OR'' E,8HXPECTED,,8H NOT FOU,
     3  8HND.')   ,8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   SYS,8HTEM FUNC,8HTION NAM,8HE EXPECT,8HED, NOT ,
     3  8HFOUND.'),8H        ,8H        ,8H        ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ''I,8HF'' CLAU,8HSE MUST ,8HBE OF CO,8HMPLETE L,
     3  8HOGICAL R,8HELATIONS,8HHIP.')  ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   PAR,8HMS NEEDE,8HD FOR '',8HGENERAL',8H' OR SCH,
     3  8HEME/UTIL,8HITY'/'  ,8H SPECIFI,8HCATION.',8H)       ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  36/
     .  8H('   TIM,8HE-SERIES,8H KEYWORD,8H NEEDED ,8HFOR ''GE,
     3  8HNERAL'' ,8HOR'/'   ,8HSCHEME/U,8HTILITY S,8HPECIFICA,
     6  8HTION.') ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   CAR,8HRYOVER K,8HEYWORD N,8HEEDED FO,8HR ''GENE,
     3  8HRAL'' OR,8H'/'   SC,8HHEME/UTI,8HLITY SPE,8HCIFICATI,
     7  8HON.')   ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NOT,8H A VALID,8H UNITS S,8HPECIFICA,8HTION.') ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
     8  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   MUL,8HTIPLE EN,8HTRIES OF,8H KEYWORD,8H NOT ALL,
     3  8HOWED.') ,8H        ,8H        ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NUM,8HBER OF V,8HALUES EN,8HTERED CA,8HN NOT BE,
     4  8H DIVIDED,8H '/'   I,8HNTO A SE,8HT OF PAI,8HRS.')   ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  41/
     .  8H('   VAL,8HUES ARE ,8HNOT IN A,8HSCENDING,8H ORDER.',
     4  8H)       ,8H        ,8H        ,8H        ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   INV,8HALID INT,8HERPOLATI,8HON SPEC,,8H SET TO ,
     4  8HDEFAULT ,8H(LINEAR),8H.')     ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   INC,8HOMPLETE ,8HINPUT FO,8HR GENERA,8HL SECTIO,
     4  8HN.')    ,8H        ,8H        ,8H        ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   TIM,8HE-SERIES,8H ALREADY,8H SPECIFI,8HED IN TH,
     4  8HIS SECTI,8HON.')   ,8H        ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   TIM,8HE-SERIES,8H DOES NO,8HT EXIST,,8H OR MISS,
     4  8HING DATA,8H NOT ALL,8HOWED.') ,8H        ,8H        ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  46/
     .  8H('   TIM,8HE-SERIES,8H DATATYP,8HE HAS WR,8HONG DIME,
     4  8HNSIONS.',8H)       ,8H        ,8H        ,8H        ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   SPE,8HCIFICATI,8HON MUST ,8HBE EVEN ,8HMULTIPLE,
     4  8H OF OPER,8HATION TI,8HME INTER,8HVAL.')  ,8H        ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   THI,8HS TIME-S,8HERIES MU,8HST HAVE ,8HSAME TIM,
     4  8HE INTERV,8HAL AS TH,8HE'/'   O,8HPERATION,8H TIME IN,
     8  8HTERVAL.',8H)       ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8HCARRYOVE,8HR FOR IN,8HFLOW HAS,8H BEEN EN,
     4  8HTERED.'),8H        ,8H        ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8HCARRYOVE,8HR FOR DI,8HSCHARGE ,8HHAS BEEN,
     5  8H ENTERED,8H.')     ,8H        ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  51/
     .  8H('   NO ,8HCARRYOVE,8HR HAS BE,8HEN ENTER,8HED FOR P,
     5  8HOOL OR S,8HTORAGE.',8H)       ,8H        ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   LEV,8HELS OF S,8HCHEME/UT,8HILITY DE,8HFINITION,
     5  8H MUST BE,8H INCREME,8HNTS OF 1,8H.')     ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8HINPUT NE,8HEDED TO ,8HDEFINE _,8H________,
     5  8H___,',T3,8H0,3A4,' ,8HLOOKING ,8HFOR'/'  ,8HNEXT SCH,
     3  8HEME/UTIL,8HITY DEFI,8HNITIONS.,8H')      ,8H        ,
C
     .  8H('   NO ,8HPARMS NE,8HEDED FOR,8H THIS SC,8HHEME/UTI,
     5  8HLITY DEF,8HINITION.,8H')      ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8HTIME-SER,8HIES NEED,8HED FOR T,8HHIS SCHE,
     5  8HME/UTILI,8HTY DEFIN,8HITION.'),8H        ,8H        ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  56/
     .  8H('   NO ,8HCARRYOVE,8HR NEEDED,8H FOR THI,8HS SCHEME,
     5  8H/UTILITY,8H DEFINIT,8HION.')  ,8H        ,8H        ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   UNE,8HXPECTED ,8HOCCURENC,8HE OF ''E,8HNDGENL'',
     5  8H TYPE CA,8HRDS.')  ,8H        ,8H        ,8H        ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   FIR,8HST STORA,8HGE IN EL,8HEV. VS S,8HTORAGE C,
     5  8HURVE MUS,8HT BE 0.0,8H, FIRST ,8HELEVATIO,8HN IS THE,
     8  8H BOTTOM ,8HOF THE R,8HESERVOIR,8H.')     ,8H        ,
C
     .  8H('   A _,8H________,8H___ KEYW,8HORD REQU,8HIRED BUT,
     5  8H NOT FOU,8HND.',T6,,8H3A4)    ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   A _,8H________,8H___ KEYW,8HORD FOUN,8HD, NOT N,
     6  8HEEDED.',,8HT6,3A4) ,8H        ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  61/
     .  8H('   ONL,8HY POSITI,8HVE VALUE,8HS ALLOWE,8HD.')    ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   INC,8HORRECT N,8HUMBER OF,8H ITEMS F,8HOUND IN ,
     6  8HLIST.') ,8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ELE,8HVATION N,8HOT WITHI,8HN BOUNDS,8H OF ELEV,
     6  8HATION VS,8H STORAGE,8H CURVE.',8H)       ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ILL,8HEGAL DEF,8HINITION ,8HOF RULE ,8HCURVE - ,
     6  8HDATE NOT,8H'/'   BE,8HTWEEN 1 ,8HAND 366.,8H')      ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ___,8H________,8H_ CURVE ,8HMUST HAV,8HE BEEN D,
     6  8HEFINED T,8HO DEFINE,8H THIS SC,8HHEME.',T,8H4,3A4)  ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  66/
     .  8H('   ___,8H________,8H_ HAS NO,8HT BEEN D,8HEFINED A,
     6  8HT REFERE,8HNCE LEVE,8HL.',T4,3,8HA4)     ,8H        ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   RUL,8HECURVE V,8HALUES NO,8HT BRACKE,8HTED PROP,
     6  8HERLY IN ,8HDEFINITI,8HON'/'   ,8HOF _____,8H_______ ,
     7  8HCURVE.',,8HT7,3A4) ,8H        ,8H        ,8H        ,
C
     .  8H('   VAL,8HUES MUST,8H BE BETW,8HEEN 0.01,8H AND 1.0,
     6  8H0.')    ,8H        ,8H        ,8H        ,8H        ,
     8  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   MUL,8HTIPLE DE,8HFINITION,8H OF THIS,8H TIME-SE,
     6  8HRIES IS ,8HNOT ALLO,8HWED.')  ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ALL,8H REQUIRE,8HD INPUT ,8HFOR THIS,8H SCHEME/,
     7  8HUTILITY ,8HNOT FOUN,8HD.')    ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  71/
     .  8H('   VAL,8HUES MUST,8H SUM TO ,8H1.00.') ,8H        ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   INC,8HORRECT V,8HALUE OF ,8HPARAMETE,8HR SPECIF,
     7  8HICATION.,8H')      ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ONL,8HY ONE TA,8HILWATER ,8HRATING C,8HURVE SPE,
     7  8HCIFICATI,8HON IS AL,8HLOWED.'),8H        ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   VAL,8HUE IS IN,8H INCORRE,8HCT ORDER,8H  WITH A,
     7  8HNOTHER P,8HARAMETER,8H VALUE.',8H)       ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   A ',8H'SET'' V,8HARIABLE ,8HNAME CAN,8H''T BE L,
     7  8HONGER TH,8HAN 12 CH,8HARACTERS,8H.')     ,8H        ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  76/
     .  8H('   A ',8H'SET'' V,8HARIABLE ,8HNAME CAN,8H''T BE A,
     7  8HLL BLANK,8H.')     ,8H        ,8H        ,8H        ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   THE,8H FIRST C,8HHARACTER,8H OF A '',8HSET'' VA,
     7  8HRIABLE N,8HAME CAN',8H'T BE'/',8H   NUMER,8HICAL.') ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   A ',8H'SET'' V,8HARIABLE ,8HNAME CAN,8H''T STAR,
     7  8HT WITH ',8H'RULE''.,8H')      ,8H        ,8H        ,
     8  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   A ',8H'SET'' V,8HARIABLE ,8HNAME CAN,8H''T STAR,
     7  8HT WITH ',8H'MAXQ''.,8H')      ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8H+,-,*,/,,8H.,(,) AN,8HD @ ALLO,8HWED IN ',
     8  8H'SET'' V,8HARIABLE ,8HNAME.') ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  81/
     .  8H('   A S,8HCHEME/UT,8HILITY NA,8HME IS NO,8HT ALLOWE,
     8  8HD IN ''S,8HET'' VAR,8HIABLE NA,8HME.')   ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   BAD,8H BLOCK S,8HTRUTURE ,8HOR INVAL,8HID KEYWO,
     8  8HRD.')   ,8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8HCOMPANIO,8HN SCHEME,8H HAS BEE,8HN FOUND ,
     8  8HFOR THIS,8H ''ENTRY,8H'' UTILI,8HTY.')   ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   EXA,8HCTLY 12 ,8HVALUES M,8HUST BE E,8HNTERED F,
     8  8HOR THIS ,8HLIST.') ,8H        ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   VAL,8HUE MUST ,8HBE GREAT,8HER THAN ,8HZERO.') ,
     8  8H        ,8H        ,8H        ,8H        ,8H        ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  86/
     .  8H('   NO ,8HINPUT LI,8HNES HAVE,8H BEEN FO,8HUND FOR ,
     8  8HSPECIFIC,8H SECTION,8H.')     ,8H        ,8H        ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   NO ,8HINPUT LI,8HNES HAVE,8H BEEN FO,8HUND FOR ,
     8  8HRCL SECT,8HION.')  ,8H        ,8H        ,8H        ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   THE,8H _______,8H_____ SC,8HHEME/UTI,8HLITY HAS,
     8  8H BEEN DE,8HFINED, B,8HUT',T8,3,8HA4/'   I,8HT HAS NO,
     8  8HT BEEN E,8HXECUTED ,8HBY A ''D,8HO'' STAT,8HEMENT.'),
C
     .  8H('   A ',8H'POWERGE,8HN''OR '',8HMINQ'' D,8HEFINITIO,
     8  8HN MUST B,8HE ACCOMP,8HANIED BY,8H'/'   A ,8H''SUMINF,
     9  8H'' DEFIN,8HITION.'),8H        ,8H        ,8H        ,
C
     .  8H('   INC,8HOMPATIBL,8HE COMPAR,8HISON BET,8HWEEN RIG,
     9  8HHT AND L,8HEFT HAND,8H SIDE OF,8H RELATIO,8HN.')    ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  91/
     .  8H('   ONL,8HY ''MAXQ,8H'' FUNCT,8HION CAN ,8HHAVE MUL,
     9  8HTIPLE RE,8HFERENCE ,8HIN IF CL,8HAUSE.') ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   MUL,8HTIPLE DE,8HFINITONS,8H OF THIS,8H UTILITY,
     9  8H ARE NOT,8H ALLOWED,8H.')     ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   INC,8HORRECT U,8HNITS, __,8H__, FOR ,8HTHIS TIM,
     9  8HE-SERIES,8H.',T21,A,8H4)      ,8H        ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   BAD,8H BLOCK S,8HTRUCTURE,8H FOR ___,8H________,
     9  8H_.',T28,,8H3A4)    ,8H        ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   ONL,8HY NON-NE,8HGATIVE V,8HALUES AL,8HLOWED.'),
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  96/
     .  8H('   FIE,8HLD LONGE,8HR THAN A,8HLLOWED.',8H)       ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   KEY,8HWORD CON,8HFLICTS W,8HITH ____,8H________,
     9  8H.',T27,3,8HA4)     ,8H        ,8H        ,8H        ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   INS,8HUFFICIEN,8HT INPUT ,8HTO CONST,8HRUCT ELE,
     9  8HVATION V,8HS MAXIMU,8HM DISCHA,8HRGE CURV,8HE.'/'   ,
     8  8HPLEASE C,8HONSULT U,8HSER''S M,8HANUAL.'),8H        ,
C
     .  8H('   KEY,8HWORD ALL,8HOWED ONL,8HY WHEN _,8H________,
     9  8H___ PRES,8HENTED.',,8HT30,3A4),8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   THE,8H 1ST KEY,8HWORD MUS,8HT BE ___,8H_____ IF,
     .  8H NEEDED,,8H THERE A,8HRE ____ ,8HCARDS BE,8HFORE IT.,
     .  8H',T28,2A,8H4,T58,I4,8H)       ,8H        ,8H        /
C
C
      DATA MG 101/
     1  8H('   DUE,8H TO THIS,8H ERROR, ,8HTHE FOLL,8HOWING ME,
     .  8HSSAGES A,8HRE NOT R,8HELIABLE.,8H')      ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   AT ,8HMOST ONE,8H _______,8H_____ AL,8HLOWED BE,
     .  8HFORE PAR,8HMS, TIME,8H-SERIES ,8HAND CARR,8HYOVER.',,
     2  8HT16,3A4),8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   THI,8HS SHOULD,8H COME BE,8HFORE ___,8H________,
     .  8H_ IN CUR,8HRENT SIT,8HUATION.',8H,T28,3A4,8H)       ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   TIM,8HE INTERV,8HAL MUST ,8HBE 24 HR,8HS FOR TI,
     .  8HME-SERIE,8HS.')    ,8H        ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   VAL,8HUES ARE ,8HNOT IN D,8HESCENDIN,8HG ORDER.,
     .  8H')      ,8H        ,8H        ,8H        ,8H        ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG 106/
     1  8H('   CON,8HTROL SCH,8HEDULE SP,8HECIFICAT,8HION IS L,
     .  8HIMITED T,8HO VALUES,8H OF 1 TH,8HROUGH 4.,8H')      ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   VAL,8HUE MUST ,8HBE BETWE,8HEN 1 AND,8H 7, INCL,
     .  8HUSIVE.'),8H        ,8H        ,8H        ,8H        ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   EXA,8HCTLY FIV,8HE VALUES,8H MUST BE,8H ENTERED,
     .  8H FOR THI,8HS LIST.',8H)       ,8H        ,8H        ,
     8  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   VAL,8HUE MUST ,8HBE BETWE,8HEN 1 AND,8H 5, INCL,
     .  8HUSIVE.'),8H        ,8H        ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   VAL,8HUE MUST ,8HBE BETWE,8HEN 1 AND,8H 10, INC,
     1  8HLUSIVE.',8H)       ,8H        ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG  111/
     1  8H('   COL,8HUMN OF I,8HNTEGERS ,8HEXPECTED,8H, BUT NO,
     1  8HT FOUND.,8H'/'   CH,8HECK REQU,8HIREMENTS,8H FOR THE,
     1  8H DECISIO,8HN TABLE ,8HSELECTED,8H (DYPTE),8H.')     ,
C
     1  8H('   PAR,8HAMETER V,8HALUE MUS,8HT BE LES,8HS THAN O,
     1  8HR EQUAL ,8HTO THE O,8HPERATION,8H TIME IN,8HTERVAL.',
     2  8H)       ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   VAL,8HUE MUST ,8HBE BETWE,8HEN 0 AND,8H 24, INC,
     1  8HLUSIVE.',8H)       ,8H        ,8H        ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   ''H,8HEADVSQ'',8H AND ''E,8HLVSMAXQ',8H' ARE MU,
     1  8HTUALLY E,8HXCLUSIVE,8H CURVES.,8H')      ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   PAR,8HAMETERS ,8HONLY ALL,8HOWED IF ,8HINSTANTA,
     1  8HNEOUS OB,8HSERVED D,8HISCHARGE,8HS ARE US,8HED IN TH,
     5  8HE ADJUST,8HMENT.') ,8H        ,8H        ,8H        /
C
C
      DATA MG 116/
     1  8H('    CA,8HRRYOVER ,8HONLY ALL,8HOWED IF ,8HINSTANTA,
     1  8HNEOUS OB,8HSERVED D,8HISCHARGE,8HS ARE US,8HED IN TH,
     6  8HE ADJUST,8HMENT.') ,8H        ,8H        ,8H        ,
C
     1  8H('   ERR,8HORS OCCU,8HRRED IN ,8HINPUT TO,8H LAG/K O,
     1  8HPERATION,8H.'/'   C,8HHECK PRE,8HCEDING O,8HUTPUT FO,
     7  8HR CAUSE ,8HOF ERROR,8H.')     ,8H        ,8H        ,
C
     1  8H('   RAT,8HING CURV,8HE       ,8H        ,8HNOT DEFI,
     1  8HNED.',T1,8H7,3A4)  ,8H        ,8H        ,8H        ,
     8  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   NUM,8HBR OF VA,8HLUES IN ,8HLIST EXC,8HEEDS ARR,
     1  8HAY DIMEN,8HSION    ,8H')      ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('   VAL,8HUS NOT I,8HN RANGE ,8HOF WHATE,8HEVER PAR,
     2  8HAMETR IT,8HS SUPOSE,8HED TO FA,8HLL WITHI,8HN       ,
     .  8H')      ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG 121/
     1  8H('    NU,8HMBER OF ,8HSPILLWAY,8HRATING P,8HOINTS MU,
     2  8HST BE LE,8HSS THAN ,8H2 TIMES ,8HTHE NUMB,8HER OF ST,
     1  8HORAGE EL,8HEVATION ,8HPOINTS  ,8H')      ,8H        ,
C
     1  8H('      ,8HNUMBER  ,8HOF DATA ,8HENTERED ,8HEXCEEDED,
     2  8H MAX. AL,8HLLOWED' ,8H)       ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('      ,8HONLY ONE,8H RULE CU,8HRVE ALLO,8HWED AND ,
     2  8HMUST ALS,8HO BE USE,8HD AND DE,8HFINED IN,8H GAGE 1 ,
     3  8H')      ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('    '),8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('    '),8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG 126/
     1  8H('    '),8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
     6  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('    '),8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
     7  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('    '),8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
     8  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('    '),8H        ,8H        ,8H        ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
     9  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     1  8H('    '),8H        ,8H        ,8H        ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG 500/
     5  8H('   MAX,8H NUMBER ,8HOF EMBED,8HDED IF'S,8H EXCEEDE,
     .  8HD - CURR,8HENTLY 10,8H.')     ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     5  8H('   MAX,8H WORDS O,8HF WORK A,8HRRAY USE,8HD - LEFT,
     .  8HW PLUS I,8HUSEW.') ,8H        ,8H        ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     5  8H('   MAX,8H NUMBER ,8HOF COMPA,8HRISION V,8HARIABLES,
     .  8H  EXCEED,8HED - CUR,8HRENTLY 5,8H0.')    ,8H        ,
     2  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     5  8H('   MAX,8H NUMBER ,8HOF ''IF',8H' CLAUSE,8HS EXCEED,
     .  8HED - CUR,8HRENTLY 5,8H0.')    ,8H        ,8H        ,
     3  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     5  8H('   EXC,8HEEDED AV,8HAILABLE ,8HSPACE FO,8HR STORIN,
     .  8HG ''IF'',8H CLAUSE.,8H')      ,8H        ,8H        ,
     4  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     5  8H('   MAX,8H NUMBER ,8HOF SCHEM,8HE/UTILIT,8HY''S DEF,
     .  8HINED.') ,8H        ,8H        ,8H        ,8H        ,
     5  8H        ,8H        ,8H        ,8H        ,8H        /
C
C
      DATA MG1000/
     .  8H('   MOR,8HE THAN 5,8H0 ERRORS,8H OCCURRE,8HD.')    ,
     .  8H        ,8H        ,8H        ,8H        ,8H        ,
     .  8H        ,8H        ,8H        ,8H        ,8H        ,
C
     .  8H('   MOR,8HE THAN 5,8H0 WARNIN,8HGS OCCUR,8HRED.')  ,
     .  8H        ,8H        ,8H        ,8H        ,8H        ,
     1  8H        ,8H        ,8H        ,8H        ,8H        /
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (NUMERR.EQ.0) GO TO 120
C
      DO 10 I = 1,3
         NMSG(I) = IEND(I) - ISTRT(I) + 1
10       CONTINUE
C
      DO 100 I=1,NUMERR
         N = IERTYP(I)
         WRITE (IPR,20) N,IERLN(I),IERFLD(I),IERPOS(I)
20       FORMAT ('0**ERROR** NUMBER ',I4,
     *           ' OCCURRED ON LINE ',I3,
     *           ', FIELD NUMBER ',I3,
     *           ', CHARACTER NUMBER',I3,
     *           ':')
C     LOCATION FOR N = 1,2,3,...IEND(1)
         IF (N.GT.IEND(1)) GO TO 30
            LOC = N
            GO TO 70
C     LOC IS THE LOCATION IN EMSG FOR N = ISTRT(2),...,IEND(2)
30       M = ISTRT(2)
         IF (.NOT. (N.GE.M.AND.N.LE.IEND(2)) ) GO TO 40
            LOC = N - (N/M)*M + NMSG(1) + 1
            GO TO 70
C     FOR N = ISTRT(3),...,IEND(3)
40       M = ISTRT(3)
         IF (.NOT. (N.GE.M.AND.N.LE.IEND(3)) ) GO TO 50
            LOC = N - (N/M)*M + NMSG(1) + NMSG(2) + 1
            GO TO 70
50       WRITE (IPR,60) N
60    FORMAT ('0**WARNING** NO INFORMATION AVAILABLE FOR ERROR NUMBER ',
     *   I4,'.')
         CALL WARN
         GO TO 100
C     START PRINTING ERROR MSG IN VARIABLE FORMAT BASED ON IERNME
70       DO 80 K=1,15
            FMT(K) = EMSG(K,LOC)
80          CONTINUE
         IF (IERNME(1,I).EQ.IBLNK) GO TO 90
            NWORDS = 3
            IF (N.EQ.93) NWORDS = 1
            WRITE (IPR,FMT) (IERNME(K,I),K=1,NWORDS)
            CALL ERROR
            GO TO 100
90       WRITE (IPR,FMT)
         CALL ERROR
100      CONTINUE
C
      IF (IERR.GT.0) THEN
         WRITE (IPR,110) IERR,VALUE1,VALUE2,IPT
110   FORMAT ('0**ERROR** ERROR NUMBER ',I5,5X,
     * 'VALUE1=',F11.2,5X,'VALUE2=',F11.2,5X,'AT POINT=',I4)
         CALL ERROR
         ENDIF
C
120   RETURN
C
      END
