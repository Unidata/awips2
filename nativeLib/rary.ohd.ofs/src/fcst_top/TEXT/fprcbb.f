C MEMBER FPRCBB
C  (from old member FCDMPCBS)
C
      SUBROUTINE FPRCBB ()
C
      INCLUDE 'common/errdat'
      INCLUDE 'common/fcary'
      INCLUDE 'common/fccvp'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcfutp'
      INCLUDE 'common/fconit'
      INCLUDE 'common/fcoppt'
      INCLUDE 'common/fcpuck'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/flarys'
      INCLUDE 'common/fnopr'
      INCLUDE 'common/fpltab'
      INCLUDE 'common/fprog'
      INCLUDE 'common/fsnw'
      INCLUDE 'common/ionum'
      INCLUDE 'common/killcd'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/toterz'
      INCLUDE 'common/where'
      COMMON /FATLGK/ IATL,C1,C2
C
      DIMENSION NAMES(3,25)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fprcbb.f,v $
     . $',                                                             '
     .$Id: fprcbb.f,v 1.1 1995/09/17 19:08:19 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NAMES/4HERRD,4HAT  ,3,4HFCAR,4HY   ,42,4HFCCV,4HP   ,20,
     1 4HFCFG,4HS   ,21,4HFCFU,4HTP  ,1,4HFCON,4HIT  ,1,
     2 4HFCOP,4HPT  ,3,4HFCPU,4HCK  ,1,4HFCSE,4HGP  ,6,
     3 4HFCTI,4HME  ,20,4HFCUN,4HIT  ,20,4HFDBU,4HG   ,24,
     4 4HFENG,4HMT  ,1,4HFLAR,4HYS  ,5,4HFNOP,4HR   ,1,
     5 4HFPLT,4HAB  ,2,4HFPRO,3HG   ,10,4HFSNW,4H    ,1,
     6 4HIONU,4HM   ,3,4HKILL,4HCD  ,3,4HSYSB,4HUG  ,22,
     7 4HTOTE,4HRZ  ,2,4HWHER,4HE   ,5,4HFATL,4HGK  ,3,
     8 4HFCTI,4HM2  ,11/
C
C     ***** COMMON BLOCK ERRDAT
      CALL FPRTCB(NAMES(1,1),NAMES(3,1),IOERR,IOERR)
C     ***** COMMON BLOCK FCARY
      CALL FPRTCB(NAMES(1,2),NAMES(3,2),IFILLC,IFILLC)
C     ***** COMMON BLOCK FCCVP
      CALL FPRTCB(NAMES(1,3),NAMES(3,3),IPROT,IPROT)
C     ***** COMMON BLOCK FCFGS
      CALL FPRTCB(NAMES(1,4),NAMES(3,4),FGID,FGID)
C     ***** COMMON BLOCK FCFUTP
      CALL FPRTCB(NAMES(1,5),NAMES(3,5),IFPR,IFPR)
C     ***** COMMON BLOCK FCONIT
      CALL FPRTCB(NAMES(1,6),NAMES(3,6),IVALUE,IVALUE)
C     ***** COMMON BLOCK FCOPPT
      CALL FPRTCB(NAMES(1,7),NAMES(3,7),ITPTR,ITPTR)
C     ***** COMMON BLOCK FCPUCK
      CALL FPRTCB(NAMES(1,8),NAMES(3,8),ICPUT,ICPUT)
C     ***** COMMON BLOCK FCSEGP
      CALL FPRTCB(NAMES(1,9),NAMES(3,9),NS,NS)
C     ***** COMMON BLOCK FCTIME
      CALL FPRTCB(NAMES(1,10),NAMES(3,10),IDARUN,IDARUN)
C     ***** COMMON BLOCK FCTIM2
      CALL FPRTCB(NAMES(1,25),NAMES(3,25),INPTZC,INPTZC)
C     ***** COMMON BLOCK FCUNIT
      CALL FPRTCB(NAMES(1,11),NAMES(3,11),KFCGD,KFCGD)
C     ***** COMMON BLOCK FDBUG
      CALL FPRTCB(NAMES(1,12),NAMES(3,12),IODBUG,IODBUG)
C     ***** COMMON BLOCK FENGMT
      CALL FPRTCB(NAMES(1,13),NAMES(3,13),METRIC,METRIC)
C     ***** COMMON BLOCK FLARYS
      CALL FPRTCB(NAMES(1,14),NAMES(3,14),LTS,LTS)
C     ***** COMMON BLOCK FNOPR
      CALL FPRTCB(NAMES(1,15),NAMES(3,15),NOPROT,NOPROT)
C     ***** COMMON BLOCK FPLTAB
      CALL FPRTCB(NAMES(1,16),NAMES(3,16),IPLHY,IPLHY)
C     ***** COMMON BLOCK FPROG
      CALL FPRTCB(NAMES(1,17),NAMES(3,17),MAINUM,MAINUM)
C     ***** COMMON BLOCK FSNW
      CALL FPRTCB(NAMES(1,18),NAMES(3,18),NOSNOW,NOSNOW)
C     ***** COMMON BLOCK IONUM
      CALL FPRTCB(NAMES(1,19),NAMES(3,19),IN,IN)
C     ***** COMMON BLOCK KILLCD
      CALL FPRTCB(NAMES(1,20),NAMES(3,20),KLCODE,KLCODE)
C     ***** COMMON BLOCK SYSBUG
      CALL FPRTCB(NAMES(1,21),NAMES(3,21),NDEBGS,NDEBGS)
C     ***** COMMON BLOCK TOTERZ
      CALL FPRTCB(NAMES(1,22),NAMES(3,22),NWARNT,NWARNT)
C     ***** COMMON BLOCK WHERE
      CALL FPRTCB(NAMES(1,23),NAMES(3,23),ISEG,ISEG)
C     ***** COMMON BLOCK FATLGK
      CALL FPRTCB(NAMES(1,24),NAMES(3,24),IATL,IATL)
C
      RETURN
      END
