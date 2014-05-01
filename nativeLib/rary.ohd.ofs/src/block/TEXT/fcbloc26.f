C MODULE FCBLOC26
C-----------------------------------------------------------------------
C
C  BLOCK DATA FOR OPERATION 26 - RES-SNGL
C
      BLOCK DATA O26BLK
C
      INCLUDE 'common/comn26'
      INCLUDE 'common/suid26'
      INCLUDE 'common/suky26'
      INCLUDE 'common/mult26'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSFCBLOC26      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/fcbloc26.f,v $
     . $',                                                             '
     .$Id: fcbloc26.f,v 1.3 2001/06/13 10:09:04 mgm Exp $
     . $' /
C    ===================================================================
C
C
C-----------------
C  CB /COMN26/
C-----------------
      DATA DIML,DIML3,DIMLT/4HL   ,4HL3  ,4HL3/T/
      DATA UMM,UM,UCMS,UCMSD  /4HMM  ,4HM   ,4HCMS ,4HCMSD/
C
C--------------
C  CB /SUID26/
C--------------
      DATA NSUID/27/
      DATA SUID/
     1   4HPASS,4HFLOW,4H    ,4HSETQ,4H    ,4H    ,4HSETH,4H    ,4H    ,
     2   4HRULE,4HCURV,4HE   ,4HFILL,4HSPIL,4HL   ,4HSPIL,4HLWAY,4H    ,
     3   4HPOOL,4HQ   ,4H    ,4HSTPO,4HOLQ ,4H    ,4HMINQ,4H    ,4H    ,
     4   4HMINH,4H    ,4H    ,4HINDS,4HRCHG,4HE   ,4HFLAS,4HHBDS,4H    ,
     5   4HPOWE,4HRGEN,4H    ,4HRULE,4HADJ ,4H    ,4HSUMI,4HNF  ,4H    ,
     6   4HRAIN,4HEVAP,4H    ,4HADJU,4HST  ,4H    ,4HBACK,4HFLOW,4H    ,
     7   4HCONV,4H24  ,4H    ,4HMAXQ,4H    ,4H    ,4HENTE,4HRISC,4H    ,
     8   4HSETM,4HIN  ,4H    ,4HSETM,4HAX  ,4H    ,4HGOFL,4HASH ,4H    ,
     9   4HWATE,4HRUSE,4H    ,4HSETD,4HQ   ,4H    ,4HSETD,4HH   ,4H    ,
     1   69*4H    /
      DATA LSUID/2,1,1,3,3,2,2,2,1,1,3,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,
     1           23*0/
      DATA NDSUID/3/
      DATA SUCODE/1010.01,1020.01,1030.01,1040.01,1050.01,1060.01,
     2            1070.01,1080.01,1090.01,1100.01,1110.01,1120.01,
     3            1130.01,1510.01,1520.01,1530.01,1540.01,1550.01,
     4            1560.01,1570.01,1580.01,1590.01,1600.01,1610.01,
     5            1620.01,1140.01,1150.01,23*0.01/
C
C--------------
C  CB /SUKY26/
C--------------
      DATA SUKYWD/4HPARM,4HS   ,4H    ,4HP   ,4H    ,4H    ,
     2            4HTIME,4H-SER,4HIES ,4HTS  ,4H    ,4H    ,
     3            4HCARR,4HYOVE,4HR   ,4HCO  ,4H    ,4H    ,
     4            4HENDP,4H    ,4H    ,4HENDP,4HARMS,4H    ,
     5            4HENDT,4H    ,4H    ,4HENDT,4HS   ,4H    ,
     6            4HENDC,4H    ,4H    ,4HENDC,4HO   ,4H    /
      DATA LSUKEY/2,1,3,1,3,1,1,2,1,2,1,2/
      DATA NSUKEY/12/
      DATA NDSUKY/3/
      DATA ENDSP/4HENDS,4HPEC /
C
C-------------------------------
C  CB /MULT26/
C-------------------------------
      DATA MULTNM/4HRULE,4HCURV,4HE   ,4HELVS,4HQ   ,4H    ,
     2            4HHEAD,4HVSQ ,4H    ,4HHTWV,4HSQ  ,4H    ,
     3            4HCRES,4HT   ,4H    ,4HSPIL,4HLWAY,4H    ,
     4            12*4H    /
C
      END
