C MEMBER PM36
C  (from old member FCEX36)
C
      SUBROUTINE PM36(PL,IPLPM,IDT)
C.......................................
C     OBTAINS PAPAMATERS FROM PL AND STORES IN FPM36.
C       ALSO PREPROCESSES SOME PARAMETERS
C.......................................
C     WRITTEN BY QINGPING ZHU -YRCC CHINA    JUNE   1988
C.......................................
      REAL K,IMP,KG,KSS,KSSD,KGD
      DIMENSION PL(1),CL(1)
C
C     COMMON BLOCKS
      COMMON/FPM36/K,IMP,WM,WUM,WLM,WDM,WMM,SM,SMM,B,EX,C,KSS,KG,
     1KSSD,KGD,CI,CG,CID,CGD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_xinan/RCS/pm36.f,v $
     . $',                                                             '
     .$Id: pm36.f,v 1.1 1995/09/17 18:58:31 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     MODEL PARAMETERS
      K=PL(IPLPM)
      IMP=PL(IPLPM+1)
      WUM=PL(IPLPM+2)
      WLM=PL(IPLPM+3)
      WDM=PL(IPLPM+4)
      SM=PL(IPLPM+5)
      B=PL(IPLPM+6)
      EX=PL(IPLPM+7)
      C=PL(IPLPM+8)
      KSS=PL(IPLPM+9)
      KG=PL(IPLPM+10)
      CI=PL(IPLPM+11)
      CG=PL(IPLPM+12)
C     COMPUTED PARAMETERS
      WM=WUM+WLM+WDM
      WMM=WM*(1.+B)/(1.-IMP)
      SMM=SM*(1+EX)
      CID=CI**(IDT/24.)
      CGD=CG**(IDT/24.)
      KSSD=(1.-(1.-(KSS+KG))**(IDT/24.))/(1+KG/KSS)
      KGD=KSSD*KG/KSS
      RETURN
      END
