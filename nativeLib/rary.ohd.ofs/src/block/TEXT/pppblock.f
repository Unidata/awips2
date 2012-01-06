C MEMBER PPPBLOCK
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/13/94.14:05:53 BY $WC20SV
C
C @PROCESS LVL(77)
C
       BLOCK DATA PPPBLK
C
C
C    INITIALIZE PREPROCESSOR PARAMETRIC DATA BASE COMMON BLOCKS
C
C
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/pppdta'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSPPPBLOCK      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/pppblock.f,v $
     . $',                                                             '
     .$Id: pppblock.f,v 1.1 1995/09/17 18:41:21 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      DATA KPPIDX/40/,KPPRM1/41/,KPPRM2/42/,KPPRM3/43/,KPPRM4/44/,
     *   KPPRM5/45/,KPPRM6/46/,KPPRM7/47/,KPPRM8/48/,KPPRM9/49/
C
      DATA MXPPPF/9/,NMPPPF/5/
C
      DATA LRECPP/16/
      DATA NPPHED/5/
C
      DATA IPDDUM/6*0/
C
      END
