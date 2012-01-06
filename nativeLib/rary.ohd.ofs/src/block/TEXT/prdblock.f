C MEMBER PRDBLOCK
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/14/94.09:22:38 BY $WC20SV
C
C @PROCESS LVL(77)
C
      BLOCK DATA PRDBLK
C
C
C    INITIALIZE PROCESSED DATA BASE COMMON BLOCKS
C
C
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/picdmg'
      INCLUDE 'prdcommon/picptr'
      INCLUDE 'prdcommon/ptsicb'
      INCLUDE 'prdcommon/preads'
      INCLUDE 'prdcommon/prdqpf'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSPRDBLOCK      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/prdblock.f,v $
     . $',                                                             '
     .$Id: prdblock.f,v 1.2 1996/12/09 20:48:57 dws Exp $
     . $' /
C    ===================================================================
C
C
C  COMMON /PUNITS/
      DATA KRFCPR/51/,KINDEX/52/
      DATA KMAPTS/53/,KGENTS/54/,KFMPTS/55/,KFUTTS/56/,KFCTTS/57/
      DATA MXPRDF/5/,NMPRDF/5/
C
C  COMMON /PDATAS/
      DATA IBLNK/4H    /,ZAPR/-999./,IDSI/4HDSI /
      DATA MONEY/4H$   /,IZAP/-99999/,IASTR/4H*   /,LRECLT/16/
      DATA LENHED/22/,LENHDC/18/,NUMFIL/5/
C
C  COMMON /PMAXDM/
      DATA MAXDTP/100/,NUMDTP/0/
C
C  COMMON /PDFTBL/
      DATA NWDDTP/18/
C
C  COMMON /PICDMG/
      DATA MTSICD/20/,NTSICD/0/,TSICDT/20*0/,TSICDN/20*0/
      DATA TSICD1/20*0/,TSICDL/20*0/
C
C  COMMON /PICPTR/
      DATA MTSICP/50/,NTSICP/0/,TSIDIC/100*0/,TSTYPE/50*0/
      DATA TSBUPT/50*0/,TSPTNX/50*0/,TSLENG/50*0/,ICBPRF/0/
C
C  COMMON /PTSICB/
      DATA MTSICB/2500/,NTSICB/0/,TSICBU/2500*0/
C
C  COMMON /PREADS/
      DATA NUMACC/50*0/,IPDUM/2*0/
C
C  COMMON /PRDQPF/
      DATA IRDQPF/0/
C
C
      END
