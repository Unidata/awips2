C MODULE PDBBLOCK
C-----------------------------------------------------------------------
C
       BLOCK DATA PDBBLK
C
C  ROUTINE TO INITIALIZE PREPROCESSOR DATA BASE COMMON BLOCKS.
C
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdnxrd'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdhshi'
      INCLUDE 'pdbcommon/pdenqx'
      INCLUDE 'pdbcommon/pdi2max'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSPDBBLOCK      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob83/rfc/ofs/src/block/RCS/pdbblock.f,v $
     . $',                                                             '
     .$Id: pdbblock.f,v 1.4 2002/05/15 17:53:45 dws Exp $
     . $' /
C    ===================================================================
C
C  COMMON /PDUNTS/
      DATA KPDSIF/80/,KPDRRS/81/,KPDDDF/82,83,84,85,86/
      DATA MXPPDF/5/,NMPPDF/4/
C
C  COMMON /PDDATA/
      DATA LHDRRS/17/
      DATA NDSTAT/18/
      DATA NRSTAT/12/
      DATA MISSNG/-9999/
      DATA MISSPP/-9/
      DATA IPDDUM/3*0/
C
C  COMMON /PDNXRD/
      DATA NEXTRD/2/
C
C  COMMON /PDHSHC/
C MAT PARAMETER (LXHSHC=5000,LXHSHC2=LXHSHC*2)
C MAT	per xfan, double current array size
      PARAMETER (LXHSHC=10000,LXHSHC2=LXHSHC*2)
      DATA IPDHSC/LXHSHC2*0/,MXHSHC/LXHSHC/
C
C  COMMON /PDHSHI/
C MAT PARAMETER (LXHSHI=5000,LXHSHI2=LXHSHI*2)
C MAT	per xfan, double current array size
      PARAMETER (LXHSHI=10000,LXHSHI2=LXHSHI*2)
      DATA IPDHSI/LXHSHI2*0/,MXHSHI/LXHSHI/
C
C  COMMON /PDENDQ/
      DATA IPDENQ/0/
C
C  COMMON /PDI2MAX/
      DATA I2MAX/32767/
      END
