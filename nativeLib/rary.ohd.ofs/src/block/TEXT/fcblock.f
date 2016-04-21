C MODULE FCBLOCK
C-----------------------------------------------------------------------
C
      BLOCK DATA FCBLK
C
C  BLOCK DATA FOR FORECAST COMPONENT COMMONS EXCEPT THOSE CONTAINING
C  THE P, C, D, T AND TS ARRAYS.
C
      INCLUDE 'common/fcary'
      INCLUDE 'common/fcio'
      INCLUDE 'common/fconit'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fciobf'
      INCLUDE 'common/fcfutp'
      INCLUDE 'common/fcoppt'
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fcsegc'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fcsgnn'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/eunit'
      INCLUDE 'common/fpwarn'
      INCLUDE 'common/ionum'
      INCLUDE 'common/killcd'
      INCLUDE 'common/flarys'
      INCLUDE 'common/fpltab'
      INCLUDE 'common/fnopr'
      INCLUDE 'common/fsglst'
      INCLUDE 'common/fsnw'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/toterz'
      INCLUDE 'common/where'
      INCLUDE 'common/oplist'
      INCLUDE 'common/modscb'
      INCLUDE 'common/fcfgid'
      INCLUDE 'common/ffgctl'
      INCLUDE 'common/frcptr'
      INCLUDE 'common/mod138'
      INCLUDE 'common/crsort'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSFCBLOCK       / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/fcblock.f,v $
     . $',                                                             '
     .$Id: fcblock.f,v 1.8 2005/01/11 20:12:48 xfan Exp $
     . $' /
C    ===================================================================
C
C
cfan    multiple definition in eeinbloc.f 07/11

      DATA IFILLC/0/,NCSTOR/0/
      DATA IDSEG/4H    ,4H    /,NCVALS/0/
      DATA IVALUE/0/
      DATA IODBUG/6/,ITRACE/0/,IDBALL/0/,NDEBUG/0/
      DATA METRIC/1/
      DATA IOERR/9/,NWARN/0/,NERRS/0/
      DATA MZZBUF/100/
      DATA IFPR/1/
      DATA ITPTR/0/,IPPTR/0/,ICPTR/0/
      DATA MSEGEX/15000/
      DATA IDC/4H    ,4H    /
      DATA IRSEG/0/,IDSEGN/4H    ,4H    /
      DATA JRSEG/0/,JDSEGN/4H    ,4H    /
      DATA KFCGD/60/,KFCRY/61/,KFFGST/62/,KFFGL/63/,KFDFLT/0/
      DATA KFSGPT/65/,KFSGST/66/,KFPARM/67/,KFRTCV/68/,KFRCPT/69/
      DATA KFCTMP/70,71,72,73,74,75,76,77,78,79/
      DATA KEPARM,KESCRA,KEPERM,KESTBL/96,90,0,0,0,0,0,97/
      DATA MODWRN/1/,IRWWRN/1/,IDTWRN/1/
      DATA IN/5/,IPR/6/,IPU/7/
      DATA KLCODE,NKILLS,KLSTOP/0,0,0/
      DATA LTS/0/,LP/0/,LC/0/,LT/0/,LD/0/
      DATA IPLHY/0/,IPRHY/0/
      DATA NOPROT/0/
      DATA MLIST/15000/,NLIST/0/
      DATA NOSNOW/0/,IPRSNW/0/
      DATA NDEBGS/0/,IALL/0/
      DATA NWARNT/0/,NERRST/0/
      DATA ISEG/4HNONE,4H YET/,IOPNUM/0/,OPNAME/4HBLOC,4HKDAT/
      DATA NOLIST/0/
CFAN
CFAN  DR19061: IFP not able to read more than 200 lines Mods
CFAN
CFAN  DATA MAXCRD/200/,NCARDS/0/
      DATA MAXCRD/1000/,NCARDS/0/
      DATA FCFGID/200*4H    /,IHASRD/0/
      DATA IFFG/0/
      DATA NRC/0/,MRC/2000/,MRCF/0/
      DATA MXDT38/31/
C
      DATA NUSEAR/10*0/
      DATA IDEFIN/30*0/
      DATA MSAVE/-1/,MC/-1/
C
      END
