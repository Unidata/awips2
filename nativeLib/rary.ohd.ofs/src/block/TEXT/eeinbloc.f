C MODULE EEINBLOC
C-----------------------------------------------------------------------
C
      BLOCK DATA EEIBLK
C
      COMMON/FILES/IFILES(99)
C     INCLUDE 'common/elimit'
C     INCLUDE 'common/ep'
C     INCLUDE 'common/ets'
C     INCLUDE 'common/esp'
cc AV multiple definition with fcblock
ccAV      INCLUDE 'common/fcary'
ccAV      INCLUDE 'common/fcio'
ccAV      INCLUDE 'common/fconit'
ccAV      INCLUDE 'common/fdbug'
ccAV      INCLUDE 'common/fengmt'
ccAV      INCLUDE 'common/errdat'
ccAV      INCLUDE 'common/fciobf'
ccAV      INCLUDE 'common/fcfutp'
ccAV      INCLUDE 'common/fcoppt'
ccAV      INCLUDE 'common/fcrunc'
ccAV      INCLUDE 'common/fcsegc'
ccAV      INCLUDE 'common/fcsegn'
ccAV      INCLUDE 'common/fcsgnn'
      INCLUDE 'common/frarsw'
cAV      INCLUDE 'common/ionum'
cAV      INCLUDE 'common/killcd'
cAV      INCLUDE 'common/flarys'
cAV      INCLUDE 'common/fpltab'
cAV      INCLUDE 'common/fnopr'
      INCLUDE 'clbcommon/crwctl'
cAV      INCLUDE 'common/fsglst'
cAV      INCLUDE 'common/fsnw'
cAV      INCLUDE 'common/sysbug'
cAV      INCLUDE 'common/toterz'
cAV      INCLUDE 'common/where'
cAV      INCLUDE 'common/oplist'
      INCLUDE 'common/edata'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSEEINBLOC      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/eeinbloc.f,v $
     . $',                                                             '
     .$Id: eeinbloc.f,v 1.5 2004/10/21 13:28:14 hank Exp $
     . $' /
C    ===================================================================
C
C
      DATA IFILES/99*0/
c     DATA MAXWIN,MAXVAR,MAXDSP/5,10,5/
c     DATA MPESP/1000/
c     DATA MTSESP/10000/
c     DATA MSPESP/1/
c     DATA IFILLC/0/,NCSTOR/0/
ccAV      DATA IDSEG/4H    ,4H    /,NCVALS/0/
c     DATA IVALUE/0/
c     DATA IODBUG/6/,ITRACE/0/,IDBALL/0/,NDEBUG/0/
c     DATA METRIC/1/
c     DATA IOERR/9/,NWARN/0/,NERRS/0/
c     DATA MZZBUF/100/
c     DATA IFPR/1/
c     DATA ITPTR/0/,IPPTR/0/,ICPTR/0/
c     DATA MSEGEX/15000/
ccAV      DATA IDC/4H    ,4H    /
ccAV      DATA IRSEG/0/,IDSEGN/4H    ,4H    /
ccAV      DATA JRSEG/0/,JDSEGN/4H    ,4H    /
      DATA NRARSW/5/
c     DATA IN/5/,IPR/6/,IPU/7/
c     DATA KLCODE,NKILLS,KLSTOP/0,0,1/
c     DATA LTS/0/,LP/0/,LC/0/,LT/0/,LD/0/
c     DATA IPLHY/0/,IPRHY/0/
c     DATA NOPROT/1/
      DATA ISTOP,IERROR,IPRINT,ITSH,NTSH,ISFN,IRTSH/0,0,1,2,0,0,0/
c     DATA MLIST/15000/,NLIST/0/
c     DATA NOSNOW/0/,IPRSNW/0/
c     DATA NDEBGS/0/,IALL/0/
c     DATA NWARNT/0/,NERRST/0/
ccAV      DATA ISEG/4HNONE,4H YET/,IOPNUM/0/,OPNAME/4HBLOC,4HKDAT/
c     DATA NOLIST/0/
      DATA MXTS,NDV/10,124/
C
      END
