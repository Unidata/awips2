C MODULE UBLOCK
C-----------------------------------------------------------------------
C
C  INITIALIZE UTILITY COMMON BLOCKS
C
      BLOCK DATA UBLOCB
C
      INCLUDE 'uiox'
      INCLUDE 'upagex'
      INCLUDE 'uerorx'
      INCLUDE 'uoptnx'
      INCLUDE 'utimrx'
      INCLUDE 'ucmdbx'
      INCLUDE 'udaiox'
      INCLUDE 'updaio'
      INCLUDE 'ufiles'
      INCLUDE 'common/udtypc'

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSUBLOCK        / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/util/src/block/RCS/ublock.f,v $
     . $',                                                             '
     .$Id: ublock.f,v 1.5 2002/02/14 18:48:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C  COMMON BLOCK UIOX
      DATA LP/6/
      DATA ICD/5/
      DATA LPD/6/
      DATA LPE/6/
      DATA ICDPUN/7/
      DATA LSYS/3/
C
C  COMMON BLOCK UPAGEX
      DATA PUSRID/' '/
      DATA IPSPAG/99*1/
      DATA IPSLIN/99*1/
      DATA NPSPAG/99*0/
      DATA NPSMLN/99*80/
      DATA NPSNLN/99*0/
      DATA NPSNLT/99*0/
      DATA IPSNWP/99*0/
C
C  COMMON BLOCK UERORX
      DATA MAXCDE/0/
      DATA IERPRT/1/
      DATA ICDPRT/1/
      DATA MXUERR/256/
      DATA LUEROR/256*0/
      DATA MAXWRN/500/
      DATA NWRN/256*0/
      DATA NTWRN/256*0/
      DATA MPGWRN/256*1000/
      DATA NPGWRN/256*0/
      DATA LPGWRN/256000*0/
      DATA MAXERR/500/
      DATA NERR/256*0/
      DATA NTERR/256*0/
      DATA MPGERR/256*1000/
      DATA NPGERR/256*0/
      DATA LPGERR/256000*0/
C
C  COMMON BLOCK UOPTNX
      DATA NOVPRT/2/
C
C  COMMON BLOCK UTIMRX
      DATA ITMTOT/0/
      DATA ITMELA/0/
      DATA ISTDAY/0/
      DATA ISTHMS/0/
      DATA ITMFIL/0/
      DATA ITMPRT/1/
C
C  COMMON BLOCK UCMDBX
      DATA ICMPRU/6/,ICMTRC/0/,ICMDBG/0/
C
C  COMMON UDAIOX BLOCK
      DATA IDAERP/1/
C
C  COMMON UPDAIO BLOCK
      DATA MUPRECL/256/,UPRECL/256*-1/
C
C  COMMON BLOCK UFILES
      DATA MFILES/256/,IFILES/256*0/
C
      PARAMETER (MTYPEZ=200)
      DATA ICFULL/0/,MTYPE/MTYPEZ/,NTYPE/0/
      DATA TYPE/MTYPEZ*4h    /,DIMN/MTYPEZ*4h    /,UNIT/MTYPEZ*4h    /
      DATA MISS/MTYPEZ*0     /,NVAL/MTYPEZ*0     /,TIME/MTYPEZ*4h    /
      DATA NADD/MTYPEZ*0     /
      END
C
C-----------------------------------------------------------------------
C
C  INCLUDE OTHER BLOCK DATA ROUTINES
C
CCC AV B-Linux port
CCC      INCLUDE 'uduntb'
CCC AV E-Linux port
      INCLUDE 'udtypb'
CCC      INCLUDE 'upvrsb'
