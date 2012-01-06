C MODULE SBLOCK
C-----------------------------------------------------------------------
C
      BLOCK DATA SBLK
C
C  ROUTINE TO INITIALIZE COMMON BLOCKS USED BY PROGRAM PPINIT.
C
C      INCLUDE 'dscommon/dsunts'
      INCLUDE 'scommon/sarryx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/swrk2x'
      INCLUDE 'scommon/sgboxx'
      INCLUDE 'scommon/stypax'
      INCLUDE 'scommon/stypsx'
      INCLUDE 'scommon/suprtx'
      INCLUDE 'scommon/sutmrx'
      INCLUDE 'scommon/sucmdx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/supagx'
      INCLUDE 'scommon/suddsx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/sntwfx'
      INCLUDE 'scommon/sntwkx'
      INCLUDE 'scommon/sordrx'
      INCLUDE 'scommon/sugnlx'
      INCLUDE 'scommon/surrsx'
      INCLUDE 'scommon/sudtrx'
      INCLUDE 'scommon/sdumyx'
      INCLUDE 'scommon/surunx'
      INCLUDE 'ucommon/uppint'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSSBLOCK        / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/sblock.f,v $
     . $',                                                             '
     .$Id: sblock.f,v 1.13 2005/03/18 21:05:10 leecr Exp $
     . $' /
C    ===================================================================
C
C
C  COMMON BLOCK  /DSUNTS/
c      DATA KDSRCF/47/
C
C  COMMON BLOCK  /SARRYX/
      DATA LARRAY/75000/,ARRAY/75000*0/
C
C  COMMON BLOCK  /SWORKX/
      DATA LSWORK/225000/,SWORK/225000*0/
C
C  COMMON BLOCK  /SWRK2X/
C      DATA LSWRK2/65000/,SWRK2/65000*0/
      DATA LSWRK2/100000/,SWRK2/100000*0/

C
C  COMMON BLOCK  /SGBOXX/
      DATA IBXFIL/0/
      DATA IGBOX/99*0/
      DATA GBOXLT/99*0./
      DATA GBOXLN/99*0./
      DATA IGBOXM/99*0/
C
C  COMMON BLOCK  /STYPAX/
      DATA MATYPE/8/
      DATA ATYPE/'MAP ','FMAP','MAT ','MAPE',
     *           'MARO','MAPX','    ','    '/
C
C  COMMON BLOCK  /STYPSX/
      DATA MSTYPE/8/
      DATA STYPE/'STAN','PCPN','TEMP','PE  ',
     *           'RRS ','    ','    ','    '/
C
C  COMMON BLOCK  /SUPRTX/
      DATA MSUPRT/10/
      DATA NSUPRT/10/
      DATA SUPRT/
     *     'DELPPD  ',   ! DELETE STATION FROM PREPROCESSOR DATA BASE
     *     'DELPPP  ',   ! DELETE STATION FROM PARAMETRIC DATA BASE
     *     'DELPRD  ',   ! DELETE STATION FROM PROCSSED DATA BASE
     *     'DELSASM ',   ! DELETE STATION FROM SASM CONTROL FILE
     *     'DELGOES ',   ! DELETE STATION FROM GOES CONTROL FILE
     *     'DELSPCPN',   ! DELETE STATION PCPN PARAMETERS
     *     'DELSTEMP',   ! DELETE STATION TEMP PARAMETERS
     *     'DELSPE  ',   ! DELETE STATION PE   PARAMETERS
     *     'DELSRRS ',   ! DELETE STATION RRS  PARAMETERS
     *     '        '/
C
C  COMMON BLOCK  /SUTMRX/
      DATA ITMAUT/0/
      DATA ITMELA/0/
      DATA ITMTOT/0/
      DATA ITMUNT/6/
      DATA ITMRUN/0/
C
C  COMMON BLOCK  /SUCMDX/
      DATA MPCMDS/32/
      DATA NPCMDS/30/
      DATA IPCMD/0/
      DATA PCMDS/'@DEBUG  ','@DEFINE ','@DELETE ','@DUMP   ',
     *           '@NETWORK','@ORDER  ','@SETOPT ','@STATUS ',
     *           '@CPUTIME','@STOP   ','@NEWPAGE','@DECODE ',
     *           '@LISTCMD','@MODE   ','@SETUNT ','@SETNSP ',
     *           '@DEF    ','@DEL    ','@NETW   ','@LISTINP',
     *           '@INCLUDE','@FILOPEN','@USRNAME','@ABORT  ',
     *           '@RUN    ','@MDRGRID','@CHANGE ','@FILINIT',
     *           '@END    ','@NEWUSER','        ','        '/
C
C  COMMON BLOCK  /SUERRX/
      DATA IOERR/6/
      DATA NWARN/0/
      DATA NERR/0/
      DATA NTWARN/0/
      DATA NTERR/0/
      DATA NPGWRN/0/
      DATA MPGWRN/500/
      DATA LPGWRN/500*0/
      DATA NPGERR/0/
      DATA MPGERR/500/
      DATA LPGERR/500*0/
      DATA KLSTOP/1/
      DATA KLCODE/0/
      DATA ICMCDE/0/
      DATA IRNCDE/0/
      DATA NRWARN/0/
      DATA NRERR/0/
C
C  COMMON BLOCK  /SUPAGX/
      DATA NPSPAG/0/
      DATA NPSMLN/80/
      DATA NPSNLN/0/
      DATA IPSNWP/0/
      DATA NPSNLT/0/
      DATA NPSNLR/0/
C
C  COMMON BLOCK  /SUDDSX/
      DATA IDBFIL/0/
      DATA IDBALC/2*0,0,6*0,-1/
      DATA IDBOPN/10*0/
      DATA IDBWRT/10*0/
      DATA IDBSAV/10*0/
C
C  COMMON BLOCK  /SUOPTX/
      DATA IOPNWP/1/
      DATA IOPPLT/1/
      DATA IOPUNT/0/
      DATA IOPTLE/2/
      DATA IOPDMP/0/
      DATA IOPCND/-1/
      DATA IOPRCK/0/
      DATA IOPOVP/1/
      DATA IOPCLG/0,1/
      DATA IOPTSO/0/
      DATA IOPMXE/500/
      DATA IOPMXW/500/
      DATA IOPSTP/0/
      DATA IOPOLG/0,8/
      DATA IOPMLV/2/
      DATA IOPNTW/1/
      DATA IOPORD/1/
C
C  COMMON BLOCK  /SNTWFX/
      DATA INFFIL/0/
      DATA IVNTWK/2/
      DATA NNWFLG/17/
C
C  COMMON BLOCK  /SNTWKX/
      DATA INWFIL/0/
      DATA MAXSNW/5000/
      DATA INAUTO/0/
      DATA INWTYP/0/
      DATA INWSRT/0/
C
C  COMMON BLOCK  /SORDRX/
      DATA MFGIDS/100/
      DATA MRMPID/500/
      DATA MRMPDP/100/
      DATA MFMPID/20000/
C
C  COMMON BLOCK  /SUGNLX/
      DATA IUGFIL/0/
C
C  COMMON BLOCK  /SUDTRX/
      DATA IURRSX/0/
      DATA MTYPCD/30/
      DATA NTYPCD/30/
C
C  COMMON BLOCK  /SUDTRX/
      DATA IURFIL/0/
      DATA MURTYP/30/
      DATA NURTYP/0/
C
C  COMMON BLOCK  /SDUMYX/
      DATA IDUMMY/10*0/
C
C  COMMON BLOCK  /SURUNX/
      DATA NDCCMD/0/
      DATA NDCCRD/0/
      DATA NRDADD/0/
      DATA NUMCMD/0/
C
C  COMMON BLOCK  /UPPINT/
      DATA IPPFLG/1/
C
      END
