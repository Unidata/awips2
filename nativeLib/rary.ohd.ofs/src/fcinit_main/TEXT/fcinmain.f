
C MODULE FCINMAIN
C-----------------------------------------------------------------------
C
      SUBROUTINE FCINMAIN_MAIN
cc      PROGRAM MAIN
C EJM modified name so that program could link with HP C++ compiler
C
C  MAIN ROUTINE FOR PROGRAM FCINIT
C
C      EXTERNAL MAIN
C
      CHARACTER*8 DDNAME
C
      INCLUDE 'upagex'
      INCLUDE 'uoptnx'
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/toterz'
      INCLUDE 'common/killcd'
      INCLUDE 'common/resetc'
      INCLUDE 'common/fc'
      INCLUDE 'common/oldc'
      INCLUDE 'common/fp'
      INCLUDE 'common/oldp'
      INCLUDE 'common/ft'
      INCLUDE 'common/oldt'
      INCLUDE 'common/fts'
      INCLUDE 'common/oldts'
      INCLUDE 'ucommon/uppint'
      INCLUDE 'clbcommon/crwctl'
      INCLUDE 'scommon/sugnlx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_main/RCS/fcinmain.f,v $
     . $',                                                             '
     .$Id: fcinmain.f,v 1.8 2004/07/21 18:02:43 dsa Exp $
     . $' /
C    ===================================================================
C
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
      INCLUDE 'cluprimo'
C
C      CALL MAIN()
C
C  BE SURE UTIL_TO_FCINIT ROUTINE LOADED BEFORE UTIL ROUTINES
      CALL UTIL_TO_FCINIT
C
C  SET STOP CODE TO NOT STOP PROGRAM IN CALB R/W IF ERROR OCCURS
      ISTOP=0
C
C  SET IPPFLG TO NOT PRINT ERROR MESSAGES FROM PRD R/W ROUTINES
      IPPFLG=1
C
C  SET USER NAME
      CALL HGTUSR (PUSRID,IERR)
C
C  SET OVERPRINT OPTION
      NOVPRT=-1
C
C  SET OPTIONS FOR UTILITY ROUTINES
      IPAGE=0
      IERPRT=0
      ICDPRT=0
      ITMPRT=1
      CALL USETOP (IPAGE,IERPRT,ICDPRT,ITMPRT)
      CALL USETO1 ('NOLINCNT',IERR)
      CALL USETO1 ('NOPAGNUM',IERR)
C
C  PRINT PAGE HEADER
      CALL UPAGE (IPR)
C
C  INITIALIZE VARIABLE IUGFIL TO ZERO
      IUGFIL=0
C
C  GET DATASET FROM WHICH PROGRAM IS BEING EXECUTED
      DDNAME='STEPLIB'
      NUNIT=0
      IPRERR=-1
      CALL UPRDSN (DDNAME,NUNIT,'NONE',IPRERR,IPR,IERR)
C
C  PRINT DATASET ATTRIBUTES
      NUNIT=IN
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,IPR,IERR)
      NUNIT=IPR
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,IPR,IERR)
      NUNIT=IPU
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,IPR,IERR)
C
C  DECODE AND PROCESS COMMANDS
      CALL FCINIT (MC,C,MOLDC,OLDC,MRSTC,RESETC,MP,P,MOLDP,OLDP,
     *   MT,T,MOLDT,OLDT,MTS,TS,MOLDTS,OLDTS)
C
C  SET KILL CODE TO ACTUALLY STOP PROGRAM IN ROUTINE STOP
10    KLSTOP=1
C
C  SET NUMBER OF ERRORS AND WARNING - SET TO ZERO IN ROUTINE FRESTR
      NERRS=NERRST
      NWARN=NWARNT
C
      CALL STOP
C
      INCLUDE 'cluprimc'
C
      STOP
C
      END
