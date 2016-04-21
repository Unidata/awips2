C MODULE EINMAIN
C-----------------------------------------------------------------------
C
      SUBROUTINE EINMAIN_MAIN
C
C   THIS IS MAIN ROUTINE FOR PROGRAM ESPINIT.
C
      CHARACTER*8 TYPERR,USERID
C
      common /CMESPINIT/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      INCLUDE 'udebug'
      INCLUDE 'upagex'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/fd'
      INCLUDE 'common/fts'
      INCLUDE 'common/ep'
      INCLUDE 'common/esp'
      INCLUDE 'common/ets'
      INCLUDE 'common/eunit'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
      INCLUDE 'common/errdat'
      INCLUDE 'clbcommon/crwctl'
      INCLUDE 'clbcommon/chfdim'
C
      EQUIVALENCE (USERID,PUSRID)
C
      DIMENSION SBNAME(2),OLDOPN(2),SETB(2),COM(18)
      DIMENSION TTS(2500),UDBG(2)
C
      LOGICAL LBUG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/espinit/RCS/einmain.f,v $
     . $',                                                             '
     .$Id: einmain.f,v 1.3 2004/07/21 18:12:02 dsa Exp $
     . $' /
C    ===================================================================
C
      DATA SBNAME,SETB/4HESPI,4HNIT ,4HSETB,4HUG  /
      DATA UDBG/4HUDEB,4HUG  /
C
C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
      INCLUDE 'cluprimo'
C
C  GET USER NAME
      CALL HGTUSR (USERID,IERR)
C
      CALL UPAGE (IPR)
C
C  SET OPTIONS FOR UTILITY ROUTINES
      IPAGE=0
      IERPRT=0
      ICDPRT=0
      ITMPRT=1
      CALL USETOP (IPAGE,IERPRT,ICDPRT,ITMPRT)
C
C  GET NAME OF DATA SET FROM WHICH PROGRAM IS BEING EXECUTED
      NUNIT=0
      IPRERR=1
      CALL UPRDSN ('STEPLIB ',NUNIT,'NONE',IPRERR,IPR,IERR)
C
C  CHECK REGION AND CPU TIME
      ICKRGN=1
      MINRGN=MPGMRG
      ICKCPU=0
      MINCPU=0
      IPRERR=-1
      IPUNIT=IPR
      TYPERR='WARNING'
      LDEBUG=0
      INCLUDE 'clugtres'
C
C  PRINT DATASET ATTRIBUTES
      NUNIT=IN
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,IPR,IERR)
      CALL UPRMPT (IPRMPT,IERR)
      NUNIT=IPR
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,IPR,IERR)
      IF (IOERR.NE.IPR) THEN
         NUNIT=IOERR
         CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,IPR,IERR)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IUSESP=1
      ISFN=0
      MTTS=2000
C
C   SET MAXD EQUAL TO THE DIMENSION OF THE D ARRAY
      MAXD=MD
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
10    OPNAME(I)=SBNAME(I)
C
20    IF (IPRMPT.EQ.1) WRITE (IPR,*) '?'
C
      READ (IN,30,END=100) COM
30    FORMAT (18A4)
      IF (COM(1).NE.SETB(1).OR.COM(2).NE.SETB(2)) GO TO 40
C
C   IF COMMAND IS SETBUG CALL FSETBG
      CALL FSETBG
      GO TO 20
C
C   CHECK IF PROCESSED DATA DEBUG IS REQUESTED
40    IF (COM(1).NE.UDBG(1).OR.COM(2).NE.UDBG(2)) GO TO 60
C
      READ (IN,50,END=100) IPRTR,IPRDB
50    FORMAT (2I5)
      GO TO 20
C
60    IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER EINMAIN'
C
C   WRITE HEADING CARD FOR THIS RUN
      WRITE (IPR,80) COM
C
80    FORMAT (1H0,20X,4H*** ,18A4,4H ***)
      CALL INSTRT
      CALL ESTRT
      CALL ERDCMD (P,MP,T,MT,TS,MTS,PESP,MPESP,SPESP,MSPESP,
     1 TSESP,MTSESP,TTS,MTTS,MAXD)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,90)
90    FORMAT (1H0,48H ** EINIT REENTERED -- CLOSING ALL DAIO DATASETS)
C
      CALL UCLOSL
C
100   LUSTOP=0
      CALL USTOP (IPR,IUSTOP)
C
      STOP
C
      END
