C MODULE INSTRT
C-----------------------------------------------------------------------
C
C  ROUTINE TO INITIALIZE FORECAST COMPONENT COMMON BLOCKS.
C
C  A NUMBER OF COMMON BLOCKS HAVE INITIAL VALUES SET WITH BLOCK DATA
C  ROUTINES FCBLOCK, FCBLOCKA AND FCBLOCKO.
C
      SUBROUTINE INSTRT
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fsglst'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fctime'
      INCLUDE 'common/frcptr'
      INCLUDE 'common/fprog'
      COMMON /FGINFO/ MAXFG,LUFGL
      INCLUDE 'common/where'
C
      DIMENSION IBUF(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/instrt.f,v $
     . $',                                                             '
     .$Id: instrt.f,v 1.3 2000/12/18 21:32:41 dws Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=0
      CALL UMEMOV ('INSTRT  ',OPNAME,2)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER INSTRT'
C
C  FCCGD:
      CALL UREADT (KFCGD,1,NSLOTS,IERR)
C
C  FCCGD1: (GET FIRST CARRYOVER GROUP ON FILE)
      CALL UREADT (KFCGD,2,CGIDC,IERR)
C
C  FCFGS:
      CALL UREADT (KFFGST,1,FGID,IERR)
      NFGREC=IDUMYG
C
C  FGINFO:
C  GET MAXIMUM NUMBER OF FORECAST GROUPS FROM RECORD 2 WORD 20 OF
C  FILE FCFGSTAT
      CALL UREADT (KFFGST,2,FGID,IERR)
      MAXFG=IDUMYG
C  READ FILE FCFGSTAT AND COMPUTE THE LAST USED RECORD IN FILE FCFGLIST
      LUFGL=0
      IF (NFGREC.GT.0) THEN
         DO 10 I=1,NFGREC
            CALL UREADT (KFFGST,I,FGID,IERR)
            IF (IREC+NSEG-1.GT.LUFGL) LUFGL=IREC+NSEG-1
10          CONTINUE
         ENDIF
C
C  FCSEGP:
      CALL UREADT (KFSGPT,1,NS,IERR)
      CALL UREADT (KFSGPT,2,NRP,IERR)
C
C  FSGLST:
      NLIST=NS
      IF (MLIST.LT.NLIST)NLIST=MLIST
      DO 20 I=1,NLIST
         J=I+2
         CALL UREADT (KFSGPT,J,IDZ(1,I),IERR)
20       CONTINUE
C
C  FRCPTR:
      CALL UREADT (KFRCPT,1,IBUF,IERR)
      NRC=IBUF(1)
      MRCF=IBUF(3)
      MMR=NRC
      IF (NRC.GT.MRC) THEN
         WRITE (IPR,30) NRC,MRC
30    FORMAT ('0**WARNING** THE NUMBER OF RATING CURVES TO BE READ (',
     *   I4,') EXCEEDS THE MAXIMUM NUMBER TO BE PROCESSED (',I4,').')
          MMR=MRC
          ENDIF
      DO 40 J=1,MMR
         I=J+1
         CALL UREADT (KFRCPT,I,RCZZ(1,J),IERR)
40       CONTINUE
C
C  FPROG:
      MAINUM=1
      VERS=0.0
      CALL UMEMST ('    ',VDATE,2)
      CALL UMEMST ('    ',PNAME,5)
      NDD=31
C
C  FCTIME:
      CALL FSETNW
C
C  CLOSE ALL FILES
      CALL UCLOSL
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT INSTRT'
C
      RETURN
C
      END
