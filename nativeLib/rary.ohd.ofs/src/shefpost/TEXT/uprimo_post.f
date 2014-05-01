C MODULE UPRIMO_POST
C  =====================================================================
C  pgm: UPRIMO_POST .. Initialize I/O units
C
C  rqd: URTIMR,UPINIO,UPCHKD,UPRIMR,UPRIMW,UPCLOS
C  rqd: SET_OFS_LOCK,FREE_OFS_LOCK
C  =====================================================================
      SUBROUTINE UPRIMO_POST()

      EXTERNAL   URTIMR,UPINIO,UPCHKD,UPRIMR,UPRIMW,UPCLOS
      EXTERNAL   SET_OFS_LOCK,FREE_OFS_LOCK

      CHARACTER*37  INSTMT
      CHARACTER*38  OTSTMT
      CHARACTER*37  PUSTMT
      CHARACTER*34  U9STMT
      CHARACTER*34  SOSTMT
      INTEGER       KOND,ISTAT

      CHARACTER*5   LOCK_TYPE

      INCLUDE 'uio'
      INCLUDE 'upagex'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
      INCLUDE 'uunits'
      INCLUDE 'updaio'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sysbug'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'dfcommon/dfunts'

C   "UIOX" is the shared library version of uio
C   "ERRDAT" is in ...common/errdat
C   "UCMDBX" is for the shared library debug

      COMMON /UIOX/ LP2,ICD2,LPD2,LPE2,ICDPU2,LSYS2
      COMMON /ERRDAT/ IOERR_,NWARN_,NERRS_
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/uprimo_post.f,v $
     . $',                                                             '
     .$Id: uprimo_post.f,v 1.7 2004/06/23 13:31:14 hank Exp $
     . $' /
C    ===================================================================
C

      DATA  INSTMT / ' *** Enter input filename, TTY or Q:'  /
      DATA  OTSTMT / ' *** Enter output filename, TTY or Q:' /
      DATA  PUSTMT / ' *** Enter punch filename, TTY or Q:'  /
      DATA  U9STMT / ' *** Enter unit 9 file, TTY or Q:'     /
      DATA  SOSTMT / ' *** Enter SHEFOUT file, TTY or Q:'     /


C  Set cpu timer, sys level i/o unit numbers
      CALL URTIMR(LAPSE,ITMBEG)
      CALL UPINIO()

C  Try to establish the OFS file lock
CHDH Change made on 2004-06-22 by Hank herr.  Use the new lock mech.
C      LOCK_TYPE='write'
C      CALL SET_OFS_LOCK(LOCK_TYPE,KOND)
      CALL HLOCKFILES('SHEFPOST',KOND)
      IF (KOND .GT. 0) STOP 16

C  Set user id
      PUSRID = ' '

C  Initialize common for function "ifbug".
      NDEBGS = 0
      IALL   = 0

      KDTYPE = 3
      LSYS2  = KDTYPE
      ITRACE = 0
      IOPCLG(2) = 1

C  Set the two globals needed by every program
      CALL UPCHKD('SYST OPER',ISTAT)

      ICD    = 0
      LP     = 0
      LPE    = 0
      KFSOUT = 0
      IF (ISTAT  .EQ. 0) CALL UPRIMR(INSTMT,'F',59,ICD)
CCC      IF (ICD    .GT. 0) CALL UPRIMW(OTSTMT,'F',6,LP)
      IF (ICD    .GT. 0) CALL UPRIMW(OTSTMT,'F',98,LP)
      IF (LP     .GT. 0) CALL UPRIMW(U9STMT,'F',9,LPE)
CCC      LPE=LP
      IF (LPE    .GT. 0) CALL UPRIMR(SOSTMT,'U',22,KFSOUT)
      IF (KFSOUT .LE. 0) THEN
         IF (LPE.GT.0 .AND. LPE.NE.1) CALL UPCLOS (LPE,' ',ISTAT)
         IF (LP .GT.0 .AND. LP .NE.1) CALL UPCLOS (LP, ' ',ISTAT)
         IF (ICD.GT.0 .AND. ICD.NE.1) CALL UPCLOS (ICD,' ',ISTAT)

CHDH  Change made on 2004-06-22 by Hank Herr.  Use new lock mech.
C         CALL FREE_OFS_LOCK (KOND)
         CALL HUNLOCKFILES('SHEFPOST',KOND)
         STOP 32
         ELSE
            LPD    = LP
            IOGDB  = LP
            IODBUG = LP
            IOERR  = LPE
            IN     = ICD
            IPR    = LP
            IPU    = LP
C        Set second common area for i/o for routines using 
C        UIOX instead of UIO
            LP2    = LP
            ICD2   = ICD
            LPD2   = LPD
            LPE2   = LPE
            ICDPU2 = ICDPUN
C        Set shared library debug parameters
            ICMPRU = LP
            ICMTRC = 0
            ICMDBG = 0
          ENDIF

      RETURN

      END
