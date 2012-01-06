C MODULE UPRIMO
C  =====================================================================
C  pgm: UPRIMO .. Initialize I/O units
C
C  rqd: URTIMR,UPINIO,UPRIMR,UPRIMW,UPRIMT,UPPFIX,UPFNCU,UDOPEN,UPCHKD
C  rqd: UPCLOS
C  rqd: COMMON: UPDAIO and other nwsrfs-ofs common
C  =====================================================================

      SUBROUTINE UPRIMO()

      EXTERNAL   URTIMR,UPINIO,UDOPEN
      EXTERNAL   UPRIMR,UPRIMW,UPRIMT,UPFNCU,UPPFIX,UPCHKD,UPCLOS

      CHARACTER*1   FM
      CHARACTER*39  INSTMT,OTSTMT,PUSTMT
      CHARACTER*32  FILNAM
      CHARACTER*128 NEWNAM
      CHARACTER*24  GLODIR
      INTEGER       FNUM,RNUM,BNUM,ISTAT,N,KOD,KOND

      INTEGER       JPRIM,JEBBB
      CHARACTER*4   CDEFN,CPRIM,CEBBB
      EQUIVALENCE ( CPRIM,JPRIM ),( CEBBB,JEBBB )

      CHARACTER*5   LOCK_TYPE

      INCLUDE 'updaio'
      INCLUDE 'uio'
C  "UIOX" is the shared library version of uio
      COMMON /UIOX/ LP2,ICD2,LPD2,LPE2,ICDPU2,LSYS2
      INCLUDE 'upvrsx'
      INCLUDE 'upagex'
      INCLUDE 'ucmdbx'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
      INCLUDE 'uunits'
      INCLUDE 'udsatx'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/ionum'
C  IOERR is also in SUERRX
CCC      INCLUDE 'common/errdat'
      COMMON /ERRDAT/ IOERR2,NWARN2,NERRS2
      INCLUDE 'common/fdbug'
      INCLUDE 'common/eunit'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'dscommon/dsunts'
      INCLUDE 'prdcommon/pmaxdm'
CCC      COMMON /ERRDAT/ IOERR2,NWARN2,NERRS2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/uprimo.f,v $
     . $',                                                             '
     .$Id: uprimo.f,v 1.7 2004/06/23 13:37:54 hank Exp $
     . $' /
C    ===================================================================
C

      DATA  CDEFN,CPRIM,CEBBB / 'DEFN','PRIM','E   ' /
      DATA  FM / 'F' /
      DATA  INSTMT / ' *** Enter input filename, TTY or Q:  ' /
      DATA  OTSTMT / ' *** Enter output filename, TTY or Q: ' /
      DATA  PUSTMT / ' *** Enter punch filename, TTY or Q:  ' /


C  Set cpu timer, sys level i/o unit numbers
        CALL URTIMR (LAPSE,ITMBEG)
        CALL UPINIO()

C  Try to establish the OFS file lock.
C  Uprimo will be used to create only initial locks.  If a program
C  later decides that it does not need all locks locked, then it can
C  close them and open only the locks it needs!  This will be the
C  case for FCST, which will only open an HCLUSER lock here and then
C  close it within hcompt.f before reopening it in either hcompt.f or
C  fun001.f.
CHDH  FCST portion below added by Hank Herr (2004-06-23).
CHDH  If the program is 'FCST', then we lock only the HCLUSER
CHDH  file.  Otherwise, we do a standard lock via set_ofs_lock.
        IF (PGMNAM.EQ.'FCST'   ) THEN
            CALL HLOCKFILES (PGMNAM,KOND)
            IF (KOND .GT. 0) THEN
               STOP 16
            ENDIF
CHDH  This performs a general lock for all non 'FCST' programs.
        ELSE
            LOCK_TYPE='read'
            CALL SET_OFS_LOCK (LOCK_TYPE,KOND)
            IF (KOND .GT. 0) THEN
               STOP 16
            ENDIF
        ENDIF

C  Set user id
        PUSRID = ' '

C  Initialize common for function "ifbug".
        NDEBGS = 0
        IALL   = 0
C
C  Set unit number for system files
        KDTYPE = 3
        LSYS2  = KDTYPE

        ITRACE = 0
        IOPCLG(2) = 1

C  Set the two globals needed by every program
        GLODIR = 'SYST OPER'
        IF (PGMNAM.EQ.'FCST'   ) GLODIR(11:19) = 'MODS GRID'
        IF (PGMNAM.EQ.'REORDER') GLODIR(11:24) = 'MODS GRID REOR'
        CALL UPCHKD (GLODIR,ISTAT)
        IF (ISTAT .NE. 0) GO TO 30

        ICD    = 0
        LP     = 0
        ICDPUN = 0

C  Set i/o units for specific programs

      IF (PGMNAM.EQ.'FILECRAT') THEN
         IF (ISTAT .EQ.0) CALL UPRIMR (INSTMT,FM,59,ICD)
CCC         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,6,LP)
         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,98,LP)
         IF (LP    .LE.0) GO TO 30
         LPD    = LP
         LPE    = LP
         IOGDB  = LP
         IN     = ICD
         IPR    = LP
         IPU    = LP
         IODBUG = LP
         IOERR  = LP
C   These values taken from EEBLOCKU so that it can be eliminated
C    from the control file (needed for Linux linking).

         KEPARM    = 96
         KEPERM(1) = 91
         KEPERM(2) = 92
         KEPERM(3) = 93
         KEPERM(4) = 94
         KEPERM(5) = 95
         KESCRA    = 90
         KESTBL    = 97
         
         GO TO 40
         ENDIF

      IF (PGMNAM.EQ.'PPDUTIL ') THEN
         IF (ISTAT .EQ.0) CALL UPRIMR (INSTMT,FM,59,ICD)
CCC         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,6,LP)
         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,98,LP)
         IF (LP    .GT.0) CALL UPRIMW (PUSTMT,FM,8,ICDPUN)
         IF (ICDPUN.LE.0) GO TO 30
         LPE    = LP
         LPD    = LPE
         IOGDB  = LP
         IOERR  = LP
         IODBUG = LP
         IN     = ICD
         IPR    = LP
         IOPDBG = LP
         IPU    = LP
         IOSDBG = LP
         IPDDB  = 0
         IPDTR  = 0
         ITRACE = 0
         NOBUG  = 0
         GO TO 40
         ENDIF

       IF (PGMNAM.EQ.'PPINIT  ') THEN
          IF (ISTAT .EQ.0) CALL UPRIMR (INSTMT,FM,59,ICD)
CCC          IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,6,LP)
          IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,98,LP)
          IF (LP    .GT.0) CALL UPRIMW (PUSTMT,FM,8,ICDPUN)
          IF (ICDPUN.LE.0) GO TO 30
          USERPP(1) = JPRIM
          USERPP(2) = JEBBB
          IPRMPT = 1
          IF ( ICD .NE. 1 ) IPRMPT = 0
          FILNAM = 'TEMP.PPINIT.02'
          CALL UPPFIX ('OPER',FILNAM,NEWNAM,NOFC)
          ICDTMP = 2
          CALL UPRIMT (NEWNAM,FM,ICDTMP,I)
C      GET UNIT NUMBER FOR FILE SASM.CONTROL
          FILNAM = 'SASM.CONTROL'
          CALL UPFNCU ('NORM',FILNAM,FNUM,NEWNAM,N,RNUM,BNUM,ISTAT)
          IF ( ISTAT.GT.0 .AND. UE.GE.0 ) WRITE(UE,10)
10        FORMAT(' uprimo      ** ERROR = error returned from upfncu')
          IF ( ISTAT.LE.0 ) KDSRCF = N
            IPRCRD = 0
            IOPNWP = 0
            LPE    = LP
            KUPARM = 4
            IOGDB  = LP
            IUTLTR = 0
            IUTLDB = 0
            IOSDBG = LP
            ISTRCE = 0
            ISDBUG = 0
            ISALL  = 0
            NSDBUG = 0
            SDBUG(1) = CDEFN
            SONEWP = 0
            NFLD   = 0
            LPD    = LP
            IOERR  = LP
            IODBUG = LP
            IN     = ICD
            IPR    = LP
            IPU    = ICDPUN
            ISDBGL = 0
            GO TO 40
         ENDIF

      IF (PGMNAM.EQ.'FCINIT  ') THEN
         IF (ISTAT .EQ.0) CALL UPRIMR (INSTMT,FM,59,ICD)
CCC         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,6,LP)
         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,98,LP)
         IF (LP    .GT.0) CALL UPRIMW (PUSTMT,FM,8,ICDPUN)
         IF (ICDPUN.LE.0) GO TO 30
         FILNAM = 'TEMP.FCINIT.02'
         CALL UPPFIX ('OPER',FILNAM,NEWNAM,NOFC)
         ICDTMP = 2
         CALL UPRIMT (NEWNAM,FM,ICDTMP,I)
         FILNAM = 'TEMP.FCINIT.89'
         CALL UPPFIX ('OPER',FILNAM,NEWNAM,NOFC)
         CALL UPRIMT (NEWNAM,FM,89,I)
         IN     = ICD
         IPR    = LP
         IPU    = ICDPUN
         LPD    = LP
         LPE    = LP
         IODBUG = LP
         IOERR2 = LP
         IOGDB  = LP
         GO TO 40
         ENDIF

      IF (PGMNAM.EQ.'ESPINIT ') THEN
         IF (ISTAT .EQ.0) CALL UPRIMR (INSTMT,FM,59,ICD)
CCC         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,6,LP)
         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,98,LP)
         IF (LP    .GT.0) CALL UPRIMW (PUSTMT,FM,8,ICDPUN)
         IF (ICDPUN.LE.0) GO TO 30
         CALL UPRIMW (' *** Output file (unit 9), TTY or Q:',FM,9,II)
         IF (II.LE.0) GO TO 30
         IN     = ICD
         IPR    = LP
         IPU    = ICDPUN
         LPD    = LP
         LPE    = LP
         IODBUG = LP
         IOERR2 = LP
         IOGDB  = LP

C   These values taken from EEBLOCKU so that it can be eliminated
C    from the control file (needed for Linux linking).

         KEPARM    = 96
         KEPERM(1) = 91
         KEPERM(2) = 92
         KEPERM(3) = 93
         KEPERM(4) = 94
         KEPERM(5) = 95
         KESCRA    = 90
         KESTBL    = 97

         GO TO 40
         ENDIF

      IF (PGMNAM.EQ.'FCST    ') THEN
         IF (ISTAT .EQ.0) CALL UPRIMR (INSTMT,FM,59,ICD)
CCC         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,6,LP)
         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,98,LP)
         IF (LP    .LE.0) GO TO 30
         CALL UPRIMW (' *** Output file (unit 1), TTY or Q:',FM,1,II)
         IF (II.LE.0) GO TO 30
         CALL UPRIMW (' *** Output file (unit 8), TTY or Q:',FM,8,II)
         IF (II.LE.0) GO TO 30
         CALL UPRIMW (' *** Output file (unit 9), TTY or Q:',FM,9,II9)
         IF (II9.LE.0) GO TO 30
         CALL UPRIMW (' *** Output file (unit 97), TTY or Q:',FM,97,II)
         IF (II.LE.0) GO TO 30
              IN     = ICD
              IPR    = LP
              IPU    = LP
              LPD    = LP
              LPE    = LP
              IODBUG = LP
              IF (II9.EQ.UTW) THEN
                 IOERR2 = LP
                 ELSE
                    IOERR2 = 9
                 ENDIF
              IOGDB  = LP
C      KOD=8 to force unformatted files, plus 1 to create or open
          KOD = 8+1
          CALL UDOPEN (88,KOD,NEWNAM,LRECL,ISTAT)
          CALL UDOPEN (70,KOD,NEWNAM,LRECL,ISTAT)
          CALL UDOPEN (71,KOD,NEWNAM,LRECL,ISTAT)
          CALL UDOPEN (72,KOD,NEWNAM,LRECL,ISTAT)
          CALL UDOPEN (73,KOD,NEWNAM,LRECL,ISTAT)
          CALL UDOPEN (74,KOD,NEWNAM,LRECL,ISTAT)
          CALL UDOPEN (75,KOD,NEWNAM,LRECL,ISTAT)
          CALL UDOPEN (76,KOD,NEWNAM,LRECL,ISTAT)
          CALL UDOPEN (77,KOD,NEWNAM,LRECL,ISTAT)
          CALL UDOPEN (78,KOD,NEWNAM,LRECL,ISTAT)
          CALL UDOPEN (79,KOD,NEWNAM,LRECL,ISTAT)
          FILNAM = 'TEMP.HCLPROC'
          CALL UPPFIX ('OPER',FILNAM,NEWNAM,NOFC)
          CALL UPRIMT (NEWNAM,FM,89,I)
          FILNAM = 'TEMP.FCST.02'
          CALL UPPFIX ('OPER',FILNAM,NEWNAM,NOFC)
          ICDTMP = 2
          CALL UPRIMT (NEWNAM,FM,ICDTMP,I)
          GO TO 40
          ENDIF

      IF (PGMNAM.EQ.'REORDER ') THEN
         IF (ISTAT .EQ.0) CALL UPRIMR (INSTMT,FM,59,ICD)
CCC         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,6,LP)
         IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,97,LP)
         IF (LP    .LE.0) GO TO 30
         CALL UPRIMW (' *** Output error file, TTY or Q:',FM,9,II)
         IF (II.LE.0) GO TO 30
         FILNAM = 'REORDER.LOG'
         CALL UPPFIX ('OPER',FILNAM,NEWNAM,NOFC)
         LPUNIT = 8
         CALL UPRIMT (NEWNAM,FM,LPUNIT,I)
         ICDPUN = LP
         IPRCRD = 0
         IOPNWP = 0
         LPE    = LP
         KUPARM = 4
         IOGDB  = LP
         IUTLTR = 0
         IUTLDB = 0
         IOSDBG = LP
         ISTRCE = 0
         ISDBUG = 0
         ISALL  = 0
         NSDBUG = 0
         SONEWP = 0
         NFLD   = 0
         LPD    = LP
         IOERR  = II
         IODBUG = LP
         IN     = ICD
         IPR    = LP
         IPU    = ICDPUN
         ISDBGL = 0
         FILNAM = '    '
         FNUM   = 0
         CALL UPFNCU ('REOR',FILNAM,FNUM,NEWNAM,N,RNUM,BNUM,ISTAT)
         IF ( ISTAT.GT.0 .AND. UE.GE.0 ) WRITE(UE,20)
20    FORMAT(' uprimo      ** ERROR = error returned from upfncu')
         GO TO 40
         ENDIF

C  Set i/o units for other programs

      IF (ISTAT .EQ.0) CALL UPRIMR (INSTMT,FM,59,ICD)
CCC      IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,6,LP)
      IF (ICD   .GT.0) CALL UPRIMW (OTSTMT,FM,98,LP)
      IF (LP    .GT.0) THEN
         LPD    = LP
         LPE    = LP
         IOGDB  = LP
         IOSDBG = LP
         IN     = ICD
         IPR    = LP
         IPU    = ICDPUN
         IODBUG = LP
         IOERR  = LP
         NOBUG  = 0
         IPRCRD = ICD
         ICDTMP = LP
         GO TO 40
         ENDIF

C  Error setting i/o units

30    IF (LP   .GT. 0) CALL UPCLOS (LP,' ',IERR)
      IF (ICD  .GT. 0) CALL UPCLOS (ICD,' ',IERR)
      IF (ICDPUN.GT.0) CALL UPCLOS (ICDPUN,' ',IERR)
      CALL FREE_OFS_LOCK (KOND)
      CALL EXIT

C  Set second common area for i/o for routines using UIOX instead of UIO
40    LP2    = LP
      ICD2   = ICD
      LPD2   = LPD
      LPE2   = LPE
      ICDPU2 = ICDPUN

C  Set shared library debug variables
      ICMPRU = LP
      ICMTRC = 0
      ICMDBG = 0

C  Make sure DSUNIT is 3380
      DSUNIT = '3380'

      IOPDBG = LP

CCC      IF (UE.GT.0) WRITE (UE,*) 'IN UPRIMO - UE=',UE
CCC      IF (UE.GT.0) WRITE (UE,*) 'IN UPRIMO - IPR=',IPR
CCC      IF (UE.GT.0) WRITE (UE,*) 'IN UPRIMO - LP=',LP
CCC      IF (UE.GT.0) WRITE (UE,*) 'IN UPRIMO - IOERR=',IOERR

      RETURN

      END
