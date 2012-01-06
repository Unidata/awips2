C MODULE FUN008
C-----------------------------------------------------------------------

C  MAIN ROUTINE FOR FUNCTION MAT
C
      SUBROUTINE FUN008
C
      DIMENSION BLANK(2),SBNAME(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/where'
      INCLUDE 'common/errdat'
      INCLUDE 'common/tuser'
      INCLUDE 'common/tscrat'
CFAN
C      INTEGER DIURNAL
C      REAL K1,K2,K3,K4,K5,K6,K7,K8,K9,K0(9,4)

C jgg-replaced the above line for DR 18651,allows 3 more sites-May,2007
C     note this change was also made in tiluvo.f, tmeanm.f, temp.f
      INTEGER DIURNAL
      REAL K1,K2,K3,K4,K5,K6,K7,K8,K9,K0(9,16)
C jgg
      COMMON/DIURNAL0/DIURNAL,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0
CFAN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob82/ohd/ofs/src/fcst_mat/RCS/fun008.f,v $
     . $',                                                             '
     .$Id: fun008.f,v 1.3 2005/03/01 19:21:00 dsa Exp jgofus $
     . $' /
C    ===================================================================
C
CFAN
      DATA K1,K2,K3,K4,K5,K6,K7,K8,K9/
     *        .55,.45,.10,.80,.10,.55,.45,.75,.25/     !Default

C jgg - DR 18651
C      DATA K0/.70,.30,.10,.83,.07,.37,.63,.79,.21,     !K0(*,1) Mar-May
C     *        .68,.32,.09,.84,.07,.33,.67,.80,.20,     !K0(*,2) Jun-Aug
C     *        .73,.27,.11,.85,.04,.41,.59,.78,.22,     !K0(*,3) Sep-Nov
C     *        .77,.23,.19,.78,.03,.43,.57,.73,.27/     !K0(*,4) Dec-Feb
CFAN

C jgg Added values for 3 more RFCs - DR 18651 - May, 2007
      DATA K0/.70,.30,.10,.83,.07,.37,.63,.79,.21,     !K0(*,1) Mar-May NWRFC
     *        .68,.32,.09,.84,.07,.33,.67,.80,.20,     !K0(*,2) Jun-Aug NWRFC
     *        .73,.27,.11,.85,.04,.41,.59,.78,.22,     !K0(*,3) Sep-Nov NWRFC
     *        .77,.23,.19,.78,.03,.43,.57,.73,.27,     !K0(*,4) Dec-Feb NWRFC
     *        .78,.22,.20,.69,.11,.21,.79,.65,.35,     !K0(*,5) Mar-May AKRFC
     *        .79,.21,.15,.69,.16,.23,.77,.68,.32,     !K0(*,6) Jun-Aug AKRFC
     *        .73,.27,.23,.64,.13,.31,.69,.64,.36,     !K0(*,7) Sep-Nov AKRFC
     *        .61,.39,.28,.53,.19,.39,.61,.58,.42,     !K0(*,8) Dec-Feb AKRFC
     *        .64,.36,.02,.87,.11,.39,.61,.80,.20,     !K0(*,9) Mar-May CBRFC
     *        .61,.39,.02,.87,.11,.39,.61,.81,.19,     !K0(*,10) Jun-Aug CBRFC
     *        .69,.31,.02,.88,.10,.47,.53,.81,.19,     !K0(*,11) Sep-Nov CBRFC
     *        .76,.24,.07,.85,.08,.48,.52,.78,.22,     !K0(*,12) Dec-Feb CBRFC
     *        .73,.27,.07,.84,.09,.35,.65,.77,.23,     !K0(*,13) Mar-May CNRFC
     *        .71,.29,.08,.85,.07,.31,.69,.77,.23,     !K0(*,14) Jun-Aug CNRFC
     *        .77,.23,.08,.86,.06,.40,.60,.78,.22,     !K0(*,15) Sep-Nov CNRFC
     *        .81,.19,.14,.82,.04,.43,.57,.77,.23/     !K0(*,16) Dec-Feb CNRFC
C jgg

      DATA SBNAME/4HFUN0,4H08  /
C
      IOPNUM=-1
      DO 10 I=1,2
   10 OPNAME(I)=SBNAME(I)
C
      REWIND KTSCR
C
      IOUNT=0
      IF (IOUNT.EQ.1) THEN
C
C   COMMON IONUM
C
      CALL HPAST (8HPPPRINT ,IPR,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN (ISTAT,8HPPPRINT )
      CALL FTEKCK (IPR,8HPPPRINT ,6,IPR,1,9)
C
C   COMMON PUDBUG
C
      CALL HPAST (8HDEBUGPR ,IOPDBG,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN (ISTAT,8HDEBUGPR )
      CALL FTEKCK (IOPDBG,8HDEBUGPR ,6,IOPDBG,1,9)
C
C   COMMON ERRDAT
C
      CALL HPAST (8HERRORPR ,IOERR,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN (ISTAT,8HERRORPR )
      CALL FTEKCK (IOERR,8HERRORPR ,9,IOERR,1,9)
C
      ENDIF
C
      CALL HPAST (8HPPTRACE ,IPTRCE,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN (ISTAT,8HPPTRACE )
      CALL FTEKCK (IPTRCE,8HPPTRACE ,0,IPTRCE,0,3)
C
C   COMMON ERRDAT
C
      NWARN=0
      NERRS=0
C
      IF (IPTRCE.GT.0) WRITE (IOPDBG,*) 'ENTER FUN008'
C
C  PRINT TITLE LINE
      CALL UPAGE (IPR)
      WRITE (IPR,600)
600   FORMAT (1H0,45X,'MAT FUNCTION')
C
      CALL RPPPCO (IER)
      IF (IER.EQ.0) GO TO 50
      WRITE (IPR,602) IER
  602 FORMAT(1H0,'ERROR IN RPPPCO, ISTAT = ',I5)
      GO TO 999
   50 CALL RPPDCO (IER)
      IF (IER.EQ.0) GO TO 60
      WRITE (IPR,604) IER
  604 FORMAT(1H0,'ERROR IN RPPDCO, ISTAT = ',I5)
      GO TO 999
   60 CONTINUE
C
C   GET REMAINING TECHNIQUES AND ARGUMENTS
C
      CALL TILUVO
C
      IF (ISTMAT.EQ.0) GO TO 100
      WRITE (IPR,610)
  610 FORMAT(1H0,10X,29H**ERROR** MAT FILE INCOMPLETE)
C
      CALL ERROR
      GO TO 999
C
  100 CALL TEMP
C
      REWIND KTSCR
C
      IF (IPTRCE.GT.0) WRITE (IOPDBG,910)
  910 FORMAT(1H0,18H*** MAT EXITED ***)
C
  999 CONTINUE
C
      CALL STOPFN(8HMAT     )
C
      RETURN
C
      END
