C MODULE UPRIMO_SGDB
C  =====================================================================
C  pgm: UPRIMO_SGDB .. Initialize I/O units
C
C  use:     CALL UPRIMO_SGDB(LPX,ICOND)
C
C  out: LPX ...... unit number for interactive prompts - INT
C  out: ICOND .... error status, 0 for no error, pos for error - INT
C
C  rqd: UPINIO, UPRIMR, UPRIMW, UPCLOS
C  rqd: SET_OFS_LOCK, UPCHKD, FREE_OFS_LOCK
C  rqd: COMMON: UPDAIO and other nwsrfs-ofs common
C  =====================================================================
      SUBROUTINE UPRIMO_SGDB(LPX,ICOND)

      EXTERNAL   UPINIO, UPRIMR, UPRIMW, UPCLOS
      EXTERNAL   SET_OFS_LOCK, UPCHKD, FREE_OFS_LOCK

      INTEGER       ILOCK,ISTAT,LPX,ICOND
      CHARACTER*5   LOCK_TYPE
  
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_sasm/RCS/uprimo_sgdb.f,v $
     . $',                                                             '
     .$Id: uprimo_sgdb.f,v 1.3 2001/06/13 13:47:41 dws Exp $
     . $' /
C    ===================================================================
C

C                         Set this routine's controlling variables

      ICOND =  0
      LPX   = -1
      ICD   = -1

C                         Set system level i/o unit numbers
C                         Open ofs lock
C                         Check for needed directories

      CALL UPINIO()

      LOCK_TYPE='write'
      CALL SET_OFS_LOCK(LOCK_TYPE,ILOCK)
      IF (ILOCK .NE. 0) THEN
        ICOND = 1
        IF (UE .GE. 0) WRITE(UE,74,IOSTAT=KOND2) ILOCK
   74   FORMAT(' uprimo      ** ERROR =',I3,', cannot open lock')
      ENDIF

      IF (ICOND .EQ. 0) CALL UPCHKD('SYST OPER',ICOND)

C                         Open input and output files

      IF (ICOND .EQ. 0) THEN
        CALL UPRIMR(' *** Enter input filename, TTY, or Q: ','F',59,ICD)
        IF (ICD .LE. 0) ICOND = 1
      ENDIF

      IF (ICOND .EQ. 0) THEN
        CALL UPRIMW(' *** Enter output filename, TTY, or Q: ','F',58,LP)
        IF (LP .LE. 0) ICOND = 1
      ENDIF

C                         Initialize i/o and other related variables
C                         LPX is unit num for interactive prompts

      IF (ICOND .EQ. 0) THEN
        IF (ICD . EQ. UTR) LPX = UTW

        IOGDB = LP
        IDETR = LP
      ENDIF

C                         If error occurred; close files, free lock

      IF (ICOND .NE. 0) THEN
        IF (ICD   .GT. 0) CALL UPCLOS(59,' ',ISTAT)
        IF (ILOCK .EQ. 0) CALL FREE_OFS_LOCK(ISTAT)
      ENDIF

      RETURN
      END
