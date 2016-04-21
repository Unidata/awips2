C MODULE MX_UPRIMO
C  =====================================================================
C  pgm: MX_UPRIMO .. Initialize unit numbers for program "mapx"
C
C  use:     CALL MX_UPRIMO(IERR)
C  =====================================================================
      SUBROUTINE MX_UPRIMO(IERR)

      INCLUDE 'uiox'
      INCLUDE 'updaio'
      INCLUDE 'ucmdbx'
      INCLUDE 'uduntx'
      INCLUDE 'scommon/sudbgx'
 
      INCLUDE 'ufiles'
      INCLUDE 'udebug'

      CHARACTER*128 FLNAM
      CHARACTER*39  INSTMT,OTSTMT
      INTEGER       NOFARG,IERR,JERR,II,JJ,IUNOUT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mx_uprimo.f,v $
     . $',                                                             '
     .$Id: mx_uprimo.f,v 1.2 2002/02/11 18:48:52 dws Exp $
     . $' /
C    ===================================================================
C
 
      DATA  INSTMT / ' *** Enter input filename, TTY, or Q:  ' /
      DATA  OTSTMT / ' *** Enter output filename, TTY, or Q: ' /
 
C  Set standard units
 
        CALL UPINIO()
 
C  Get filenames for input, output, punch unit numbers;

        IUNOUT = 8
        ICD    = 9
        LP     = IUNOUT

C  Get input file from command argument list, or prompt for it

        IERR = 0
        JERR = -1
        NOFARG = IARGC()
        IF (NOFARG .GT. 0) THEN
          CALL GETARG(1,FLNAM)
          CALL UPOPEN(ICD,FLNAM,0,'F',JERR)
          IF (JERR .NE. 0) IERR = 1
        ELSE
          CALL UPRIMR(INSTMT,'F',9,ICD)
          IF (ICD .LE. 0 ) IERR = 1
        ENDIF

        IF (IERR .EQ. 0) THEN
          IF (NOFARG .GT. 1) THEN
            CALL GETARG(2,FLNAM)
            IF (FLNAM .EQ. 'tty') THEN
              UE = UW
              UU = -1
              LP = UTW
            ELSE
              CALL UPEXIS(LP,FLNAM,IERR)
              IF (IERR .EQ. 0) CALL UPDELE(LP,FLNAM,IERR)
              CALL UPOPEN(LP,FLNAM,0,'F',IERR)
            ENDIF
          ELSE
            CALL UPRIMW(OTSTMT,'F',IUNOUT,LP)
            IF (LP .LE. 0) IERR = 1
          ENDIF
        ENDIF

        IF (IERR .EQ. 0) THEN

          LPD    = LP
          LPE    = LP
          ICDPUN = LP
          LSYS   = 3

C  Set other unit numbers, block data values

C           for 'ucmdbx'

          ICMPRU = LP

C           for 'scommon/sudbgx'

          IOSDBG = LP

C           for 'uduntx'
C            (see the include file, uduntb, that is called
C             from a block data, usually ublock.f)

          IDUFIL = 0
          MDDIMN = 13
          MDUNIT = 12
          DO 21 II=1,MDDIMN
            DUDIMN(II) = '    '
            DUTFAC(II) = -999.0
            DO 20 JJ=1,MDUNIT
              DUUNIT(II,JJ) = '    '
              DUCFAC(II,JJ) = -999.0
              NDUNDC(II,JJ) = -999
   20       CONTINUE
   21     CONTINUE
          NNUCNV = 0
          MNUCNV = 5
          DO 22 II=1,MNUCNV
            UCNVAL(II) = 0.0
   22     CONTINUE

C           for ufiles and udebug

          MFILES = 256
          DO 100 I=1,MFILES
  100       IFILES(I) = 0

          IOGDB  = LP
          IPPTR  = 0
          IPPDB  = 0

        ELSE
          IF (JERR .EQ. 0) CALL UPCLOS(LP,' ',JERR)
        ENDIF

      RETURN
      END
