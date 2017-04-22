C$PRAGMA C (GETARG)
C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: SHOPIT .. Open files for shefpars driver used as a filter
C
C  use:     CALL SHOPIT(LUIN,LUOT,LUSH,LUER,LUCP)
C
C  out: LUIN ....... logical unit num opened for "shefin" file - INT
C  out:              (if neg, then cannot set up i/o files)
C  out: LUOT ....... logical unit num opened for "shefout" file - INT
C  out: LUSH ....... logical unit num opened for "shefparm" file - INT
C  out: LUER ....... logical unit num opened for shef error msgs - INT
C  out: LUCP ....... logical unit num opened for all shef msgs - INT
C   in: (cmd args) . command args all optional as:
C   in:                 [input-file] [output-file] [error-file]
C   in:              if not given, use stdin, stdout, stderr
C
C  rqd: GET_APPS_DEFAULTS,SHOPFL,SHFMOT,SHHELP
C  rqd: system rtn - GETARG
C
C  cmt: One of the following options can be given as the first argument:
C  cmt:     -1 ... Text output as one long line, full quotes (default)
C  cmt:     -2 ... Text output as two lines, quotes limited to 66 chars
C  cmt:     -b ... Binary output if output is to a file
C  cmt:     -h ... Output a help message on how to run shefit (stdout)
C
C  cmt: Values of logical unit numbers in original version:
C  cmt:     variable  stdin-run  file-run
C  cmt:       LUIN        5         11
C  cmt:       LUOT        6          6
C  cmt:       LUSH       10         10
C  cmt:       LUER        6          6
C  cmt:       LUCP       -1         -1
C  =====================================================================
      SUBROUTINE SHOPIT(LUIN,LUOT,LUSH,LUER,LUCP)

      INTRINSIC      ICHAR,CHAR
C     EXTERNAL       GETARG,GET_APPS_DEFAULTS
      EXTERNAL       SHOPFL,SHFMOT,SHHELP

      INTEGER        LUIN,LUOT,LUSH,LUER,LUCP,IU
      INTEGER        IE,IE2,IEND,IAR,ICHAR,DIFF
      CHARACTER*1    KHUN,FTYP,CHAR,SMLA,SMLZ,CAPA
      CHARACTER*9    FM
      CHARACTER*11   UF,FUX
      CHARACTER*128  FSH,FIN,FOT,FER
      LOGICAL        EXS

      PARAMETER( SMLA='a', SMLZ='z', CAPA='A' )

      DATA  FM,UF / 'FORMATTED', 'UNFORMATTED' /

C                   Open local error unit (default 7 for HPUX)

        IU = 7
        CALL GET_APPS_DEFAULTS('fortran_stderr',14,KHUN,IEND)
         IF (IEND .EQ. 1) READ(KHUN,'(I1)',IOSTAT=IE) IU
         IF (IEND .EQ. 2) READ(KHUN,'(I2)',IOSTAT=IE) IU

C                   Get pathname of SHEFPARM file (search cur dir first)

        FSH = 'SHEFPARM'
        INQUIRE(FILE=FSH,IOSTAT=IE,EXIST=EXS)
        IF (IE.EQ.0 .AND. .NOT.EXS) THEN
          CALL GET_APPS_DEFAULTS('rfs_sys_dir',11,FSH,IEND)
          IF (IEND .GT. 0) FSH = FSH(1:IEND) // '/' // 'SHEFPARM'
          IF (IEND .LE. 0) IE = -1
        ENDIF

C                   Check for input/output/err files in arguments, else
C                    use standards; enter output format in buffer

        IAR = 1
        CALL GETARG(IAR,FIN)

          FTYP = 'D'
   70     IF (FIN(1:1) .NE. '-') GOTO 80
            FTYP = FIN(2:2)
            IF( FTYP.GE.SMLA .AND. FTYP.LE.SMLZ ) THEN
              DIFF = ICHAR(SMLA)-ICHAR(CAPA)
              FTYP = CHAR( ICHAR(FTYP)-DIFF )
            ENDIF
            IAR = IAR+1
            CALL GETARG(IAR,FIN)
            GOTO 70
   80     CONTINUE

C                   If "-H" option is given, output help to stdout
C                    and skip the rest (IE = 1)

        IF (FTYP .EQ. 'H') THEN
          CALL SHHELP()
          IE = 1
        ELSE

C                   Get input and output filenames from args if given

          CALL GETARG(IAR+1,FOT)
          CALL GETARG(IAR+2,FER)

C                   Open SHEFPARM file unit

          CALL SHOPFL(IE,IU,LUSH,10,10,FSH,'OLD',FM,'open SHEFPARM!')
        ENDIF

C                   Open input file

        IF (IE .EQ. 0) THEN
          CALL SHOPFL(IE,IU,LUIN,5,11,FIN,'OLD',FM,'open input file!')
          IF (IE .NE. 0) CLOSE(LUSH,IOSTAT=IE2)
        ENDIF

C                   Open output file

        IF (IE .EQ. 0) THEN
           FUX = FM
           IF (FTYP.EQ.'B' .AND. FOT.NE.' ') FUX  = UF
           IF (FTYP.EQ.'B' .AND. FOT.EQ.' ') FTYP = 'D'
          CALL SHFMOT('PUT_VALUE   ',FTYP)
          CALL SHOPFL(IE,IU,LUOT,6,12,FOT,'NEW',FUX,'open output file!')
          IF (IE .NE. 0) CLOSE(LUSH,IOSTAT=IE2)
          IF (IE .NE. 0) CLOSE(LUIN,IOSTAT=IE2)
        ENDIF

C                   Open error file

        IF (IE .EQ. 0) THEN
          CALL SHOPFL(IE,IU,LUER,IU,13,FER,'NEW',FM,'open error file!')
          IF (IE .NE. 0) CLOSE(LUSH,IOSTAT=IE2)
          IF (IE .NE. 0) CLOSE(LUIN,IOSTAT=IE2)
          IF (IE .NE. 0) CLOSE(LUOT,IOSTAT=IE2)
        ENDIF

C                   Open input-message copy file (no file here, -1)

        LUCP = -1

C                   If error status is not zero, set LUINP to -1

        IF (IE .NE. 0) THEN
          LUIN = -1
          LUOT = -1
          LUSH = -1
          LUER = -1
          LUCP = -1
        ENDIF

      RETURN
      END
