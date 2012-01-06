C  =====================================================================
C  pgm: SHVERN .. Output or get parser version number
C
C  use:     CALL SHVERN(CMD,LENC,VERN,LENV)
C
C   in: CMD ....... command or message to control operations - CHAR*(*)
C   in:               'INITIALIZE' ..... set or reset parameter, units
C   in:               'WRITE_VERSION' .. write out num of errs/warns
C   in:               'GET_VERSION' .... get the version number
C   in: LENC ...... number of characters in input command, CMD - INT
C  out: VERN ...... version number - CHAR*8
C  out: LENV ...... number of last non-blank char in VERN - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFERROR',<number>)
C
C  rqd: SHSAVU
C
C  cmt: Initialization is done automatically during first access
C  =====================================================================
      SUBROUTINE SHVERN(CMD,LENC,VERN,LENV)

      EXTERNAL       SHSAVU
      INTRINSIC      LEN

      CHARACTER*(*)  CMD
      CHARACTER*16   XCMD
      CHARACTER*8    VERN,VERNUM
      CHARACTER      BLANK
      INTEGER        INITZ,LUNE,LSTAT,LENC,LENV,IEND,NEXT,LEN
      PARAMETER      ( BLANK=' ' )

      SAVE           INITZ,LUNE,VERNUM,IEND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/shefpars_driv/RCS/shvern.f,v $
     . $',                                                             '
     .$Id: shvern.f,v 1.10 2007/03/20 17:34:17 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA           INITZ  / 0 /
      DATA           VERNUM / 'ob8     ' /

      XCMD = CMD(1:LENC)

      IF (XCMD.EQ.'INITIALIZE' .OR. INITZ.EQ.0) THEN

          INITZ = 1
          CALL SHSAVU('G_SHEFERROR ',LUNE)

          IEND = 1
          NEXT = LEN(VERNUM)
  100     IF (NEXT .LE. IEND) GOTO 110
            IF (VERNUM(NEXT:NEXT) .NE. BLANK) IEND = NEXT
            NEXT = NEXT - 1
             GOTO 100
  110     CONTINUE

      ENDIF

      IF (XCMD.EQ.'WRITE_VERSION') THEN
          IF (LUNE .GE. 0) THEN
           WRITE(LUNE,'(''    (parsing routines: '',A,'')'')',
     $           IOSTAT=LSTAT) VERNUM(1:IEND)
          ENDIF
      ELSEIF (XCMD.EQ.'GET_VERSION') THEN
          VERN = VERNUM
          LENV = IEND
      ENDIF

      RETURN
      END
