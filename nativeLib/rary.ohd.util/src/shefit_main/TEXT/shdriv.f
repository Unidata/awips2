C  =====================================================================
C  pgm: SHDRIV .. Driver routine for parsing SHEF data files
C
C  use:     CALL SHDRIV(LUINP,LUOUT,LUPARM,LUERR,LUCPY)
C
C   in: LUINP ...... logical unit num opened for "shefin" file - INT
C   in: LUOUT ...... logical unit num opened for "shefout" file - INT
C   in: LUPARM ..... logical unit num opened for "shefparm" file - INT
C   in: LUERR ...... logical unit num opened for shef error msgs - INT
C   in: LUCPY ...... logical unit num opened for all shef msgs - INT
C
C  rqd: SHSAVU,SHDCOD,SHERR,SHVERN
C
C  cmt: Note, LUERR and LUCPY can be (and usually are) the same.  Either
C  cmt:   or both can be set to -1 for no error or message line output.
C  cmt:   LUINP, LUOUT, and LUPARM must be unique between 0 and 99.
C  =====================================================================
      SUBROUTINE SHDRIV(LUINP,LUOUT,LUPARM,LUERR,LUCPY)

      EXTERNAL       SHSAVU,SHDCOD,SHERR,SHVERN

      INTEGER        LUINP,LUOUT,LUPARM,LUERR,LUCPY,KFLAG
      INTEGER        LUNI,LUNO,LUNP,LUNE,LUNC
      CHARACTER      DMYCH1*1,DMYCH8*8
      INTEGER        DMYINT

C                   Set run flag

        KFLAG = 0

C                   Make arguments local variables

        LUNI = LUINP
        LUNO = LUOUT
        LUNP = LUPARM
        LUNE = LUERR
        LUNC = LUCPY

C                   Check for validity of unit numbers

        IF (LUNI.LT.0 .OR. LUNI.GT.99) KFLAG = 1
        IF (LUNO.LT.0 .OR. LUNO.GT.99) KFLAG = 1
        IF (LUNP.LT.0 .OR. LUNP.GT.99) KFLAG = 1
        IF (LUNI.EQ.LUNO .OR. LUNI.EQ.LUNP .OR. LUNP.EQ.LUNO) KFLAG=1
        IF (LUNE.EQ.LUNI .OR. LUNC.EQ.LUNI) KFLAG = 1
CCC     IF (LUNE.EQ.LUNO .OR. LUNC.EQ.LUNO) KFLAG = 1
        IF (LUNE.EQ.LUNP .OR. LUNC.EQ.LUNP) KFLAG = 1

C                   If unit numbers are good:
C                     a) set unit number values
C                     b) run shef decode
C                     c) output warnings/errors in error message file
C                     d) output version number in error message file

        IF (KFLAG .EQ. 0) THEN

          CALL SHSAVU('P_SHEFIN    ',LUNI)
          CALL SHSAVU('P_SHEFOUT   ',LUNO)
          CALL SHSAVU('P_SHEFPARM  ',LUNP)
          CALL SHSAVU('P_SHEFERROR ',LUNE)
          CALL SHSAVU('P_SHEFCOPY  ',LUNC)

          CALL SHDCOD()

          CALL SHERR('S',0,DMYINT,DMYCH1)
          CALL SHVERN('WRITE_VERSION',13,DMYCH8,DMYINT)

        ENDIF

      RETURN
      END
