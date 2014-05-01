C  =====================================================================
C  pgm: SHERRS .. Output summary of error and warning messages
C
C  use:     CALL SHERRS(CMD,NWAR,NERR)
C
C   in: CMD ....... command or message to control operations - CHAR*16
C   in:               'INITIALIZE' ..... set or reset parameter, units
C   in:               'WRITE_SUMMARY' .. write out num of errs/warns
C   in: NWAR ...... total number of warning messages
C   in: NERR ...... total number of error messages
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFERROR',<number>)
C
C  rqd: SHSAVU
C
C  cmt: Initialization is done automatically during first access
C  =====================================================================
      SUBROUTINE SHERRS(CMD,NWAR,NERR)

      EXTERNAL       SHSAVU

      CHARACTER*16   CMD
      INTEGER        NWAR,NERR,INITZ,LUNE,LSTAT

      SAVE           INITZ,LUNE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sherrs.f,v $
     . $',                                                             '
     .$Id: sherrs.f,v 1.2 1996/12/10 16:07:49 dws Exp $
     . $' /
C    ===================================================================
C

      DATA           INITZ / 0 /

      IF (CMD.EQ.'INITIALIZE      ' .OR. INITZ.EQ.0) THEN
          INITZ = 1
          CALL SHSAVU('G_SHEFERROR ',LUNE)
      ENDIF

      IF (CMD.EQ.'WRITE_SUMMARY   ') THEN
          IF (LUNE .GE. 0) THEN
           WRITE(LUNE,'(/,''    NUMBER OF WARNINGS  ....'',I8  )',
     $           IOSTAT=LSTAT) NWAR
           WRITE(LUNE,'(  ''    NUMBER OF ERRORS  ......'',I8,/)',
     $           IOSTAT=LSTAT) NERR
          ENDIF
      ENDIF

      RETURN
      END
