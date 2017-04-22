C  =====================================================================
C  pgm: SHFMOT .. Get the format type for output
C
C  use:     CALL SHFMOT(CMD,FTYP)
C
C   in: CMD ....... command or message to control operations - CHAR*12
C   in:               'INITIALIZE' ..... force read shefparm file
C   in:               'GET_VALUE' ...... get shefparm values
C   in:               'PUT_VALUE' ...... put shefpars values
C  i/o: FTYP ...... format type as follows - CHAR*1
C  i/o:               D for default format of "shout" routine
C  i/o:               B for binary file output (shefout file)
C  i/o:               1 for one line of text (up to 160 chars)
C  i/o:               2 for two lines of text (second line for quotes)
C   in: (file) .... sequential access file called "shefparm" - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFPARM',<number>)
C
C  rqd: SHPABG,SHSAVU
C  =====================================================================
      SUBROUTINE SHFMOT(CMD,FTYP)

            EXTERNAL       SHPABG,SHSAVU

      CHARACTER*12   CMD
      CHARACTER*2    KHFIND
      CHARACTER*1    FTYP,SVFTYP,K
      INTEGER        INITZ,LUNP,IERR

      SAVE       INITZ,SVFTYP

      DATA       INITZ,SVFTYP,KHFIND / 0, 'D', '*9' /

C                   Put format type char into save variable, or
C                   if first pass, get format type from "shefparm" file

        IF (CMD .EQ. 'PUT_VALUE   ') THEN
          INITZ  = 1
          SVFTYP = FTYP
        ELSEIF (CMD.EQ.'INITIALIZE  ' .OR. INITZ.EQ.0) THEN
          INITZ = 0
          CALL SHSAVU('G_SHEFPARM  ',LUNP)
          CALL SHPABG(LUNP,KHFIND,IERR)

          IF (IERR .EQ. 0) THEN
            INITZ = 1
            READ(LUNP,'(A1)',IOSTAT=IERR) K
           ELSE
            K = 'D'
          ENDIF

          SVFTYP = K
        ENDIF

C                   Get format type char from save variable

        IF (CMD .EQ. 'GET_VALUE   ') THEN
          FTYP = SVFTYP
        ENDIF

      RETURN
      END
