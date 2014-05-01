C  =====================================================================
C  pgm: SHMAXE .. Get max num of errors/message set (read "parm" file)
C
C  use:     CALL SHMAXE(CMD,KHAR,KHPOS,NOFER)
C
C   in: CMD ....... command or message to control operations - CHAR*12
C   in:               'INITIALIZE' ..... force read shefparm file
C   in:               'GET_VALUE' ...... get shefparm value(s)
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: NOFER ..... Max number of errors before stopping shef decode
C   in: (file) .... sequential access file called "shefparm" - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFPARM',<number>)
C
C  rqd: SHPABG,SHSAVU,SHERR
C  =====================================================================
      SUBROUTINE SHMAXE(CMD,KHAR,KHPOS,NOFER)

      EXTERNAL       SHPABG,SHSAVU,SHERR

      CHARACTER*12   CMD
      CHARACTER*2    KHFIND
      CHARACTER*1    KHAR
      INTEGER        KHPOS,NOFER,NUMBR,LUNP,IERR,INITZ

      SAVE       INITZ,NUMBR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shmaxe.f,v $
     . $',                                                             '
     .$Id: shmaxe.f,v 1.4 1998/07/22 12:34:12 page Exp $
     . $' /
C    ===================================================================
C

      DATA       INITZ,KHFIND,NUMBR / 0, '**', 500 /

C                   If first pass, get NOFER from "shefparm" file

        IF (CMD.EQ.'INITIALIZE  ' .OR. INITZ.EQ.0) THEN
          INITZ = 0
          CALL SHSAVU('G_SHEFPARM  ',LUNP)
          CALL SHPABG(LUNP,KHFIND,IERR)

          IF (IERR .EQ. 0) THEN
            INITZ = 1
            READ(LUNP,'(I4)',IOSTAT=IERR) NUMBR
          ENDIF

          IF (IERR .GT. 0) THEN
            CALL SHERR('E',77,KHPOS,KHAR)
            KHPOS = -1
          ENDIF
        ENDIF

        IF (CMD .EQ. 'GET_VALUE   ') THEN
          NOFER = NUMBR
        ENDIF

      RETURN
      END
