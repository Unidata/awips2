C  =====================================================================
C  pgm: SHOPFL .. Open file using status (delete old file if required)
C
C  use:     CALL SHOPFL(IE,IU,LUXX,NUXXDF,NUXX,FNAM,STUS,FMT,MESSG)
C
C  i/o: IE ...... error status, 0=no error; must enter as 0 - INT
C   in: IU ...... unit number to output error messg in this rtn - INT
C  out: LUXX .... unit number obtained for given file, else stndrd- INT
C   in: NUXXDF .. default (standard) unit number if no file given - INT
C   in: NUXX .... unit number to be used if file is given - INT
C   in: FNAM .... pathname of file to open, else use stnd (NUXXDF) - INT
C   in: STUS .... expected status of file 'NEW' or 'OLD' - CHAR*(*)
C   in: FMT ..... format of file 'FORMATTED' or 'UNFORMATTED' - CHAR*(*)
C   in: MESSG ... output message for error statement in here - CHAR*(*)
C  =====================================================================
      SUBROUTINE SHOPFL(IE,IU,LUXX,NUXXDF,NUXX,FNAM,STUS,FMT,MESSG)

      INTEGER        IE,IU,LUXX,NUXXDF,NUXX
      CHARACTER*(*)  STUS,FMT,MESSG
      CHARACTER*128  FNAM
      LOGICAL        EXS

        LUXX = NUXXDF
        IF (IE.EQ.0 .AND. FNAM.NE.' ') THEN
          LUXX = NUXX
          INQUIRE(FILE=FNAM,IOSTAT=IE,EXIST=EXS)
          IF (IE.EQ.0) OPEN(LUXX,FILE=FNAM,FORM=FMT,IOSTAT=IE)
          IF (IE.EQ.0 .AND. .NOT.EXS .AND. STUS.EQ.'OLD') IE = -1
          IF (IE.EQ.0 .AND. EXS .AND. STUS.EQ.'NEW') THEN
            CLOSE(LUXX,IOSTAT=IE,STATUS='DELETE')
            IF (IE .EQ. 0) THEN
              OPEN(LUXX,FILE=FNAM,FORM=FMT,IOSTAT=IE,STATUS='NEW')
            ENDIF
          ENDIF
          IF (IE .NE. 0) THEN
            WRITE(IU,'(''ABORT: ('',I6,'') cannot '',A)') IE,MESSG
            WRITE(IU,'('' file: '',A)') FNAM
          ENDIF
        ENDIF

      RETURN
      END
