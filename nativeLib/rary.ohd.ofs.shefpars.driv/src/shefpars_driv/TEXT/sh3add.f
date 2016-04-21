C  =====================================================================
C  pgm: SH3ADD .. Adjust for correct duration code
C
C  use:      CALL SH3ADD(KHAR,KHPOS,IDUR)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: IDUR ...... corrected duration code - INT
C
C  rqd: SHERR,SHSAVI,SHSAVJ,SHSAVP
C  =====================================================================
      SUBROUTINE SH3ADD(KHAR,KHPOS,IDUR)

      EXTERNAL       SHERR,SHSAVI,SHSAVJ,SHSAVP

      CHARACTER*1    KHAR
      CHARACTER*8    PARCOD
      INTEGER        KHPOS,ICODD,IDCODD,IDUR,III
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3add.f,v $
     . $',                                                             '
     .$Id: sh3add.f,v 1.4 1999/04/26 12:02:26 page Exp $
     . $' /
C    ===================================================================
C

        IDUR = 0

        IF (KHPOS .NE. 1) THEN

          CALL SHSAVJ('G',III,ICODD)
          IF (ICODD .EQ. 5003) THEN
              CALL SHSAVI('G',III,IDCODD)
              IF (IDCODD .EQ. 5000) THEN
                CALL SHERR('E',32,KHPOS,KHAR)
              ELSE
                IDUR = IDCODD
              ENDIF
          ELSE
              IDUR = ICODD
          ENDIF

          CALL SHSAVP('G',III,PARCOD)

C           If warnings are to be issued for particular bad duration
C            codes, it should go here (ex. if PARCOD(1:2) .EQ. 'PC'
C            then check if PARCOD(3:3) is correct else put out warning).

        ENDIF

      RETURN
      END
