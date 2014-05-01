C  =====================================================================
C  pgm: SHPCOD .. Get parameter code from message
C
C  use:     CALL SHPCOD(KHAR,KHPOS)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C
C  rqd: SHGETK,SH2BLA,SHERR,SHSAVP
C
C  cmt: Routine will NOT process if current char is "end-of-line".
C  cmt: "KHAR" must be guaranteed not blank if "KHPOS" is less than 2.
C  =====================================================================
      SUBROUTINE SHPCOD(KHAR,KHPOS)

      EXTERNAL       SHGETK,SH2BLA,SHERR,SHSAVP

      CHARACTER*8    PARCOD
      CHARACTER*1    KHAR
      INTEGER        KHPOS,NOC,NN,III
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shpcod.f,v $
     . $',                                                             '
     .$Id: shpcod.f,v 1.3 1998/04/07 19:14:39 page Exp $
     . $' /
C    ===================================================================
C

        CALL SH2BLA(KHAR,KHPOS,NOC)

C                   If current char position is in a message line

        IF (KHPOS .GT. 2) THEN

          CALL SHSAVP('G',III,PARCOD)

C                   Loop while char is not blank nor is output str full

          NN = 0
   30     IF(KHPOS.LE.2.OR.NN.GE.7.OR.KHAR.EQ.' '.OR.KHAR.EQ.'/')GOTO 40

            IF (KHAR.GE.'A' .AND. KHAR.LE.'Z') THEN
              NN = NN+1
              PARCOD(NN:NN) = KHAR
              CALL SHGETK(KHAR,KHPOS)
            ELSEIF (NN.EQ.3 .OR. NN.EQ.4 .OR. NN.EQ.6) THEN
              IF (KHAR.GE.'1' .AND. KHAR.LE.'9') THEN
                NN = NN+1
                PARCOD(NN:NN) = KHAR
                CALL SHGETK(KHAR,KHPOS)
              ELSE
                CALL SHERR('E',29,KHPOS,KHAR)
              ENDIF
            ELSE
              CALL SHERR('E',29,KHPOS,KHAR)
            ENDIF

            GOTO 30
   40     CONTINUE

          CALL SHSAVP('P',III,PARCOD)

          CALL SH2BLA(KHAR,KHPOS,NOC)

          IF (KHPOS.GT.2 .AND. KHAR.NE.'/') THEN
            IF (NOC .EQ. 0) CALL SHERR('E',54,KHPOS,KHAR)
          ENDIF

          IF (KHPOS.NE.1 .AND. PARCOD(2:2).EQ.'        ') THEN
            CALL SHERR('E',64,KHPOS,KHAR)
          ENDIF

        ENDIF

      RETURN
      END
