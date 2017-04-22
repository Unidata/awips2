C  =====================================================================
C  pgm: SHPDEC .. Decode and update the parameter codes
C
C  use:     CALL SHPDEC(KHAR,KHPOS,CODP)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: CODP ...... probability code - REAL
C
C  rqd: SHFACT,SHPROB,SHSEND,SHDURA,SHTSCD,SHEXCD,SHSAVJ,SHSAVN,SHSAVP
C
C  cmt: Codes *1 and *2 (PE and Duration) codes are required in SHEFPARM
C  =====================================================================
      SUBROUTINE SHPDEC(KHAR,KHPOS,CODP)

      EXTERNAL       SHFACT,SHPROB,SHSEND,SHDURA,SHTSCD,SHEXCD,SHSAVJ
      EXTERNAL       SHSAVN,SHSAVP

      CHARACTER*1    KHAR
      CHARACTER*8    PARCOD
      INTEGER        KHPOS,NFLAG,ISEND,ICODD,III
      REAL               CODP
      DOUBLE PRECISION   FACTOR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shpdec.f,v $
     . $',                                                             '
     .$Id: shpdec.f,v 1.3 1996/07/11 20:00:44 dws Exp $
     . $' /
C    ===================================================================
C

C                   Set defaults, get parameter code (PARCOD)

        ICODD = 0
        NFLAG = 0
        CODP  = -1.0

        CALL SHSAVP('G',III,PARCOD)

        IF (KHPOS.NE.1 .AND. PARCOD.NE.'        ') THEN

C                   Validate the parameter codes

          CALL SHFACT('GET_VALUE   ',KHAR,KHPOS,PARCOD,FACTOR)

          IF (KHPOS .GE. 0) THEN
            CALL SHSEND('GET_VALUE   ',KHAR,KHPOS,PARCOD,ISEND,NFLAG)
            CALL SHDURA('GET_VALUE   ',KHAR,KHPOS,PARCOD,ISEND,ICODD)
            CALL SHTSCD('GET_VALUE   ',KHAR,KHPOS,PARCOD)
            CALL SHEXCD('GET_VALUE   ',KHAR,KHPOS,PARCOD)
            CALL SHPROB('GET_VALUE   ',KHAR,KHPOS,PARCOD,CODP)

            CALL SHSAVP('P',III,PARCOD)
            CALL SHSAVN('P',III,NFLAG)
            CALL SHSAVJ('P',III,ICODD)
          ENDIF

        ENDIF

      RETURN
      END
