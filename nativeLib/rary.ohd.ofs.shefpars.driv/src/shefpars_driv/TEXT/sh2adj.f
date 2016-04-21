C  =====================================================================
C  pgm: SH2ADJ .. Adjust calendar date by adding date/time increment
C
C  use:     CALL SH2ADJ(KHAR,KHPOS,JYR,JMO,JDA,JHR,JMN,JSE,INC,II)
C
C  i/o: KHAR ...... last buffer char obtained (current char) - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o: JYR ....... year number (1753-2199), else unchanged - INT
C  i/o: JMO ....... month number (1-12), else unchanged - INT
C  i/o: JDA ....... day number (1-31), else unchanged - INT
C  i/o: JHR ....... hour number (0-24), else unchanged - INT
C  i/o: JMN ....... minute number (0-59), else unchanged - INT
C  i/o: JSE ....... second number (0-59), else unchanged - INT
C   in: INC ....... date or time increment (plus or minus) number - INT
C   in: II ........ date or time code for increment number - INT
C   in:               1 = minutes    4 = months     7 = seconds
C   in:               2 = hours      5 = years
C   in:               3 = days       6 = end-of-mons
C
C  rqd: SHERR,SHGCAL,SHGJUL,SHIYR,SHIMO,SHIJUL,SHIHR,SHIMN,SHISE,SHIEOM
C  =====================================================================
      SUBROUTINE SH2ADJ(KHAR,KHPOS,JYR,JMO,JDA,JHR,JMN,JSE,INC,II)

      EXTERNAL       SHERR,SHGCAL,SHGJUL
      EXTERNAL       SHIYR,SHIMO,SHIJUL,SHIHR,SHIMN,SHISE,SHIEOM

      CHARACTER*1   KHAR
      INTEGER       KHPOS,JYR,JMO,JDA,JHR,JMN,JSE,INC,II,JUL,ISTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2adj.f,v $
     . $',                                                             '
     .$Id: sh2adj.f,v 1.3 1998/04/07 19:08:20 page Exp $
     . $' /
C    ===================================================================
C

C                   II controls the type of time increment

      IF (KHPOS.NE.1 .AND. INC.NE.0) THEN

C                   Adjust seconds

        IF (II .EQ. 7) THEN
          CALL SHISE(JMN,JSE,INC)
          CALL SHIMN(JHR,JMN,0)
          CALL SHIHR(JDA,JHR,0)
          CALL SHGJUL(JUL,JYR,JMO,JDA)
          CALL SHIJUL(JUL,JYR,0)
          CALL SHGCAL(JUL,JYR,JMO,JDA)

C                   Adjust minutes

        ELSEIF (II .EQ. 1) THEN
          CALL SHIMN(JHR,JMN,INC)
          CALL SHIHR(JDA,JHR,0)
          CALL SHGJUL(JUL,JYR,JMO,JDA)
          CALL SHIJUL(JUL,JYR,0)
          CALL SHGCAL(JUL,JYR,JMO,JDA)

C                   Adjust hours

        ELSEIF (II .EQ. 2) THEN
          CALL SHIHR(JDA,JHR,INC)
          CALL SHGJUL(JUL,JYR,JMO,JDA)
          CALL SHIJUL(JUL,JYR,0)
          CALL SHGCAL(JUL,JYR,JMO,JDA)

C                   Adjust days

        ELSEIF (II .EQ. 3) THEN
          CALL SHGJUL(JUL,JYR,JMO,JDA)
          CALL SHIJUL(JUL,JYR,INC)
          CALL SHGCAL(JUL,JYR,JMO,JDA)

C                   Adjust months

        ELSEIF (II .EQ. 4) THEN
          CALL SHIMO(JYR,JMO,INC)

C                   Adjust years

        ELSEIF (II .EQ. 5) THEN
          CALL SHIYR(JYR,INC,ISTAT)
          IF (ISTAT .EQ. 2) CALL SHERR('E',39,KHPOS,KHAR)

C                   Adjust month for end-of-month

        ELSEIF (II .EQ. 6) THEN
          CALL SHIEOM(JYR,JMO,JDA,INC,ISTAT)
          IF (ISTAT .EQ. 1) CALL SHERR('E',38,KHPOS,KHAR)
          IF (ISTAT .EQ. 2) CALL SHERR('E',39,KHPOS,KHAR)

        ENDIF

      ENDIF

      RETURN
      END
