C  =====================================================================
C  pgm: SH3DT0 .. Convert next 4 to 8 digits to calendar date
C
C  use:     CALL SH3DT0(KHAR,KHPOS,IYR,IMO,IDA)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: IYR ....... year number (1753-2199) and may be altered - INT
C  out: IMO ....... month number (1-12), else unchanged - INT
C  out: IDA ....... day number (1-31), else unchanged - INT
C
C  rqd: SH2NU2,SHYEAR,SHERR,SHCDAT,SH4DT0
C
C  cmt: Possible date combinations are: ccyymmdd, yymmdd, mmdd
C  =====================================================================
      SUBROUTINE SH3DT0(KHAR,KHPOS,IYR,IMO,IDA)

      EXTERNAL       SH2NU2,SHYEAR,SHERR,SHCDAT,SH4DT0

      CHARACTER*1    KHAR
      INTEGER        KHPOS,ICN,IYR,IMO,IDA,KOD,II,J1,J2,J3,J4
      INTEGER        CURCN,CURYR,CURMO,CURDA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dt0.f,v $
     . $',                                                             '
     .$Id: sh3dt0.f,v 1.3 1998/04/07 19:10:14 page Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .GT. 2 ) THEN

          ICN = -1
          IYR = -1
          IMO = -1
          IDA = -1

          J1  = -1
          J2  = -1
          J3  = -1
          J4  = -1

          KOD = 1
          IF (KOD .EQ. 1) CALL SH2NU2(KHAR,KHPOS,KOD,J1)
          IF (KOD .EQ. 1) CALL SH2NU2(KHAR,KHPOS,KOD,J2)
          IF (KOD .EQ. 1) CALL SH2NU2(KHAR,KHPOS,KOD,J3)
          IF (KOD .EQ. 1) CALL SH2NU2(KHAR,KHPOS,KOD,J4)

          IF (KHPOS .NE. 1) THEN

C                   Make sure 4 digits are given; set date
C                   If year or century num is missing, get from shyear

            IF (J1.EQ.-1 .OR. J2.EQ.-1) THEN
              CALL SHERR('E',15,KHPOS,KHAR)
            ELSE
              IF (J3 .EQ. -1) THEN
                IMO = J1
                IDA = J2
                CALL SHYEAR('G',CURCN,CURYR,CURMO,CURDA)
                CALL SH4DT0(CURCN,CURYR,CURMO,CURDA,ICN,IYR,IMO,IDA)
              ELSEIF (J4 .EQ. -1) THEN
                IYR = J1
                IMO = J2
                IDA = J3
                CALL SHYEAR('G',CURCN,CURYR,CURMO,CURDA)
                CALL SH4DT0(CURCN,CURYR,CURMO,CURDA,ICN,IYR,IMO,IDA)
              ELSE
                ICN = J1
                IYR = J2
                IMO = J3
                IDA = J4
                IYR = 100*ICN + IYR
              ENDIF

C                   Check date for good numbers

              CALL SHCDAT(IYR,IMO,IDA,II)
              IF (II .GT. 0) CALL SHERR('E',16,KHPOS,KHAR)
            ENDIF

          ENDIF

        ENDIF

      RETURN
      END
