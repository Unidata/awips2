C  =====================================================================
C  pgm: KKSMLS .. Change all capital letters to smalls
C
C  use:     CALL KKSMLS(STRING)
C
C  i/o: STRING ... char string containg chars to be changed - CHAR*(*)
C  =====================================================================
      SUBROUTINE KKSMLS(STRING)

      INTRINSIC      ICHAR
      INTEGER        ICHAR

      INTRINSIC      LEN
      INTEGER        LEN

      INTRINSIC      CHAR
      CHARACTER*1    CHAR

      CHARACTER*(*)  STRING
      CHARACTER*1    KHAR,CAPA,CAPZ,SMLA
      INTEGER        II,JJ,DIFF

      PARAMETER( CAPA='A', CAPZ='Z', SMLA='a' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kksmls.f,v $
     . $',                                                             '
     .$Id: kksmls.f,v 1.1 1995/09/17 19:02:07 dws Exp $
     . $' /
C    ===================================================================
C



        DIFF = ICHAR(SMLA)-ICHAR(CAPA)

        JJ = LEN(STRING)
        II = 1
  100   IF( II .GT. JJ ) GO TO 110
          KHAR = STRING(II:II)
          IF( KHAR.GE.CAPA .AND. KHAR.LE.CAPZ ) THEN
            STRING(II:II) = CHAR( ICHAR(KHAR)+DIFF )
          END IF
          II = II+1
           GO TO 100
  110   CONTINUE



      RETURN
      END
