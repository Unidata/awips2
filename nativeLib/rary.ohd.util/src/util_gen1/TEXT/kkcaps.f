C  =====================================================================
C  pgm: KKCAPS .. Change all small letters to capitals
C
C  use:     CALL KKCAPS(STRING)
C
C  i/o: STRING ... char string containg chars to be changed - CHAR*(*)
C  =====================================================================
      SUBROUTINE KKCAPS(STRING)

      INTRINSIC      ICHAR
      INTEGER        ICHAR

      INTRINSIC      LEN
      INTEGER        LEN

      INTRINSIC      CHAR
      CHARACTER*1    CHAR

      CHARACTER*(*)  STRING
      CHARACTER*1    KHAR,SMLA,SMLZ,CAPA
      INTEGER        II,JJ,DIFF

      PARAMETER( SMLA='a', SMLZ='z', CAPA='A' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kkcaps.f,v $
     . $',                                                             '
     .$Id: kkcaps.f,v 1.1 1995/09/17 19:02:03 dws Exp $
     . $' /
C    ===================================================================
C



        DIFF = ICHAR(SMLA)-ICHAR(CAPA)

        JJ = LEN(STRING)
        II = 1
  100   IF( II .GT. JJ ) GO TO 110
          KHAR = STRING(II:II)
          IF( KHAR.GE.SMLA .AND. KHAR.LE.SMLZ ) THEN
            STRING(II:II) = CHAR( ICHAR(KHAR)-DIFF )
          END IF
          II = II+1
           GO TO 100
  110   CONTINUE



      RETURN
      END
