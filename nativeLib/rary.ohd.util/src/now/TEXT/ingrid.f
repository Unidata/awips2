C  =====================================================================
C  pgm: INGRID .. Initialize grid data buffer to all "-999"
C
C  use:     CALL INGRID(VALUE,NCOL,NROW,BUFF,LBUFF,ISTAT)
C
C   in: VALUE ....... initial value for every grid short integer - INT*2
C   in: NCOL ........ number of columns (short integers per row) - INT
C   in: NROW ........ number of rows - INT
C  out: BUFF(1) ..... integr values row after row initz to -999 - INT(2)
C   in: LBUFF ....... maximum dimension of "BUFF" - INT*4
C  out: ISTAT ....... status code: - INT
C  out:                 0 ... no errors
C  out:                 1 ... error such as buffer too small
C  =====================================================================
      SUBROUTINE INGRID(VALUE,NCOL,NROW,BUFF,LBUFF,ISTAT)

      INTEGER    NCOL,NROW,ISTAT
      INTEGER*4  LBUFF,ICOU,LIM
      INTEGER*2  BUFF(1),VALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/ingrid.f,v $
     . $',                                                             '
     .$Id: ingrid.f,v 1.1 1995/09/17 19:00:33 dws Exp $
     . $' /
C    ===================================================================
C

        LIM = NCOL*NROW
        ISTAT = 0
        IF( LIM .GT. LBUFF ) ISTAT = 1
        IF( LIM .GT. LBUFF ) LIM = LBUFF

        DO 100 ICOU=1,LIM
  100     BUFF(ICOU) = VALUE

      RETURN
      END
