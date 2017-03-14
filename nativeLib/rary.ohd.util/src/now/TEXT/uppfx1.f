C  =====================================================================
C  pgm: UPPFX1 ..  Small rtn used by "UPPFIX" to check path, apend "/"
C
C  use:      CALL UPPFX1( pathnam, pathlen )
C
C  i/o: pathnam ....... input pathname, if bad set to "." - CHAR*(*)
C  i/o:                 (append a slash "/" at the end)
C  i/o: pathlen ....... length of pathname, incremented for slash - INT
C  =====================================================================
      SUBROUTINE UPPFX1(NMDIR,LNDIR)

      INTRINSIC     LEN
      CHARACTER*(*) NMDIR
      INTEGER       LNDIR,LEN,MAXLEN,MAXM2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/uppfx1.f,v $
     . $',                                                             '
     .$Id: uppfx1.f,v 1.1 1996/12/10 16:00:04 dws Exp $
     . $' /
C    ===================================================================
C

        MAXLEN = LEN(NMDIR)
        MAXM2  = MAXLEN - 2

        IF ( LNDIR.LT.1 .OR. LNDIR.GT.MAXM2 ) THEN
          LNDIR = 1
          NMDIR(LNDIR:LNDIR) = '.'
        ENDIF

        LNDIR = LNDIR+1
        NMDIR(LNDIR:LNDIR) = '/'

        NMDIR(LNDIR+1:MAXLEN) = ' '

      RETURN
      END
