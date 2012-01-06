C MODULE UPRIMO_PXPP
C  =====================================================================
C  pgm: UPRIMO_PXPP .. Initialize unit numbers for program "pxpp"
C
C  use:     CALL UPRIMO_PXPP()
C  =====================================================================
      SUBROUTINE UPRIMO_PXPP()

      INCLUDE 'common/ionum'

      CHARACTER*39  INSTMT,OTSTMT,PUSTMT
      CHARACTER*1   FM
      INTEGER       JSYS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pxpp/RCS/uprimo_pxpp.f,v $
     . $',                                                             '
     .$Id: uprimo_pxpp.f,v 1.4 2002/02/11 14:09:02 michaelo Exp $
     . $' /
C    ===================================================================
C

      DATA  FM     / 'F' /
      DATA  INSTMT / ' *** Enter input filename, TTY or Q:  ' /
      DATA  OTSTMT / ' *** Enter output filename, TTY or Q: ' /
      DATA  PUSTMT / ' *** Enter punch filename, TTY or Q:  ' /

C  Set standard units

        CALL UPINIO()

C  Get filenames for input, output, punch unit numbers;
C  Open them on units 9, 6, 8 respectively

        IN  = 5
        IPR = 6
        IPU = 7

        CALL UPRIMR(INSTMT,FM,9,IN)
          IF( IN .LE. 0 ) GO TO 800
CCC        CALL UPRIMW(OTSTMT,FM,6,IPR)
        CALL UPRIMW(OTSTMT,FM,98,IPR)
          IF( IPR .LE. 0) GO TO 800
        CALL UPRIMW(PUSTMT,FM,8,IPU)
          IF( IPU .LE. 0) GO TO 800

C  Set other unit numbers that may be involved (i.e. for debug stmts)

        JSYS = 3
        CALL UPRIMI(IN,IPR,IPU,IPR,IPR,JSYS)
          GOTO 900

  800 CALL EXIT

  900 RETURN

      END
C      INCLUDE 'uduntb'
