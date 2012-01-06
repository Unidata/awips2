C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: FTN_STD_ERR .. Get FORTRAN standard err unit from .Apps_defaults
C
C  use:     CALL FTN_STD_ERR()
C
C  out: (FTN_STD_ERR) ... the standard error unit num is returned - INT
C  out:                   (default is to return -1 meaning no unit)
C
C  rqd: GET_APPS_DEFAULTS
C
C  cmt: No attemp is made to check for bad char data from the
C  cmt:  "apps-defaults" file other than limiting output to -1 thru 99.
C  =====================================================================
      INTEGER FUNCTION FTN_STD_ERR()

      INTRINSIC   ICHAR
      INTEGER     ICHAR

      INTEGER        NU,LN,IZRO
      CHARACTER*128  STD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/ftn_std_err.f,v $
     . $',                                                             '
     .$Id: ftn_std_err.f,v 1.2 2002/02/11 16:33:05 dws Exp $
     . $' /
C    ===================================================================
C



        IZRO = ICHAR('0')

        CALL GET_APPS_DEFAULTS('fortran_stderr',14,STD,LN)

        NU = -1
        IF(LN.EQ.1) NU = ICHAR(STD(1:1))-IZRO
        IF(LN.EQ.2) NU = 10*(ICHAR(STD(1:1))-IZRO)+ICHAR(STD(2:2))-IZRO

        IF( NU.LT.-1 .OR. NU.GT.99 ) NU = -1

        FTN_STD_ERR = NU



      RETURN
      END
