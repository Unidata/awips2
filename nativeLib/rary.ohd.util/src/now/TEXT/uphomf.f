C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: UPHOMF .. make full pathname for the given file in the $HOME dir
C
C  use:     CALL UPHOMF(FIL,PTH)
C
C   in: FIL ....... name of file to be in the home dir
C  out: PTH ....... pathname of the given file in the user's home dir
C
C  rqd: env variable   - HOME
C
C  cmt: NOTE, this routine uses a system dependent subroutine!!!
C  =====================================================================
      SUBROUTINE UPHOMF(FIL,PTH)

      EXTERNAL         GETENV
      CHARACTER*(*)    FIL,PTH
      CHARACTER*150    FILX,HOMX
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/uphomf.f,v $
     . $',                                                             '
     .$Id: uphomf.f,v 1.4 2002/02/11 13:58:22 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      PTH=' '
      MPTH=LEN(PTH)    
      LFIL=LENSTR(FIL)
C
C  GET HOME DIRECTORY
      HOMX=' '
      CALL GET_APPS_DEFAULTS ('HOME',4,HOMX,LHOMX)
      HOMX(LHOMX+1:LHOMX+1)=' '
C
      LPTH=LHOMX+1+LFIL
C
      IF (HOMX.NE.' '.AND.FILX.NE.' '.AND.LPTH.LE.MPTH) THEN
         PTH=HOMX(1:LHOMX)//'/'//FIL(1:LFIL)
         ENDIF
C
      RETURN
C
      END
