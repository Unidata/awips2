C$PRAGMA C (SHCURD)
C  =====================================================================
C  pgm: SHYEAR .. Get current date (may be set for testing in SHEFPARM)
C
C  use:     CALL SHYEAR(CMD,CURCN,CURYR,CURMO,CURDA)
C
C   in: CMD ....... command or message to control operations - CHAR*12
C   in:               'I' ..... force read shefparm file
C   in:               'G' ..... get shefparm values
C   in:               'P' ..... put shefparm values
C  i/o: CURCN ..... current or default century number (17-20) - INT
C  i/o: CURYR ..... current or default 2-digit year (0-99) - INT
C  i/o: CURMO ..... current or default month (1-12) - INT
C  i/o: CURDA ..... current or default day (1-31) - INT
C   in: (file) .... sequential access file called "shefparm" - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFPARM',<number>)
C
C  rqd: SHPABG,SHSAVU
C  rqd: SHCURD
C
C  cmt: The current year, month, and day come from "SHCURD" which uses
C  cmt:  the "C" routines "time" and "localtime".
C  =====================================================================
      SUBROUTINE SHYEAR(CMD,CURCN,CURYR,CURMO,CURDA)

Cfan  SHCURD taken out of external statement to satisfy pgf90 - jul 2001
      EXTERNAL       SHPABG,SHSAVU

      CHARACTER*(*)  CMD
      CHARACTER*1    CMDX
      CHARACTER*2    KHFIND
      INTEGER        CUCN,CUYR,CUMO,CUDA,LUNP,IERR,INITZ
      INTEGER        CURCN,CURYR,CURMO,CURDA

      SAVE       INITZ,CUCN,CUYR,CUMO,CUDA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shyear.f,v $
     . $',                                                             '
     .$Id: shyear.f,v 1.5 2002/02/11 21:15:37 dws Exp $
     . $' /
C    ===================================================================
C

      DATA       INITZ,KHFIND / 0, '*8' /
      DATA       CUCN,CUYR,CUMO,CUDA / 0, 0, 0, 0 /

        CMDX = CMD(1:1)

C                   Put cur date values in save variables, or
C                   if first pass, get cur date from "shefparm" file

        IF (CMDX .EQ. 'P') THEN
          INITZ = 1
          CUCN  = CURCN
          CUYR  = CURYR
          CUMO  = CURMO
          CUDA  = CURDA
        ELSEIF (CMDX.EQ.'I' .OR. INITZ.EQ.0) THEN
          INITZ = 0
          CALL SHSAVU('G_SHEFPARM  ',LUNP)
          CALL SHPABG(LUNP,KHFIND,IERR)

          IF (IERR .EQ. 0) THEN
            INITZ = 1
            READ(LUNP,'(I2,I2,I3,I3)',IOSTAT=IERR) CUCN,CUYR,CUMO,CUDA
          ELSE
            CUCN = 0
          ENDIF

          IF (CUCN.LT.17 .OR. CUCN.GT.20) CUCN = 0
          IF (CUYR.LT.0  .OR. CUYR.GT.99) CUCN = 0
          IF (CUMO.LT.1  .OR. CUMO.GT.12) CUCN = 0
          IF (CUDA.LT.1  .OR. CUDA.GT.31) CUCN = 0

          IF (CUCN .EQ. 0) THEN

C                   Get current local date during first pass thru rtn
C                   (*** NOTE, it is assumed that the year number
C                    is four digit, any replacement routine for a
C                    particular platform MUST return four digits)

              CALL SHCURD(CUYR,CUMO,CUDA)

              CUCN = CUYR/100
              CUYR = CUYR - 100*CUCN

          ENDIF

        ENDIF

        IF (CMDX .EQ. 'G') THEN
          CURCN = CUCN
          CURYR = CUYR
          CURMO = CUMO
          CURDA = CUDA
        ENDIF

      RETURN
      END
