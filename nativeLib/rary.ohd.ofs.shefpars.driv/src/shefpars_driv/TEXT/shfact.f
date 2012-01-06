C  =====================================================================
C  pgm: SHFACT .. Get PE conversion factor (read "shefparm" file)
C
C  use:     CALL SHFACT(CMD,KHAR,KHPOS,PARCOD,FACTOR)
C
C   in: CMD ....... command or message to control operations - CHAR*12
C   in:               'INITIALIZE' ..... force read shefparm file
C   in:               'GET_VALUE' ...... get shefparm value(s)
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: PARCOD .... up to 8-char parameter code - CHAR*8
C  out: FACTOR .... data conversion factor - DOUBLE PRECISION
C   in: (file) .... sequential access file called "shefparm" - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFPARM',<number>)
C
C  rqd: SHPABG,SHERR,SHSAVU
C  =====================================================================
      SUBROUTINE SHFACT(CMD,KHAR,KHPOS,PARCOD,FACTOR)

      EXTERNAL       SHPABG,SHERR,SHSAVU

      INTRINSIC      ICHAR
      INTEGER        ICHAR

      CHARACTER*12   CMD
      CHARACTER*1    KHAR,KH1,KH2
      CHARACTER*2    KHFIND
      CHARACTER*8    PARCOD
      DOUBLE PRECISION   FACTOR,RNUM,PECNVT(676)
      INTEGER        KHPOS,INITZ,ICHRA,II,NU1,NU2,IERR,LUNP

      SAVE       INITZ,ICHRA,PECNVT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shfact.f,v $
     . $',                                                             '
     .$Id: shfact.f,v 1.4 1998/07/22 12:32:47 page Exp $
     . $' /
C    ===================================================================
C

      DATA       INITZ,PECNVT,KHFIND / 0, 676*-9D0, '*1' /

C                   If first pass, get PECNVT array from "shefparm" file

        IF (CMD.EQ.'INITIALIZE  ' .OR. INITZ.EQ.0) THEN
          INITZ = 0
          ICHRA = ICHAR('A')
          CALL SHSAVU('G_SHEFPARM  ',LUNP)
          CALL SHPABG(LUNP,KHFIND,IERR)

          IF (IERR .EQ. 0) THEN
            INITZ = 1
            READ(LUNP,'(A1,A1,1X,G20.0)',IOSTAT=IERR) KH1,KH2,RNUM
  130       IF (IERR.NE.0 .OR. KH1.EQ.'*') GOTO 140
              NU1 = ICHAR(KH1) - ICHRA
              IF (NU1.GE.0 .AND. NU1.LE.25) THEN
                NU2 = ICHAR(KH2) - ICHRA
                IF (NU2.GE.0 .AND. NU2.LE.25) THEN
                  II = 26*NU1 + NU2 + 1
                  PECNVT(II) = RNUM
                ENDIF
              ENDIF
              READ(LUNP,'(A1,A1,1X,G20.0)',IOSTAT=IERR) KH1,KH2,RNUM
              GOTO 130
  140       CONTINUE
          ELSEIF (IERR .LT. 0) THEN
            CALL SHERR('E',71,KHPOS,KHAR)
            KHPOS = -1
          ENDIF

          IF (IERR .GT. 0) THEN
            CALL SHERR('E',77,KHPOS,KHAR)
            KHPOS = -1
          ENDIF
        ENDIF

C                   Get conversion factor, FACTOR, from PECNVT array

        IF (CMD .EQ. 'GET_VALUE   ') THEN
          FACTOR = 1D0
          IF (KHPOS .GT. 1) THEN
            KH1 = PARCOD(1:1)
            KH2 = PARCOD(2:2)
            II  = 26*(ICHAR(KH1)-ICHRA) + (ICHAR(KH2)-ICHRA) + 1
            IF (II.GE.1 .AND. II.LE.676) THEN
              FACTOR = PECNVT(II)
            ELSE
              CALL SHERR('E',29,KHPOS,KHAR)
            ENDIF
          ENDIF
          IF( FACTOR.LT.-1.5D0 ) CALL SHERR('E',62,KHPOS,KHAR)
        ENDIF

      RETURN
      END
