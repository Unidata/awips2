C  =====================================================================
C  pgm: SH3TZ0 .. Get time zone adjustment and related hour number
C
C  use:     CALL SH3TZ0(KHAR,KHPOS,IHR,IMN,ISE)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: IHR ....... hour number as 12 or 24, else 0 for error - INT
C  out: IMN ....... hour number set to 0 - INT
C  out: ISE ....... second number set to 0 - INT
C
C  rqd: SH2ST1,SHSAVA,SHSAVS
C  =====================================================================
      SUBROUTINE SH3TZ0(KHAR,KHPOS,IHR,IMN,ISE)

      EXTERNAL       SH2ST1,SHSAVA,SHSAVS
 
      CHARACTER*1    KHAR,KHARX
      CHARACTER*4    KHTZ,TZ1(32)
      INTEGER        KHPOS,IHR,IMN,ISE,LADJ,LSVG,III
      INTEGER        NOFIA,IA1(32),IA2(32),MAXII,II,KHPOSX,NOFTZ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3tz0.f,v $
     . $',                                                             '
     .$Id: sh3tz0.f,v 1.4 2006/03/07 15:18:11 dws Exp $
     . $' /
C    ===================================================================
C

      DATA  NOFIA / 32 /
      DATA  TZ1   / 'Z', 'N','NS','ND', 'A','AS','AD', 'E','ES','ED',
     $                   'C','CS','CD', 'M','MS','MD', 'P','PS','PD',
     $                   'Y','YS','YD', 'L','LS','LD', 'H','HS','HD',
     $                   'B','BS','BD', 'J'                           /
      DATA  IA1   /   0, 210, 210, 210, 240, 240, 180, 300, 300, 240,
     $                   360, 360, 300, 420, 420, 360, 480, 480, 420,
     $                   540, 540, 480, 540, 540, 480, 600, 600, 600,
     $                   660, 660, 600,-480                           /
      DATA  IA2   /   0,   0,   0,   0,   1,   0,   0,   1,   0,   0,
     $                     1,   0,   0,   1,   0,   0,   1,   0,   0,
     $                     1,   0,   0,   1,   0,   0,   0,   0,   0,
     $                     1,   0,   0,   0                           /

        IMN  = 0
        ISE  = 0

C                   If current char position is in a message line

        IF (KHPOS .GT. 2) THEN

C                   Save current char and position in case no tz found,
C                    then get next letter string (time zone code)

          KHARX  = KHAR
          KHPOSX = KHPOS
          LADJ   = -9999
          LSVG   = 0
          IHR    = 0
          CALL SH2ST1(KHAR,KHPOS,NOFTZ,KHTZ)

C                   If 1 or 2 chars, loop thru tz list to get min offset

          IF (NOFTZ.EQ.1 .OR. NOFTZ.EQ.2) THEN
              MAXII = NOFIA
              II    = 0
   20         IF (II .GE. MAXII) GOTO 30
                II = II+1
                IF (KHTZ .EQ. TZ1(II)) THEN
                  MAXII = II
                  IHR   = 24
                  IF (KHTZ .EQ. 'Z   ') IHR = 12
                  LADJ = IA1(II)
                  LSVG = IA2(II)
                ENDIF
                GOTO 20
   30         CONTINUE
          ENDIF

C                   If not found; set defaults, revert to old khar-khpos

          IF (LADJ .EQ. -9999) THEN
              KHAR  = KHARX
              KHPOS = KHPOSX
              IHR   = 12
              LADJ  = 0
              LSVG  = 0
          ENDIF

C                   Else if no time on message line, set defaults

        ELSE

          IHR   = 12
          LADJ  = 0
          LSVG  = 0

        ENDIF

        CALL SHSAVA('P',III,LADJ)
        CALL SHSAVS('P',III,LSVG)

      RETURN
      END
