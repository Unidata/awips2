C  =====================================================================
C  pgm: PAGRID .. Get pathname of griddb file from dir, type, cal-date
C
C  use:     CALL PAGRID(CMD,PTH,DIR,TYP,YR,MO,DA,HR)
C
C   in: CMD ........... control command for type of filename: = CHAR*(*)
C   in:                   'orig' ... original name as (typ)mmddyyhhz
C   in:                   'good' ... new name as (typ)yyyymmddhhz
C   in:                   'bad' .... funky name as (typ)mmddyyyyhhz
C  out: PTH ........... stageii type pathname, (typ)mmddyyhhz - CHAR*(*)
C   in: DIR ........... directory of file pathname - CHAR*(*)
C   in: TYP ........... name describing type of file - CHAR*(*)
C   in: YR ............ 2 or 4 digit year number - INT
C   in: MO ............ month number (1 to 12) - INT
C   in: DA ............ day number (1 to 31) - INT
C   in: HR ............ hour number (0 to 23) - INT
C
C  rqd: KKTRIM,DDYCDL
C  =====================================================================
      SUBROUTINE PAGRID(CMD,PTH,DIR,TYP,YR,MO,DA,HR)

      INTRINSIC      LEN
      EXTERNAL       KKTRIM,DDYCDL

      CHARACTER*(*)  PTH,DIR,TYP,CMD
      CHARACTER*256  TMP
      INTEGER        YR,MO,DA,HR,LEN,LPTH,LTMP,YRX,MAX
      INTEGER        LX,LY,LDIR,LTYP,BDIR,BTYP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/pagrid.f,v $
     . $',                                                             '
     .$Id: pagrid.f,v 1.1 1998/07/06 11:29:19 page Exp $
     . $' /
C    ===================================================================
C

C                         Get length of strings
 
        CALL KKTRIM(DIR,BDIR,LDIR)
        CALL KKTRIM(TYP,BTYP,LTYP)
        LPTH = LEN(PTH)
        LTMP = LEN(TMP)

C                         Skip making string if it will be too long

        MAX = LDIR-BDIR + LTYP-BTYP + 14
        IF (MAX.GT.LPTH .OR. MAX.GT.LTMP) THEN
          PTH = ' '

C                         Else enter dir and "/" into temporary string

        ELSE
          TMP = DIR(BDIR:LDIR)
          LY = LDIR-BDIR+2
          TMP(LY:LY) = '/'

C                         Now enter type (data file prefix) into string

          LX = LY+1
          LY = LX+LTYP-BTYP
          TMP(LX:LY) = TYP(BTYP:LTYP)

C                         If CMD = 'o', enter date as mmddyyhh'z'
C                          else if CMD = 'b', enter as mmddyyyyhh'z'
C                          else by default enter as yyyymmddhh'z'
C                         Note, ddycdl uses a 90/10 year rule to get
C                          a 4 digit year number if it is only 2 digits

          LX = LY+1
          YRX = YR
          IF (CMD(1:1) .EQ. 'o') THEN
            YRX = YRX - (YRX/100)*100
            LY = LX+8
            WRITE(TMP(LX:LY),'(I2.2,I2.2,I2.2,I2.2,''z'')') MO,DA,YRX,HR
          ELSEIF (CMD(1:1) .EQ. 'b') THEN
            CALL DDYCDL(YRX,MO,DA)
            LY = LX+10
            WRITE(TMP(LX:LY),'(I2.2,I2.2,I4.4,I2.2,''z'')') MO,DA,YRX,HR
          ELSE
            CALL DDYCDL(YRX,MO,DA)
            LY = LX+10
            WRITE(TMP(LX:LY),'(I4.4,I2.2,I2.2,I2.2,''z'')') YRX,MO,DA,HR
          ENDIF

          PTH = TMP
        ENDIF

      RETURN
      END
