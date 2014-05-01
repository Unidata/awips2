      SUBROUTINE write_xmrg(XOR,YOR,MAXX,MAXY,IROW,fname,lenf,
     *   user,lenus,datetimes,proc_flag,lenpf,mosaic,datetimev,
     *   maxval,vernum,irc)
C
C  subroutine to write data to xmrg file
C
C    Add ".temp" to filename while it is being written to.
c    File is then renamed by dropping ".temp" after it is closed.
c    This is done to prevent access to file while it is being
c    written to.
C
c   calling subroutine: save_merged_RFCW                                      
c   subroutines called: system      
c
      INTEGER       XOR, YOR
      INTEGER*2     MOSAIC(MAXX)
      character user*10,us*10,proc_flag*8,pf*8
      character datetimes*20,dts*20,datetimev*20,dtv*20
      CHARACTER*128 FILENAME,fname
      CHARACTER*256 LINE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/whfs/dev/HP/whfs/source/utilfunc/mpe_util/src/RCS/write_xmrg.f,v $
     . $',                                                             '
     .$Id: write_xmrg.f,v 1.1 2001/12/13 20:12:53 pst Exp $
     . $' /
C    ===================================================================
C
C
      irc = 0
      lenfn = index(fname,' ') -1 
      if(lenfn.eq.-1) lenfn=128
      len=lenf
      if(lenfn.lt.lenf) len=lenfn
      FILENAME = fname(1:len) // '.temp'
      LNTEMP = len+5
c
      us='          '
      us = user(1:lenus)
      pf='        '
      pf = proc_flag(1:lenpf)
      dts = '                    '
      dts = datetimes(1:19)
      dtv = '                    '
      dtv = datetimev(1:19)
c
      IF (IROW .EQ. 0) THEN
        OPEN(33,FILE=FILENAME(1:LNTEMP),FORM='UNFORMATTED',
     $       STATUS='UNKNOWN',err=998,iostat=ios)
        WRITE(33,err=999) XOR, YOR, MAXX, MAXY
        WRITE(33,err=999) us, dts, pf, dtv, maxval, vernum
      ENDIF
C
      WRITE(33,err=999) MOSAIC
C
      IF (IROW .EQ. MAXY-1) THEN
        CLOSE(33,err=998,iostat=ios)
        LINE = 'mv '//FILENAME(1:LNTEMP)//' '//fname(1:len)
        CALL system(LINE)
      ENDIF
C
      RETURN
  998 continue
      irc = ios
      return
  999 continue
      irc = -1
      return
      END
