      SUBROUTINE WRITEARRAY(XOR,YOR,MAXX,MAXY,fname,lenf,
     *   user,lenus,datetimes,proc_flag,lenpf,real_mosaic,
     *   datetimev,maxval,vernum,irc)
      INTEGER       XOR, YOR, lenpf
      REAL          REAL_MOSAIC(MAXX,MAXY)
      INTEGER*2     MOSAIC(MAXX)
      character     user*10,proc_flag*8
      character     datetimes*20,datetimev*20
      CHARACTER*128 fname
    
      
      DO 100 J=1,MAXY
      DO  90 I=1,MAXX
        MOSAIC(I)=INT(REAL_MOSAIC(I,J))
   90 CONTINUE
C
      CALL write_xmrg(XOR,YOR,MAXX,MAXY,J-1,fname,lenf,
     *   user,lenus,datetimes,proc_flag,lenpf,mosaic,datetimev,maxval,
     *   vernum,irc)
  100 CONTINUE
      RETURN
      END
