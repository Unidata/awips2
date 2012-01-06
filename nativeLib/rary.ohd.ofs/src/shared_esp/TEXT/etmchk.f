C MODULE ETMCHK
C-----------------------------------------------------------------------
C   THIS ROUTINE WILL :
C   1) SET CO DATE FOR START OF RUN
C   2) CALCULATE START TIME OF RUN IN LST,
C   3) CALCULATE SHIFT NECESSARY TO MAKE RUN TIMES COMPATIBLE
C      WITH IDTE,
C   4) CALCULATE IDARUN,
C   5) CONVERT WINDOWS TO LST AND SHIFT,
C   6) CHECK IF START OF WINDOW LT START OF RUN,
C   7) CHECK IF END OF WINDOW LT START OF WINDOW,
C   8) CALCULATE END OF RUN AS END OF LAST WINDOW,
C   9) CALCULATE LDARUN.
C
C   THIS ROUTINE WAS ORIGINALLY WRITTEN BY GERALD N. DAY .
C
      SUBROUTINE ETMCHK
C
      LOGICAL LBUG
C
      INCLUDE 'common/etime'
      INCLUDE 'common/esprun'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcsegc'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/elimit'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fcrunc'
      include 'common/egentr'
C
      DIMENSION SBNAME(2),OLDOPN(2),NDAYS(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/etmchk.f,v $
     . $',                                                             '
     .$Id: etmchk.f,v 1.6 2003/03/14 18:53:58 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME / 4hETMC,4hHK  /, ILT/2hLT/, DEBUG/4hETIM/
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** ETMCHK ENTERED)
C
      LBUG=.FALSE.
      IF(IFBUG(DEBUG).EQ.0) GO TO 20
      LBUG=.TRUE.
   20 CONTINUE
C
C   SET LDACPD,LHRCPD=0
C
cew Do not set these here anymore because they are now set from
cew technique in eiluvo
cew      LDACPD=0
cew      LHRCPD=0
C
C   GET CO BASED ON START TIME IN INTERNAL TIME OF
C   FORECAST PROGRAM.

      if(igen.eq.0) then
         CALL ECOBCK(IJDFC,IHFC,IER)
      endif
C
C   CALCULATE START TIME OF RUN IN LST - WHICH IS THE
C   SAME AS THE INTERNAL TIME FOR ESP.
C
      CALL EFCLST(IJDFC,IHFC,IJDLST,IHLST)
C
C   CALCULATE SHIFT NECESSARY TO MAKE START TIME COMPATIBLE
C   WITH MINDT.
C
Cew Always use a 24 hour mindt  deleted several lines of code here

       IDTE=24
C
      CALL ESHFT(IJDLST,IHLST,IDTE,IMOVE)
      ISHIFT=IMOVE
C
C   CALCULATE IDARUN
      CALL MDYH1(IJDLST,IHLST,KM,KD,KY,KH,100,0,TZC)
C
      IF(KH.LT.24) GO TO 152
      KH=0
      KD=KD+1
      NUMDA=NDAYS(KM)
      IF(KM.EQ.2.AND.KY.EQ.(4*(KY/4))) NUMDA=NUMDA+1
      IF(KD.LE.NUMDA) GO TO 152
      KD=1
      KM=KM+1
      IF(KM.LE.12) GO TO 152
      KM=1
      KY=KY+1
  152 CONTINUE
      KY=IHYR
      IF(KM.GE.10) KY=KY-1
      KDLOOP=KD
C
C   IF IDARUN IS 2/29, AND THIS IS NOT A LEAP YEAR,
C   SET IDARUN = 2/28.
C
      IF(KM.NE.2.OR.KD.NE.29) GO TO 155
      IF(KY.EQ.(4*(KY/4))) GO TO 155
      KDLOOP=28
  155 CALL FCTZC(100,0,TZC)
      CALL JULDA(IDARUN,IHRRUN,KM,KDLOOP,KY,KH,100,0,TZC)
C
C   CONVERT WINDOWS FROM INTERNAL TIME TO LOCAL STANDARD TIME
C
      DO 200 I=1,NUMWIN
      CALL EFCLST(IWJD(I),IWH(I),KDX,KHX)
      IWJD(I)=KDX
      IWH(I)=KHX
      CALL EFCLST(LWJD(I),LWH(I),KDX,KHX)
      LWJD(I)=KDX
      LWH(I)=KHX
C
C  CALCULATE SHIFT NEEDED FOR WINDOWS
C
      CALL ESHFT(IWJD(I),IWH(I),IDTE,IMOVE)
      CALL ESHFT(LWJD(I),LWH(I),IDTE,IMOVE)
C
C   CHECK IF START OF WINDOW IS LESS THAN START TIME.
C
      CALL FDATCK(IWJD(I),IWH(I),IJDLST,IHLST,ILT,ISW)
      IF(ISW.EQ.0) GO TO 160
      IWJD(I)=IJDLST
      IWH(I)=IHLST
      WRITE(IPR,605)
  605 FORMAT(1H0,10X,35H**WARNING** START OF WINDOW WAS SET,
     1 16H TO START OF RUN)
      CALL WARN
C
C   CHECK IF END OF WINDOW IS LESS THAN START OF WINDOW
C
  160 CALL FDATCK(LWJD(I),LWH(I),IWJD(I),IWH(I),ILT,ISW)
      IF(ISW.EQ.0) GO TO 200
      WRITE(IPR,610)
  610 FORMAT(1H0,10X,28H**ERROR** END OF WINDOW WAS ,
     1 25HLESS THAN START OF WINDOW)
      IWJD(I)=0
      CALL ERROR
      GO TO 999
  200 CONTINUE
C
C   FIND WINDOW WITH THE LAST END DATE. SET RUN END DATE
C   EQUAL TO WINDOW END.
C
      IEND=1
      IF(NUMWIN.EQ.1) GO TO 260
      DO 250 I=2,NUMWIN
      CALL FDATCK(LWJD(I),LWH(I),LWJD(IEND),LWH(IEND),ILT,ISW)
      IF(ISW.EQ.0) IEND=I
  250 CONTINUE
  260 LJDLST=LWJD(IEND)
      LHLST=LWH(IEND)
c
c   compute number of conditional months -
c   make certain non-leap years will not require an additional
c   month.
c
      call mdyh1(ijdlst,ihlst,im1,id1,iy1,ih1,100,0,tzc)
      call mdyh1(ljdlst,lhlst,im2,id2,iy2,ih2,100,0,tzc)

      if( (ih1.eq.24) .and. (id1.ge.ndays(im1)) ) then
         if(id1.ne.28) then
            im1 = im1 + 1
            if(im1.gt.12) then
               im1 = 1
               iy1 = iy1+1
            endif
         endif
      endif
c
      ileap=0
      ncm=(im2+iy2*12)-(im1+iy1*12)+1
      if(id2.ge.ndays(im2)) then
         mm=im1
         my=iy1
         do 265 i=1,ncm
            if(mm.eq.2) then
               if(i.eq.ncm) then
                  if(id2.eq.29) then
                     ileap=1
                  endif
               elseif(my.eq.(4*(my/4))) then
                  ileap=1
                  go to 267
               endif
            endif
            mm=mm+1
               if(mm.gt.12) then
                  mm=mm-12
                  my=my+1
               endif
  265    continue
  267    continue
      endif
      ncm=ncm+ileap
C
C   CALCULATE LAST DAY OF RUN - LDARUN
C
      LCOND=LJDLST-IJDLST
      KY=LHYR
      IF(KM.GE.10) KY=KY-1
  270 KDLOOP=KD
      IF(KM.NE.2.OR.KD.NE.29) GO TO 275
      IF(KY.EQ.(4*(KY/4))) GO TO 275
      KDLOOP=28
  275 CALL FCTZC(100,0,TZC)
      CALL JULDA(IDLOOP,IHLOOP,KM,KDLOOP,KY,KH,100,0,TZC)
      LDARUN=IDLOOP+LCOND
      LHRRUN=LHLST
      CALL MDYH1(LDARUN,LHRRUN,LM,LD,LY,LH,100,0,TZC)
      IF(LM.GE.10) LY=LY+1
      IF(LY.LE.LHYR) GO TO 280
      KY=KY-1
      GO TO 270
  280 CONTINUE

cew added check to be sure that idarun < ldarun
      if(idarun .ge. ldarun) then
        write(ipr,905) KM,KDLOOP,KY, lm, ld, ly
905     format('**ERROR**  START OF RUN IS THE SAME AS OR AFTER THE',
     +          'END OF THE RUN.'/ '   THE START OF RUN IS ',
     +          i2,'/',i2,'/',i4,' AND THE END OF THE RUN IS ',
     +          i2,'/',i2,'/',i4)
      endif
C
C
      IF(.NOT.LBUG) GO TO 999
      WRITE(IODBUG,910) IJDFC,IHFC,IJDLST,IHLST
  910 FORMAT(1H0,10X,24HIJDFC,IHFC,IJDLST,IHLST=,5X,4I10)
C
      WRITE(IODBUG,920) ISHIFT
  920 FORMAT(1H0,10X,7HISHIFT=,5X,I5)
C
      WRITE(IODBUG,930) IDARUN,IHRRUN,LDARUN,LHRRUN
  930 FORMAT(1H0,10X,28HIDARUN,IHRRUN,LDARUN,LHRRUN=,5X,4I10)
C
C
  999 CONTINUE
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(2)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
C
      END
