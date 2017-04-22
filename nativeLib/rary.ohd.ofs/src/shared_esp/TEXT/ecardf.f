C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE ECARDF
C-----------------------------------------------------------------------
C   THIS ROUTINE READS DATA FROM CARD IMAGE FILES
C   THIS ROUTINE WAS WRITTEN BY EDWIN WELLES.
C
      SUBROUTINE ECARDF(D,LD,TSESP,LOC,NPDT,IDLOOP,LJDCON,KNTYR,NYRS,
     1 IER)
C
      LOGICAL LBUG
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'clbcommon/crwctl'
      INCLUDE 'common/fctime'
      INCLUDE 'common/eswtch'
      INCLUDE 'common/esprun'
      INCLUDE 'common/etsunt'
C
      DIMENSION SBNAME(2),OLDOPN(2),STAID(3),D(1),TSESP(1)
C
      integer unitno, am1, am2, idt, kmo,kyr, dir_lngth

      real afac, mfac

      character*80   dirnam
      character*112  filnam
      character*32   tsname
      character*12   oformat
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ecardf.f,v $
     . $',                                                             '
     .$Id: ecardf.f,v 1.6 2002/02/11 20:27:56 dws Exp $
     . $' /
C    ===================================================================
C



C     Initialize for this routine...
      filnam=' '
      dirnam=' '
      tsname=' '
      unitno=0


      DATA SBNAME/4HECAR,4HDF  /,BLANK/4H    /,DEBUG/4HETSR/
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** ECARDF ENTERED)
C
      LBUG=.FALSE.
      IF(IFBUG(DEBUG).EQ.0) GO TO 20
      LBUG=.TRUE.
   20 CONTINUE
C
      IER=0
      CALL MDYH1(IDA,24,KMO,IDUM1,KYR,IDUM2,100,0,TZC)
c
      IF(IDA.GT.IDLOOP) GO TO 100
C
c
C  the unit number position is set to -99
c  in espinit to indicate the first
c time around
c skip cardlo stuff if not first time around
      if(tsesp(loc+13).ne.-99)  goto 100

C   FIRST DAY OF LOOP - MUST CALL CARDLO


        dtype=tsesp(loc+52)

cew change to getting calb_area_ts_dir from Apps_defaults
cew and not from the file

c        write(dirnam,24) (tsesp(loc+i), i = 21, 40)
c  24    format(20a4)

cew get the default directory for calibration card data
      call get_apps_defaults('calb_area_ts_dir',16,dirnam, length)

        write(tsname,25) (tsesp(loc+i), i = 41, 48)
  25    format(8a4)
cew        dir_lngth=tsesp(loc+20)
        dir_lngth=length


cew add the pre directory if the preadj technique is on
cew (on = 1 off = 0)
        if (dir_lngth.gt.0) then
          if(preadj .eq. 1) then
               filnam=dirnam(1:dir_lngth)//'/pre/'//tsname
          else
               filnam=dirnam(1:dir_lngth)//'/'//tsname
          endif
        else
           if(preadj .eq. 1) then
               filnam='/pre/'//tsname
          else
               filnam=tsname
          endif
        endif


      IF(JHSS.EQ.0.AND.JASS.EQ.0) GO TO 75
      IF(IEPASS.EQ.4) GO TO 75
C
      IF(KNTYR.LE.NYRS) GO TO 60
      JDAY=LDARUN
      GO TO 85
C
C   CALCULATE THE LAST MONTH OF HISTORICAL AND/OR ADJUSTED LOOP
C
   60 JMO=KMO-1
      JYR=KYR+1
      IF(JMO.GT.0) GO TO 70
      JMO=12
      JYR=KYR
   70 CALL FCTZC(100,0,TZC)
      CALL JULDA(JDAY,JHR,JMO,1,JYR,24,100,0,TZC)
      IF(JDAY.GT.LJDCON) GO TO 80
   75 JDAY=LJDCON
   80 IF(JDAY.GT.LDARUN) JDAY=LDARUN
   85 CALL MDYH1(JDAY,LHRRUN,JMO,IDUM1,JYR,IDUM2,100,0,TZC)
C
      IF(LBUG) WRITE(IODBUG,910) KMO,KYR,JMO,JYR
  910 FORMAT(1H0,10X,31HCARDLO CALLED, KMO,KYR,JMO,JYR=,4I5)
C
C
      CALL FDCODE(DTYPE,UNITS,DIM,MSG,NPDT,TSC,NADD,IERR)
cew
              CALL CARDLO(kmo, kyr,jyr,jmo, m1, y1, m2, y2,
     +                    units, out_units, filnam, dtype, idt, staid,
     +                    desc, unitno, oformat, mfac, afac, am1, am2,
     +                    ierror )
c
cew return info to tsesp array depending on file type id
cew ie card 1 field 5 of espinit

      tsesp(loc+13)=unitno
      tsesp(loc+14)=am1
      tsesp(loc+15)=am2
      tsesp(loc+16)=mfac
      tsesp(loc+17)=afac
      tsesp(loc+18)=idt
      read(oformat,26) (tsesp(loc+i), i = 49, 51)
  26  format(3a4)
C

      IF(IERROR.EQ.0) GO TO 120
      IERROR=0
      IER=1
      GO TO 999
C
C   READ THE DATA
C
  100 CONTINUE
      idt=tsesp(loc+18)
      unitno=tsesp(loc+13)
      write(oformat,26) (tsesp(loc+i), i = 49, 51)
      am1=tsesp(loc+14)
      am2=tsesp(loc+15)
      mfac=tsesp(loc+16)
      afac=tsesp(loc+17)
C
  120 LENGTH=(24/IDT)*NPDT*31
C
      LASTD=LD+LENGTH-1

cew save last d for blending if this is a genr ts

cew add in fdcode call here because first call is skipped
cew second etc times around
      dtype=tsesp(loc+52)
      CALL FDCODE(DTYPE,UNITS,DIM,MSG,NPDT,TSC,NADD,IERR)

      IF(LBUG) WRITE(IODBUG,920) KMO,KYR
  920 FORMAT(1H0,10X,23HRDFILE CALLED, KMO,KYR=,2I5)
C
      CALL CARDRD(1, unitno, oformat, am1, am2, mfac,
     +            afac, idt, kmo, kyr, d(ld),
     +             msg, ierror )
      IF(IERROR.EQ.0) GO TO 130
      WRITE(IPR,600)
  600 FORMAT(1H0,10X,'**ERROR** PRECEEDING ERROR OCCURRED DURING READ',
     +       ' OF TS:')
      write(ipr,601) (tsesp(loc+i), i = 41, 48),kmo,kyr
  601    format(20x,8a4,5x,i2,'/',i4)
      IER=1
      IERROR=0
      CALL ERROR
      GO TO 999
C
  130 IF(LBUG) WRITE(IODBUG,930) (D(I),I=LD,LASTD)
  930 FORMAT(11X,10F10.2)
C
C
  999 CONTINUE
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
      RETURN
      END
