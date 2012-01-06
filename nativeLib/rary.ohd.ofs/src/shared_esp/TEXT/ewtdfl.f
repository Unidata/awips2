C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (CLN_FILE)
C MODULE EWTDFL
C-----------------------------------------------------------------------
C
C
      SUBROUTINE EWTDFL(D,LOCD,EXT,TSID,DTYPE,IDT,NVPDT,
     1 KNTYR,kntmo,NYRS,ihzero,ljdcon,IERR)
C
C THIS ROUTINE IS USED TO WRITE TS DATA TO THE ESP
C SCRATCH/PERMANENT TIME SERIES FILES.
C
C   THIS ROUTINE WAS ORIGINALLY WRITTEN BY GERALD N. DAY .
C
      LOGICAL LBUG
      character csegid*8,ctsid*8,cdtype*4,cidt*2,type*2,cdate*6,
     1 ctime*6, tempfile*80,condfile*80,histfile*80,direspts*128,
     2 filename*128,tmpstr*64, dot, cfgid*8, ccgid*8
      integer tmplen, dirlen, filelen, nmcount, msngflag, firstmo,
     1 lastmo
C
      INCLUDE 'common/eperm'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/escrat'
      INCLUDE 'common/eswtch'
      INCLUDE 'common/elimit'
      INCLUDE 'common/fctime'
      INCLUDE 'common/esprun'
      INCLUDE 'common/etsdat'
      INCLUDE 'common/eunit'
      INCLUDE 'common/etime'
      INCLUDE 'common/etsunt'
      INCLUDE 'common/fcsegn'
      INCLUDE 'upagex'
C
      DIMENSION D(1),EXT(18),TSID(2),SBNAME(2),OLDOPN(2),segidf(3),
     1 TSIDF(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ewtdfl.f,v $
     . $',                                                             '
     .$Id: ewtdfl.f,v 1.8 2004/01/29 17:59:58 hank Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME/4HEWTD,4HFL  /
      DATA BLANK/4H    /,DEBUG/4HETSF/,DEBUG2/4HETS /
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** EWTDFL ENTERED)                                  
C
      LBUG=.FALSE.
      IF(IFBUG(DEBUG2).EQ.0) GO TO 15
      LBUG=.TRUE.
   15 CONTINUE
C
      IBUG=IFBUG(DEBUG)
C
c   set initial data record on file
c
      irec=2
C
c   set maximum length of record equal to the length of the
c   tsdat array.  Set a flag for writing a missing month 
c   to zero.
c
      mxlrc=ltsdat
      msngflag=0
c
      ILD=LOCD
C
C   FIND MONTH,YEAR TO BE WRITTEN
C
      CALL MDYH1(IDA,24,IM,IDUM1,IY,IDUM2,100,0,TZC)
C
C   BRING IN EXTERNAL LOCATION INFO
C
      TSIDF(1)=EXT(1)
      TSIDF(2)=EXT(2)
      DTYPEF=EXT(3)
      IDTF=EXT(4)
C
      DO 16 I=1,3
      segidf(I)=EXT(4+I)
   16 CONTINUE
      ISCRT=EXT(8)
      iunitc=ext(9)
      iunith=ext(10)
      iunita=ext(11)
      imo=ext(14)
      iyr=ext(15)
c
      IF(IUNITC.NE.0) GO TO 200
C
C   FIRST MONTH OF WRITE
C
C
      imo=im
      iyr=iy
      if(imo.eq.3) then
C Hank Herr (2003-09-08): I'm not sure why this line used to use idarun.
C   But, idarun causes problems when it calculates jrec for the first time
C   which is used far below in the writing of tsdat to the file (line ~450).
C   Specifically, jrec was starting at 2 during a run for 3/1 when it should
C   have been 3.
C         call mdyh1(idarun,24,imonth,iday,idum1,idum2,100,0,tzc)
         call mdyh1(ida,24,imonth,iday,idum1,idum2,100,0,tzc)
         if( (imonth.eq.2) .and. (iday.eq.28) ) then
            imo=imonth
         endif
      endif
c
c  get the path of the directory in which the file will be written
c
      tmplen=9
      tmpstr='espts_dir'
      tmpstr(10:10)=char(0)
      call get_apps_defaults(tmpstr,tmplen,direspts,dirlen)

cew added espotdir technique value to the directory name
      if(ioutdirlen .gt. 0) then
        direspts(dirlen+1:dirlen+1) = '/'
        direspts=direspts(1:dirlen+1)//esp_out_dir(1:ioutdirlen)
        dirlen = dirlen+ioutdirlen+1
      endif

      direspts(dirlen+1:dirlen+1) = '/'
      dirlen = dirlen+1

cew make sure that path name is not too big
      if(dirlen.gt.120) then
        write(ipr,208)
208     format('** ERROR ** THE ESP OUTPUT DIRECTORY EXCEEDS 120 ',
     +       'CHARACTERS.  THE PATH TO THE ESP FILES MUST BE ',
     +       'SHORTENED.')
        ierr=1
        call error
        goto 999
       endif


c
      if(lbug) write(iodbug,22) iscrt,iperm
   22 format(1x,'iscrt , iperm =',2i5)
c
      if(iscrt.eq.0.and.iperm.eq.1)  then
         continue
      else 
         write(ipr,599)
  599    format(1h0,10x,'**WARNING** SCRATCH DIRECTORY NO LONGER USED')
c         direspts(dirlen+1:dirlen+8)='SCRATCH/'
c                 dirlen = dirlen + 8
      endif
c
c  gather filename components
c
      write(csegid,17) (segidf(j),j=1,2)
      write(ctsid, 17) (tsidf(j),j=1,2)
      write(cdtype,18) dtypef
      write(cidt,  19) idt
      type = 'CS' 
cew these two lines do not seem to have been implemented
c     write(cdate, 20) mod(now(3),100), now(2), now(1)
c     write(ctime, 21) now(4), now(5)/100
      dot = '.'
   17 format(2a4)
   18 format(a4)
   19 format(i2.2)
   20 format(i2.2,i2.2,i2.2)
   21 format(i4.4,i2.2)
C
C     Create the filename by manually concatenating strings.  Do this so
C     embedded spaces are handled correctly.
C
      tempfile=' '
      tempfile=csegid//dot//ctsid//dot//cdtype//dot//cidt//
     1 dot//type
c
c  for now, we are leaving out the date stamp from the file name
c    1 dot//type//dot//cdate//dot//ctime
c
c  take out the spaces
c
      j = 1
      condfile=' '
      do 40 i = 1,80
          if (tempfile(i:i) .ne. ' ') then
             condfile(j:j) = tempfile(i:i)
c
c  save position of simulation type (pos 27 in tempfile)
c
             if (i .eq. 27) isave = j
             j = j+1
          endif
   40 continue
c
      call ulenth(condfile,80,tmplen)
      filename=direspts(1:dirlen)//condfile(1:tmplen)
      filelen=dirlen+tmplen
      filename(filelen+1:filelen+1)=char(0)

      if(lbug) write(ipr,*) 'in eewtdfl:  filename=',filename

      call cln_file(filename, filelen, istat)
      if(istat .eq. 2) then
         write(ipr,602) filename
  602    format(1h0,10x,'**ERROR** NO WRITE PERMISSIONS ON THE FILE'/
     1   2x,A128)
         ierr=1
         call error
         go to 999
      elseif(istat.gt. 1) then
         write(ipr,601) filename
  601    format(1h0,10x,'**ERROR** REMOVING EXISTING CONDITIONAL FILE'/
     1   2x,A128)
         ierr=1
         call error
         go to 999
      endif
c
      call opfile(filename,'ESPTS ','DIRECT','UNKNOWN',
     1 'UNFORMATTED',496,iunitc,ier)
c
      if(ier.ne.0) then
         write(ipr,600) filename
  600    format(1h0,10x,'**ERROR** OPENING CONDITIONAL FILE'/
     1   2x,A128)
         ierr=1
         call error
         go to 999
      endif
c     
      nepts=nepts+1
      if(nepts.gt.maxets) then
         write(ipr,605) maxets
  605    format(1h0,10x,'**ERROR** MAXIMUM NUMBER OF OPEN ESP TIME ',
     1   'SERIES HAS BEEN EXCEEDED',i5)
         ierr=1
         call error
         go to 999
      else
         ieunit(nepts)=iunitc
      endif
c
      if(jhss.eq.1) then
C        The filename has already been set above.  Just replace ".CS"
C        with ".HS".
         type = 'HS'
         histfile = condfile
         histfile(isave:isave+1)=type
         filename(isave+dirlen:isave+dirlen+1)=type

         call cln_file(filename, filelen, istat)
         if(istat.gt. 1) then
            write(ipr,611) filename
  611       format(1h0,10x,'**ERROR** REMOVING EXISTING HISTORICAL ',
     1      'FILE',2x,A128)
            ierr=1
            call error
            go to 999
         endif
c
         call opfile(filename,'ESPTS ','DIRECT','UNKNOWN',
     1   'UNFORMATTED',496,iunith,ier)
         if(ier.ne.0) then
            write(ipr,610) filename
  610       format(1h0,10x,'**ERROR** OPENING HISTORICAL FILE'/
     1      2x,a128)
            ierr=1
            call error
            go to 999
         endif
c
         nepts=nepts+1
         if(nepts.gt.maxets) then
            write(ipr,605) maxets
            ierr=1
            call error
            go to 999
         else
            ieunit(nepts)=iunith
         endif 
      endif
c
c   adjusted simulation option is not activated - open
c   statement should go here.
c
      ext(9)=iunitc+.01
      ext(10)=iunith*jhss+.01
      ext(11)=iunita*jass+.01
      ext(12)=nyrs+.01
      ext(13)=ncm+.01
      ext(14)=im+.01
      ext(15)=iy+.01
      ext(16)=.01
c
      do 30 i=1,ltsdat
      tsdat(i)=0.0
   30 continue
c
c   fill the tsdat array with the file header information
      tsdat(1)=1.01
      tsdat(2)=segidf(1)
      tsdat(3)=segidf(2)
      tsdat(4)=tsidf(1)
      tsdat(5)=tsidf(2)
      tsdat(6)=dtypef
      tsdat(7)=idtf+.01
      tsdat(8)=3.01
      CALL FDCODE(DTYPE,UNITS,DIM,MSG,NPDT,TSCALE,NADD,IERR)
      if(ierr.ne.0) then
         write(ipr,630)
  630    format(1h0,10x,'**ERROR** INVALID DATATYPE')
         call error
         go to 999
      endif
      tsdat(9)=units
      tsdat(10)=now(1)
      tsdat(11)=now(2)
      tsdat(12)=now(3)
      tsdat(13)=now(4)
      tsdat(14)=now(5)
      tsdat(15)=im+.01
      tsdat(16)=iy+.01
      tsdat(17)=idarun+.01
      tsdat(18)=ldarun+.01
      tsdat(19)=ijdlst+.01
      tsdat(20)=ihlst+.01
      tsdat(21)=ljdlst+.01
      tsdat(22)=lhlst+.01
      tsdat(23)=nyrs+.01
      tsdat(24)=ncm+.01
      tsdat(25)=nlstz
      tsdat(26)=noutds
      tsdat(27)=irec+.01
      tsdat(28)=dim
      tsdat(29)=tscale
      tsdat(30)=sgdscr(1)
      tsdat(31)=sgdscr(2)
      tsdat(32)=sgdscr(3)
      tsdat(33)=sgdscr(4)
      tsdat(34)=sgdscr(5)
      tsdat(35)=xlat
      tsdat(36)=xlong
      write(cfgid,17) (ifgid(i),i=1,2)
      read(cfgid,17) (tsdat(36+i), i=1,2)
      write(ccgid,17) (icgid(i),i=1,2)
      read(ccgid,17) (tsdat(38+i), i=1,2)
      read(pusrid,17) (tsdat(40+i), i=1,2)
      read(condfile,640) (tsdat(42+i), i=1,20)
c   skip over the hcl file name and comment for now      
c   and move on
cew   prsf flag
       read(prsf,18) tsdat(63)
     
c
      write(unit=iunitc,rec=1) tsdat
      if(iunith.ne.0) then
         tsdat(8)=1.01
         read(histfile, 640) (tsdat(42+i), i=1,20)
         
cew  put in a new ending date for the historical simulation 
c           tsdat(21)= ljdlst+.01
           tsdat(21)=ijdlst+365.01
c           tsdat(23)=nyrs+.01
c  becuase of the way esp runs if the number of conditional months
c  is less than 12 you will get an inomplete last month
c  in the historical simulation.  therefore set the  number of years
c  accordingly.
           if(ncm .lt. 12) tsdat(23)=nyrs - 1 + 0.01
c           tsdat(24)=ncm+.01
           tsdat(24)= 12 + 0.01
          
         write(unit=iunith,rec=1) tsdat
         
      endif
 640  format(20A4)
  200 continue
c
c     write out a month of missing data first if necessary to 
c     deal with non leap years during a run that starts on 
c     2/28 at hour 24.  
c
      firstmo = 0
      lastmo = 0
      if(iepass .le. 2) then
         if( (kntmo .eq. 1) .and. (kntyr .eq. 1) ) then
             firstmo = 1
         endif
         if(lda .ge. ldarun) then
             lastmo = 1
         endif
      else
         if(kntmo .eq. 1) then
             firstmo = 1
         endif
         if (lda .ge. ljdcon) then
             lastmo = 1
         endif
      endif
      
C      if( (firstmo .eq. 1) .and. (im .ne. imo) ) then
C          im = imo
C          msngflag = 1
C      endif
c
  205 if(iepass.eq.3) then
         iunit=iunitc
      elseif(iepass.eq.1) then
         iunit=iunith
      elseif(iepass.eq.2) then
         iunit=iunita
      endif
c
c   compute nmos to skip on file
c
      if(iepass.eq.3) then
         nm=ncm
         ltm=ncm
      else
         nm=12
         ltm=mod(ncm,12)
      endif
      nmos=(kntyr-1)*nm+(iy*12+im)-(iyr+kntyr-1)*12-imo+1
c
      ihr1=ihzero+idt
      j1=ild+((ida-idadat)*24/idt+(ihr1-1)/idt)*nvpdt
      j2=ild+((lda-idadat)*24/idt+(lhr/idt))*nvpdt-1
c
      LENGTH=(24/IDT)*NVPDT*31
      NTVAL=LENGTH*(NMOS-1)+1
      NRECS=NTVAL/MXLRC
      NXWD=NTVAL-(NRECS*MXLRC)
      JREC=IREC+NRECS
      if(lbug) write(iodbug,*)'lenth=',length,'ntval=',ntval,'nmos=',     
     +nmos,'mxlrc=',mxlrc,'nrecs=',nrecs,'irec=',irec
      IF(NXWD.NE.0) GO TO 300
      NXWD=MXLRC
      JREC=JREC-1
c 
  300 IF(LBUG) WRITE(IODBUG,940) IM,IY,JREC,NXWD
  940 FORMAT(1H0,10X,16HIM,IY,JREC,NXWD=,4I5)                           
C
      IF(NXWD.NE.1) GO TO 312
      DO 310 I=1,LTSDAT
      TSDAT(I)=-999.0
  310 CONTINUE
      GO TO 315
  312 read(unit=iunit,rec=jrec) tsdat
C
  315 KOUNT=LENGTH
      LEFT=MXLRC-NXWD+1
      IF(LEFT.LT.LENGTH) KOUNT=LEFT
C
C This is a debug line... don't uncomment:
C      write (*,*)'FLAG msngflag=',msngflag,' iepass=',iepass
C
      do 320 i=1,kount
      ld=ild+i-1
      if((ld.ge.j1.and.ld.le.j2) .and. (msngflag .ne. 1)) then
         tsdat(nxwd)=d(ld)
      else
         tsdat(nxwd)=-999.0
      endif
      nxwd=nxwd+1
  320 continue
c
      write(unit=iunit,rec=jrec) tsdat
C
      IF(LBUG) WRITE(IODBUG,950) TSDAT
C
      LENGTH=LENGTH-KOUNT
      IF(LENGTH.EQ.0) GO TO 330
      NXWD=1
      ild=ild+KOUNT
      JREC=JREC+1
      GO TO 315
c
c     Check for the rare instance in which a month of missing
c     data has been written before the actual data begins. In this case
c     the real month of data now needs to be written.
c
  330 if( (im.eq.imo) .and. (msngflag.eq.1) ) then
         im = im + 1
         msngflag = 0
         go to 205
      endif 
c
c     Make sure that we write a record for the last month, just
c     in case this is the last month in the trace but we 
c     haven't reached ncm yet.
c
      nmcount = im - imo + 1 + 12*(iy - iyr - (kntyr - 1))
      if ((nmcount .eq. ltm - 1) .and. (lastmo .eq. 1)) then
         im = im + 1
         msngflag = 1
         go to 205
      endif
C
  999 CONTINUE
      IF(LBUG) WRITE(IODBUG,950) (D(I),I=j1,j2)
  950 FORMAT(11X,10F10.2)
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
C
      END
