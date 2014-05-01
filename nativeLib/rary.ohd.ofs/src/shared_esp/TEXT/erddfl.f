C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE ERDDFL
C
      SUBROUTINE ERDDFL(D,LOCD,TSESP,LOC,TSID,DTYPE,IDT,
     1 NVPDT,IDLOOP,IHZERO,KNTYR,NYRS,IER)
c
C   THIS SUBROUTINE IS USED TO READ TIME SERIES DATA FROM THE
C   ESP SCRATCH/PERMANENT TIME SERIES FILES.
C
C
C   THIS SUBROUTINE WAS ORIGINALLY WRITTEN BY GERALD N. DAY .
C
      LOGICAL LBUG
      logical*4 exists

c Added RTi, July 2003
c Added the vsmonth character variable to hold the 3 char
c month representation.
      character csegid*8,ctsid*8,cdtype*4,cidt*2,
     1 cdate*6,ctime*6, tempfile*80,condfile*80,histfile*80,
     2 direspts*128,filename*128,tmpstr*64, dot, vsmonth*3,
     3 filenamevs*128,cyear*4,cday*2,vsfile*80,filenamecs*128,
     4 filenamehs*128, filenamehsvs*128,appstr*64, hsvsmonth*3,
     5 hsvsfile*80
      integer cslen, dirlen, dirlensv, filelen, isave, igensav,
     +     vslen,applen,filelencs,filelenvs,hsvslen,
     +     ijdlst_temp, ihlst_temp
c  Hank Herr (11/19/01) -- removed the integers scrtlen and isscrt.
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/esprun'
      INCLUDE 'common/eunit'
      INCLUDE 'common/etsdat'
      INCLUDE 'common/elimit'
      INCLUDE 'common/escrat'
      INCLUDE 'common/egentr'
      INCLUDE 'common/eswtch'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'
      INCLUDE 'common/eperm'
      INCLUDE 'common/efdate'
      INCLUDE 'common/where'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/etime'
      include 'common/etsunt'
C
      DIMENSION TSESP(1),D(1),TSID(2),TSIDF(2)
      DIMENSION SBNAME(2),OLDOPN(2),IDATE(5)
      dimension segidf(3),itimec(5),itimeh(5)

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/erddfl.f,v $
     . $',                                                             '
     .$Id: erddfl.f,v 1.8 2004/09/22 14:06:27 edwin Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME/4HERDD,4HFL  /,BLANK/4H    /,ILT/2HLT/,IGT/2HGT/
      DATA DEBUG/4HETSF/,DEBUG2/4HETS /
C
      IOLDOP=IOPNUM
      IOPNUM=0

      DO 10 I=1,2
       OLDOPN(I)=OPNAME(I)
   10  OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900  FORMAT(1H0,17H** ERDDFL ENTERED)
C
      LBUG=.FALSE.
      IF(IFBUG(DEBUG2).EQ.0) GO TO 15
       LBUG=.TRUE.
   15 CONTINUE
C
      IBUG=IFBUG(DEBUG)

c  set initial data record on file
c
      irec=2
c
c   set maximum length of record equal to the length of the
c   tsdat array.
c
      mxlrc=ltsdat
c
      IER=0
      LD=LOCD
      IDL=IDLOOP
C
C   FIND MONTH,YEAR NEEDED
C
      CALL MDYH1(IDA,24,IM,IDUM1,IY,IDUM2,100,0,TZC)
C
C   BRING IN EXTERNAL LOCATION INFO
C
      TSIDF(1)=TSESP(LOC+13)
      TSIDF(2)=TSESP(LOC+14)
      DTYPEF=TSESP(LOC+15)
      IDTF=TSESP(LOC+16)
      DO 20 I=1,3
       segidf(I)=TSESP(LOC+16+I)
   20 CONTINUE
      IDEL=TSESP(LOC+20)
      iunitc=tsesp(loc+21)
      iunith=tsesp(loc+22)
      iunita=tsesp(loc+23)
      ntracef=tsesp(loc+24)
      ncmf=tsesp(loc+25)
      imof=tsesp(loc+26)
      iyrf=tsesp(loc+27)
      itoff=tsesp(loc+28)

      if(lbug) write (iodbug,19919) (segidf(j),j=1,2),
     +(tsidf(j),j=1,2),dtypef,idtf
19919 format('** in erddfl ** tsesp=',2a4,'tsidf=',2a4,'dtypef=',a4,
     +'idtf=',i2)
C
      IF(iunitc.NE.0) GO TO 310
C
C   first month of read, must find and open file
c
       applen=9
       appstr='espts_dir'
       appstr(10:10) = char(0)
       call get_apps_defaults(appstr,applen,direspts,dirlen)
c
c
cew add espindir technique value to the directory name
       if(indirlen .gt. 0) then
         direspts(dirlen+1:dirlen+1) = '/'
         direspts=direspts(1:dirlen+1)//esp_in_dir(1:indirlen)
         dirlen = dirlen+indirlen+1
       endif

       direspts(dirlen+1:dirlen+1) = '/'
       dirlen = dirlen+1


cew make sure that path name is not too big
       if(dirlen.gt.120) then
         write(ipr,208)
208      format('** ERROR ** THE ESP TIME SERIES DIRECTORY EXCEEDS ',
     +       '120 CHARACTERS.  THE PATH TO THE ESP FILES MUST BE ',
     +       'SHORTENED.')
         ierr=1
         call error
         goto 999
       endif
c
c  gather filename components
c
       write(csegid,22) (segidf(j),j=1,2)
       write(ctsid, 22) (tsidf(j),j=1,2)
       write(cdtype,23) dtypef
       write(cidt,  24) idt

       dot = '.'
   22  format(2a4)
   23  format(a4)
   24  format(i2.2)
   25  format(i2.2,i2.2,i2.2)
   26  format(i4.4,i2.2)
   27  format(i4)
   28  format(i2.2)


C     Create the filename by manually concatenating strings.  Do this so
C     embedded spaces are handled correctly.

      tempfile=' '
      tempfile=csegid//dot//ctsid//dot//cdtype//dot//cidt//
     1 dot//'CS'

c  take out the spaces
c
      j = 1
      condfile=' '
      do 30 i = 1,80
        if (tempfile(i:i) .ne. ' ') then
           condfile(j:j) = tempfile(i:i)
c
c  save position of simulation type (pos 27 in tempfile)
c   but it may be different (less then 27) if spaces
c   have been removed in front of it
c
           if (i .eq. 27) isave = j
           j = j+1
        endif
   30  continue

c
       call ulenth(condfile,80,cslen)

c make the cs file name
        filenamecs=direspts(1:dirlen)//condfile(1:cslen)
        filelencs=dirlen+cslen


c Modified by RTi July 2003
c Here the file and directory is determined. If it is
c a verification run then the Mon, Day, and Year of the
c carryover which needs to be appended to the filename
c is determined and added.
      if(igen.EQ.1 .or. ncstor .gt. 0) then

cew put the shift back in so the dates will match with the original input
        ihlst_temp  = ihlst-ishift
        ijdlst_temp = ijdlst

      if (ihlst_temp .gt. 24) then
       ihlst_temp=ihlst_temp - 24
       ijdlst_temp=ijdlst_temp +1
      endif

cew        call mdyh1(ijdlst,ihlst,ijmonth,ijday,ijyear,ijhour,
        call mdyh1(ijdlst_temp,ihlst_temp,ijmonth,ijday,ijyear,ijhour,
     +            timezn, NOUTDS,icode)
c
        if(ijmonth.EQ.1) then
          vsmonth='Jan'
        else if(ijmonth.EQ.2) then
          vsmonth='Feb'
        else if(ijmonth.EQ.3) then
          vsmonth='Mar'
        else if(ijmonth.EQ.4) then
          vsmonth='Apr'
        else if(ijmonth.EQ.5) then
          vsmonth='May'
        else if(ijmonth.EQ.6) then
          vsmonth='Jun'
        else if(ijmonth.EQ.7) then
          vsmonth='Jul'
        else if(ijmonth.EQ.8) then
          vsmonth='Aug'
        else if(ijmonth.EQ.9) then
          vsmonth='Sep'
        else if(ijmonth.EQ.10) then
          vsmonth='Oct'
        else if(ijmonth.EQ.11) then
          vsmonth='Nov'
        else
          vsmonth='Dec'
        endif
c
c Now the year and day values are converted to char
        write(cyear,27) ijyear
        write(cday,28) ijday

c      Create the filename by manually concatenating strings.  Do this so
C     embedded spaces are handled correctly.
C
        tempfile=' '
        tempfile=csegid//dot//ctsid//dot//cdtype//dot//cidt//
     1   dot//'VS'//dot//cyear//vsmonth//cday
c
c  take out the spaces
c
        j = 1
        vsfile=' '
        do 33 i = 1,80
          if (tempfile(i:i) .ne. ' ') then
            vsfile(j:j) = tempfile(i:i)
            j = j+1
          endif
   33    continue
c
        call ulenth(vsfile,80,vslen)

        filenamevs=direspts(1:dirlen)//vsfile(1:vslen)
        filelenvs=dirlen+vslen

c end of if on igen ncstor
      endif


cew commented out char(0) to see if this cleans up the print in esp
c      filename(filelen+1:filelen+1)=char(0)
c
c Added RTi, July 2003
c Here the filename is checked before opfile is called. For a verification
c  run, first look for a VS file then look for a CS file.
c
c       write(*,*) 'filenamevs ', filenamevs
c       write(*,*) 'filenamecs ', filenamecs

       if(igen.EQ.1 .or. ncstor .gt. 0) then
      	 INQUIRE(FILE=filenamevs,EXIST=exists)
      	 if(exists.EQV..TRUE.) then
c the vs file exists so use it
          filename=filenamevs
          filelen=vslen
         else
	   write(ipr,*)"NOTE: DID NOT FIND VERIFICATION FILE:"
	   write(ipr,*) filenamevs
	   write(ipr,*) "WILL USE CS FILENAME INSTEAD."
	   write(ipr,*) " "

          filename=filenamecs
          filelen=cslen
         endif
       else
        filename=filenamecs
        filelen=cslen
       endif

      call opfile(filename,'ESPTS ','DIRECT','OLD',
     1 'UNFORMATTED',496,iunitc,ierr)
c
c      write (*,*) '############ NCSTOR = ', ncstor

       if(ierr.NE.0) then

        if (igen .eq. 1 .or. ncstor .gt. 0) then
         write(ipr,599) filename
  599    format(1h0,10x,'**ERROR** OPENING VERIFICATION FILE '/
     1   2x,A128)
         ier=1
         call error
         goto 999

       else

         write(ipr,600) filename
  600    format(1h0,10x,'**ERROR** OPENING CONDITIONAL FILE '/
     1   2x,A128)
         ier=1
         call error
         goto 999
        endif

c end of ierr if
       endif
c RTi, add done
c
c
       nepts=nepts+1
       if(nepts.gt.maxets) then
         write(ipr,601) maxets
  601    format(1h0,10x,'**ERROR** MAXIMUM NUMBER OF OPEN ESP TIME ',
     1   'SERIES HAS BEEN EXCEEDED',i5)
         ier=1
         call error
         go to 999
      endif
c

      if(jhss.eq.1) then
C        The filename has already been set above.  Just replace ".CS"
C        with ".HS".
         type = 2hHS
         histfile = condfile
         histfile(cslen-1:cslen)='HS'
         filenamehs=direspts(1:dirlen)//histfile(1:cslen)

c Modified by RTi, July 2003
c Like above the historic data is found in the verification dirs
c if it is a verification run
c

          tempfile=histfile(1:cslen)//dot//vsmonth//cday

c  take out the spaces
c
          j = 1
          hsvsfile=' '
          do 34 i = 1,80
           if (tempfile(i:i) .ne. ' ') then
            hsvsfile(j:j) = tempfile(i:i)
            j = j+1
           endif
   34     continue
c
        call ulenth(hsvsfile,80,hsvslen)
c        write(*,*) hsvsfile
        filenamehsvs=direspts(1:dirlen)//hsvsfile(1:hsvslen)

c
c       write(*,*) 'filenamehsvs ', filenamehsvs
c  old hs file not available use regular one
      if(igen.EQ.1 .or. ncstor .gt. 0) then
        INQUIRE(FILE=filenamehsvs,EXIST=exists)
         if(exists.EQV..TRUE.) then
           filename=filenamehsvs

          else
            filename=filenamehs
         endif
       else
            filename=filenamehs
       endif
c If it can not find the files and it is a verification run
c and esp_in_dir has been defined then it needs to remove
c that portion of the path and look only in the verification
c directory! Then check again.

      call opfile(filename,'ESPTS ','DIRECT','OLD',
     1   'UNFORMATTED',496,iunith,ierr)
c

      if(ierr.NE.0) then

         write(ipr,610) filename
  610    format(1h0,10x,'**ERROR** OPENING HISTORICAL FILE '/
     1   2x,A128)
         ier=1
         call error
         goto 999
      endif

c RTi, add done
c
         nepts=nepts+1
         if(nepts.gt.maxets) then
            write(ipr,605) maxets
  605    format(1h0,10x,'**ERROR** MAXIMUM NUMBER OF OPEN ESP TIME ',
     1   'SERIES HAS BEEN EXCEEDED',i5)
            ier=1
            call error
            go to 999
         endif
c
         if(filename(dirlen+1:dirlen+8).eq.'SCRATCH/'
     1   .and.idel.eq.1) then
            ieunit(nepts)=-iunith
         else
            ieunit(nepts)=iunith
         endif

cew  end of jhss if
      endif
c
c   adjusted simulation should go here
c
c   read header record for conditional t.s.
c
      read(unit=iunitc,rec=1) tsdat
c     CALL UREADT(iunitc,1,tsdat,ISTAT)
      if(ibug.ne.1) go to 40
      write(iodbug,910) tsdat
  910 format(10(1x,f9.2))
      write(iodbug,915) tsdat
  915 format(10(6x,a4))
c
   40 continue
c
c   interpret header
c
      do 50 i=1,5
      itimec(i)=tsdat(9+i)
   50 continue
c
      units=tsdat(9)
      imof=tsdat(15)
      iyrf=tsdat(16)
      idp=tsdat(17)
      ldp=tsdat(18)
      idf=tsdat(19)
      ihf=tsdat(20)
      ldf=tsdat(21)
      lhf=tsdat(22)
      ntracef=tsdat(23)
      ncmf=tsdat(24)
      ftzc=tsdat(25)
      irec=tsdat(27)
c
c   fill external array
c
      tsesp(loc+21)=iunitc+.01
      tsesp(loc+22)=iunith+.01
      tsesp(loc+23)=iunita+.01
      tsesp(loc+24)=ntracef+.01
      tsesp(loc+25)=ncmf+.01
      tsesp(loc+26)=imof+.01
      tsesp(loc+27)=iyrf+.01
      itoff=(idarun-idp)/365
      tsesp(loc+28)=itoff+.01
c
      if(jhss.eq.1) then
c
c   read header for historical t.s.
c
         read(unit=iunith,rec=1) tsdat
c        CALL UREADT(iunith,1,tsdat,ISTAT)
         do 60 i=1,5
         itimeh(i)=tsdat(9+i)
   60    continue
c
c   check if creation time matches that for the conditional
c   t.s. - if not problems.
c
         do 65 i=1,5
         if(itimeh(i).ne.itimec(i)) then
c         write(*,*) 'creation ', itimeh(i), itimec(i)
            write(ipr,618)
  618       format(1h0,10x,'**ERROR** HISTORICAL TIME SERIES ',
     +      'CREATION TIME DOES NOT MATCH CONDITIONAL TIME ',
     +      'CREATION TIME.  REMOVE THE ESP TIME SERIES AND ',
     +      'RERUN.')
            ier=1
            call error
            go to 999
         endif
   65    continue
      endif
c
      IF(IHF.NE.24) GO TO 280
      IDP=IDP+1
      IDF=IDF+1
  280 CALL MDYH1(IDP,IHF,IPM,IPD,IPY,IDUM,100,0,TZC)
      CALL MDYH1(LDP,LHF,LPM,LPD,LPY,IDUM,100,0,TZC)
      CALL MDYH1(IDF,IHF,IFM,IFD,IFY,IDUM,100,0,TZC)
      CALL MDYH1(LDF,LHF,LFM,LFD,LFY,IDUM,100,0,TZC)
      IF(IHF.NE.24) GO TO 281
      IDP=IDP-1
      IDF=IDF-1
c
  281 IF(IDARUN.GE.IDP) GO TO 283
      WRITE(IPR,620) IPM,IPD,IPY,filename
  620 FORMAT(1H0,10x,'**ERROR** START DAY OF ESP RUN IS BEFORE INITIAL',
     1 ' DAY OF DATA, ',I2,'/',I2,'/',I4,/,
     1 '  FOUND IN FILE ' , A128)
      IER=1
      CALL ERROR
      GO TO 999
cew changed le to ge
  283 IF(LDP.GE.LDARUN) GO TO 285
      WRITE(IPR,630) LPM,LPD,LPY,filename
  630 FORMAT(1H0,10x,'**ERROR** END DAY OF ESP RUN IS AFTER LAST',
     1 ' DAY OF DATA, ',I2,'/',I2,'/',I4,/,
     1 '  FOUND IN FILE ' , A128)
      IER=1
      CALL ERROR
      GO TO 999
  285 CALL FDATCK(IJDLST,IHLST,IDF,IHF,ILT,ISW)
      IF(ISW.EQ.0) GO TO 290
      WRITE(IPR,640) IFM,IFD,IFY,filename
  640 FORMAT(1H0,10X,44H**ERROR** START OF FORECAST PERIOD IS BEFORE,
     1 33H START OF FORECAST PERIOD ON FILE,2X,I2,1H/,I2,1H/,I4/,
     1 'FOR FILE =' , A128)
      IER=1
      CALL ERROR
      GO TO 999
C
  290 CALL FDATCK(LJDLST,LHLST,LDF,LHF,IGT,ISW)
      IF(ISW.EQ.0) GO TO 310
      WRITE(IPR,650) LFM,LFD,LFY,filename
  650 FORMAT(1H0,10X,45H**ERROR** END OF FORECAST PERIOD IS AFTER END,
     1 27H OF FORECAST PERIOD ON FILE,2X,I2,1H/,I2,1H/,I4/,
     1 'FOR FILE =' ,A128)
      IER=1
      CALL ERROR
      GO TO 999
c
c  Jump to here from top of file if iunitc !=0, conditional file is alredy open.
  310 continue
      if(iepass.eq.3) then
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
         nm=ncmf
      else
         nm=12
      endif
      nmos=(itoff+kntyr-1)*nm+(iy*12+im)-(iyrf+itoff+kntyr-1)*12-imof+1
c
      ihr1=ihzero+idt
      j1=ld+((ida-idadat)*24/idt+(ihr1-1)/idt)*nvpdt
      j2=ld+((lda-idadat)*24/idt+(lhr/idt))*nvpdt-1
c
      LENGTH=(24/IDT)*NVPDT*31
      NTVAL=LENGTH*(NMOS-1)+1
      NRECS=NTVAL/MXLRC
      NXWD=NTVAL-(NRECS*MXLRC)
      JREC=IREC+NRECS
      IF(NXWD.NE.0) GO TO 400
      NXWD=MXLRC
      JREC=JREC-1
C
  400 IF(LBUG) WRITE(IODBUG,940) IM,IY,JREC,NXWD
  940 FORMAT(1H0,10X,16HIM,IY,JREC,NXWD=,4I5)
C
      read(unit=iunit,rec=jrec) tsdat
c     CALL UREADT(IUNIT,JREC,TSDAT,ISTAT)
C
      IF(LBUG) WRITE(IODBUG,950) TSDAT
C
C
  410 KOUNT=LENGTH
      LEFT=MXLRC-NXWD+1
      IF(LEFT.LT.LENGTH) KOUNT=LEFT
      DO 420 I=1,KOUNT
      D(LD+I-1)=TSDAT(NXWD)
      NXWD=NXWD+1
  420 CONTINUE
C
      LENGTH=LENGTH-KOUNT
      IF(LENGTH.EQ.0) GO TO 999
      NXWD=1
      LD=LD+KOUNT
      JREC=JREC+1
      read(unit=iunit,rec=jrec) tsdat
c     CALL UREADT(IUNIT,JREC,TSDAT,ISTAT)
      GO TO 410
C
  999 CONTINUE
      IF(LBUG) WRITE(IODBUG,950) (D(I),I=j1,j2)
  950 FORMAT(11X,10F10.2)
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
      RETURN
      END
