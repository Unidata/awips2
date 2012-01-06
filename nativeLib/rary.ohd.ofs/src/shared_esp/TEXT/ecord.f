C$PRAGMA C (GET_APPS_DEFAULTS)
      subroutine ecord(segid,ijdlst,ihlst,c,maxc,ier)
c
C.................................
C     Modified by Scott Townsend RTi July 2003
C         Added code to fix a bug which kept an esp verification run
C         from completing since expected carryover values were not
C         written to the carryover file which was needed by the ESP
C         historic traces run. Added code checks to keep a segment
C         fault from occuring if the carryover file did not contain
C         all of the expected data. An error message is written instead
C         of a hard crash.
C.................................
      include 'common/fctime'
      include 'common/ionum'
      include 'common/fdbug'
      include 'common/where'
      include 'common/egentr'
      include 'common/fcio'
      include 'common/fcunit'
c
      dimension segid(2)
      dimension sbname(2),oldopn(2),segidf(2),c(1)
c
cfan  integer tmplen,dirlen 
      integer tmplen,dirlen,filelen    !cfan
c
      character diresp*256,file*22,tempfile*22,filename*256,
     1 tmpstr*64,csegid*8,cmo*2,cda*2,chr*2,ctzc*4,dot*1,tzc*4
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ecord.f,v $
     . $',                                                             '
     .$Id: ecord.f,v 1.5 2006/04/10 16:13:02 xfan Exp $
     . $' /
C    ===================================================================
C
c
      data sbname/4hecor,4hd   /
      data name/4hecor/
c
      ioldop=iopnum
      iopnum=0
      do 10 i=1,2
      oldopn(i)=opname(i)
      opname(i)=sbname(i)
   10 continue
c
      if(itrace.ge.1) write(iodbug,900)
  900 format(1h0,'ecord entered')
c
      lbug=.false.
      if(ifbug(name).eq.1) lbug=.true.
c
      ier=0
c
      call mdyh2(ijdlst,ihlst,ijmonth,ijday,iyear,ijhour,100 , 0 ,
     1 tzc)
c
      tmplen=10
      tmpstr='ens_files'
      tmpstr(10:10)=char(0)
c
      call get_apps_defaults(tmpstr,tmplen,diresp,dirlen)
c
      if(dirlen.gt.234) then
	 write(ipr,600)
  600    format('**ERROR** THE ESP CARRYOVER DIRECTORY EXCEEDS ',
     1   '234 CHARACTERS.  THE PATH MUST BE SHORTENED.')
	 ier=1
         call error
	 go to 999
      endif
c
      diresp(dirlen+1:dirlen+11)='/carryover/'
c
      write(csegid,21) (segid(j),j=1,2)
   21 format(2a4)
   22 format(i2.2)
      write(cmo,22) ijmonth
      write(cda,22) ijday
      write(chr,22) ijhour
      write(ctzc,21) timezn
c
      dot='.'
      tempfile=' '
      tempfile=csegid//dot//cmo//dot//cda//dot//chr//dot//ctzc
c
      j=1
      file=' '
      do 30 i=1,22
	if (tempfile(i:i).ne.' ') then
	   file(j:j)=tempfile(i:i)
	   j=j+1
        endif
   30 continue
c
      call ulenth(file,22,tmplen)
      filename=diresp(1:dirlen+11)//file(1:tmplen)
      filelen=dirlen+11+tmplen
      filename(filelen+1:filelen+1)=char(0)
c
      iunit=79
      lrecl=-1
c     call upopen(iunit,filename,lrecl,'u',ic)
      open(unit=iunit,file=filename,access='sequential',
     !  form='unformatted',status='old', iostat=ioerr)
      if(ioerr.ne.0) then
	 write(ipr,604) filename
  604    format('**ERROR** OPENING HISTORICAL CARRYOVER FILE',A)
	 ier=1
	 call error
	 go to 999
      endif
c
   40 read(iunit) segidf,iwocry,icday,ichour,nc,ncops
      call mdyh2(icday,ichour,ifmonth,ifday,ifyear,ifhour,lstz,ids,
     !  timezn)
c
      ijdtest=ijdlst*24+ihlst
      icdtest=icday*24+ichour
c
c Added RTi July 2003
c  Testing to make sure the index values are not assigned a huge value
c  i.e. the conditional check to see if we reached the end of the data
c  is broken. This would happen if for some reason not all of the carryover
C  written to the carryover file. If so we just write an error msg.
      icheck1=1
      icheck2=1
      if(icdtest.lt.ijdtest) then
      do 44 n=1,ncops
        read(iunit,end=990) ic1,ic2,(c(i),i=ic1,ic2)
c Added RTi July 2003
c  The following condition checks to see if an actual integer was
c  read in to the pointers or a float in binary integer form! If a
c  float then an error in the following array assignment would cause
c  a hard crash so we exit with an error message.
        if(ic1.ge.icheck1*1000)go to 46
        if(ic2.ge.icheck2*1000)go to 46
        icptr=c(ic1+1)
        if(icptr.eq.nc)go to 40

c Added RTi July 2003
c  Update the checking values.
        icheck1=ic1
        icheck2=ic2

   44 continue
      write(ipr,605)
  605 format('*** ERROR **  NC DOES NOT AGREE WITH CARRYOVER POINTERS')    
c
      elseif(icdtest.eq.ijdtest) then
      do 45 n=1,ncops
	 read(iunit,end=990) ic1,ic2,(c(i),i=ic1,ic2)

c Added RTi July 2003
c  The following condition checks to see if an actual integer was
c  read in to the pointers or a float in binary integer form! If a
c  float then an error in the following array assignment would cause
c  a hard crash so we exit with an error message.
        if(ic1.ge.icheck1*1000)go to 46
        if(ic2.ge.icheck2*1000)go to 46

        icptr=c(ic1+1)
        if(icptr.eq.nc)go to 999

c Added RTi July 2003
c  Update the checking values.
        icheck1=ic1
        icheck2=ic2

   45 continue
   46 continue
      write(ipr,605)
c
      endif
c
  990 continue
      write(ipr,606)
  606 format('**ERROR** DATE NOT FOUND ON HISTORICAL CARRYOVER',
     1'FILE')
      ier=1
      call error
c
  999 continue
      opname(1)=oldopn(1)
      opname(2)=oldopn(2)
      return
      end
      
