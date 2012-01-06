c  =====================================================================
c  pgm:  pingro (idur,narea,istat)
c
c  out: istat  .... completion code
c  =====================================================================
c
      subroutine pingro (idur,narea,istat)
c
c..............................................................
c
c  Input parameters for gridded threshold runoff
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character*4 cdur,di,crow,ckol,cnok
      character*6 cro
      character*8 aid
      character*20 adesc
      character*80 record
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/gridpm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/pingro.f,v $
     . $',                                                             '
     .$Id: pingro.f,v 1.4 2004/01/30 17:50:50 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('pingro',1,1,ibug)
ccc      ibug = 1
c
      istat = 0
c
c  read record
10    read (iuu,'(a)',end=80,err=30) record
      read (record,20,end=80,err=30) cdur,di,crow,ckol,cnok,cro
20    format (2a4,1x,a4,1x,2a4,a6)
      irec = irec + 1
      if (ibug.eq.1) write (iud,*) 'irec=',irec,' cdur=',cdur,
     +   ' di=',di,' crow=',crow,' ckol=',ckol,' cnok=',cnok,
     +   ' cro=',cro
c
c  check for area id and description record
30    if (di.ne.' |||') then
         ibeg = 5
         aid=record(ibeg:ibeg+len(aid))
         if (aid.eq.'RIZ008') then
            if (ibug.eq.1) write (iud,*) 'in pingro - aid=',aid
            endif
         ibeg = 29
         adesc=record(ibeg:ibeg+len(adesc))
         write (iutw,35) aid,adesc
35    format (' NOTE: processing area ',a,' (description=',a,').')
         narea=narea+1
         go to 10
         endif
c
      inderr = 0
c
c  get duration
      call uc2ir (cdur,kdur,rvalue,itype,istat2)
      inderr = inderr + istat2
c
c  get HRAP row
      call uc2ir (crow,krow,rvalue,itype,istat2)
      inderr = inderr + istat2
c
c  get HRAP column
      call uc2ir (ckol,kol,rvalue,itype,istat2)
      inderr = inderr + istat2
c
c  get duration
      call uc2ir (cnok,nok,rvalue,itype,istat2)
      inderr = inderr + istat2
c
c  get runoff
      call uc2ir (cro,ivalue,ro,itype,istat2)
      inderr = inderr + istat2
c
      if (inderr.gt.0) then
         write (iutw,40)
40    format (' ERROR: converting input values.')
         istat = 1
         go to 10
         endif
c
c  check duration
      if (kdur.ne.idur) then
         write (iutw,43) ldur,irec,idur
43    format (' ERROR: duration (',i4,') on record ',i4,
     +   ' is not equal to specified duration ( ',i4,').')
         istat = 1
         endif
c
c  check if row and column are in FFG HRAP subset
      if (krow.lt.msrow) then
         write (iutw,45) 'row',krow,irec,'less','southern row',
     +      msrow
45    format (' ERROR: ',a,' number (',i4,') on record ',i4,' is ',a,
     +   ' than the ',a,' ( ',i4,') of the FFG HRAP subset.')
         istat = 1
         endif
      mnrow = msrow + nrow - 1
      if (krow.gt.mnrow) then
         write (iutw,45) 'row',krow,irec,'greater','northern row',
     +      mnrow
         istat = 1
         endif
      if (kol.lt.mwcol) then
         write (iutw,45) 'column',kol,irec,'less','western column',
     +      mwcol
         istat = 1
         endif
      mecol = mwcol + ncol - 1
      if (kol.gt.mecol) then
         write (iutw,45) 'column',kol,irec,'greater','eastern column',
     +      mecol
         istat = 1
         endif
c
c  use default duration iddur if kdur not given
      if (kdur.le.0) kdur = iddur
      do 50 i=1,5
         if (kdur.eq.idurt(i)) then
            ldur = i
            mval = mval + 1
            go to 60
            endif
50       continue
      go to 10
c
60    if (ibug.eq.1) write (iud,*) 'aid=',aid,' kdur=',kdur,
     +   ' krow=',krow,' kol=',kol,' nok=',nok,' ro=',ro
      go to 90
c
c  end-of-file encountered
80    kgridf = 2
c
90    return
c
      end

