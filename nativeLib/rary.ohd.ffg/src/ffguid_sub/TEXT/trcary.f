c  =====================================================================
c  pgm:  trcary (ident,mfx,mpv,fx,pv)
c
c   in: ident  .... identifier
c   in: mfx    .... number of words in fx array
c   in: mpv    .... number of words in pv array for state variables
c   in: fx     .... array containing ffg parameters
c   in: pv     .... array containing state variables
c  =====================================================================
c
      subroutine trcary (ident,mfx,mpv,fx,pv)
c
c.......................................................................
c
c  Transfer rainfall-runoff and snow model state variables from OFS
c  database
c
c.....................................................................
c     Program initially written by
c           Tim Sweeney, HRL - Dec 1993
c.....................................................................
c
      character*2 bname
      character*4 filtyp
      character*4 accmode
      character*8 ident
      character*128 pthnam
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
c
      dimension fx(*),pv(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/trcary.f,v $
     . $',                                                             '
     .$Id: trcary.f,v 1.6 2004/09/13 14:24:07 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('trcary',1,1,ibug)
c
      filtyp='cary'
      bname=' '
c
c  version
      pv(1) = 1.0
c
c  transfer ID
      pv(2) = fx(2)
      pv(3) = fx(3)
c
c  type
      call umemov (filtyp,pv(4),1)
c
c  description
      do 10 i=1,5
         pv(i+5) = fx(i+3)
10       continue
c
c  duration flag:
c    0 = 1, 3 and 6 hours
c    1 = 1, 3, 6 and 12 hours
c    2 = 1, 3, 6, 12 and 24 hours
      pv(11) = fx(13)
c
c  location of rainfall-runoff info in pv array
      lr = 19
      pv(12) = lr
c
c  location of snow info in pv array
      isloc = fx(14)
      if (isloc.lt.1) then
         ls = 0
         else
            ls = lr + 15
         endif
      pv(13) = ls
c
c  unused
      do 20 i=1,4
         pv(13+i) = 0.01
20       continue
c
c  date and time LSTCMPDY
      pv(18) = fx(17)
c
c  duration
      idur = ifix(fx(13))
c
      irloc = 42 + idur*8
c
c  rainfall-runoff parameters - 5 values:
c  model type (2), model name (2) and number of carryover values
      do 30 i=1,5
         k = i - 1
         pv(lr+k) = fx(irloc+k)
30       continue
      ncorr = ifix(fx(irloc+4) )
      irlc = irloc + 4
      jrlc = lr + 4
c
c  rainfall-runoff co values
      do 40 i=1,ncorr
         pv(jrlc+i) = fx(irlc+i)
40       continue
c
c  number of words for parameters
      iusev = 32
c
c  snow parameters - 5 values:
c  model type (2), model name (2) and number of carryover values
      if (ls.le.0) go to 70
      do 50 i=1,5
         k = i - 1
         pv(ls+k) = fx(isloc+k)
50       continue
      islc = isloc + 4
      jslc = ls + 4
      ncosn = pv(ls+4)
c
c  snow CO
      do 60 i=1,ncosn
         pv(jslc+i) = fx(islc+i)
60       continue
c
c  number of words required for CO parameters
      iusev = jslc + ncosn
c
70    pv(5) = iusev
c
c  open file
      accmode = 'rw'
      kod = 0
      call fixopn (ident,filtyp,pthnam,accmode,kod,ipdv,istat)
      if (istat.le.1) then
         if (istat.eq.0) rewind (ipdv)
         else
            call upclos (ipdv,bname,ic)
            write (iutw,80) ident(1:lenstr(ident))
80    format (' ERROR: file ',a,' not found or could not create.')
            nerr = nerr + 1
            go to 130
         endif
c
c  write to file
      call wppfil (ipdv,iusev,pv,istat)
      call pstcod (istat,ident,filtyp,ipdv)
      call upclos (ipdv,bname,ic)
      nvar = nvar + 1
      j = lr + 3
c
      if (ibug.gt.0) write (iud,90) (pv(i),i=1,5)
90    format (' pv(1)=',f5.2,' pv(2)=',2a4,' pv(4)=',a4,' pv(5)=',i3)
      if (ibug.gt.2) then
         write (iud,100) ident,(pv(i),i=lr,j),iusev
100   format (' ident=',a,' stored ',2a4,1x,2a4,' carryovers in ',i3,
     +   ' words')
         write (iud,110) (pv(i),i=1,iusev)
110   format ((8x,8a4))
         write (iud,120) (pv(i),i=1,iusev)
120   format ((8x,8f8.2))
         endif
c
130   return
c
      end
