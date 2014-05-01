c  =====================================================================
c  pgm:  procid (ident,type,mpo,po,istat)
c
c   in: ident  .... identifier
c   in: type   .... type code
c   in: mpo    .... maximum words in array po
c   in: po     .... parameter array
c  out: istat  .... status code
c  =====================================================================
c
      subroutine procid (ident,type,mpo,po,istat)
c
c.......................................................................
c
c  Routine to read parameters from a file and call the appropriate 
c  routine to process the parameters.
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL - Apr 1992
c
c  Added stage 3 precip
c        Tim Sweeney, HRL                                July 17, 1996
c
c  Added error message when id/type not found
c        Tim Sweeney, HRL                                Jan 21, 1997
c.......................................................................
c
      character*2 bname
      character*4 type,labltype
      character*8 ident
      parameter (mdeftype=10)
      character*4 deftype(mdeftype)/
     +           'grpp','prod','hffg','affg','text',
     +           'gffg','cary','lang','wsup','siii' /
      character*4 deftypeu(mdeftype)/
     +           'GRPP','PROD','HFFG','AFFG','TEXT',
     +           'GFFG','CARY','LANG','WSUP','SIII' /
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/timez'
      include 'ffg_inc/iodno'
      include 'ffg_inc/count'
      include 'ffg_inc/ghfld'
c
      dimension po(mpo)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/procid.f,v $
     . $',                                                             '
     .$Id: procid.f,v 1.5 2004/01/30 17:58:10 scv Exp $
     . $' /
C    ===================================================================
C
      data bname/ '  ' /
c
c
      call prbug ('procid',1,1,ibug)
c
      if (ibug.eq.1) write (iud,*) 'in procid - ident=',ident
c
c  check identifier
      if (ident.eq.'SKIPLINE'.or.ident.eq.'skipline') then
         write (iuf,10)
10    format (':')
      else if (ident.eq.'ISSUED'.or.ident.eq.'issued') then
         call gpissu
      else if (ident(1:4).eq.'DOTB'.or.ident(1:4).eq.'dotb') then
         call gpdotb (ident(5:8),type)
      else if (ident(1:4).eq.'LABL'.or.ident(1:4).eq.'labl') then
         labltype = ident(5:8)
         call gplabl (labltype)
      else if (ident(1:4).eq.'LABW'.or.ident(1:4).eq.'labw') then
         labltype = 'wsup'
         call gplabl (labltype)
      else
         go to 20
      endif
c
      go to 170
c
c  check type code
20    do 30 ideftype=1,mdeftype
         if (type.eq.deftype(ideftype).or.
     +       type.eq.deftypeu(ideftype)) then
            type = deftype(ideftype)
            go to 40
            endif
30       continue
      write (iutw,35) type,ident
      if (iupr.ne.iutw) write (iupr,35) type,ident
35    format (' ERROR: parameter type ',a,' for identifier ',a,
     *   ' is invalid.')
      nerr = nerr + 1
      go to 170
c
c  read parameters
40    kod = 1
      call rppfil (ident,type,kod,kpdv,mpo,po,npo,istat)
      if (ibug.eq.1) write (iutw,*) 'in procid - ident=',ident,
     +   ' type=',type,' istat=',istat
      if (istat.eq.0) then
         call upclos (kpdv,bname,ic)
         else if (type.eq.'grid'.or.type.eq.'GRID') then
            go to 60
         else
            write (iutw,50) ident,type
            if (iupr.ne.iutw) write (iupr,50) ident,type
50    format (' ERROR: parameters for identifier ',a,' and type ',a,
     +   ' not defined.')
            call upclos (kpdv,bname,ic)
            nerr = nerr + 1
            go to 170
         endif
c
c  process parametric array based on type
60    go to (70,80,90,100,110,120,130,140,150,160),ideftype
      write (iutw,65) type,ident
      if (iupr.ne.iutw) write (iupr,65) type,ident
65    format (' ERROR: parameter type ',a,' for identifier ',a,
     *   ' cannot be processed.')
      go to 170
c
c  grpp parameters
70    call gpgrpp (po)
      go to 170
c
c  prod parameters
80    call gpprod (mpo,po)
      go to 170
c
c  hffg parameters
90    call gphed (po)
      go to 170
c
c  affg parameters
100   call gparea (po)
      go to 170
c
110   call gptext (po)
      go to 170
c
c  gffg parameters
120   ibline = 0
      call gpgffg (mpo,po,ihfld,ibline)
      go to 170
c
c  cary parameters
130   call gpcary (po)
      go to 170
c
c  lang parameters
140   call gplang (po)
      go to 170
c
c  wsup parameters
150   call gpsup (po)
      go to 170
c
c  siii parameters
160   call gpsiii (po)
c
170   return
c
      end
