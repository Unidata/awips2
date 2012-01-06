c  =====================================================================
c  pgm:  putgpm (iuse,po,istat)
c
c  out: iuse   .... number of words in po
c  out: po     .... parameter array
c  i/o: istat  .... status
c                  
c  =====================================================================
      subroutine putgpm (iuse,po,istat)
c.......................................................................
c  put data type gdpm parameters in po array
c
c.......................................................................
c     Initially written by
c          Tim Sweeney, HRL                                 Mar 1992
c
c     Expanded capabilities
c          Tim Sweeney, HRL                                 Nov 1997
c.......................................................................
c
      character*4 type
      character*8 sname
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/igparm'
c
      dimension ldur(5),po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/putgpm.f,v $
     . $',                                                             '
     .$Id: putgpm.f,v 1.1 2001/08/16 17:42:55 dws Exp $
     . $' /
C    ===================================================================
C
c
      data sname/ 'putgpm  '/
      data ldur/1,3,6,12,24/
c
      call prbug (sname,1,1,ibug)
      if(istat.gt.0) goto 9999
c
      igptyp = 'gdpm'
c
c          transfer to po array
c  file version number
      fvers = 1.1
      po(1) = fvers
c
c  identifier
      call umemov(iffgid,po(2),2)
c
c  type code
      call umemov(igptyp,po(4),1)
c
c  unused
      do 1120 i=6,7
 1120 po(i) = -999.0
c
      nxt = 16
c
c  location of high flow adjust parameters
      if(iqoptg.gt.0) then
        lqag = nxt
        nxt  = lqag + 10
      else
        lqag = 0
      endif
      po(8) = lqag + 0.01
c
c  location of runoff/ffg adjust parameters
      if(iroptg.gt.0) then
        lrag = nxt
        nxt  = lrag + 6
      else
        lrag = 0
      endif
      po(9) = lrag + 0.01
c
c  location of overbank parameter
      lob = nxt
      po(10) = lob
c
c  unused
      do 1150 i=11,15
 1150 po(i) = -999.0
c
c  high flow adjust option
      if(iqoptg.gt.0) then
        po(lqag) = iqoptg
        do 1180 i=1,5
 1180   po(lqag+i) = taqg(i)
        call umemov (qtsidg,po(lqag+6),2)
        call umemov (dtcqg,po(lqag+8),1)
        po(lqag+9) = intqg
      endif
c
c  runoff/ffg adjust option
      if(iroptg.gt.0) then
        po(lrag) = iroptg
      
c  runoff/ffg values
        do 1210 i=1,5
 1210   po(lrag+i) = rinten(i)
      endif
c
c  overbank factor
      po(lob) = bank
c
c  percent impervious area
      po(lob+1) = pcimpg
      nxt = nxt + 1
c
c  number of words used
      iusei = nxt
      po(5) = float(iusei)
      iuse  = iusei
c
 9999 return
c
      end
