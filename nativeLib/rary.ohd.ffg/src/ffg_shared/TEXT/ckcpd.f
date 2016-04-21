c  =====================================================================
c  pgm:  ckcpd (ident,cpd,lident,lffcpd,nwarn)
c
c   in: id     .... identifier
c   in: cpd    .... date and time as julian hour
c  i/o: lident .... identifier of most recent date and time
c  i/o: lffcpd .... julian hour of most recent date and time
c  i/o: nwarn  .... warning counter
c
c  =====================================================================
c
      subroutine ckcpd (ident,cpd,lident,lffcpd,nwarn)
c
c.......................................................................
c
c  check computation date of rainfall-runoff curves
c
c  in FFG Operation:
c     lstcmpdy = (ldacpd - 1)*24 + lhrcpd
c     ncpd is lstcmpdy in this routine
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL - Nov 1994
c.......................................................................
c
      character*4 code
      character*8 ident,lident
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'common/fctime'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/ckcpd.f,v $
     . $',                                                             '
     .$Id: ckcpd.f,v 1.5 2003/08/20 13:05:54 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('ckcpd',1,1,ibug)
c
      ncpd = ifix(cpd)
      if (ibug.ge.2) write (iud,*) 'ident=',ident,' ncpd=',ncpd,
     +   ' lffcpd=',lffcpd
c
      if (ncpd.gt.0.and.ncpd.ne.lffcpd) then
         if (lffcpd.gt.0) then
            lda = lffcpd/24 + 1
            lhr = mod(lffcpd,24)
            call mdyh1 (lda,lhr,lm,ld,ly,lh,0,0,code)
            jda = ncpd/24 + 1
            jhr = mod(ncpd,24)
            call mdyh1 (jda,jhr,jm,jd,jy,jh,0,0,code)
            write (iutw,10) ident,jm,jd,jy,jh,
     +         lident,lm,ld,ly,lh
            if (iupr.ne.iutw) write (iupr,10) ident,jm,jd,jy,jh,
     +         lident,lm,ld,ly,lh
10    format (' WARNING: date FFG Operation rainfall-runoff curves ',
     +       'were updated for ',a,' (',
     +       2(i2.2,'/'),i4,'-',i2.2,'Z',')' /
     +   10x,'is different than for ',a,' (',
     +       2(i2.2,'/'),i4,'-',i2.2,'Z',').'
     +       )
            nwarn = nwarn + 1
            endif
         if (ncpd.gt.lffcpd) then
            lident = ident
            lffcpd = ncpd
            endif
         endif
c
      return
c
      end
