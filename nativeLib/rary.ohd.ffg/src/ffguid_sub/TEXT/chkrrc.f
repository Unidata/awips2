c  =====================================================================
c  pgm:  chkrrc
c
c  =====================================================================
c
      subroutine chkrrc (ident,nrrdur,rrc,iunitd,irrbeg,nrrset,ilocrr,
     +   otyprr,onamrr,iprint)
c
c.......................................................................
c
c  check rainfall-runoff curves
c
c.......................................................................
c
      character*8 ident,otyprr,onamrr
c
      dimension rrc(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
      include 'ffg_inc/uinfo'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/chkrrc.f,v $
     . $',                                                             '
     .$Id: chkrrc.f,v 1.1 2003/08/20 13:05:25 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('chkrrc',1,1,ibug)
c
      irrbeg = 18
      nrrset = 8
c
c  set rainfall-runoff model type and name
      ilocrr = irrbeg + nrrdur*nrrset
      call umemov (rrc(ilocrr),otyprr,2)
      call umemov (rrc(ilocrr+2),onamrr,2)
      if (ibug.gt.0) write (iunitd,*) 'otyprr=',otyprr,
     +   ' onamrr=',onamrr
c
      if (ibug.gt.0) then
c     print rainfall-runoff curves
         do 280 idr=1,nrrdur
            m = irrbeg + nrrset*(idr-1)
            n = m + nrrset - 1
            write (iunitd,270) idr,m,n,(rrc(i),i=m,n)
270   format (' idr=',i2,' m=',i2,' n=',i2,
     +   ' rainfall-runoff curves : ',4(f6.3,1x,f6.3,3x))
280         continue
         endif
c
c  check rainfall-runoff curves
      if (nrrdur.gt.1) then
         do 310 idr=1,nrrdur-1
            do 300 ipair=1,4
               m = irrbeg + nrrset*(idr-1) + 2*(ipair-1)
               irf1 = m
               iro1 = m+1
               vrf1 = rrc(irf1)
               vro1 = rrc(iro1)
               irf2 = m + nrrset
               iro2 = m + 1 + nrrset
               vrf2 = rrc(irf2)
               vro2 = rrc(iro2)
               if (ibug.eq.1) write (iunitd,'(4(1x,a,i2,1x,a,f6.3))')
     +            'irf1=',irf1,' vrf1=',vrf1,
     +            'irf2=',irf2,' vrf2=',vrf2,
     +            'iro1=',iro1,' vro1=',vro1,
     +            'iro2=',iro2,' vro2=',vro2
               if (vrf1.ne.vrf2) then
                  write (iutw,290) ipair,
     +               'rainfall',idurt(idr+1),
     +               vrf2,'not equal to',idurt(idr),vrf1,ident
                  if (iupr.ne.iutw) write (iupr,290) ipair,
     +               'rainfall',idurt(idr+1),
     +               vrf2,'not equal to',idurt(idr),vrf1,ident
290   format (' WARNING: pair ',i1,' ',a,' value for ',
     +   i2,' hour duration (',f6.3,
     +   ') ',a,' for ',
     +   i2,' hour duration (',f6.3,
     +   ') for Headwater ',a,'.')
                  nwarn = nwarn + 1
                  iprint = 1
                  endif
               ickgt=0
               if (ickgt.eq.1.and.vro1.gt.vro2) then
                  write (iutw,290) ipair,
     +               'runoff',idurt(idr),
     +               vro1,'greater than',idurt(idr+1),vro2,ident
                  if (iupr.ne.iutw) write (iupr,290) ipair,
     +               'runoff',idurt(idr),
     +               vro1,'greater than',idurt(idr+1),vro2,ident
                  nwarn = nwarn + 1
                  iprint = 1
                  endif
               icklt=0
               if (icklt.eq.1.and.vro1.lt.vro2) then
                  write (iutw,290) ipair,
     +               'runoff',idurt(idr),
     +               vro1,'less than',idurt(idr+1),vro2,ident
                  if (iupr.ne.iutw) write (iupr,290) ipair,
     +               'runoff',idurt(idr),
     +               vro1,'less than',idurt(idr+1),vro2,ident
                  nwarn = nwarn + 1
                  iprint = 1
                  endif
300            continue
310         continue
         if (iprint.eq.1) then
            write (iupr,320) otyprr
320   format (10x,'Rainfall-Runoff curves from Operation ',a,':')
            do 340 idr=1,nrrdur
               m = irrbeg + nrrset*(idr-1)
               n = m + nrrset - 1
               write (iupr,330) idurt(idr),(rrc(i),i=m,n)
330   format (10x,i2,' hour: ',4(f6.3,1x,f6.3 : ' | '))
340            continue
            endif
         endif
c
      return
c
      end
