c  =====================================================================
c  pgm:  RO43 (nrel,ri,prec,rocomp) 
c
c   in: nrel   .... number of relationship
c   in: ri     .... final runoff index
c   in: prec   .... storm precipitation value
c  out: rocomp .... Runoff (quad III)
c  =====================================================================
      subroutine ro43 (nrel,ri,prec,rocomp)
c.......................................................................
c  This routine is the rainfall/runoff relationship for the Northeast
c  RFC (NERFC).
c  The three relationships for the RFC are contained in separate data
c  initialization routines as follows:
c
c         nrel     routine     Area
c        =====   ==========    ==========================
c           1       ron43      New England, Eastern NY
c           2       rob43      Buffalo area
c           3       ror43      Rochester area
c
c     len -  ENDING POSITION OF TABLE
c........................................................................
c  Initially written by
c     Tim Sweeney   Hydrologic Research Lab                2/10/95
c     Ken Mack      NERFC                                  8/11/95
c........................................................................
c
      include 'common/fdbug'
      include 'common/ionum'
      common /tblbug/ ikh,nrelh
c
      dimension ro(2),sname(2)
      INTEGER*4 RLINE(13),RINDX(15,13),ROT(15,13)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/ro43.f,v $
     . $',                                                             '
     .$Id: ro43.f,v 1.1 1996/03/21 15:53:00 page Exp $
     . $' /
C    ===================================================================
C
c
      data sname/4hRO43,4h    /,nop/43/
c
      call fprbug (sname,2,nop,ibug)
c
      iri = ri
      rocomp = 0.00
      ipcpn  = prec * 100.
      if(prec.le.0.00) goto 900
c
c  get ro relationship for specific area
      if(nrel.eq.1) then
         call ron43 (len,rline,rindx,rot)
      else if(nrel.eq.2) then
         call rob43 (len,rline,rindx,rot)
      else if(nrel.eq.3) then
         call ror43 (len,rline,rindx,rot)
      else
         write(ipr,5000) nrel
 5000    format(10x,'** ERROR **    INVALID RELATION NUMBER',I4)
         CALL ERROR
         GOTO 900
      endif
c
      if(nrel.eq.nrelh) then
        goto 400
      else
        nrelh = nrel
      endif
c
      if(itrace.gt.2) then
        write(iodbug,3100) nrel
        write(iodbug,3200) (rline(j),(rindx(i,j),i=1,15),j=1,len)
        write(iodbug,3220)
        write(iodbug,3200) (rline(j),(rot(i,j),i=1,15),j=1,len)
      endif
c
  400 do 470 i=1,len
      IF(ipcpn-rline(i)) 500,690,470
  470 continue
      i = len
      goto 690
c
  500 x = ipcpn - rline(i-1)
      y = rline(i) - rline(i-1)
      factor = x/y
      n = i
c
C  DO LOOP 660 DETERMINES THE 'ro' FOR A GIVEN ri VALUE
C  FOR THE SURROUNDING precipitation LINES
      do 660 i=1,2
      do 580 j=1,15
      IF(iri-rindx(J,N)) 610,640,580
  580 continue
      ro(i) = 0.
      goto 650
  610 ro(i) = rot(j,n)+(rot(j-1,n)-rot(j,n))* (rindx(j,n)
     1  -iri)/(rindx(j,n)-rindx(j-1,n))
      ro(i) = ro(i) * 0.01
      goto 650
c
  640 ro(i) = rot(j,n) * 0.01
  650 n = n - 1
  660 continue
      rocomp = ro(2) + ((ro(1) - ro(2)) * factor)
      goto 900
c
  690 n = i
      do 720 j=1,15
      IF(iri-rindx(J,N)) 730,780,720
  720 continue
  730 rocomp = rot(j,n)+(rot(j-1,n)-rot(j,n))*(rindx(j,n)
     1  -iri)/(rindx(j,n)-rindx(j-1,n))
      rocomp = rocomp * 0.01
  750 if(ipcpn.le.rline(len) ) goto 900
      rocomp = rocomp + ((ipcpn - rline(len)) * 0.01)
      goto 900
  780 rocomp = rot(j,n) * 0.01
      goto 750

  900 if(itrace.gt.1) write(iodbug,3400) ri,prec,rocomp
      return
c
c  debug formats
 3100 format(5x,'nrel=',i2)
 3200 format(5x,16i5)
 3220 format()
 3400 format(5x,'ri=',f5.2,4x,'prec=',f5.2,4x,'rocomp=',f5.2)
c
      end
