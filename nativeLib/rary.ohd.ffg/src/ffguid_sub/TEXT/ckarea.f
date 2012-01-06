c  =====================================================================
c  pgm:  ckarea (ffgid,gbid,bx)
c
c   in: ffgid  .... flash flood guidance area id
c   in: gbid   .... geo boundary id
c   in: bx     .... geo parameter array
c
c  =====================================================================
      subroutine ckarea_file (ffgid,gbid,bx)
c.......................................................................
c  determine type of area, then compute average, maximum, and minimum
c  basin area from basin boundary info
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL                           Mar 1999
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/bsnsiz'
c
      character*1 aid,cnum
      character*4 gbid(2),ffgid(2)
      character*8 sname
c
      dimension bx(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/ckarea.f,v $
     . $',                                                             '
     .$Id: ckarea.f,v 1.1 2001/08/16 17:42:57 dws Exp $
     . $' /
C    ===================================================================
C
c
      data sname/ 'cpsize  '/
c
c      call prbug (sname,1,1,ibug)
c
      ipos = 1
      iwidth = 1
      num = 1
      aid = ffgid(1)(3:3)
      cnum = ffgid(1)(4:4)
      area = bx(11)
c
c  check ffgid for county area
      call ffa2i (cnum,ipos,iwidth,num,inum,istat)
      if((aid.eq.'C'.or.aid.eq.'Z').and.istat.eq.0) then
         call cksize_local (area,earea,gbid,ncarea,cavg,csize,cbsid)
         write(iupr,30) gbid,earea
   30    format(59x,'gbid=',2a4,3x,f6.0,' sq mi  c')
c
c  ffgid is a basin area
      else
        call cksize_local (area,earea,gbid,nbarea,bavg,bsize,bbsid)
        write(iupr,50) gbid,earea
   50   format(59x,'gbid=',2a4,3x,f6.0,' sq mi')
      endif
c
      return
      end
c===============================================================
      subroutine cksize_local (area,earea,gbid,no,averg,asize,aid)
c
      character*4 gbid(2),aid(2,*)
      real asize(*)
c
      earea = area*0.3861
c
      if(no.gt.0) then
        sum = averg*no
        sum = sum + earea
      else
        sum = earea
        ipos = 1
        do 1010 i=1,1000
 1010   asize(i) = 0.
        goto 200
      endif
c
      do 1200 ipos=1,no
      if(earea.ge.asize(ipos)) then
c  move down
        do 1100 i=no,ipos,-1
        asize(i+1) = asize(i)
        aid(1,i+1) = aid(1,i)
 1100   aid(2,i+1) = aid(2,i)
        goto 200
      endif
 1200 continue
c
c  insert
  200 asize(ipos) = earea
      aid(1,ipos) = gbid(1)
      aid(2,ipos) = gbid(2)
c
c  increment counter and compute new average
      no = no + 1
      averg = sum/no
c
      return
      end
