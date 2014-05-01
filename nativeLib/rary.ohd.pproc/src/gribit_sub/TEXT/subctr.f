C$PRAGMA C (GET_APPS_DEFAULTS)
c  =====================================================================
c  pgm:  subctr (iutw,iupr,iud,iupr,user,mwcol,icode,senof)
c
c   in:  iutw   .... unit number for print output
c   in:  iupr   .... unit number for log output
c   in:  iud    .... unit number for debug output
c   in:  user   .... sub-center identifier or sending office
c   in:  mwcol  .... most west HRAP column
c  out:  icode  .... sub-center code
c  out:  senof  .... sending office
c  =====================================================================
c
      subroutine subctr_sub (iutw,iupr,iud,user,mwcol,icode,senof)
c
c.......................................................................
c
c  routine selects sub-center code for appropriate office according
c  to GRIB Table C
c
c.......................................................................
c  Initially written by
c     Tim Sweeney, HRL                                 July 26, 1996
c
c  Updated sub-center codes.  Added algorithm to handle SJU, Puerto
c  Rico, OFS files used at SERFC.
c     Tim Sweeney, HRL                                 Aug 1998
c
c  Added sub-center OFSTEST as code 163 (not used in GRIB).
c     Tim Sweeney, HRL                                 Nov 1998
c
c  Changed logic to always search list of sub-centers.
c     Tim Sweeney, HL                                  Jan 2001
c
c  For Puerto Rico message changed sub-center to KSJU
c  and sending office to KALR (SERFC).
c     Tim Sweeney, HL                                  Mar 2001
c
c  Changed sub-center OFSTEST to 200 (testing only).
c     Tim Sweeney, HL                                  Mar 2001
c
c  Added check of grib_set_subcenter_0 token           Jun 2006
c.......................................................................
c
      character*2 subcenter0
      character*4 suser,senof,subc
      character*10 user
      character*25 appsvar
c
      include 'grib_tbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/pproc/src/gribit_sub/RCS/subctr.f,v $
     . $',                                                             '
     .$Id: subctr.f,v 1.1 2006/10/19 16:06:04 dsa Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('subctr',1,1,ibug)
ccc      ibug = 1
c
c  check value of grib_set_subcenter_0 token
c  if set to ON, then set icode to 999
c
      appsvar='grib_set_subcenter_0'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,subcenter0,lsub0)
      if(subcenter0.eq.'ON'.or.subcenter0.eq.'on'
     *      .or.subcenter0.eq.'On') then
         write (iupr,1)
         icode=999
         return
      end if
c
      icode = -1
      lsuser = 0
c
20    subc = senof
      if (senof.eq.'KALR'.and.mwcol.gt.1121) subc = 'TSJU'
c
c  get sub-center number from sub-center Table C
      do 30 k=1,255
         if (subc.eq.scid9c(k)) icode = k
30       continue
c
c     sub-center not found
c
      if (icode.lt.0) then
         appsvar='awips_rfc_id'
         lappsvar=lenstr(appsvar)
         call get_apps_defaults (appsvar,lappsvar,suser,lsuser)
c
         if (lsuser.gt.0) then
            write (iutw,37) suser(1:lsuser),
     +         appsvar(1:lappsvar)
            if (iupr.ne.iutw) write (iupr,37) suser(1:lsuser),
     +         appsvar(1:lappsvar)
            go to 39
         endif
c
         appsvar='awips_send_id'
         lappsvar=lenstr(appsvar)
         call get_apps_defaults (appsvar,lappsvar,suser,lsuser)
c
         if (lsuser.gt.0) then
            write (iutw,37) suser(1:lsuser),
     +         appsvar(1:lappsvar)
            if (iupr.ne.iutw) write (iupr,37) suser(1:lsuser),
     +         appsvar(1:lappsvar)
            go to 39
         endif
c
         write (iutw,35)
         if (iupr.ne.iutw) write (iupr,35)
         suser='????'
         lsuser=lenstr(suser)
         go to 50
c
   39    continue
         if (lsuser.gt.0.and.lsuser.le.3) then
            if (suser.ne.'ACR ') then
               suser = 'K'//suser(1:lsuser)
            else
               suser = 'P'//suser(1:lsuser)
            endif
c
            lsuser = 4
         endif
c
         if (ibug.eq.1) write (iud,*) 'suser=',suser,' lsuser=',lsuser
         senof = suser
         icode = 0
         go to 20
      endif
c
      if (icode.eq.0) then
         if (lsuser.eq.0) then
            write (iupr,40) user
            if (iupr.ne.iutw) write (iupr,40) user
         else
            write (iupr,40) suser
            if (iupr.ne.iutw) write (iupr,40) suser
         endif
c
      endif
c
50    return
c***************************************************************
    1 format (' token set for setting subcenter to 0')
35    format (' ERROR in subctr: GRIB sub-center cannot be set ',
     +   'from apps_defaults.')
37    format (' NOTE in subctr: GRIB sub-center (',a,') set ',
     +   'from apps_default ',a,'.')
40    format ('ERROR: invalid sub-center : ',a)
      end
