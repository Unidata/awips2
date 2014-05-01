C$PRAGMA C (GET_APPS_DEFAULTS)
c  =====================================================================
c  pgm:  envfix
c
c  variables are passed thru common blocks
c  =====================================================================
c
      subroutine envfix
c
c.......................................................................
c
c  Routine to get values of environment variables.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                                 Jun 1993
c
c  Added table variables
c       Tim Sweeney, HRL                                 Apr 1996
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/paths'
      include 'ffg_inc/uinfo'
c
      character*1 lvl
      character*2 ron
      character*128 appsval
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/envfix.f,v $
     . $',                                                             '
     .$Id: envfix.f,v 1.4 2003/03/14 18:26:20 dws Exp $
     . $' /
C    ===================================================================
C
c
c  error output
      call get_apps_defaults ('ffg_error_output',16,appsval,i)
      ron = appsval(1:2)
      igerr = 0
      if (ron.eq.'on'.or.ron.eq.'ON'.or.ron.eq.'On') igerr = 1
c
c  debug output
      igdbug = 0
      ienv = 0
      call get_apps_defaults ('ffg_debug_output',16,appsval,j)
      ron = appsval(1:2)
      lvl = appsval(1:1)
      if (ron.eq.'on'.or.ron.eq.'ON'.or.ron.eq.'On') then
         igdbug = 1
         else if (ron.eq.'ev'.or.ron.eq.'EV') then
            ienv = 1
         else if (lvl.eq.'1') then
            igdbug = 1
         else if (lvl.eq.'2') then
            igdbug = 2
         else
         endif
c
c  log file output
      call get_apps_defaults ('ffg_log_output',14,appsval,k)
      ron = appsval(1:2)
      if (ron.eq.'on'.or.ron.eq.'ON'.or.ron.eq.'On') iupr = iul
c
c  alternate files for testing
      iofs = 0
      call get_apps_defaults ('ffg_alt_ofs',11,appsval,i)
      ron = appsval(1:2)
      if (ron.eq.'on'.or.ron.eq.'ON'.or.ron.eq.'On') iofs = 1
c
c  directory names
      call get_apps_defaults ('ofs_level',9,ofslvl,lofsl)
      call get_apps_defaults ('ffg_level',9,ffglvl,lffgl)
      call get_apps_defaults ('ffg_usr_dir',11,usrpa,lusr)
      call get_apps_defaults ('ffg_area_dir',12,arpa,lar)
      call get_apps_defaults ('ffg_gridff_dir',14,gffpa,lgff)
      call get_apps_defaults ('ffg_gridpm_dir',14,igpmpa,lgpm)
      call get_apps_defaults ('ffg_gridro_dir',14,gropa,lgro)
      call get_apps_defaults ('ffg_hwatr_dir',13,hdwpa,lhdw)
      call get_apps_defaults ('ffg_group_dir',13,grpppa,lgrpp)
      call get_apps_defaults ('ffg_prod_dir',12,prodpa,lprod)
      call get_apps_defaults ('ffg_text_dir',12,txtpa,ltxt)
      call get_apps_defaults ('ffg_output',10,outpa,lout)
      if (lout.le.5) then
         call get_apps_defaults ('ffg_out_dir',11,outpa,lout)
         endif
      call get_apps_defaults ('ffg_cary_dir',12,carypa,lcary)
      call get_apps_defaults ('ffg_wsup_dir',12,wsuppa,lwsup)
ccc      call get_apps_defaults ('ffg_lang_dir',12,langpa,llang)
ccc      call get_apps_defaults ('ffg_tabl_dir',12,tablpa,ltabl)
      if (iofs.gt.0) then
ccc         call get_apps_defaults ('ffg_basn_dir',12,basnpa,lbasn)
ccc         call get_apps_defaults ('ffg_ffg_dir',11,ffoppa,lffop)
ccc         call get_apps_defaults ('ffg_ratcv_dir',13,rcpa,lrc)
ccc         call get_apps_defaults ('ffg_tser_dir',12,tspa,lts)
         endif
c
c  turn debug off if only environmental variables desired
      if (ienv.gt.0) iud = -1
c
      return
c
      end
