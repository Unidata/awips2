c  =====================================================================
c  block data blocgr
c
c  block data to set dimensions for gridded arrays in FFGS
c
c  current sizes:
c     common     array    size
c     ------     -----    ----
c     ghfld      ihfld    250000
c     ffgrid     gg       5,500,500
c     rogrid     tro      5,500,500
c     xgpo       gpo      250000
c
c  =====================================================================
c
      block data blocgr
c
c.......................................................................
c  dimensions set by Tim Sweeney, HRL                    Feb 1998
c
c.......................................................................
c
      include 'ffg_inc/ghfld'
      include 'ffg_inc/gridsz'
      include 'prodgen_inc/xgpo'
ccc      include 'ffg_inc/ffgrid'
ccc      include 'ffg_inc/rogrid'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      COMMON / RCSBLOCGR        / RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/block/RCS/blocgr.f,v $
     . $',                                                             '
     .$Id: blocgr.f,v 1.4 2002/10/10 13:52:13 xfan Exp $
     . $' /
C    ===================================================================
C
c
c  common block ghfld
      data mhfld /250000/,ihfld/250000*0/
c
c  common block gridsz
      data mxd /5/
      data mxr /500/
      data mxc /500/
c
c  common block xgpo
      data mgpo /250000/,gpo/250000*0.0/
c
      end
      
