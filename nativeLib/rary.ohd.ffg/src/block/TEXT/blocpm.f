c  =====================================================================
c  block data blocpm
c
c  block data to set dimensions for parameter arrays in FFGS
c
c    current sizes
c          array    size
c          -----    ----
c          cidx     2,2000
c          fx       400
c          po       2000
c  =====================================================================
      block data blocpm
c.......................................................................
c  dimensions set by Tim Sweeney, HRL                    Feb 1998
c
c.......................................................................
c
      include 'ffg_inc/gpo'
      include 'ffg_inc/gfx'
      include 'ffg_inc/gidx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSBLOCPM        / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/block/RCS/blocpm.f,v $
     . $',                                                             '
     .$Id: blocpm.f,v 1.1 2001/08/16 17:42:37 dws Exp $
     . $' /
C    ===================================================================
C
c
c.......................................................................
c
      data mpo /2000/
      data mfx /400/
      data mcidx /2000/
c
      end
      
