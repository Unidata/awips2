C MEMBER MODS
C  (from old member DUMTOP)
C
      SUBROUTINE mods
      include 'common/sionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob83/ohd/calb/src/top_stubs/RCS/mods.f,v $
     . $',                                                             '
     .$Id: mods.f,v 1.1 1996/05/21 16:08:25 dws Exp aivo $
     . $' /
C    ===================================================================
C
cav-dcs3481      write(ISTDERR,*) 'Enter MODS dummy.  Routine is not active.'
      END
