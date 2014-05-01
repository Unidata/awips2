      Subroutine read_seg_names(fg_id, nseg_ret, idsegn_ret)
c
      INCLUDE 'common/fcrunc'                                           RTi     
      INCLUDE 'common/fcsegn'                                           RTi     
      INCLUDE 'common/fd'                                               RTi     
      INCLUDE 'common/ionum'                                            RTi     
c
      CHARACTER*8  fg_id
      Integer      nseg_ret
      Character*8  idsegn_ret(*)
c
      Integer      iopt, noparm, type_of_run
      Integer      id(2)
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/read_seg_names.f,v $
     . $',                                                             '
     .$Id: read_seg_names.f,v 1.1 1995/09/08 15:01:15 page Exp $
     . $' /
C  =====================================================================
C
c
      IOPT   = 1
      NOPARM = 1
      type_of_run = 2
C
C      FILL CB FCRUNC - with list of segments in this fgroup
C
      CALL FCORDR(type_of_run, fg_id, IER, D, MD)
c
      if(ier .ne. 0) WRITE(IPR, 950) fg_id, ier
  950 FORMAT(" *** RETURN FROM FCORDR, fgroup = ", a,
     1       "status = ", i3)
c
      Do 10 i = 1, nsegex
c
      CALL FGETSG(id, IRSGEX(i), 1,X, 1,X, 1,X,
     1            IOPT, NOPARM, IER)
C
      IF(IER .ne. 0) WRITE(IPR, 601) ier
  601 FORMAT(" *** RETURN FROM FGETSG, status = ", i3)
c
c  copy segment id from common /FCSEGN/
c  id is not returned through fgetsg argument list
c
      Call umemov(idsegn, idsegn_ret(i), 2)
c
 10   Continue
c
      nseg_ret = nsegex
c
      Return
      End
