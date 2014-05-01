      Subroutine read_ts_array(seg_id, nts_arg, ts_arg)
c
      INCLUDE 'common/fp'                                               RTi     
      INCLUDE 'common/ft'                                               RTi     
      INCLUDE 'common/fts'                                              RTi     
      INCLUDE 'common/ionum'                                            RTi     
c
      CHARACTER*8  seg_id
      Integer      nts_arg
      Real         ts_arg(*)
c
      Integer      iopt, noparm, irseg
      Integer      ncopy, i
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/read_ts_array.f,v $
     . $',                                                             '
     .$Id: read_ts_array.f,v 1.1 1995/09/08 15:01:16 page Exp $
     . $' /
C  =====================================================================
C
c
      IOPT   = 0
      NOPARM = 0
c
      CALL FGETSG(seg_id, irseg, mp, p, mt, t, mts, ts,
     1            IOPT, NOPARM, IER)
C
      IF(IER .ne. 0) WRITE(IPR, 601) seg_id, ier
  601 FORMAT(" *** RETURN FROM FGETSG, segment = ", a,
     1       ", status = ", i3)
c
      if(nts_arg .lt. mts) then
	write(ipr, 602) mts, nts_arg, nts_arg
 602    format(" *** The ts array needs ", i5, "words of space."/
     1         5x,"Only ", i5, "words have been passed to ",
     2         "subroutine read_ts_array."/
     3         "The first ", i5, "words of the ts array will be ",
     4         "copied and returned.")
	ncopy = nts_arg
      else
	ncopy = mts
      end if
c
      do 10 i = 1, ncopy
 10   ts_arg(i) = ts(i)
c
      nts_arg = ncopy
c
      Return
      End
