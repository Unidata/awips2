      Subroutine write_parameters(output_file_name,
     1                     p_float, t_int, ts_float,
     2                     c_float, loct_wanted)
c
      Integer    t_int
      Character*50  output_file_name
      Dimension  p_float(1), ts_float(1), c_float(1)
      Dimension  t_int(1)
c
      COMMON/FCOPPT/LOCT,LPM,LCO
      INCLUDE 'common/ionum'                                            RTi     
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/write_params.f,v $
     . $',                                                             '
     .$Id: write_params.f,v 1.2 2000/03/16 12:03:23 page Exp $
     . $' /
C  =====================================================================
C
c
      loct_hold = loct
      lpm_hold = lpm
      lco_hold = lcp
c
      ipr_hold = ipr
      ipr = 17
c
      OPEN (UNIT=IPR,
     1      FILE=output_file_name,
     2      STATUS='UNKNOWN',ERR=991)
c
      loct = loct_wanted
      lpm = t_int(loct + 2)
      lco = t_int(loct + 3)
c
      numop = t_int(loct)
      iopt = 1
c
      if(numop .gt.0 .and. numop .lt. 20)
     1   Call propt1(p_float, 1, c_float, 1,
     2     t_int, 1, ts_float, 1, iopt, numop, ierr)
c
      if(numop .gt.19 .and. numop .lt. 41)
     1   Call propt2(p_float, 1, c_float, 1,
     2     t_int, 1, ts_float, 1, iopt, numop, ierr)
c
      if(numop .gt. 40 .and. numop .le. 65)
     1   Call propt3(p_float, 1, c_float, 1,
     2     t_int, 1, ts_float, 1, iopt, numop, ierr)
c
      if(numop .lt.1 .or. numop .gt. 65) Write(*,600) numop
 600  Format("In write_parameters, invalid numop = ", i4)
c
      Rewind(ipr)
      Close(ipr)
c
      go to 999
c
 991  Write(*,993)output_file_name
 993  Format(' Problem opening file ',A)
c
 999  ipr = ipr_hold
c
      loct = loct_hold
      lpm  = lpm_hold
      lcp  = lco_hold
c
      return
      end
