C$PRAGMA C (post_start_ifp_atom)
C$PRAGMA C (post_rerun_plot_num_atom)
C$PRAGMA C (turn_autorepeat_on)
C$PRAGMA C (post_no_tulsa_plot_atom)
C$PRAGMA C (wait_for_next_segment_atom)
C$PRAGMA C (put_next_in_current)
C$PRAGMA C (put_upstream_in_current)
C$PRAGMA C (post_nwsrfs_eventloop_atom)
C$PRAGMA C (udatl)
C$PRAGMA C (start_jvm,close_jvm)
C
      Subroutine main_fort()
c
      INCLUDE 'common/fc'                                                 
      INCLUDE 'common/fd'                                               
      INCLUDE 'common/fp'                                                
      INCLUDE 'common/ft'                                               
      INCLUDE 'common/fts'                                               
      INCLUDE 'common/ionum'                                         
c
      Integer event_loop_exit_status, prev_event_loop_exit_status
      Common /cex_exit_status/ event_loop_exit_status

c  Declare variables to be passed to uppfix - dp 12 Sept. 1994
      character*128   dumpth
      integer         dumlen
c
c  Declare variable to be passed to wait_for_next_or_goto_atom
      integer         next_segment_atom
c      
c  Declare variable to be passed to rerun_plot_num_atom - dp 25 Feb 97
      integer         rerun_plot_num
c
c  Set values in elapsed cpu and wall clock common block.
c  This coded copied from fcstmain.f function.
c  Added by gfs - hrl - 10 Sept 1994
c
      COMMON /HCPUCK/ ITOTCP,IBGSEC,ILASEC
c
      DIMENSION MDUM1(6)
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob82/ohd/ifp/src/ifp_nwsrfs/RCS/main_fort.f,v $
     . $',                                                             '
     .$Id: main_fort.f,v 1.7 2007/05/16 16:52:16 aivo Exp $
     . $' /
C  =====================================================================
C
C
C  INITIALIZE TOTAL CPU TIME AND INCREMENTAL COUNTER FOR CPU
C
      ITOTCP=0
cc DHM -start java virtual machine - 
      call start_jvm()
      
      CALL URTIMR (LAPSE,ITOTCP)
C
C  SET BEGINNING WALL CLOCK TIME
C
      CALL udatl (MDUM1)
C
      IBGSEC=(MDUM1(5)/100)*3600+MOD(MDUM1(5),100)*60+(MDUM1(6)+50)/100
      ILASEC=IBGSEC
c
c  End of code added 10 Sept 1994 to set cpu and wall clock timers.
c
      Call upinio()
c
c  Dummy variables dumpth and dumlen are not filled by uppfix.
      Call uppfix('opnw', 'ifp_fs5files', dumpth, dumlen)
c
      Call rpdbci(istat)
      if(istat .ne. 0) then
	Write(ipr, 98)istat
 98     Format("BIG PROBLEM, status returned from rpdbci =", i3)
      end if
c
 10   prev_event_loop_exit_status = event_loop_exit_status
      event_loop_exit_status = 0
c
      Call filcom()
c
      Call faze0()
c
      Call post_start_ifp_atom()
c
c     Added code to set the rerun_plot_num_atom to 0
c     for all cases except during a rerun 
      if(prev_event_loop_exit_status .ne. 1) then
         rerun_plot_num = 0
         Call post_rerun_plot_num_atom(rerun_plot_num) 
      endif           
c
      Call faze2(mc, c, md, d, mp, p, mt, t, mts, ts)
c
      Call turn_autorepeat_on()
c
      if(event_loop_exit_status .lt. 1) then
	 if(event_loop_exit_status .eq. -1) then
	    Write(ipr,  99)
 99         Format('Skipping Tulsa plot.')
	 else
	    Write(ipr, 100)
 100        Format('No Tulsa plot in current segment.')
	 endif
	 Call post_no_Tulsa_plot_atom()
c
c  Add argument to wait_for_next_segment_atom to specify
c   whether a next segment or a goto upstream segment atom
c   was received.  This case is needed when the last segment
c   in a forecast group does not have a Tulsa plot.  In
c   that case the only way to continue with processing is
c   to quit, choose a new forecast group, or goto an 
c   upstream segment.  Based on value of argument returned,
c   either put the next segment into the current and continue
c   or put the chosen upstream segment into the current and
c   continue with processing.
c  Modified by gfs - hrl - 3 Oct 1994
c
	 Call wait_for_next_segment_atom(next_segment_atom)
         if(next_segment_atom .eq. 0) then
	    Call put_next_in_current()
	    Write(ipr, 102)
         else if(next_segment_atom .eq. 1) then
            Call put_upstream_in_current()
	    Write(ipr, 103)
	 else if(next_segment_atom .eq. 2) then
	    go to 104  
         endif
	 Call close_all_open_files()
c--         Call fork_NWSRFS()
	 go to 10
c
      else if(event_loop_exit_status .eq. 1) then
	 Write(ipr, 101)
 101     Format('Rerun current segment.')
	 Call close_all_open_files()
c--         Call fork_NWSRFS()
	 go to 10
c
      else if(event_loop_exit_status .eq. 2) then
	 Call put_next_in_current()
	 Write(ipr, 102)
 102     Format('Go to next segment.')
	 Call close_all_open_files()
c--         Call fork_NWSRFS()
	 go to 10
c
      else if(event_loop_exit_status .eq. 4) then
	 Call put_upstream_in_current()
	 Write(ipr, 103)
 103     Format('Go to upstream segment.')
	 Call close_all_open_files()
c--         Call fork_NWSRFS()
	 go to 10
c
      else if(event_loop_exit_status .eq. 5) then
c
c  Call NWSRFS function "stop" to write banner with cpu and
c   wall clock time used by this run of ifp.
c  Added by gfs - hrl - 10 Sept 1994.
c
 104     Call stop()
	 Write(ipr, 105)
 105     Format('Normal end of Run_NWSRFS.')
	 Call close_all_open_files()
c
      else if(event_loop_exit_status .eq. 6) then
c
c  Call NWSRFS function "stop" to write banner with cpu and
c   wall clock time used by this run of ifp.
c  Added by gfs - hrl - 10 Sept 1994.
c
         Call stop()
	 Write(ipr, 106)
 106     Format('Normal end of Run_NWSRFS.'/
     1          'Do not save output time series.')
	 Call close_all_open_files()
c
      else if(event_loop_exit_status .eq. 7) then
c
c  Post "entering_NWSRFS_eventLoop_atom" to make IFP_Map program
c   know that ifp_nwsrfs has ended.  This will have happened
c   when the event_loop for ifp Tulsa plot finishes for normal
c   end of ifp_nwsrfs program.
c  Added by gfs - hrl - 10 Sept 1994.
c
         Call post_nwsrfs_eventloop_atom()
         Write(ipr, 107)
 107     Format('Fatal error - end of Run_NWSRFS.'/
     1          'Do not save output time series.')
	 Call close_all_open_files()
	 stop
c
      else
	 Write(ipr, 110) event_loop_exit_status
 110     Format('Unknown event_loop_exit_status =', i10)
      end if
cc closing java virtual Machine for DHM operation
      call close_jvm()
c
      stop
      end
