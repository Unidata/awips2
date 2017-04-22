C.......................................
C File: ex64.f
C Author(s): A. Vo, A. Voellmy, L. Cajina, W. Kwock
C Date Created: 5/5/06
C Development group: OHD HSEB
C Purpose: This subroutine executes the DHM-OP operation.
C Module(s): ex64(po,co,outputData,indicesToInputData)

C Date Modified:
C 10/02/06 added changes to pass a geo coordinate path and filename to dhm_setting and dhm_op
C 2/07 changed signature to read in multiple inflows; 3rd argument is the entire D array
C     last argument are the indices to input data in the D array
C 3/07 added call to function initialize_runner_with_args to create runner and pass any upstream flow basin ids and time series to Java Runner
C 4/07 added gridOutputConfigurationPath variable to tell runner path with file telling which grids to write (only used for IFP)
C 10/07  add variable (int useRainPlusMelt (1=yes, 0=no) to tell runner whether precipitation is from rain plus melt grids
C.......................................

C$PRAGMA C (dhm_op,get_dhm_settings,get_formatted_output_for_dhm)
C$PRAGMA C (initialize_runner_with_args)
C$PRAGMA C (set_properties_for_dhm_mods)
C


      subroutine ex64(po,co,dataArray,outputData,indicesToInputData)

      integer maxNumberOfCarryovers, maxTSlength
      parameter (maxNumberOfCarryovers = 20)
      parameter (maxTSlength = 744)
      real po(1)
      real co(1)
      real outputData(1)
      real dataArray(1)
      real outletTS(maxTSlength)
      real TSInfo(1)

      character*9 basinID
      character*120 precipDataPath /''/
      character*120 DHMModelDataPath /''/
      character*120 d2dDataPath /''/
      character*120 dhmNotifyDataPath /''/
      character*120 geoCoordPath /''/
      character*120 precipXmrgSrcPath /''/
      character*120 DHMModelDataSrcPath /''/
      character*120 gridOutputConfigurationPath /''/
      character*2048 formattedOutput
      character*2048 dhmReturnMessage
      character*2048 initializeReturnMessage
      character*9 operationID

      integer indicesToInputData(1)
      integer startOfTSindex, ninflow
      integer endOfTSindex
      integer tmpStartObsDate, tmpStartObsMonth, tmpStartObsYear,
     1        tmpStartObsHour
      integer tmpEndDate, tmpEndMonth, tmpEndYear, tmpEndHour
      integer tmpStartForecastDate, tmpStartForecastMonth,
     1        tmpStartForecastYear, tmpStartForecastHour
      integer carryoverYearList(maxNumberOfCarryovers),
     1        carryoverMonthList(maxNumberOfCarryovers),
     1        carryoverDayList(maxNumberOfCarryovers),
     1        carryoverHourList(maxNumberOfCarryovers)
      integer formattedOutputLength
      integer hoursInRun
      integer pointsInTS
      integer upBasinIdLocInParray
      integer dhmReturnMessageLength
      integer initializeReturnMessageLength
      integer useRainPlusMelt

      include 'common/ionum'
      common/fcfutp/ifpr
      common/fdbug/iodbug,itrace,idball,ndebug,idebug(20)
      common/fctime/idarun,ihrrun,ldarun,lhrrun,ldacpd,lhrcpd,now(5),
     1 local,noutz,noutds,nlstz,ida,ihr,lda,lhr,idadat
      common/fcary/ifillc,ncstor,icday(20),ichour(20)
      common/where/iseg(2),iopnum,opname(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source$
     . $',                                                             '
     .$Id$
     . $' /
C    ===================================================================
C

c  get dhm basin id from po array store in character variable
      write(basinID,'(2a4)',iostat=irwerr) (po(jj),jj=6,7)

c po(38) number of inflows
      ninflow = po(38)
      useRainPlusMelt = 0
c PO(28) Location of first upstream basin ID
      upBasinIdLocInParray = 28


      rtnnam=4hex64

c get identifier of this operation and store in character variable
      write(operationID,'(2a4)',iostat=irwerr) (opname(jj),jj=1,2)

c     trace level for this subroutine=1.
      if (itrace.ge.1 .or. ibug.eq.1) write(iodbug,900)
      useRainPlusMelt = PO(39)

      call get_dhm_settings(
     1    precipDataPath,
     1	  DHMModelDataPath,
     1    d2dDataPath,
     1    dhmNotifyDataPath,
     1    geoCoordPath,
     1    precipXmrgSrcPath,
     1    DHMModelDataSrcPath,
     1    gridOutputConfigurationPath,
     1    useRainPlusMelt)


       call mdyh2(idarun, ihrrun, tmpStartObsMonth, tmpStartObsDate,
     1      tmpStartObsYear, tmpStartObsHour, dum1, dum2,'Z   ')
       call mdyh2(ldarun, lhrrun, tmpEndMonth, tmpEndDate, tmpEndYear,
     1      tmpEndHour, dum1, dum2, 'Z   ')
       call mdyh2(ldacpd, lhrcpd, tmpStartForecastMonth,
     1      tmpStartForecastDate, tmpStartForecastYear,
     2      tmpStartForecastHour, dum1, dum2, 'Z   ')

c convert julian carryover dates to regular dates
      do i = 1 , ncstor
          call mdyh2( icday(i), ichour(i), carryoverMonthList(i),
     1        carryoverDayList(i), carryoverYearList(i),
     2        carryoverHourList(i), dum1, dum2,'Z   ')
      end do


      call get_formatted_output_for_dhm(
     1     tmpStartObsMonth,
     1     tmpStartObsDate,
     1     tmpStartObsYear,
     1     tmpStartObsHour,
     1     tmpStartForecastMonth,
     1     tmpStartForecastDate,
     1     tmpStartForecastYear,
     1     tmpStartForecastHour,
     1     tmpEndMonth,
     1     tmpEndDate,
     1     tmpEndYear,
     1     tmpEndHour,
     1     basinID,
     1     po(28),
     1     ninflow,
     1     precipDataPath,
     1     DHMModelDataPath,
     1     d2dDataPath,
     1     dhmNotifyDataPath,
     1     ncstor,
     1     carryoverMonthList,
     1     carryoverDayList,
     1     carryoverYearList,
     1     carryoverHourList,
     1     IFPR,
     1     useRainPlusMelt,
     1     formattedOutput,
     1      formattedOutputLength)

      write (ipr, '(A)') formattedOutput(1:formattedOutputLength)
      call set_properties_for_dhm_mods(operationID,
     1        basinID,DHMModelDataPath)
C  compute starting point for t.s. using day(ida), hour(ihr), and timestep(idt)
C  idadat is from common block fctime; represents first julian day in D array
C  ida, ihr, lda, and lhr represent beginning and end of run period
      idt = po(5)

      startOfTSindex = (ida-idadat)*24/idt+ihr/idt
      endOfTSindex = (lda-idadat)*24/idt+lhr/idt

C added call below to create runner and pass any upstream flow basin ids and time series to Java Runner
      call initialize_runner_with_args(
     1    basinID,
     1    dataArray,
     1    indicesToInputData,
     1    po(upBasinIdLocInParray),
     1    startOfTSindex,
     1    endOfTSindex,
     1    ninflow,
     1    precipDataPath,
     1    DHMModelDataPath,
     1    d2dDataPath,
     1    dhmNotifyDataPath,
     1    geoCoordPath,
     1    precipXmrgSrcPath,
     1    DHMModelDataSrcPath,
     1    gridOutputConfigurationPath,
     1    tmpStartObsMonth,
     1    tmpStartObsDate,
     1    tmpStartObsYear,
     1    tmpStartObsHour,
     1    initializeReturnMessage,
     1    intializeReturnMessageLength)

      if (intializeReturnMessageLength .ne. 0) then
          write (ipr,910)
     1    initializeReturnMessage(1:intializeReturnMessageLength)
          call error
          goto 110
      endif

C     need to fill in from p array


      call dhm_op(
     1    tmpStartObsMonth,
     1    tmpStartObsDate,
     1    tmpStartObsYear,
     1    tmpStartObsHour,
     1    tmpStartForecastMonth,
     1    tmpStartForecastDate,
     1    tmpStartForecastYear,
     1    tmpStartForecastHour,
     1    tmpEndMonth,
     1    tmpEndDate,
     1    tmpEndYear,
     1    tmpEndHour,
     1    basinID,
     1    ncstor,
     1    carryoverMonthList,
     1    carryoverDayList,
     1    carryoverYearList,
     1    carryoverHourList,
     1    ifpr,
     1    dhmReturnMessageLength,
     1    dhmReturnMessage,
     1    outletTS,
     1    pointsInTS,
     1    operationID,
     1    useRainPlusMelt)

      if (dhmReturnMessageLength.ne.0) then
          write (ipr,910)dhmReturnMessage(1:dhmReturnMessageLength)
          call error
          goto 110
      endif

      DO 200 I = startOfTSindex, startOfTSindex+pointsInTS
          outputData(I) = outletTS(I)
  200 CONTINUE


c     check if carryover needs to be saved.
      if(ifillc.eq.0) go to 110
      co = 1
c     check if intermediate carryover values need to be saved.
      if(ncstor.eq.0) go to 110
      do 105 j = 1, ncstor
          kda = icday(j)
          khr = ichour(j)
      call fcwtco( kda, khr, co, 1 )
  105 continue

  110 if (itrace.ge.1) write (iodbug,*) 'exit ',rtnnam

      return

  900  format ('entered ex64 - dhm-op')
  910  format("**error** ",a)

      end
