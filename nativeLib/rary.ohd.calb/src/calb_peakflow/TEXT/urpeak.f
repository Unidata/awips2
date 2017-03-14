C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE URPEAK
C------------------------------------------------------------
C
C     ROUTINE URPEAK READS USGS PEAKFLOW RECORDS AND RETURNS DATA
C     IN ARRAYS.
C
c    this routine was modified by Bryce Finnerty (cbf) October, 1997.
c    Modifications were made to read peakflow data ascii files that
c    were obtained from the usgs via ftp or the internet.  the code
c    gets an input file name and appends it to the 'peakflow_data_dir'
c    token.  opens the file, reads the data, and passes it back to the
c    operation peakflow pin routine where the data is stored for use in
c    the peakflow calibration operation.
c  
      SUBROUTINE URPEAK (PEAKID,IFSTMO,IFSTYR,ILSTMO,ILSTYR,
     *   MAXPKS,DATAFN,IUNITS,IBUG,
     *   NUMPKS,peak,ISTAT)
C
C  --------INPUT--------
C  PEAKID         CHAR*8      STATION REQUESTED
C  IFSTMO         INT         FIRST MONTH REQESTED
C  IFSTYR         INT         FIRST YEAR REQUESTED
C  ILSTMO         INT         LAST MONTH REQUESTED
C  ILSTYR         INT         LAST YEAR REQUESTED
C  MAXPKS         INT         maximum number of peaks expected 
c  DATAFN         CHAR*32     Input file name containing peakflow data
C  IUNITS         INT         0=ENGLISH   1=METRIC
c  IBUG           INT         0=NO DEBUG OUTPUT   1=YES DEBUG OUTPUT
C  --------OUTPUT-------
c  NUMPKS       INT         NUMBER OF PEAKFLOW EVENTS IN THE 
c                          REQUESTED TIME PERIOD.
C  PEAK(10,MAXPKS) REAL     1 YEAR OF DATA
C                           2 PEAK FLOW MONTH
C                           3 PEAK FLOW DAY
C                           4 PEAK FLOW, MAX DISCHARGE FOR THE YEAR
C                           5 GAGE HEIGHT, MAX DISCHARGE FOR THE YEAR
c
C  ISTAT          INT         0=NORMAL COMPLETION
c                           1=error opening peakflow data input file.
c                           2=error reading peakflow data input file.
c                           3=error closing peakflow data input file.
C    ---------------------------------------------------
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
      CHARACTER*8   PEAKID,ista,SNAME
      CHARACTER*32  DATAFN
      CHARACTER     dirpeak*128, tokstr*64, filename*128
C
      INTEGER       MAXPKS,NUMPKS,IUNITS,ISTAT,IBUG
      INTEGER       toklen, dirlen, filelen, datalen
      DIMENSION     peak(10,MAXPKS)
C
      CHARACTER*1   flagh,flagq
      REAL          xflagh,xflagq
      EQUIVALENCE (flagq,xflagq)
      EQUIVALENCE (flagh,xflagh)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_peakflow/RCS/urpeak.f,v $
     . $',                                                             '
     .$Id: urpeak.f,v 1.8 2002/02/11 13:30:03 michaelo Exp $
     . $' /
C    ===================================================================
C
      DATA  SNAME / 'URPEAK  ' /
C
C   TRACE LEVEL FOR ROUTINE=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,47,IBUG)
C
      NUMPKS=0
      ISTAT=0
C
C
      IF (IBUG.GT.0) THEN
        WRITE(IPR,320)
        WRITE(IPR,325)
        WRITE(IPR,310)PEAKID,ifstmo,ifstyr,ilstmo,ilstyr,MAXPKS,DATAFN
 310    FORMAT(/,10X,'PEAKID=',A8,'  ifstmo=',I2,2X,'ifstyr=',I4,2X,
     &  'ilstmo=',I2,2X,'ilstyr=',I4,2X,'MAXPKS=',I6,2X,'DATAFN=',A32)
 320   FORMAT(/,10X,'INPUT PARAMETERS PASSED TO ROUTINE URPEAK')
 325   FORMAT(/,10X,'TESTING')
      END IF
C 
c
c  get the path of the directory in which the file will be written
c
      toklen=17
      tokstr='peakflow_data_dir'
      tokstr(18:18)=char(0)
      call get_apps_defaults(tokstr,toklen,dirpeak,dirlen)
      dirpeak(dirlen+1:dirlen+1) = '/'
      dirlen = dirlen+1 
c
c  
      IF (IBUG .GT. 0)  THEN
        WRITE(IPR,330)tokstr,dirpeak
        WRITE(IPR,335)toklen,dirlen
  330   FORMAT('TOKEN=',A64,/,'dirpeak=',A128)
  335   FORMAT('toklen=',I5,5x,'dirlen=',I5)
      END IF
c
      call ulenth(datafn,32,datalen)
      filename=dirpeak(1:dirlen)//datafn(1:datalen)
      filelen=dirlen+datalen
      filename(filelen+1:filelen+1)=char(0)

      if (ibug .ge. 1) then
        write(ipr,*) 'in urpeak:  filename=',filename
      end if
c
c   open peakflow data file.
      call opfile( filename,'PEAKDATA ','SEQUENTIAL','OLD',
     & 'FORMATED',0,iounit,IERR)
c      write(ipr,*)'ierr open=',ierr,'  iounit open=',iounit   
      if (ierr .ne. 0 ) then
        write(ipr,340)filename
  340 format(10x,'**WARNING** Error opening Peakflow data file =',a128)
        call warn
        istat=1
        goto 999
      end if
c   ***************************************************************
c    read the peakflow input data file with fixed format.  if any data
c    is in the wrong format the thing will exit the routine with istat=2
c    it will skip over comment cards and header cards because of the
c    iflag1 is read as i2 when it is really i1. this will cause an error
c    on the comments and header lines.
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 10   read(iounit,500,err=1000,end=900)iflag1,ista,iyr,imn,iday,peakq,
     & flagq,peakh,flagh
 500  format(i2,6x,a8,8x,i4,1x,i2,1x,i2,6x,f7.0,1x,a1,7x,f5.2,3x,a1)
      istat=0
c    
       if (iyr .lt. ifstyr) go to 10
       if (iyr .gt. ilstyr) go to 10
       if ((iyr .eq. ifstyr) .and. (imn .lt. ifstmo)) go to 10
       if ((iyr .eq. ilstyr) .and. (imn .gt. ilstmo)) go to 10
         numpks=numpks+1
         istat=0
         peak(1,numpks)=iyr
         peak(2,numpks)=imn
         peak(3,numpks)=iday
         peak(4,numpks)=peakq
         peak(5,numpks)=peakh
         peak(6,numpks)=xflagq
         peak(7,numpks)=xflagh
         peak(8,numpks)=iflag1
c
      goto 10  
 1000 continue
      istat=2
c     
      goto 10
 900  continue     
c
c   close peakflow input data file
c
       call clfile('PEAKDATA ',iounit,ierr)
c     
       if (ierr .ne. 0 ) then
       write(ipr,350)filename
 350   format(10x,'**WARNING** Error closing Peakflow data file =',a128)
         call warn
         istat=3
         goto 999
      end if
c     
C                 
 999  continue
      IF (ITRACE.EQ.1) WRITE(IPR,345)
 345  FORMAT(/,10X,'** EXIT SUBROUTINE - URPEAK **')
C
      RETURN
      END
