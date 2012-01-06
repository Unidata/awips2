
C Note: a patch to the Fortran Compiler performed during Apr 1995
C requires
C this to be compiled with the +e option, or else Byte variable are
C treated
C as Logical*1 variables with logical operators instead of Integer*1.

      Subroutine FortConBuf(Array,Work,mnx,nx,ny,
     &                      scale,offset,mode,seed,
     &                      xpoints,ypoints,npoints,
     &                      badlo,badhi,
     &                      status)

C Assumes user sets up world coordinates from 1.0 to float(nx) and
C 1.0 to float(ny).

C  Inputs:
C   Array(mnx,ny)   R*4   Array to contour.
C   Work(nx,ny)     I*4   Workspace.
C   mnx             I*4   First dimension of input arrays.
C   nx              I*4   First (horizontal) dimension of input grid.
C   ny              I*4   Second (vertical) dimension of input grid.
C   scale           R*4   Contour grid as if it were multiplied by this
C   ....
C   offset          R*4   and then added to by this before contouring.
C   mode            I*4   If mode<0 then -mode is approximate number of
C                         contours and seed is minimum contour
C                         increment.
C                         If mode=0 then seed is contour increment.  If
C                         mode>0 seed is array of contour values to use,
C                         mode is number of contours.
C  seed(*)          R*4   Value(s) that control contouring (see mode).
C  xpoints(*)       R*4   x-coords in grid space of contours.
C  ypoints(*)       R*4   y-coords in grid space of contours.
C  npoints          I*4   Number of points in the buffer.
C  badlo,badhi      R*4   Smallest and largest values which will be ignored
C                         upon contouring.  If badlo>badhi, then no missing
C                         value handling is done.

C  Outputs:
C  status           I*4   Logically true if input was meaningful.

      Implicit None
      include 'IntrinsicFunctions.inc'

c declare arguments
      Integer*4 mnx,nx,ny
      Real*4    Array(mnx,*)
      Byte      Work(nx,ny,4)
      Real*4    scale,offset
      Integer*4 mode
      Real*4    seed(*)
      Real*4    xpoints(*),ypoints(*)
      Real*4    badhi,badlo
      Integer*4 status
c
      Integer*4 mwi,mnc
      Parameter (mwi=1500,mnc=200)

      Integer*4 ncon,nconp,mmm,npoints,numSteps

      Logical   mirror,ascend,enulab

      Byte      bbb

      Integer*4 i,j,iq,jq,kq,c1,
     &          xmode,sedsgn,
     &          nxm,nym,
     &          lstat,
     &          turn1,turn2,turn3,turn4,ii,jj

      Real*4    val,val1,val2,dval,minval,maxval,t10,t5,t2,
     &          D2,sedwrk,
     &          rawmin,rawmax

      Integer*4 celcnt,labsep,dlx,dld,dly,c,labsep2,
     &          Patern(mnc),ChrN(mnc)
      Real*4    ConVal(mnc),ConVals(mnc),MxAvg(mnc),MnAvg(mnc)
      Byte      Cmask(mnc)
c unused variable      Byte      b52/'52'X/
c unused variable      Byte      bD2/'D2'X/
c unused variable      Byte      b12/'12'X/
c unused variable      Byte      b02/'02'X/
      Byte      b40/'40'X/
      Byte      bC0/'C0'X/
      Character LabStr(mnc)*7
      Common   /ConNewCmnBuf/celcnt,labsep,dlx,dld,dly,c,
     &                    labsep2,Patern,LabStr,ChrN,
     &                    ConVal,ConVals,MxAvg,MnAvg,CMask

      Data  Cmask/1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     & 		      1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     & 			  1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     & 			  1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     & 			  1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     & 			  1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     & 			  1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     &			  1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     & 			  1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     &  		  1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     & 			  1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     & 			  1,2,4,8,16,32,64,-128,1,2,4,8,16,32,64,-128,
     & 			  1,2,4,8,16,32,64,-128/

c  Initialize data range calculation.
      rawmax=-1e37
      rawmin=+1e37
      npoints = 0

c  Diagnostic output of unlabelable side masking.
c      Call ZeroItOutBuf(Work,nx*ny)
c      Do 1515 labsep2=0,5
c      Call MarkHrz(Work(1,1,3),Work(1,1,4),nx,ny,6,6)
c      Print *,'Hrz labsep2 = ',labsep2
c1515  Call ShowMark(Work,nx,ny)
c      Call ZeroItOutBuf(Work,nx*ny)
c      Do 1516 labsep2=0,5
c      Call MarkVrt(Work(1,1,3),Work(1,1,4),nx,ny,6,6)
c      Print *,'Vrt labsep2 = ',labsep2
c1516  Call ShowMark(Work,nx,ny)
c      If (i.eq.i) Stop

c  Zero out workspace.
      Call ZeroItOutBuf(Work,nx*ny)

c  Case of we have to consider missing values.
      If (badlo.lt.badhi) Then

c  Figure out which vertical sides can accept contours.
          Do 982 i=1,nx
          val2=Array(i,ny)
          Do 981 j=ny-1,1,-1
          val1=Array(i,j)
          If (val1.lt.val2) Then
              If (val1.gt.badhi .or. val2.lt.badlo) Then
                  If (val1.lt.rawmin) rawmin=val1
                  Work(i,j,4)=b40
              Else
                  Work(i,j,4)=1
              End If
          Else
              If (val2.gt.badhi .or. val1.lt.badlo) Then
                  If (val2.lt.rawmin) rawmin=val2
                  If (val2.lt.val1) Work(i,j,4)=bC0
              Else
                  Work(i,j,4)=1
              End If
          End If
981       val2=val1
982       Continue

c  Figure out which horizontal sides can accept contours.
          Do 984 j=1,ny
          val2=Array(nx,j)
          Do 983 i=nx-1,1,-1
          val1=Array(i,j)
          If (val1.lt.val2) Then
              If (val1.gt.badhi .or. val2.lt.badlo) Then
                  If (val2.gt.rawmax) rawmax=val2
                  Work(i,j,3)=b40
              Else
                  Work(i,j,3)=1
              End If
          Else
              If (val2.gt.badhi .or. val1.lt.badlo) Then
                  If (val1.gt.rawmax) rawmax=val1
                  If (val2.lt.val1) Work(i,j,3)=bC0
              Else
                  Work(i,j,3)=1
              End If
          End If
983       val2=val1
984       Continue

      Else  !  No missing values.

c  Determine "sense" of vertical sides.
          Do 992 i=1,nx
          val2=Array(i,ny)
          Do 991 j=ny-1,1,-1
          val1=Array(i,j)
          If (val1.lt.val2) Then
              If (val1.lt.rawmin) rawmin=val1
              Work(i,j,4)=b40
          Else
              If (val2.lt.rawmin) rawmin=val2
              If (val2.lt.val1) Work(i,j,4)=bC0
          End If
991       val2=val1
992       Continue

c  Determine "sense" of horizontal sides.
          Do 994 j=1,ny
          val2=Array(nx,j)
          Do 993 i=nx-1,1,-1
          val1=Array(i,j)
          If (val1.lt.val2) Then
              If (val2.gt.rawmax) rawmax=val2
              Work(i,j,3)=b40
          Else
              If (val1.gt.rawmax) rawmax=val1
              If (val2.lt.val1) Work(i,j,3)=bC0
          End If
993       val2=val1
994       Continue

      End If ! done finding data range, average gradient and missing values.

c  Handle case of no good points.
      status=0
      If (rawmin.gt.rawmax .or.
     &    nx.gt.mnx .or. nx.gt.mwi .or. ny.gt.mwi) Return
      status=1
      If (rawmin.eq.rawmax) Return

c  Scale absolute extrema from grid as specified.
      If (scale.lt.0.0) Then
          minval=rawmax*scale+offset
          maxval=rawmin*scale+offset
      Else If (scale.gt.0.0) Then
          minval=rawmin*scale+offset
          maxval=rawmax*scale+offset
      Else
          minval=rawmin
          maxval=rawmax
      End If

c  Limit number of contours by size of work arrays, check for alternate modes.
      If (mode.gt.0) Then
          xmode=mode/1000
          mmm=jmin0(mnc,mode-xmode*1000)
      Else
          xmode=(-mode)/1000
          mmm=jmax0(-mnc,mode+xmode*1000)
      End If
      If (xmode.eq.1) Then
          If (mmm.le.0) Then
              minval=amax1(minval,seed(2))
              maxval=amin1(maxval,seed(3))
          End If
          xmode=0
      Else If (xmode.eq.2) Then
          If (mmm.le.0) Then
              xmode=0
          Else If (seed(1).lt.0.0 .or. seed(mmm).lt.seed(1)) Then
              xmode=0
          Else
              mmm=jmin0(mnc/2,mmm)
          End If
      Else
          xmode=0
      End If

c  Decode line label format control string, see if we have enumerated labels.
      enulab=(mmm.gt.0 .and. .not.i4_odd(lstat) .and. xmode.ne.3)

c  Not enumerated contours.
      If (mmm.le.0) Then

c Make sure contouring increment has some finite positive value and that no
c more contours are drawn than we have room for in work arrays.
          val=(maxval-minval)/mnc
          dval=seed(1)
          If (dval.lt.val) Then
              dval=val
              If (mmm.eq.0) mmm=-25
            End If

c  Handle case of routine picks contouring interval.
          If (mmm.lt.0) Then

              dval=10.0**(jint(alog10(dval)+100.01)-100)
              t10=(-5.0)*mmm
              t5 =(-2.0)*mmm
              t2 =(-1.0)*mmm
1005          val=(maxval-minval)/dval
              If (val.gt.t10) Then
                   dval=dval*10.0
                   Goto 1005
                  End If
              If (val.gt.t5) dval=dval*5.0
              If (val.gt.t2) dval=dval*2.0
          End If

c  See to it that contour values are multiples of dval.
          val1=dval*anint(minval/dval)
          If (val1.lt.minval) val1=val1+dval
          val2=dval*anint(maxval/dval)
          maxval=amax1(val1+dval,maxval)
          If (val2.gt.maxval) val2=val2-dval
          If (val1.gt.val2) Return

c  Load up array with contour values.
c          If (scale.ge.0.0) Then
c              ncon=0
c              Do 1010 val=val1,val2,dval
c              ncon=ncon+1
c1010          ConVal(ncon)=val
c             Else
c              ncon=0
c              Do 1012 val=val2,val1,-dval
c              ncon=ncon+1
c1012          ConVal(ncon)=val
c          End If

c These next 11 lines of code replace the above commented out code in order to
c avoid a warning with g77 ("val" is type REAL or DOUBLE PRECISION
c -- unexpected behavior likely) which is likely for very large loops
c with
c very small increments.  A very bad way to fix this warning is to make
c the index variable an integer.  Mixed variables in a do loop will cause
c an infinite loop with f77 and a floating point exception with g77 when
c the increment is less than 1.  See p.1063 in the HP9000 F77 Fortran
c Programmer's Reference.
          numSteps = (val2-val1)/dval+1
          if (scale .lt. 0.0) then
             val = val2
             dval= -(dval)
          else
             val = val1
          endif
          do 1010 ncon=1,numSteps
             ConVal(ncon) = val
 1010        val = val + dval
          ncon = ncon - 1

      Else  !  User enumerated contours.

          ncon=0
          dval=1e37
          val1=1e37
          val2=-1e37
          mirror=(xmode.eq.2)
          ascend=((seed(1).gt.seed(mmm)).neqv.mirror)
          If (scale.lt.0.0 .and. .not.mirror) ascend=.not.ascend
          If (mirror .and. scale.ge.0.0) Then
              sedsgn=-1
          Else
              sedsgn=1
          End If

c  Handle a negative scale and/or descending order in seed values.
1049     If (ascend) Then
             iq=mmm
             jq=1
             kq=-1
         Else
             iq=1
             jq=mmm
             kq=1
         End If

c  Load values for enumerated contours into internal work array, record max,
c  min and approximation of delta value.
          Do 1050 c=iq,jq,kq
          sedwrk=sedsgn*seed(c)
          If (mirror .and. sedwrk.eq.0.0) Goto 1050
          If (sedwrk.lt.minval .or. sedwrk.gt.maxval) Goto 1050
          If (ncon.gt.0) Then
              val=abs(sedwrk-ConVal(ncon))
              If (val.ne.0.0) dval=amin1(dval,val)
             End If
          If (sedwrk.ne.0.0) dval=amin1(dval,abs(sedwrk))
          val1=amin1(val1,sedwrk)
          val2=amax1(val2,sedwrk)
          ncon=ncon+1
          ConVal(ncon)=sedwrk
          If (ncon.ge.mnc) Goto 1055
1050      Continue
1055        Continue  ! loop escape

c  See if we loop back for xmode.eq.2, negative version of input list.
          If (mirror) Then
              mirror=.false.
              ascend=.not.ascend
              sedsgn=-sedsgn
              If (ncon.lt.mnc) Goto 1049
          End If

      End If  !  End loading contour value array.

c  Get plotting coordinates ranges.
1939  Continue
      nxm=nx-1
      nym=ny-1

C  Escape point for bypassing locating maxes and mins...Skip drawing
C  contours if there are none.
77776 If (ncon.eq.0) Return

c  Fill line pattern array, scale contour value array, and set use count mask.
      Do 4001 c=1,ncon
      ConVals(c)=ConVal(c)
      If (scale.ne.0.0) ConVal(c)=(ConVal(c)-offset)/scale
4001  Continue

C  Set bracketing values which define whether cell side is best suited for
C  labeling a particular contour.
      c1=0
      Do 4012 c=1,ncon
      If (c1.eq.0) Then
          MnAvg(c)=-1e37
      Else
          MxAvg(c1)=ConVal(c)+ConVal(c1)
          MnAvg(c)=MxAvg(c1)
      End If
      c1=c
4012  Continue
      MxAvg(c1)=+1e37
      nconp=ncon+1

C  Initialize spiral search for places to start contours.
      turn1=nx
      turn2=ny
      turn3=1
      turn4=2
      c=(ncon+1)/2
      i=0
      ii=1
      j=1
      jj=1

C  Move along side 1 after missing values.
10011 i=ii
      If (i.ge.turn1) Then
          If (j.ge.turn2) Return
          turn1=turn1-1
          Goto 20011
        End If
      ii=i+1
      bbb=Work(i,j,3)
      If (iand (int(bbb),1).eq.1) Goto 10011
      If (bbb.eq.0) Goto 10011

c  Cell side does not have missing values, reinitialize search.
      D2=Array(i,j)
      If (bbb.gt.0) Then
          Do 10014 c=1,ncon
10014     If (ConVal(c).gt.D2) Goto 10022
          Goto 10021
      Else
          Do 10015 c=ncon,1,-1
10015     If (ConVal(c).lt.D2) Goto 10032
          Goto 10031
      End If

c  Move along side 1, values increasing as we go.
10021 i=ii
      If (i.ge.turn1) Then
          If (j.ge.turn2) Return
          turn1=turn1-1
          Goto 20021
        End If
      ii=i+1
      bbb=Work(i,j,3)
      If (iand (int(bbb),1).eq.1) Goto 10011
      If (bbb.lt.0) Then
          c=c-1
          If (c.eq.0) Goto 10031
          Goto 10032
      End If
      If (c.eq.nconp) Goto 10021
      If (bbb.eq.0) Goto 10021

c  Step through possible contours we can start.
10022 D2=Array(ii,jj)
10023 If (ConVal(c).lt.D2) Then
          If (iand(Work(i,j,1),CMask(c)).eq.0)
     &        Call Contr1_B(i,j, 1,Array,Work,mnx,nx,ny,
     &                      xpoints,ypoints,npoints)
          c=c+1
          If (c.eq.nconp) Goto 10021
          Goto 10023
      End If
      Goto 10021

c  Move along side 1, values decreasing as we go.
10031 i=ii
      If (i.ge.turn1) Then
          If (j.ge.turn2) Return
          turn1=turn1-1
          Goto 20031
        End If
      ii=i+1
      bbb=Work(i,j,3)
      If (iand (int(bbb),1).eq.1) Goto 10011
      If (bbb.gt.0) Then
          c=c+1
          If (c.eq.nconp) Goto 10021
          Goto 10022
      End If
      If (c.eq.0) Goto 10031
      If (bbb.eq.0) Goto 10031

c  Step through possible contours we can start.
10032 D2=Array(ii,jj)
10033 If (ConVal(c).gt.D2) Then
          If (iand(Work(i,j,1),CMask(c)).eq.0)
     &        Call Contr1_B(i,j, 1,Array,Work,mnx,nx,ny,
     &                      xpoints,ypoints,npoints)
          c=c-1
          If (c.eq.0) Goto 10031
          Goto 10033
      End If
      Goto 10031

C  Move up side 2 after missing values.
20011 j=jj
      If (j.ge.turn2) Then
          If (i.le.turn3) Return
          turn2=turn2-1
          Goto 30011
        End If
      jj=j+1
      bbb=Work(i,j,4)
      If (iand (int(bbb),1).eq.1) Goto 20011
      If (bbb.eq.0) Goto 20011

c  Cell side does not have missing values, reinitialize search.
      D2=Array(i,j)
      If (bbb.gt.0) Then
          Do 20014 c=1,ncon
20014     If (ConVal(c).gt.D2) Goto 20022
          Goto 20021
      Else
          Do 20015 c=ncon,1,-1
20015     If (ConVal(c).lt.D2) Goto 20032
          Goto 20031
      End If

c  Move up side 2, values increasing as we go.
20021 j=jj
      If (j.ge.turn2) Then
          If (i.le.turn3) Return
          turn2=turn2-1
          Goto 30021
        End If
      jj=j+1
      bbb=Work(i,j,4)
      If (iand (int(bbb),1).eq.1) Goto 20011
      If (bbb.lt.0) Then
          c=c-1
          If (c.eq.0) Goto 20031
          Goto 20032
      End If
      If (c.eq.nconp) Goto 20021
      If (bbb.eq.0) Goto 20021

c  Step through possible contours we can start.
20022 D2=Array(ii,jj)
20023 If (ConVal(c).lt.D2) Then
          If (iand(Work(i,j,2),CMask(c)).eq.0)
     &        Call Contr1_B(i,j, 2,Array,Work,mnx,nx,ny,
     &                      xpoints,ypoints,npoints)
          c=c+1
          If (c.eq.nconp) Goto 20021
          Goto 20023
      End If
      Goto 20021

c  Move up side 2, values decreasing as we go.
20031 j=jj
      If (j.ge.turn2) Then
          If (i.le.turn3) Return
          turn2=turn2-1
          Goto 30031
        End If
      jj=j+1
      bbb=Work(i,j,4)
      If (iand (int(bbb),1).eq.1) Goto 20011
      If (bbb.gt.0) Then
          c=c+1
          If (c.eq.nconp) Goto 20021
          Goto 20022
      End If
      If (c.eq.0) Goto 20031
      If (bbb.eq.0) Goto 20031

c  Find group of contours which are bracketed.
20032 D2=Array(ii,jj)
20033 If (ConVal(c).gt.D2) Then
          If (iand(Work(i,j,2),CMask(c)).eq.0)
     &        Call Contr1_B(i,j, 2,Array,Work,mnx,nx,ny,
     &                      xpoints,ypoints,npoints)
          c=c-1
          If (c.eq.0) Goto 20031
          Goto 20033
      End If
      Goto 20031

C  Move back along side 3 after missing values.
30011 i=ii
      If (i.le.turn3) Then
          If (j.le.turn4) Return
          turn3=turn3+1
          Goto 40011
        End If
      ii=i-1
      bbb=Work(ii,jj,3)
      If (iand (int(bbb),1).eq.1) Goto 30011
      If (bbb.eq.0) Goto 30011

c  Cell side does not have missing values, reinitialize search.
      D2=Array(i,j)
      If (bbb.lt.0) Then
          Do 30014 c=1,ncon
30014     If (ConVal(c).gt.D2) Goto 30022
          Goto 30021
      Else
          Do 30015 c=ncon,1,-1
30015     If (ConVal(c).lt.D2) Goto 30032
          Goto 30031
      End If

c  Move back along side 3, values increasing as we go.
30021 i=ii
      If (i.le.turn3) Then
          If (j.le.turn4) Return
          turn3=turn3+1
          Goto 40021
        End If
      ii=i-1
      bbb=Work(ii,jj,3)
      If (iand (int(bbb),1).eq.1) Goto 30011
      If (bbb.gt.0) Then
          c=c-1
          If (c.eq.0) Goto 30031
          Goto 30032
      End If
      If (c.eq.nconp) Goto 30021
      If (bbb.eq.0) Goto 30021

c  Step through possible contours we can start.
30022 D2=Array(ii,jj)
30023 If (ConVal(c).lt.D2) Then
          If (iand(Work(ii,jj,1),CMask(c)).eq.0)
     &        Call Contr1_B(ii,jj, 3,Array,Work,mnx,nx,ny,
     &                      xpoints,ypoints,npoints)
          c=c+1
          If (c.eq.nconp) Goto 30021
          Goto 30023
      End If
      Goto 30021

c  Move back along side 3, values decreasing as we go.
30031 i=ii
      If (i.le.turn3) Then
          If (j.le.turn4) Return
          turn3=turn3+1
          Goto 40031
        End If
      ii=i-1
      bbb=Work(ii,jj,3)
      If (iand (int(bbb),1).eq.1) Goto 30011
      If (bbb.lt.0) Then
          c=c+1
          If (c.eq.nconp) Goto 30021
          Goto 30022
      End If
      If (c.eq.0) Goto 30031
      If (bbb.eq.0) Goto 30031

c  Step through possible contours we can start.
30032 D2=Array(ii,jj)
30033 If (ConVal(c).gt.D2) Then
          If (iand(Work(ii,jj,1),CMask(c)).eq.0)
     &        Call Contr1_B(ii,jj, 3,Array,Work,mnx,nx,ny,
     &                      xpoints,ypoints,npoints)
          c=c-1
          If (c.eq.0) Goto 30031
          Goto 30033
      End If
      Goto 30031

C  Move down side 4 after missing values.
40011 j=jj
      If (j.le.turn4) Then
          If (i.ge.turn1) Return
          turn4=turn4+1
          Goto 10011
        End If
      jj=j-1
      bbb=Work(ii,jj,4)
      If (iand (int(bbb),1).eq.1) Goto 40011
      If (bbb.eq.0) Goto 40011

c  Cell side does not have missing values, reinitialize search.
      D2=Array(i,j)
      If (bbb.lt.0) Then
          Do 40014 c=1,ncon
40014     If (ConVal(c).gt.D2) Goto 40022
          Goto 40021
      Else
          D2=Array(i,j)
          Do 40015 c=ncon,1,-1
40015     If (ConVal(c).lt.D2) Goto 40032
          Goto 40031
      End If

c  Move down side 4, values increasing as we go.
40021 j=jj
      If (j.le.turn4) Then
          If (i.ge.turn1) Return
          turn4=turn4+1
          Goto 10021
        End If
      jj=j-1
      bbb=Work(ii,jj,4)
      If (iand (int(bbb),1).eq.1) Goto 40011
      If (bbb.gt.0) Then
          c=c-1
          If (c.eq.0) Goto 40031
          Goto 40032
      End If
      If (c.eq.nconp) Goto 40021
      If (bbb.eq.0) Goto 40021

c  Step through possible contours we can start.
40022 D2=Array(ii,jj)
40023 If (ConVal(c).lt.D2) Then
          If (iand(Work(ii,jj,2),CMask(c)).eq.0)
     &        Call Contr1_B(ii,jj, 4,Array,Work,mnx,nx,ny,
     &                      xpoints,ypoints,npoints)
          c=c+1
          If (c.eq.nconp) Goto 40021
          Goto 40023
      End If
      Goto 40021

c  Move down side 4, values decreasing as we go.
40031 j=jj
      If (j.le.turn4) Then
          If (i.ge.turn1) Return
          turn4=turn4+1
          Goto 10031
        End If
      jj=j-1
      bbb=Work(ii,jj,4)
      If (iand (int(bbb),1).eq.1) Goto 40011
      If (bbb.lt.0) Then
          c=c+1
          If (c.eq.nconp) Goto 40021
          Goto 40022
      End If
      If (c.eq.0) Goto 40031
      If (bbb.eq.0) Goto 40031

c  Step through possible contours we can start.
40032 D2=Array(ii,jj)
40033 If (ConVal(c).gt.D2) Then
          If (iand(Work(ii,jj,2),CMask(c)).eq.0)
     &        Call Contr1_B(ii,jj, 4,Array,Work,mnx,nx,ny,
     &                      xpoints,ypoints,npoints)
          c=c-1
          If (c.eq.0) Goto 40031
          Goto 40033
      End If
      Goto 40031

      End

      Subroutine Contr1_B(istart,jstart,sstart,
     -                    Dat,Work,mnx,nx,ny,
     &                    xpoints,ypoints,npoints)

      Implicit None
c declare arguments
      Integer*4 istart,jstart,sstart,mnx,nx,ny
      Real*4    Dat(mnx,*)
      Byte      Work(nx,ny,4)
      Real*4    xpoints(*),ypoints(*)
      Integer*4 npoints
c
      Integer*4 mwi,mnc,mp
      Parameter (mwi=1500,mnc=200,mp=2000)

      Logical*4 GGG1,GGG2,GGG3,GGG4,GGGD,
     -          backok,clos1,clos2,clos3,clos4,loop

      Integer*4 icell,jcell,iplus,jplus

      Byte      cmw

      Real*4    D1,D2,D3,D4,val4,
     -          val,minavg,maxavg

c unused variable      Byte      b02/'02'X/

      Real*4    Ipnt(-mp:2*mp),Jpnt(-mp:2*mp)
      Integer*4 kpnt,kpnt1,kpnt2,dkpnt,k

      Integer*4 celcnt,labsep,dlx,dld,dly,c,labsep2,
     &          Patern(mnc),ChrN(mnc)
      Real*4    ConVal(mnc),ConVals(mnc),MxAvg(mnc),MnAvg(mnc)
      Byte      Cmask(mnc)
      Byte      LabStr(7,mnc)
      Common   /ConNewCmnBuf/celcnt,labsep,dlx,dld,dly,c,
     &                    labsep2,Patern,LabStr,ChrN,
     &                    ConVal,ConVals,MxAvg,MnAvg,CMask

      Integer*4 npasses
      Real*4    wgt1
      Common   /SmoothCmnBuf/npasses,wgt1
c unused variable      Byte      LCpnt(-mp:2*mp)
c unused variable      Data    LCPnt/mp*-1,-1,mp*-1,mp*-1/
      Data      npasses/0/

C  Nomenclature for cell (i,j)

c      point 4   (i,j+1)-----side 3------(i+1,j+1)  point 3
c                  |                         |
c                  |                         |
c                  |                         |
c                side 4                   side 2
c                  |                         |
c                  |                         |
c                  |                         |
c      point 1   (i,j)-------side 1------(i+1,j)  point 2

C  Initialize.
      val=ConVal(c)
      minavg=MnAvg(c)
      maxavg=MxAvg(c)
      cmw=Cmask(c)
      loop=.false.
      kpnt=0
      kpnt1=0
      kpnt2=0
      dkpnt=1
      val4=4*val
      clos1=.false.
      clos2=.false.
      clos3=.false.
      clos4=.false.
      backok=.false.
      Goto (51,52,53,54) sstart

C Start contour on side one.
51    If (jstart.gt.1) Then
          backok=.true.
      End If
61    jcell=jstart
      jplus=jstart+1
      icell=istart
      iplus=istart+1
      clos1=.true.  !@@@@
      D3=Dat(iplus,jcell)
      D4=Dat(icell,jcell)
      GGG3=val.ge.D3
      GGG4=val.ge.D4
      If (GGG3.eqv.GGG4) Goto 9000
      Work(icell,jcell,1)=ior(Work(icell,jcell,1),cmw)
      Ipnt(0)=icell+(val-D4)/(D3-D4)
      Jpnt(0)=jcell
      Goto 0101

C Start contour on side two.
52    If (istart.lt.nx) Then
          backok=.true.
      End If
62    icell=istart-1
      iplus=istart
      jcell=jstart
      jplus=jstart+1
      clos2=.true.  !@@@@
      D1=Dat(iplus,jcell)
      D4=Dat(iplus,jplus)
      GGG1=val.ge.D1
      GGG4=val.ge.D4
      If (GGG1.eqv.GGG4) Goto 9000
      Work(iplus,jcell,2)=ior(Work(iplus,jcell,2),cmw)
      Ipnt(0)=iplus
      Jpnt(0)=jcell+(val-D1)/(D4-D1)
      Goto 0102

C Start contour on side three.
53    If (jstart.lt.ny) Then
          backok=.true.
      End If
63    jcell=jstart-1
      jplus=jstart
      icell=istart
      iplus=istart+1
      clos3=.true.  !@@@@
      D1=Dat(icell,jplus)
      D2=Dat(iplus,jplus)
      GGG1=val.ge.D1
      GGG2=val.ge.D2
      If (GGG1.eqv.GGG2) Goto 9000
      Work(icell,jplus,1)=ior(Work(icell,jplus,1),cmw)
      Ipnt(0)=icell+(val-D1)/(D2-D1)
      Jpnt(0)=jplus
      Goto 0103

C Start contour on side four.
54    If (istart.gt.1) Then
          backok=.true.
      End If
64    icell=istart
      iplus=istart+1
      jcell=jstart
      jplus=jstart+1
      clos4=.true.  !@@@@
      D3=Dat(icell,jplus)
      D2=Dat(icell,jcell)
      GGG3=val.ge.D3
      GGG2=val.ge.D2
      If (GGG3.eqv.GGG2) Goto 9000
      Work(icell,jcell,2)=ior(Work(icell,jcell,2),cmw)
      Ipnt(0)=icell
      Jpnt(0)=jcell+(val-D2)/(D3-D2)
      Goto 0104

C Entering side one, establish cell boundary information.
0101  GGG1=GGG4
      D1=D4
      GGG2=GGG3
      D2=D3
      D3=Dat(iplus,jplus)
      D4=Dat(icell,jplus)
      GGG3=val.ge.D3
      GGG4=val.ge.D4

c      Write (*,*) icell,jcell,1
c      Write (*,*) GGG4,D4,D3,GGG3
c      Write (*,*) GGG1,D1,D2,GGG2

C handle case of missing data.
      If (iand(int(Work(icell,jplus,3)),1).eq.1) Then
          If (iand(int(Work(icell,jcell,4)),1).eq.0) Then
              If (GGG4.neqv.GGG2) Goto 9000
              celcnt=celcnt+dld
              Goto 4422
          Else If (iand(int(Work(iplus,jcell,4)),1).eq.0) Then
              If (GGG3.neqv.GGG1) Goto 9000
              celcnt=celcnt+dld
              Goto 2244
          End If
          Goto 9000
      End If
c      Write (*,*) 'Passes missing value test'

C  Determine proper path through cell from side one.
      If (GGG3.eqv.GGG4) Then
          celcnt=celcnt+dld
          If (GGG2.eqv.GGG3) Goto 4422
          Goto 2244
      Else
          If (GGG1.eqv.GGG4) Then
              celcnt=celcnt+dly
              Goto 3311
            End If
          celcnt=celcnt+dld
          GGGD=val4.ge.(D1+D2+D3+D4)
          If (GGG1.neqv.GGGD) Goto 4422
          Goto 2244
      End If

C Entering side two, establish cell boundary information.
0102  GGG2=GGG1
      D2=D1
      GGG3=GGG4
      D3=D4
      D1=Dat(icell,jcell)
      D4=Dat(icell,jplus)
      GGG1=val.ge.D1
      GGG4=val.ge.D4

c      Write (*,*) icell,jcell,2
c      Write (*,*) GGG4,D4,D3,GGG3
c      Write (*,*) GGG1,D1,D2,GGG2

C handle case of missing data.
      If (iand(int(Work(icell,jcell,4)),1).eq.1) Then
          If (iand(int(Work(icell,jplus,3)),1).eq.0) Then
              If (GGG4.neqv.GGG2) Goto 9000
              celcnt=celcnt+dld
              Goto 3311
          Else If (iand(int(Work(icell,jcell,3)),1).eq.0) Then
              If (GGG3.neqv.GGG1) Goto 9000
              celcnt=celcnt+dld
              Goto 1133
          End If
          Goto 9000
      End If
c      Write (*,*) 'Passes missing value test'

C  Determine proper path through cell from side two.
      If (GGG1.eqv.GGG4) Then
          celcnt=celcnt+dld
          If (GGG3.eqv.GGG4) Goto 1133
          Goto 3311
      Else
          If (GGG1.eqv.GGG2) Then
              celcnt=celcnt+dlx
              Goto 4422
            End If
          celcnt=celcnt+dld
          GGGD=val4.ge.(D1+D2+D3+D4)
          If (GGG2.neqv.GGGD) Goto 1133
          Goto 3311
      End If

C Entering side three, establish cell boundary information.
0103  GGG3=GGG2
      D3=D2
      GGG4=GGG1
      D4=D1
      D1=Dat(icell,jcell)
      D2=Dat(iplus,jcell)
      GGG1=val.ge.D1
      GGG2=val.ge.D2

c      Write (*,*) icell,jcell,3
c      Write (*,*) GGG4,D4,D3,GGG3
c      Write (*,*) GGG1,D1,D2,GGG2

C handle case of missing data.
      If (iand(int(Work(icell,jcell,3)),1).eq.1) Then
          If (iand(int(Work(iplus,jcell,4)),1).eq.0) Then
              If (GGG4.neqv.GGG2) Goto 9000
              celcnt=celcnt+dld
              Goto 2244
          Else If (iand(int(Work(icell,jcell,4)),1).eq.0) Then
              If (GGG3.neqv.GGG1) Goto 9000
              celcnt=celcnt+dld
              Goto 4422
          End If
          Goto 9000
      End If
c      Write (*,*) 'Passes missing value test'

C  Determine proper path through cell from side three.
      If (GGG1.eqv.GGG2) Then
          celcnt=celcnt+dld
          If (GGG1.eqv.GGG4) Goto 2244
          Goto 4422
      Else
          If (GGG2.eqv.GGG3) Then
              celcnt=celcnt+dly
              Goto 1133
            End If
          celcnt=celcnt+dld
          GGGD=val4.ge.(D1+D2+D3+D4)
          If (GGG3.neqv.GGGD) Goto 2244
          Goto 4422
      End If

C Entering side four, establish cell boundary information.
0104  GGG4=GGG3
      D4=D3
      GGG1=GGG2
      D1=D2
      D3=Dat(iplus,jplus)
      D2=Dat(iplus,jcell)
      GGG3=val.ge.D3
      GGG2=val.ge.D2

c      Write (*,*) icell,jcell,4
c      Write (*,*) GGG4,D4,D3,GGG3
c      Write (*,*) GGG1,D1,D2,GGG2

C handle case of missing data.
      If (iand(int(Work(iplus,jcell,4)),1).eq.1) Then
          If (iand(int(Work(icell,jcell,3)),1).eq.0) Then
              If (GGG4.neqv.GGG2) Goto 9000
              celcnt=celcnt+dld
              Goto 1133
          Else If (iand(int(Work(icell,jplus,3)),1).eq.0) Then
              If (GGG3.neqv.GGG1) Goto 9000
              celcnt=celcnt+dld
              Goto 3311
          End If
          Goto 9000
      End If
c      Write (*,*) 'Passes missing value test'

C  Determine proper path through cell from side four.
      If (GGG2.eqv.GGG3) Then
          celcnt=celcnt+dld
          If (GGG1.eqv.GGG2) Goto 3311
          Goto 1133
      Else
          If (GGG3.eqv.GGG4) Then
              celcnt=celcnt+dlx
              Goto 2244
            End If
          celcnt=celcnt+dld
          GGGD=val4.ge.(D1+D2+D3+D4)
          If (GGG4.neqv.GGGD) Goto 3311
          Goto 1133
      End If

C Cross to side 1 and enter new cell from side 3.
1133  kpnt=kpnt+dkpnt
      Ipnt(kpnt)=icell+(val-D1)/(D2-D1)
      Jpnt(kpnt)=jcell
      Work(icell,jcell,1)=ior(Work(icell,jcell,1),cmw)
      If (jcell.eq.1) Goto 9000

c  Check if contour has closed off, advance values for cell bounds indices.
1199  If (clos3) Then
          If (icell.eq.istart .and. jcell.eq.jstart) Goto 8999
        End If
c      Write (*,*) 'Passes loop test'
      jplus=jcell
      jcell=jcell-1
      Goto 0103

C Cross to side 2 and enter new cell from side 4.
2244  kpnt=kpnt+dkpnt
      Ipnt(kpnt)=iplus
      Jpnt(kpnt)=jcell+(val-D2)/(D3-D2)
      Work(iplus,jcell,2)=ior(Work(iplus,jcell,2),cmw)
      If (iplus.eq.nx) Goto 9000

c  Check if contour has closed off, advance values for cell bounds indices.
2299  If (clos4) Then
          If (iplus.eq.istart .and. jcell.eq.jstart) Goto 8999
        End If
c      Write (*,*) 'Passes loop test'
      icell=iplus
      iplus=iplus+1
      Goto 0104

C Cross to side 3 and enter new cell from side 1.
3311  kpnt=kpnt+dkpnt
      Ipnt(kpnt)=icell+(val-D4)/(D3-D4)
      Jpnt(kpnt)=jplus
      Work(icell,jplus,1)=ior(Work(icell,jplus,1),cmw)
      If (jplus.eq.ny) Goto 9000

c  Check if contour has closed off, advance values for cell bounds indices.
3399  If (clos1) Then
          If (icell.eq.istart .and. jplus.eq.jstart) Goto 8999
        End If
c      Write (*,*) 'Passes loop test'
      jcell=jplus
      jplus=jplus+1
      Goto 0101

C Cross to side 4 and enter new cell from side 2.
4422  kpnt=kpnt+dkpnt
      Ipnt(kpnt)=icell
      Jpnt(kpnt)=jcell+(val-D1)/(D4-D1)
      Work(icell,jcell,2)=ior(Work(icell,jcell,2),cmw)
      If (icell.eq.1) Goto 9000

c  Check if contour has closed off, advance values for cell bounds
c indices.
4499  If (clos2) Then
          If (icell.eq.istart .and. jcell.eq.jstart) Goto 8999
        End If
c      Write (*,*) 'Passes loop test'
      iplus=icell
      icell=icell-1
      Goto 0102

C Finished.
8999  backok=.false.
      loop=.false.
9000  If (dkpnt.eq.1) Then
          kpnt2=kpnt
      Else
          kpnt1=kpnt
      End If
      If (backok) Then
          backok=.false.
          clos1=.false.
          clos2=.false.
          clos3=.false.
          clos4=.false.
          kpnt=0
          dkpnt=-1
          Goto (63,64,61,62) sstart
      End If

c  Check if we have anything to draw.
      If (kpnt1.eq.kpnt2) Return

c  Sentinel for the contour value.
      npoints=npoints+1
      xpoints(npoints) = -99999
      ypoints(npoints) = ConVals(c)

c  Put points into global buffer
      Do 9019 k=kpnt1,kpnt2
      npoints=npoints+1
      xpoints(npoints) = IPnt(k)
      ypoints(npoints) = JPnt(k)
9019  Continue

      Return
      End

      Subroutine Smoothing_B(smoothness,npass)
C  Inputs:
C   smoothness  R*4  Must be >=0.  1.0 is nominal.
C   npass       I*4  Number of smoothing passes.  0 to 10.

      Implicit  None
c declare arguments
      Real*4    smoothness
      Integer*4 npass
c
      Integer*4 npasses
      Real*4    wgt1
      Common   /SmoothCmnBuf/npasses,wgt1
Common block `smoothcmn' already initialized by Subroutine Contr1_B()
c      Data      npasses/0/

      If (smoothness.lt.0.0) Return
      If (npass.lt.0 .or. npass.gt.10) Return
      If (smoothness.eq.0.0 .or. npass.eq.0) Then
          npasses=0
      Else
          npasses=npass
          wgt1=smoothness
      End If

      Return
      End

      Subroutine ZeroItOutBuf(Work,nn)
      Implicit None
c declare arguments
      Byte      Work(*)
      Integer*4  nn
c
      Integer*4  i

      Do 1 i=1,nn
1       Work(i)=0
      Return
      End
