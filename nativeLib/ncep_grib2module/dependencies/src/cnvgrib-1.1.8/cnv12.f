      subroutine cnv12(ifl1,ifl2,ipack,usemiss,imiss,uvvect)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    cnv12 
C   PRGMMR: Gilbert        ORG: W/NP11    DATE: 2003-06-11
C
C ABSTRACT: This subroutine converts every GRIB1 field in a file
C   to a GRIB2 field.  U and V wind component fields are combined
C   into a single GRIB2 message.
C
C PROGRAM HISTORY LOG:
C 2003-06-11  Gilbert
C 2003-05-19  Gilbert  - Changed Master Table Version Number from 1 to 2.
C                      - Added check for grib1 table version with params 191
C                        and 192 for ensemble probs.
C 2007-03-26  Gordon   - Added check for ECMWF data to reference ECMWF
C                        Conversion tables.
C 2007-10-11  Vuong    - Added check for ensemble probs if the kpds > 28
C 2008-01-28  Vuong    - Fixed the V-GRD BY SETTING THE LPDS(22)=-1 and
C                        increase the array size MAXPTS
C 2008-05-14  Vuong    - Add option -m0 No explicit missing values included
C                        within data values
C
C USAGE:    CALL cnv12(ifl1,ifl2,ipack)
C   INPUT ARGUMENT LIST:
C     ifl1   - Fortran unit number of input GRIB1 file
C     ifl2   - Fortran unit number of output GRIB2 file
C     ipack  - GRIB2 packing option:
C              0     = simple packing
C              2     = group packing
C              31    = group pack with 1st order differencing
C              32    = group pack with 2nd order differencing
C              40    = JPEG2000 encoding
C              40000 = JPEG2000 encoding (obsolete)
C              41    = PNG encoding
C              40010 = PNG encoding (obsolete)
C              if ipack .ne. one of the values above, 31 is used as a default.
C    usemiss - uses missing value management (instead of bitmaps), for use
C              ipack options 2, 31, and 32.
C    imiss   - Missing value management:
C              0     = No explicit missing values included within data values
C              1     = Primary missing values included within data values
C    uvvect  - .true. = combine U and V wind components into one GRIB2 msg.
C              .flase. = does not combine U and V wind components
C
C   INPUT FILES:   See ifl1
C
C   OUTPUT FILES:  See ifl2
C
C REMARKS: None
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C$$$

      use params
      use params_ecmwf
      integer,intent(in) :: ifl1,ifl2,ipack
      logical,intent(in) :: usemiss,uvvect

      PARAMETER (MAXPTS=40000000,msk1=32000)
      CHARACTER(len=1),allocatable,dimension(:) :: cgrib,cgribin
      integer KPDS(200),KGDS(200),KPTR(200)
      integer LPDS(200),LGDS(200),KENS(200),LENS(200)
      integer KPROB(2),KCLUST(16),KMEMBR(80)
      real XPROB(2)
      real,allocatable,dimension(:) :: FLD
      real,allocatable,dimension(:) :: FLDV
      real,allocatable,dimension(:) :: coordlist
      integer :: listsec0(2)=(/0,2/),imiss
      integer :: listsec1(13)=(/7,0,2,1,1,0,0,0,0,0,0,0,0/)
      integer :: ideflist(MAXPTS),idefnum
      integer :: igds(5)=(/0,0,0,0,0/),igdstmpl(200),ipdstmpl(200)
      integer :: ipdstmplv(200)
      integer :: idrstmpl(200),idrstmplv(200)
      integer :: currlen=0
      integer,parameter :: mingrib=500
      logical :: ensemble,ecmwf
      Logical*1,allocatable,dimension(:) :: bmp,bmpv
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      ICND=0
      IFLI1=0
      allocate(fld(maxpts))
      allocate(coordlist(maxpts))
      allocate(bmp(maxpts))
!      
      iseek=0
      currlen=0
      do 
        call skgb(ifl1,iseek,msk1,lskip,lgrib)
        if (lgrib.eq.0) exit                ! end loop at EOF or problem
         if (lgrib.gt.currlen) then
            if (allocated(cgribin)) deallocate(cgribin)
            allocate(cgribin(lgrib),stat=is)
            currlen=lgrib
            lcgrib=lgrib*2
            if (lcgrib .lt. mingrib) lcgrib=mingrib
            if (allocated(cgrib)) deallocate(cgrib)
            allocate(cgrib(lcgrib),stat=is)
         endif
        call baread(ifl1,lskip,lgrib,lengrib,cgribin)
        if (lgrib.eq.lengrib) then
           call w3fi63(cgribin,KPDS,KGDS,BMP,FLD,KPTR,IRET)
           numpts=KPTR(10)
           if (iret.ne.0) then
              print *,' cnvgrib: Error unpacking GRIB field.',iret
              iseek=lskip+lgrib
              cycle
           endif
        else
           print *,' cnvgrib: IO Error on input GRIB file.'
           stop
           cycle
        endif
        iseek=lskip+lgrib
        !print *,'kpds:',kpds(1:28)
        !print *,'kpds:',kpds(1:45)
        if ((kpds(5).eq.34).AND.uvvect) cycle ! V-comp already processed with U
        listsec1(1)=kpds(1)
        listsec1(2)=kpds(23)
        listsec1(5)=1
        if (kpds(16).eq.1) listsec1(5)=0
        listsec1(6)=((kpds(21)-1)*100)+kpds(8)
        listsec1(7)=kpds(9)
        listsec1(8)=kpds(10)
        listsec1(9)=kpds(11)
        listsec1(10)=kpds(12)
        listsec1(13)=1
        if (kpds(16).eq.1) listsec1(13)=0
        ensemble=.false.
        if ( (kpds(23).eq.2) .or.
     &       (kptr(3).gt.28 .and. kpds(19).eq.2 .and. 
     &       (kpds(5).eq.191.or.kpds(5).eq.192) ) ) then        ! ensemble forecast
           ensemble=.true.
        endif
        if (ensemble) then    ! ensemble forecast
           call gbyte(cgribin(9),ilast,0,24)
           call pdseup(kens,kprob,xprob,kclust,kmembr,ilast,cgribin(9))
           if (kens(2).eq.1) listsec1(13)=3
           if (kens(2).eq.2.OR.kens(2).eq.3) listsec1(13)=4
           if (kens(2).eq.5) listsec1(13)=5
        endif
        ecmwf=.false.
        if (kpds(1).eq.98) ecmwf=.true.
        if (ecmwf) then         ! treat ecmwf data conversion seperately
           call param_ecmwf_g1_to_g2(kpds(5),kpds(19),listsec0(1),idum,
     &          jdum)           ! set discipline
        else
           if (ensemble.and.(kpds(5).eq.191.or.kpds(5).eq.192)) then
              !kprob(1)=61
              call param_g1_to_g2(kprob(1),kpds(19),listsec0(1),idum,
     &             jdum)        ! set discipline
           else
              call param_g1_to_g2(kpds(5),kpds(19),listsec0(1),idum,
     &             jdum)        ! set discipline
           endif
        endif
        call gribcreate(cgrib,lcgrib,listsec0,listsec1,ierr)
        if (ierr.ne.0) then
          write(6,*) ' ERROR creating new GRIB2 field = ',ierr
          cycle
        endif
! 
!-----------------------------------------------------------------------
! convert grid info
        call gds2gdt(kgds,igds,igdstmpl,idefnum,ideflist,ierr)
        if (ierr.ne.0) then
          cycle
        endif
        if (listsec1(1) .eq. 7 ) igdstmpl(1)=6    ! FOR NWS/NCEP
        call addgrid(cgrib,lcgrib,igds,igdstmpl,200,ideflist,
     &               idefnum,ierr)
        if (ierr.ne.0) then
          write(6,*) ' ERROR adding GRIB2 grid = ',ierr
          cycle
        endif
!-----------------------------------------------------------------------
! set PDS Template
        if (ensemble) then    ! ensemble forecast
           call pds2pdtens(kpds,kens,kprob,xprob,kclust,kmembr,
     &                     ipdsnum,ipdstmpl,numcoord,coordlist,ierr)
        else
           call pds2pdt(kpds,ipdsnum,ipdstmpl,numcoord,coordlist,ierr)
        endif
        if (ierr.ne.0) then
          cycle
        endif
!-----------------------------------------------------------------------
! set bitmap flag
        idrstmpl=0
        if (btest(kpds(4),6)) then
          ibmap=0
          !fld=pack(fld,mask=bmp(1:numpts))
          !itemp=count(bmp(1:numpts))
          !numpts=itemp
          !
          !   convert bitmap to "missing" values, if requested.
          !
          if ( (usemiss) .AND. (ipack.eq.2 .OR. ipack.eq.31 .OR.
     &                          ipack.eq.32) ) then
             ibmap=255
             rmiss=minval(fld(1:numpts))
             if ( rmiss .lt. -9999.0 ) then
                rmiss=rmiss*10.0
             else
                rmiss=-9999.0
             endif
             do i=1,numpts
                if ( .NOT. bmp(i) ) then
                   fld(i)=rmiss
                   bmp(i)=.true.
                endif
             enddo
             idrstmpl(7)=imiss                   ! Missing value management 
             call mkieee(rmiss,idrstmpl(8),1)
          endif
        else
          ibmap=255
          idrstmpl(7)=0                   ! No missing values
        endif

!-----------------------------------------------------------------------
!   Set DRT info  ( packing info )
        if ( ipack.eq.0 ) then
           idrsnum=0
        elseif ( ipack.eq.2 ) then
           idrsnum=2
           idrstmpl(6)=1                   ! general group split
        elseif ( ipack.eq.31.OR.ipack.eq.32 ) then
           idrsnum=ipack/10
           idrstmpl(6)=1                   ! general group split
           idrstmpl(17)=mod(ipack,10)      ! order of s.d.
        elseif ( ipack.eq.40 .OR. ipack.eq.41 .OR.
     &           ipack.eq.40000 .OR. ipack.eq.40010 ) then
           idrsnum=ipack
           idrstmpl(6)=0
           idrstmpl(7)=255
           !idrstmpl(6)=1
           !idrstmpl(7)=15
        else
           idrsnum=3
           idrstmpl(17)=1                  ! order of s.d.
           idrstmpl(6)=1                   ! general group split
           if (kpds(5).eq.61) idrsnum=2
        endif
        idrstmpl(2)=KPTR(19)       ! binary scale
        idrstmpl(3)=kpds(22)       ! decimal scale
        !idrstmpl(2)=-4       ! binary scale
        !idrstmpl(3)=0       ! decimal scale
        call addfield(cgrib,lcgrib,ipdsnum,ipdstmpl,200,
     &                coordlist,numcoord,idrsnum,idrstmpl,200,
     &                fld,numpts,ibmap,bmp,ierr)
c       print *,'done with addfield'
        if (ierr.ne.0) then
          write(6,*) ' ERROR adding GRIB2 field = ',ierr
          cycle
        endif

        if ((kpds(5).eq.33) .AND. uvvect) then
          if (.not.allocated(fldv)) allocate(fldv(maxpts))
          if (.not.allocated(bmpv)) allocate(bmpv(maxpts))
          LGDS=KGDS
          LENS=KENS
          LPDS=KPDS
          LPDS(22)=-1
          LPDS(5)=34
          jsrch=0
          CALL GETGBE(IFL1,IFLI1,MAXPTS,jsrch,LPDS,LGDS,LENS,NUMPTSO,
     *         jsrch,KPDS,KGDS,KENS,BMPV,FLDV,ICND)
          if (icnd.ne.0) then
            write(6,*) ' ERROR READING/UNPACKING GRIB1 V = ',icnd
            exit
          endif
          ipdstmplv=ipdstmpl
          if (ecmwf) then       ! treat ecmwf data conversion seperately
c            print *,' param_ecmwf call 2'
             call param_ecmwf_g1_to_g2(kpds(5),kpds(19),idum,
     &            ipdstmplv(1),ipdstmplv(2))
c            print *,' done with call 2'
          else
             call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmplv(1),
     &            ipdstmplv(2))
          endif
! set bitmap flag
          idrstmplv=0
          if (btest(kpds(4),6)) then
            !fldv=pack(fldv,mask=bmpv(1:numpts))
            if ( ANY(bmp(1:igds(2)) .NEQV. bmpv(1:igds(2))) ) then
               !print *,'SAGT: BITMAP different'
               ibmap=0
               !   convert bitmap to "missing" values, if requested.
               if ( (usemiss) .AND. (ipack.eq.2 .OR. ipack.eq.31 .OR.
     &                               ipack.eq.32) ) then
                  ibmap=255
                  rmiss=minval(fldv(1:numpts))
                  if ( rmiss .lt. -9999.0 ) then
                     rmiss=rmiss*10.0
                  else
                     rmiss=-9999.0
                  endif
                  do i=1,numpts
                     if ( .NOT. bmpv(i) ) then
                        fldv(i)=rmiss
                        bmpv(i)=.true.
                     endif
                  enddo
                  idrstmplv(7)=imiss                   ! Missing values management
                  call mkieee(rmiss,idrstmplv(8),1)
               endif
            else
               !print *,'SAGT: BITMAP SAME'
               ibmap=254
            endif
          else
            ibmap=255
            idrstmplv(7)=0                   ! No missing values
          endif
          !   Set DRT info  ( packing info )
          if ( ipack.eq.0 ) then
             idrsnum=0
          elseif ( ipack.eq.2 ) then
             idrsnum=2
             idrstmplv(6)=1                   ! general group split
          elseif ( ipack.eq.31.OR.ipack.eq.32 ) then
             idrsnum=ipack/10
             idrstmplv(6)=1                   ! general group split
             idrstmplv(17)=mod(ipack,10)      ! order of s.d.
          elseif ( ipack.eq.40 .OR. ipack.eq.41 .OR.
     &             ipack.eq.40000 .OR. ipack.eq.40010 ) then
             idrsnum=ipack
             idrstmplv(6)=0
             idrstmplv(7)=255
             !idrstmplv(6)=1
             !idrstmplv(7)=15
          else
             idrsnum=3
             idrstmplv(17)=1                  ! order of s.d.
             idrstmplv(6)=1                   ! general group split
             if (kpds(5).eq.61) idrsnum=2
          endif
          idrstmplv(2)=KPTR(19)       ! binary scale
          idrstmplv(3)=kpds(22)       ! decimal scale
          !idrstmplv(2)=-4       ! binary scale
          !idrstmplv(3)=0       ! decimal scale
          call addfield(cgrib,lcgrib,ipdsnum,ipdstmplv,200,
     &                coordlist,numcoord,idrsnum,idrstmplv,200,
     &                fldv,numpts,ibmap,bmpv,ierr)
          if (ierr.ne.0) then
            write(6,*) ' ERROR adding second GRIB2 field = ',ierr
            cycle
          endif
        endif
! End GRIB2 field
        call gribend(cgrib,lcgrib,lengrib,ierr)
        if (ierr.ne.0) then
          write(6,*) ' ERROR ending new GRIB2 message = ',ierr
          cycle
        endif
!        print *,' writing ',lengrib,' bytes...'
        call wryte(ifl2,lengrib,cgrib)

      enddo

      if (allocated(cgribin)) deallocate(cgribin)
      if (allocated(cgrib)) deallocate(cgrib)
      if (allocated(fld)) deallocate(fld)
      if (allocated(fldv)) deallocate(fldv)
      if (allocated(coordlist)) deallocate(coordlist)
      if (allocated(bmp)) deallocate(bmp)
      if (allocated(bmpv)) deallocate(bmpv)

      return
      end
