      subroutine cnv22(ifl1,ifl2,ipack,usemiss,imiss)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    cnv22
C   PRGMMR: Gilbert        ORG: W/NP11    DATE: 2003-06-11
C
C ABSTRACT: This subroutine converts every GRIB2 field in a file
C   to another GRIB2 field, most likely one using a different
C   packing option.
C
C PROGRAM HISTORY LOG:
C 2003-06-11  Gilbert
C 2008-05-14  Vuong    - Add missing value management option 0
C
C USAGE:    CALL cnv22(ifl1,ifl2,ipack,usemiss,imiss)
C   INPUT ARGUMENT LIST:
C     ifl1   - Fortran unit number of input GRIB2 file
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

      use grib_mod
      use params
      use re_alloc

      integer,intent(in) :: ifl1,ifl2,ipack
      logical,intent(in) :: usemiss

      CHARACTER(len=1),pointer,dimension(:) :: cgrib
      CHARACTER(len=8) :: ctemp
      type(gribfield) :: gfld,prevfld
      integer,dimension(200) :: jids,jpdt,jgdt
      integer :: listsec0(2)=(/0,2/)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer :: idrstmpl(200)
      integer :: currlen=1000000
      logical :: unpack=.true.
      logical :: open_grb=.false.
      logical*1,target,dimension(1) :: dummy
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      allocate(cgrib(currlen))
      IFLI1=0
      jdisc=-1
      jids=-9999
      jpdt=-9999
      jgdt=-9999
      jpdtn=-1
      jgdtn=-1
!      
      npoints=0
      icount=0
      jskp=0
      do 
         prevfld=gfld
         call getgb2(ifl1,ifli1,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &               unpack,jskp,gfld,iret)
         if ( iret.ne.0) then
            if ( iret.eq.99 ) exit
            print *,' getgb2 error = ',iret
            cycle
            !call errexit(17)
         endif
         icount=icount+1
         !
         !  Ensure that cgrib array is large enough
         !
         if (gfld%ifldnum == 1 ) then         ! start new GRIB2 message
            npoints=gfld%ngrdpts
         else
            npoints=npoints+gfld%ngrdpts
         endif
         newlen=npoints*4
         if ( newlen.gt.currlen ) then
            !if (allocated(cgrib)) deallocate(cgrib)
            !allocate(cgrib(newlen),stat=is)
            call realloc(cgrib,currlen,newlen,is)
            currlen=newlen
         endif
         !
         !  Start new GRIB2 message, if necessary.
         !  May have to finish the current message though.
         !
         if (gfld%ifldnum == 1 ) then         ! start new GRIB2 message
            if (open_grb) then           ! close previous GRIB2 message first
               call gribend(cgrib,lcgrib,lengrib,ierr)
               if (ierr.ne.0) then
                 write(6,*) ' ERROR ending new GRIB2 message = ',ierr
                 cycle
               endif
               open_grb=.false.
               call wryte(ifl2,lengrib,cgrib)
            endif
            !
            !   Create new GRIB Message
            !
            listsec0(1)=gfld%discipline
            listsec0(2)=gfld%version
            call gribcreate(cgrib,lcgrib,listsec0,gfld%idsect,ierr)
            if (ierr.ne.0) then
               write(6,*) ' ERROR creating new GRIB2 field = ',ierr
               cycle
            endif
            open_grb=.true.
         endif
         !
         !   Add grid to GRIB message, if previous grid in same
         !   message is not the same.
         !
         previgds=igds
         igds(1)=gfld%griddef
         igds(2)=gfld%ngrdpts
         igds(3)=gfld%numoct_opt
         igds(4)=gfld%interp_opt
         igds(5)=gfld%igdtnum
         if ( .NOT. associated(gfld%list_opt) ) 
     &                          allocate(gfld%list_opt(1))
         if (gfld%ifldnum == 1 ) then         ! add grid to GRIB2 message
            call addgrid(cgrib,lcgrib,igds,gfld%igdtmpl,gfld%igdtlen,
     &                   gfld%list_opt,gfld%num_opt,ierr)
         else         ! check if previous grid is the same as the current
           if ( gfld%igdtlen.ne.prevfld%igdtlen .OR.
     &          gfld%num_opt.ne.prevfld%num_opt .OR.
     &          any(igds.ne.previgds) .OR.
     &          any(gfld%igdtmpl(1:gfld%igdtlen).NE.
     &           prevfld%igdtmpl(1:prevfld%igdtlen)) .OR.
     &          any(gfld%list_opt(1:gfld%num_opt).NE.
     &           prevfld%list_opt(1:prevfld%num_opt)) ) then
            call addgrid(cgrib,lcgrib,igds,gfld%igdtmpl,gfld%igdtlen,
     &                   gfld%list_opt,gfld%num_opt,ierr)
           endif
         endif
         if (ierr.ne.0) then
           write(6,*) ' ERROR adding GRIB2 grid = ',ierr
           cycle
         endif
         call gf_free(prevfld)
         idrstmpl=0
         !
         !   if usemiss is specified, change any bitmaps to 
         !   missing value management for DRTs 5.2 and 5.3.
         !   OR carry on missing value management for fields
         !   already using it.
         !
         if ( usemiss .AND.
     &        (ipack.eq.2 .OR. ipack.eq.31 .OR. ipack.eq.32) ) then
            if ( gfld%ibmap.eq.0 .OR. gfld%ibmap.eq.254) then
               ! change bit-map to missing value mngmt.
               gfld%ibmap=255
               rmiss=minval(gfld%fld(1:gfld%ngrdpts))
                if ( rmiss .lt. -9999.0 ) then
                   rmiss=rmiss*10.0
                else
                   rmiss=-9999.0
                endif
                do i=1,gfld%ngrdpts
                   if ( .NOT. gfld%bmap(i) ) then
                      gfld%fld(i)=rmiss
                      gfld%bmap(i)=.true.
                   endif
                enddo
                idrstmpl(7)=imiss                   ! Primary missing values
                call mkieee(rmiss,idrstmpl(8),1)
            elseif ( gfld%idrtnum.EQ.2 .OR. gfld%idrtnum.EQ.3 ) then
                idrstmpl(7)=gfld%idrtmpl(7)     ! Missing value mgmt
                idrstmpl(8)=gfld%idrtmpl(8)     ! Primary missing value
                idrstmpl(9)=gfld%idrtmpl(9)     ! Secondary missing value
            endif
         endif
         !
         !  If converting from a field using missing value management
         !  in DRTs 5.2 and 5.3 to a DRT that does not support missing
         !  values, convert missings to a bitmap.
         !
         if ( (.NOT. usemiss) .AND.
     &        ( gfld%idrtnum.EQ.2 .OR. gfld%idrtnum.EQ.3 ) .AND.
     &        ( gfld%idrtmpl(7).EQ.1 .OR. gfld%idrtmpl(7).EQ.2) ) then
            call rdieee(gfld%idrtmpl(8),rmissp,1)
            if ( gfld%idrtmpl(7) .EQ. 2) then
                 call rdieee(gfld%idrtmpl(9),rmisss,1)
            else 
                 rmisss=rmissp
            endif
            allocate(gfld%bmap(gfld%ngrdpts))
            do j=1,gfld%ngrdpts
               if ( gfld%fld(j).EQ.rmissp .OR. 
     &              gfld%fld(j).EQ.rmisss ) then
                  gfld%bmap(j)=.false.
               else  
                  gfld%bmap(j)=.true.
               endif  
            enddo
            gfld%ibmap=0
            idrstmpl(7)=0
            idrstmpl(8)=0
            idrstmpl(9)=0
         endif
         !
         !   Add field to GRIB message
         !
         !   Set DRT info  ( packing info )
         if ( ipack.eq.0 ) then
            idrsnum=0
         elseif ( ipack.eq.2 ) then
            idrsnum=2
            idrstmpl(6)=1
         elseif ( ipack.eq.31.OR.ipack.eq.32 ) then
            idrsnum=ipack/10
            idrstmpl(6)=1
            idrstmpl(17)=mod(ipack,10)      ! order of s.d.
        elseif ( ipack.eq.40 .OR. ipack.eq.41 .OR.
     &           ipack.eq.40000 .OR. ipack.eq.40010 ) then
           idrsnum=ipack
           idrstmpl(6)=0
           idrstmpl(7)=255
         else
            idrsnum=3
            idrstmpl(17)=1                  ! order of s.d.
            idrstmpl(6)=1                   ! general group split
            ctemp=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),
     &            gfld%ipdtmpl(2))
            if (ctemp.eq.'A PCP   ') idrsnum=2
         endif
         idrstmpl(2)=gfld%idrtmpl(2)
         idrstmpl(3)=gfld%idrtmpl(3)
         if ( .NOT. associated(gfld%coord_list) ) 
     &                        allocate(gfld%coord_list(1))
         if ( gfld%ibmap.ne.0 .AND. gfld%ibmap.ne.254) then
            if ( .NOT. associated(gfld%bmap) ) allocate(gfld%bmap(1))
         endif
         !
         !   Add field to GRIB message
         !
         call addfield(cgrib,lcgrib,gfld%ipdtnum,gfld%ipdtmpl,
     &                 gfld%ipdtlen,gfld%coord_list,gfld%num_coord,
     &                 idrsnum,idrstmpl,200,
     &                 gfld%fld,gfld%ngrdpts,gfld%ibmap,gfld%bmap,ierr)
         if (ierr.ne.0) then
           write(6,*) ' ERROR adding GRIB2 field = ',ierr
           cycle
         endif

      enddo

      if (open_grb) then           ! close last GRIB2 message
         call gribend(cgrib,lcgrib,lengrib,ierr)
         if (ierr.ne.0) then
           write(6,*) ' ERROR ending new GRIB2 message = ',ierr
           if (associated(cgrib)) deallocate(cgrib)
           call gf_free(gfld)
           call gf_free(prevfld)
           return
         endif
         open_grb=.false.
         call wryte(ifl2,lengrib,cgrib)
      endif

      if (associated(cgrib)) deallocate(cgrib)
      call gf_free(gfld)
      call gf_free(prevfld)

      return
      end

