      subroutine cnv21(ifl1,ifl2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    cnv21 
C   PRGMMR: Gilbert        ORG: W/NP11    DATE: 2003-06-11
C
C ABSTRACT: This subroutine converts every GRIB2 field in a file
C   to a GRIB1 field.  If a GRIB2 message contains more than one
C   data field, then each field is saved in individual GRIB1
C   messages.
C
C PROGRAM HISTORY LOG:
C 2003-06-11  Gilbert
C 2008-05-14  Vuong    - Add option -m0 No explicit missing values included
C                        within data values
C
C USAGE:    CALL cnv21(ifl1,ifl2)
C   INPUT ARGUMENT LIST:
C     ifl1   - Fortran unit number of input GRIB2 file
C     ifl2   - Fortran unit number of output GRIB1 file
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
      integer,intent(in) :: ifl1,ifl2

      CHARACTER(len=1),allocatable,dimension(:) :: cgrib
      CHARACTER(len=8) :: ctemp
      type(gribfield) :: gfld
      integer,dimension(200) :: jids,jpdt,jgdt
      integer :: kpds(200),kgds(200),kens(200),kprob(2)
      integer :: kclust(16),kmembr(80)
      integer :: currlen=0
      integer :: igds(5)=(/0,0,0,0,0/)
      real :: xprob(2)
      logical*1,target,dimension(1) :: dummy
      logical :: unpack=.true.
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      IFLI1=0
      jdisc=-1
      jids=-9999
      jpdt=-9999
      jgdt=-9999
      jpdtn=-1
      jgdtn=-1
!      
      icount=0
      jskp=0
      do 
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
         newlen=4*gfld%ngrdpts
         if ( newlen.gt.currlen ) then
            if (allocated(cgrib)) deallocate(cgrib)
            allocate(cgrib(newlen),stat=is)
            currlen=newlen
         endif
         !
         !   Construct GDS
         !
         igds(1)=gfld%griddef
         igds(2)=gfld%ngrdpts
         igds(3)=gfld%numoct_opt
         igds(4)=gfld%interp_opt
         igds(5)=gfld%igdtnum
         if ( .NOT. associated(gfld%list_opt) ) 
     &              allocate(gfld%list_opt(1))
         call gdt2gds(igds,gfld%igdtmpl,gfld%num_opt,gfld%list_opt,
     &                kgds,igrid,iret)
         if (iret.ne.0) then
           print *,'cnv21: could not create gds'
           cycle
         endif
         !print *,' SAGT: NCEP GRID: ',igrid
         !
         !   Construct PDS
         !
         call makepds(gfld%discipline,gfld%idsect,gfld%ipdtnum,
     &                gfld%ipdtmpl,gfld%ibmap,gfld%idrtnum,
     &                gfld%idrtmpl,kpds,iret)
         if (iret.ne.0) then
           print *,'cnv21: could not create pds'
           cycle
         endif
         kpds(3)=igrid
C
C  Check for Coastal Ocean circulation and UKMET grib grid id.
C  ON 388 defined grid id 238 same as grid 244 
C  If the process model is 45, and UK Met(74), the grid id is 2 or 45 
C  If the process model is 121, the grid id is 238 
C  If the process model is 123, the grid id is 244 
C
         if (kpds(1).eq.7.AND.kpds(2).eq.121) kpds(3)=238
         if (kpds(1).eq.7.AND.kpds(2).eq.123) kpds(3)=244
         if (kpds(1).eq.74) then
            if (kpds(2).eq.45.AND.kpds(3).eq.2)  kpds(3)=2
            if (kpds(2).eq.15.AND.kpds(3).eq.45) kpds(3)=45
            if (kpds(2).eq.45.AND.kpds(3).eq.45) kpds(3)=45
         end if
         !
         !   Construct Ensemble info, if necessary
         !
         if ( (gfld%ipdtnum.ge.1.AND.gfld%ipdtnum.le.6).OR.
     &        (gfld%ipdtnum.ge.9.AND.gfld%ipdtnum.le.14) ) then
            call makepdsens(gfld%ipdtnum,gfld%ipdtmpl,kpds,kens,kprob,
     &                     xprob,kclust,kmembr,iret)
         endif
         !
         !   If not using bit-map, must assign dummy bit-map
         !
         if (gfld%ibmap.ne.0 .AND. gfld%ibmap.ne.254) then
            !gfld%bmap => dummy
            if ( (gfld%idrtnum.eq.2 .OR. gfld%idrtnum.eq.3) .AND.
     &            gfld%idrtmpl(7).ne.0 ) then       ! convert missings to bitmap
                allocate(gfld%bmap(gfld%ngrdpts))
                kpds(4)=ior(kpds(4),64)
                if ( gfld%idrtmpl(7).eq.1 ) then
                   call rdieee(gfld%idrtmpl(8),rmiss1,1)
                   do i=1,gfld%ngrdpts
                      if ( gfld%fld(i) .eq. rmiss1 ) then
                         gfld%bmap(i)=.false.
                      else
                         gfld%bmap(i)=.true.
                      endif
                   enddo
                endif
                if ( gfld%idrtmpl(7).eq.2 ) then
                   call rdieee(gfld%idrtmpl(8),rmiss1,1)
                   call rdieee(gfld%idrtmpl(9),rmiss2,1)
                   do i=1,gfld%ngrdpts
                      if ( gfld%fld(i).eq.rmiss1 .OR. 
     &                     gfld%fld(i).eq.rmiss2) then
                         gfld%bmap(i)=.false.
                      else
                         gfld%bmap(i)=.true.
                      endif
                   enddo
                endif
            endif
            if ( (gfld%idrtnum.eq.2 .OR. gfld%idrtnum.eq.3) .AND.
     &            gfld%idrtmpl(7).eq.0 ) then       ! convert missings to bitmap
                allocate(gfld%bmap(gfld%ngrdpts))
                kpds(4)=ior(kpds(4),64)
                call rdieee(gfld%idrtmpl(8),rmiss1,1)
                if ( rmiss1 .lt. -9999.0 ) then
                   rmiss1=rmiss1*10.0
                else
                   rmiss1=-9999.0
                endif
                do i=1,gfld%ngrdpts
                   if ( gfld%fld(i) .eq. rmiss1 ) then
                      gfld%bmap(i)=.false.
                   else
                      gfld%bmap(i)=.true.
                   endif
                enddo
            endif
         endif
         !
         !   Pack and write GRIB 1 field
         !
         ibs=gfld%idrtmpl(2)
         !print *,'SAGT:before putgbexn'
         if ( .NOT. associated(gfld%bmap) ) allocate(gfld%bmap(1))
         imug=0
         call putgbexn(ifl2,gfld%ngrdpts,kpds,kgds,kens,kprob,
     &                 xprob,kclust,kmembr,ibs,imug,gfld%bmap,
     &                 gfld%fld,iret)
         !print *,'SAGT:after putgbexn'
         if ( iret.ne.0) then
            print *,' putgbexn error = ',iret
            cycle
            !call errexit(17)
         endif

         call gf_free(gfld)

      enddo

      if (allocated(cgrib)) deallocate(cgrib)

      return
      end
