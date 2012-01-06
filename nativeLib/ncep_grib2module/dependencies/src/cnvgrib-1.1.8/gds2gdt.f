        subroutine gds2gdt(kgds,igds,igdstmpl,idefnum,ideflist,iret)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    gds2gdt
C   PRGMMR: Gilbert        ORG: W/NP11    DATE: 2003-06-17
C
C ABSTRACT: This routine converts a GRIB1 GDS ( in format specfied in
C   w3fi63.f) to necessary info for a GRIB2 Grid Definition Section.
C
C PROGRAM HISTORY LOG:
C 2003-06-17  Gilbert
C 2004-04-27  Gilbert - Added support for Gaussian grids.
C 2007-04-16  Vuong   - Added Curvilinear Orthogonal grids.
C 2007-05-29  Vuong   - Added Rotate Lat/Lon E-grid (203)
C
C USAGE:    CALL gds2gdt(kgds,igds,igdstmpl,idefnum,ideflist,iret)
C   INPUT ARGUMENT LIST:
C     kgds()   - GRIB1 GDS info as returned by w3fi63.f
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     igds()   - Contains information read from the appropriate GRIB Grid
C                Definition Section 3 for the field being returned.
C                Must be dimensioned >= 5.
C                igds(1)=Source of grid definition (see Code Table 3.0)
C                igds(2)=Number of grid points in the defined grid.
C                igds(3)=Number of octets needed for each
C                            additional grid points definition.
C                            Used to define number of
C                            points in each row ( or column ) for
C                            non-regular grids.
C                            = 0, if using regular grid.
C                igds(4)=Interpretation of list for optional points
C                            definition.  (Code Table 3.11)
C                igds(5)=Grid Definition Template Number (Code Table 3.1)
C     igdstmpl() - Grid Definition Template values for GDT 3.igds(5)
C     idefnum    - The number of entries in array ideflist.  
C                  i.e. number of rows ( or columns )
C                  for which optional grid points are defined.
C     ideflist() - Optional integer array containing
C                  the number of grid points contained in each row (or column).
C     iret     - Error return value:
C                  0  = Successful
C                  1  = Unrecognized GRIB1 grid data representation type
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$
! 
        integer,intent(in) :: kgds(*)
        integer,intent(out) :: igds(*),igdstmpl(*),ideflist(*)
        integer,intent(out) :: idefnum,iret

        iret=0
        if (kgds(1).eq.0) then       !  Lat/Lon grid
           idefnum=0
           igds(1)=0                 ! grid def specfied in template
           igds(2)=kgds(2)*kgds(3)   ! num of grid points
           igds(3)=0                 ! octets for further grid definition
           igds(4)=0                 ! interpretation of optional list
           igds(5)=0                 ! Grid Definition Template number
           if ( btest(kgds(6),6) ) then     ! shape of Earth
             igdstmpl(1)=2
           else
             igdstmpl(1)=0
           endif
           igdstmpl(2)=0
           igdstmpl(3)=0
           igdstmpl(4)=0
           igdstmpl(5)=0
           igdstmpl(6)=0
           igdstmpl(7)=0
           igdstmpl(8)=kgds(2)      !Ni
           igdstmpl(9)=kgds(3)      !Nj
           igdstmpl(10)=0
           igdstmpl(11)=0
           igdstmpl(12)=kgds(4)*1000      ! Lat of 1st grid point
           if ( kgds(5).lt.0 ) then       ! Lon of 1st grid point
              igdstmpl(13)=(360000+kgds(5))*1000    ! convert W to E
           else
              igdstmpl(13)=kgds(5)*1000
           endif
           igdstmpl(14)=0                 ! Resolution and Component flags
           if ( btest(kgds(6),7) ) igdstmpl(14)=48
           if ( btest(kgds(6),3) ) igdstmpl(14)=igdstmpl(14)+8
           igdstmpl(15)=kgds(7)*1000      ! Lat of last grid point
           if ( kgds(8).lt.0 ) then       ! Lon of last grid point
              igdstmpl(16)=(360000+kgds(8))*1000    ! convert W to E
           else
              igdstmpl(16)=kgds(8)*1000
           endif
           igdstmpl(17)=kgds(9)*1000      ! Di
           igdstmpl(18)=kgds(10)*1000     ! Dj
           igdstmpl(19)=kgds(11)          ! Scanning mode
           if (kgds(20).ne.255) then         !  irregular grid (eg WAFS)
               igds(2)=kgds(21)              ! num of grid points
               !idefnum=kgds(19)
               if (kgds(2).eq.65535) idefnum=kgds(3)
               if (kgds(3).eq.65535) idefnum=kgds(2)
               imax=0
               do j=1,idefnum
                  ideflist(j)=kgds(21+j)
                  if (ideflist(j).gt.imax) imax=ideflist(j)
               enddo
               igds(3)=1                 ! octets for further grid definition
               if (imax.gt.255) igds(3)=2
               if (imax.gt.65535) igds(3)=3
               if (imax.gt.16777215) igds(3)=4
               igds(4)=1                 ! interpretation of optional list
               igdstmpl(8)=-1
               igdstmpl(17)=-1
           endif
        elseif (kgds(1).eq.1) then       !  Mercator grid
           idefnum=0
           igds(1)=0                 ! grid def specfied in template
           igds(2)=kgds(2)*kgds(3)   ! num of grid points
           igds(3)=0                 ! octets for further grid definition
           igds(4)=0                 ! interpretation of optional list
           igds(5)=10                 ! Grid Definition Template number
           if ( btest(kgds(6),6) ) then     ! shape of Earth
             igdstmpl(1)=2
           else
             igdstmpl(1)=0
           endif
           igdstmpl(2)=0
           igdstmpl(3)=0
           igdstmpl(4)=0
           igdstmpl(5)=0
           igdstmpl(6)=0
           igdstmpl(7)=0
           igdstmpl(8)=kgds(2)              ! Ni
           igdstmpl(9)=kgds(3)              ! Nj
           igdstmpl(10)=kgds(4)*1000        ! Lat of 1st grid point
           if ( kgds(5).lt.0 ) then         ! Lon of 1st grid point
              igdstmpl(11)=(360000+kgds(5))*1000    ! convert W to E
           else
              igdstmpl(11)=kgds(5)*1000
           endif
           igdstmpl(12)=0                   ! Resolution and Component flags
           if ( btest(kgds(6),7) ) igdstmpl(12)=48
           if ( btest(kgds(6),3) ) igdstmpl(12)=igdstmpl(12)+8
           igdstmpl(13)=kgds(9)*1000        ! Lat intersects earth
           igdstmpl(14)=kgds(7)*1000        ! Lat of last grid point
           if ( kgds(8).lt.0 ) then         ! Lon of last grid point
              igdstmpl(15)=(360000+kgds(8))*1000    ! convert W to E
           else
              igdstmpl(15)=kgds(8)*1000
           endif
           igdstmpl(16)=kgds(11)            ! Scanning mode
           igdstmpl(17)=0                   ! Orientation of grid
           igdstmpl(18)=kgds(12)*1000       ! Di
           igdstmpl(19)=kgds(13)*1000       ! Dj
        elseif (kgds(1).eq.3) then       ! Lambert Conformal Grid
           idefnum=0
           igds(1)=0                 ! grid def specfied in template
           igds(2)=kgds(2)*kgds(3)   ! num of grid points
           igds(3)=0                 ! octets for further grid definition
           igds(4)=0                 ! interpretation of optional list
           igds(5)=30                 ! Grid Definition Template number
           if ( btest(kgds(6),6) ) then     ! shape of Earth
             igdstmpl(1)=2
           else
             igdstmpl(1)=0
           endif
           igdstmpl(2)=0
           igdstmpl(3)=0
           igdstmpl(4)=0
           igdstmpl(5)=0
           igdstmpl(6)=0
           igdstmpl(7)=0
           igdstmpl(8)=kgds(2)              ! Nx
           igdstmpl(9)=kgds(3)              ! Ny
           igdstmpl(10)=kgds(4)*1000        ! Lat of 1st grid point
           if ( kgds(5).lt.0 ) then         ! Lon of 1st grid point
              igdstmpl(11)=(360000+kgds(5))*1000    ! convert W to E
           else
              igdstmpl(11)=kgds(5)*1000
           endif
           igdstmpl(12)=0                   ! Resolution and Component flags
           if ( btest(kgds(6),7) ) igdstmpl(12)=48
           if ( btest(kgds(6),3) ) igdstmpl(12)=igdstmpl(12)+8
           igdstmpl(13)=kgds(12)*1000       ! Lat where Dx and Dy specified
           if ( kgds(7).lt.0 ) then         ! Lon of orientation
              igdstmpl(14)=(360000+kgds(7))*1000    ! convert W to E
           else
              igdstmpl(14)=kgds(7)*1000
           endif
           igdstmpl(15)=kgds(8)*1000        ! Dx
           igdstmpl(16)=kgds(9)*1000        ! Dy
           igdstmpl(17)=kgds(10)            ! Projection Center Flag
           igdstmpl(18)=kgds(11)            ! Scanning mode
           igdstmpl(19)=kgds(12)*1000       ! Latin 1
           igdstmpl(20)=kgds(13)*1000       ! Latin 2
           igdstmpl(21)=kgds(14)*1000       ! Lat of S. Pole of projection
           if ( kgds(15).lt.0 ) then        ! Lon of S. Pole of projection
              igdstmpl(22)=(360000+kgds(15))*1000    ! convert W to E
           else
              igdstmpl(22)=kgds(15)*1000
           endif
        elseif (kgds(1).eq.4) then       !  Gaussian Lat/Lon grid
           idefnum=0
           igds(1)=0                 ! grid def specfied in template
           igds(2)=kgds(2)*kgds(3)   ! num of grid points
           igds(3)=0                 ! octets for further grid definition
           igds(4)=0                 ! interpretation of optional list
           igds(5)=40                ! Grid Definition Template number
           if ( btest(kgds(6),6) ) then     ! shape of Earth
             igdstmpl(1)=2
           else
             igdstmpl(1)=0
           endif
           igdstmpl(2)=0
           igdstmpl(3)=0
           igdstmpl(4)=0
           igdstmpl(5)=0
           igdstmpl(6)=0
           igdstmpl(7)=0
           igdstmpl(8)=kgds(2)      !Ni
           igdstmpl(9)=kgds(3)      !Nj
           igdstmpl(10)=0
           igdstmpl(11)=0
           igdstmpl(12)=kgds(4)*1000      ! Lat of 1st grid point
           if ( kgds(5).lt.0 ) then       ! Lon of 1st grid point
              igdstmpl(13)=(360000+kgds(5))*1000    ! convert W to E
           else
              igdstmpl(13)=kgds(5)*1000
           endif
           igdstmpl(14)=0                 ! Resolution and Component flags
           if ( btest(kgds(6),7) ) igdstmpl(14)=48
           if ( btest(kgds(6),3) ) igdstmpl(14)=igdstmpl(14)+8
           igdstmpl(15)=kgds(7)*1000      ! Lat of last grid point
           if ( kgds(8).lt.0 ) then       ! Lon of last grid point
              igdstmpl(16)=(360000+kgds(8))*1000    ! convert W to E
           else
              igdstmpl(16)=kgds(8)*1000
           endif
           igdstmpl(17)=kgds(9)*1000      ! Di
           igdstmpl(18)=kgds(10)          ! D - Number of parallels
           igdstmpl(19)=kgds(11)          ! Scanning mode
        elseif (kgds(1).eq.5) then       ! Polar Stereographic Grid
           idefnum=0
           igds(1)=0                 ! grid def specfied in template
           igds(2)=kgds(2)*kgds(3)   ! num of grid points
           igds(3)=0                 ! octets for further grid definition
           igds(4)=0                 ! interpretation of optional list
           igds(5)=20                 ! Grid Definition Template number
           if ( btest(kgds(6),6) ) then     ! shape of Earth
             igdstmpl(1)=2
           else
             igdstmpl(1)=0
           endif
           igdstmpl(2)=0
           igdstmpl(3)=0
           igdstmpl(4)=0
           igdstmpl(5)=0
           igdstmpl(6)=0
           igdstmpl(7)=0
           igdstmpl(8)=kgds(2)              ! Nx
           igdstmpl(9)=kgds(3)              ! Ny
           igdstmpl(10)=kgds(4)*1000        ! Lat of 1st grid point
           if ( kgds(5).lt.0 ) then         ! Lon of 1st grid point
              igdstmpl(11)=(360000+kgds(5))*1000    ! convert W to E
           else
              igdstmpl(11)=kgds(5)*1000
           endif
           igdstmpl(12)=0                   ! Resolution and Component flags
           if ( btest(kgds(6),7) ) igdstmpl(12)=48
           if ( btest(kgds(6),3) ) igdstmpl(12)=igdstmpl(12)+8
           igdstmpl(13)=60000000            ! Lat where Dx and Dy specified
           if ( btest(kgds(10),7) ) igdstmpl(13)=-60000000
           if ( kgds(7).lt.0 ) then         ! Lon of orientation
              igdstmpl(14)=(360000+kgds(7))*1000    ! convert W to E
           else
              igdstmpl(14)=kgds(7)*1000
           endif
           igdstmpl(15)=kgds(8)*1000        ! Dx
           igdstmpl(16)=kgds(9)*1000        ! Dy
           igdstmpl(17)=kgds(10)            ! Projection Center Flag
           igdstmpl(18)=kgds(11)            ! Scanning mode
         elseif (kgds(1).eq.204) then       ! Curivilinear Orthogonal Grid (Used by RTOFS)
           idefnum=0
           igds(1)=0                 ! grid def specfied in template
           igds(2)=kgds(2)*kgds(3)   ! num of grid points
           igds(3)=0                 ! octets for further grid definition
           igds(4)=0                 ! interpretation of optional list
           igds(5)=204               ! Grid Definition Template number
           if ( btest(kgds(6),6) ) then     ! shape of Earth
             igdstmpl(1)=2
           else
             igdstmpl(1)=0
           endif
           igdstmpl(2)=0
           igdstmpl(3)=0
           igdstmpl(4)=0
           igdstmpl(5)=0
           igdstmpl(6)=0
           igdstmpl(7)=0
           igdstmpl(8)=kgds(2)      !Ni - No of points along x-grid direction
           igdstmpl(9)=kgds(3)      !Nj - No of points along y-grid direction
           igdstmpl(10)=0
           igdstmpl(11)=0
           igdstmpl(12)=0
           igdstmpl(13)=0
           igdstmpl(14)=0                 ! Resolution and Component flags
           if ( btest(kgds(6),7) ) igdstmpl(14)=48
           if ( btest(kgds(6),3) ) igdstmpl(14)=igdstmpl(14)+8
           igdstmpl(15)=0
           igdstmpl(16)=0
           igdstmpl(17)=0
           igdstmpl(18)=0
           igdstmpl(19)=kgds(11)          ! Scanning mode
         elseif (kgds(1).eq.203) then    !  Rot Lat/Lon grid (Arakawa)
            idefnum=0
            igds(1)=0                 ! grid def specfied in template
            igds(2)=kgds(2)*kgds(3)   ! num of grid points
            igds(3)=0                 ! octets for further grid definition
            igds(4)=0                 ! interpretation of optional list
            igds(5)=32768             ! Grid Definition Template number
            if ( btest(kgds(6),6) ) then     ! shape of Earth
              igdstmpl(1)=2
            else
              igdstmpl(1)=0
            endif
            igdstmpl(2)=0
            igdstmpl(3)=0
            igdstmpl(4)=0
            igdstmpl(5)=0
            igdstmpl(6)=0
            igdstmpl(7)=0
            igdstmpl(8)=kgds(2)      !Ni
            igdstmpl(9)=kgds(3)      !Nj
            igdstmpl(10)=0
            igdstmpl(11)=0
            igdstmpl(12)=kgds(4)*1000      ! Lat of 1st grid point
            if ( kgds(5).lt.0 ) then       ! Lon of 1st grid point
               igdstmpl(13)=(360000+kgds(5))*1000    ! convert W to E
            else
               igdstmpl(13)=kgds(5)*1000
            endif
            igdstmpl(14)=0                 ! Resolution and Component flags
            if ( btest(kgds(6),7) ) igdstmpl(14)=48
            if ( btest(kgds(6),3) ) igdstmpl(14)=igdstmpl(14)+8
              igdstmpl(15)=kgds(7)*1000      ! Lat of last grid point
              if ( kgds(8).lt.0 ) then       ! Lon of last grid point
                 igdstmpl(16)=(360000+kgds(8))*1000    ! convert W to E
              else
                 igdstmpl(16)=kgds(8)*1000
              endif
            igdstmpl(17)=kgds(9)*1000      ! Di
            igdstmpl(18)=kgds(10)*1000     ! Dj
            igdstmpl(19)=kgds(11)          ! Scanning mode
        else
           Print *,'gds2gdt: Unrecognized GRIB1 Grid type = ',kgds(1)
           iret=1
        endif
        return
        end
