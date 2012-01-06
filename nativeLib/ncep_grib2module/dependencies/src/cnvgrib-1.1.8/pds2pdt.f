        subroutine pds2pdt(kpds,ipdsnum,ipdstmpl,numcoord,coordlist,
     &                     iret)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    pds2pdt
C   PRGMMR: Gilbert        ORG: W/NP11     DATE: 2003-06-12
C
C ABSTRACT: This routine converts a GRIB1 PDS (Section 1) info
C   to a GRIB2 PDS (Section 4) info with appropriate Product Definition
C   Template.
C
C PROGRAM HISTORY LOG:
C 2003-06-12  Gilbert
C 2005-04-19  Gilbert    - Changed scaling factor used with potential
C                          vorticity surfaces.
C 2007-02-07  Gilbert    - fixed end date calculation
C 2007-03-26  Gordon     - Added check for ECMWF data to reference ECMWF
C                          Conversion tables.
C 2007-05-14  Boi Vuong  - Added Time Range Indicator 51 (Climatological 
C                          Mean Value)
C
C USAGE:    CALL pds2pdt(kpds,ipdsnum,ipdstmpl,numcoord,coordlist,iret)
C   INPUT ARGUMENT LIST:
C     kpds() - GRIB1 PDS info as specified in W3FI63.
C
C   OUTPUT ARGUMENT LIST:
C     ipdsnum    - GRIB2 Product Definition Template Number
C     ipdstmpl() - GRIB2 Product Definition Template entries for PDT 4.ipdsnum
C     numcoord   - number of vertical discretisation values ( not implemented )
C     coordlist()- vertical discretisation values ( not implemented )
C     iret       - Error return value:
C                  0  = Successful
C                  1  = Unrecognized GRIB1 Time Range Indicator
C
C REMARKS:  Use pds2pdtens for ensemble related PDS
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C$$$
        
        use params
        use params_ecmwf

        integer,intent(in) :: kpds(*)
        integer,intent(out) :: ipdstmpl(*)
        real,intent(out) :: coordlist(*)
        integer,intent(out) :: ipdsnum,numcoord,iret

        integer :: idat(8),jdat(8)
        real :: rinc(5)
        logical :: ecmwf

        iret=0
        numcoord=0
        ecmwf=.false.
        if (kpds(1).eq.98) ecmwf=.true.
        if (kpds(16).eq.0.or.kpds(16).eq.1.or.kpds(16).eq.10) then
          ipdsnum=0
          !  get GRIB2 parameter category and number from GRIB1 
          !  parameter number
          if (ecmwf) then       ! treat ecmwf data conversion seperately
             call param_ecmwf_g1_to_g2(kpds(5),kpds(19),idum,
     &            ipdstmpl(1),ipdstmpl(2))
          else
             call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1),
     &            ipdstmpl(2))
          endif
          if (kpds(16).eq.1) then
            ipdstmpl(3)=0
          else
            ipdstmpl(3)=2
          endif
          ipdstmpl(4)=0
          ipdstmpl(5)=kpds(2)
          ipdstmpl(6)=0
          ipdstmpl(7)=0
          ipdstmpl(8)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(8)=13
          !if (kpds(16).eq.10) then
          !  ipdstmpl(9)=(kpds(14)*256)+kpds(15)
          !else
            ipdstmpl(9)=kpds(14)
          !endif
          call cnvlevel(kpds(6),kpds(7),ipdstmpl)
        elseif (kpds(16).ge.2.AND.kpds(16).le.5) then
          ipdsnum=8
          !  get GRIB2 parameter category and number from GRIB1 
          !  parameter number
          if (ecmwf) then       ! treat ecmwf data conversion seperately
             call param_ecmwf_g1_to_g2(kpds(5),kpds(19),idum,
     &            ipdstmpl(1),ipdstmpl(2))
          else
             call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1),
     &                        ipdstmpl(2))
          endif
          ipdstmpl(3)=2
          ipdstmpl(4)=0
          ipdstmpl(5)=kpds(2)
          ipdstmpl(6)=0
          ipdstmpl(7)=0
          ipdstmpl(8)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(8)=13
          ipdstmpl(9)=kpds(14)
          call cnvlevel(kpds(6),kpds(7),ipdstmpl)
          !  calculate ending time using initial ref-time, idat,
          !  and increment rinc.
          idat=0
          idat(1)=((kpds(21)-1)*100)+kpds(8)
          idat(2)=kpds(9)
          idat(3)=kpds(10)
          idat(4)=-500     ! EST
          idat(5)=kpds(11)
          idat(6)=kpds(12)
          rinc=0.0
          if ( ipdstmpl(8).eq.0 ) then
             rinc(3)=kpds(15)
          elseif ( ipdstmpl(8).eq.1 ) then
             rinc(2)=kpds(15)
          elseif ( ipdstmpl(8).eq.2 ) then
             rinc(1)=kpds(15)
          elseif ( ipdstmpl(8).eq.10 ) then
             rinc(2)=kpds(15) * 3
          elseif ( ipdstmpl(8).eq.11 ) then
             rinc(2)=kpds(15) * 6
          elseif ( ipdstmpl(8).eq.12 ) then
             rinc(2)=kpds(15) * 12
          elseif ( ipdstmpl(8).eq.13 ) then
             rinc(4)=kpds(15)
          endif
          call w3movdat(rinc,idat,jdat)     ! calculate end date/time
          ipdstmpl(16)=jdat(1)                       ! year of end time
          ipdstmpl(17)=jdat(2)                       ! month of end time
          ipdstmpl(18)=jdat(3)                       ! day of end time
          ipdstmpl(19)=jdat(5)                       ! hour of end time
          ipdstmpl(20)=jdat(6)                       ! minute of end time
          ipdstmpl(21)=jdat(7)                       ! second of end time
          ipdstmpl(22)=1                             ! # of time ranges
          ipdstmpl(23)=kpds(20)                      ! # of values missing
          if (kpds(16).eq.2) then                    ! statistical process
             ipdstmpl(24)=255
          elseif (kpds(16).eq.3) then
             ipdstmpl(24)=0
          elseif (kpds(16).eq.4) then
             ipdstmpl(24)=1
          elseif (kpds(16).eq.5) then
             ipdstmpl(24)=4
          endif
          ipdstmpl(25)=2
          ipdstmpl(26)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(26)=13
          ipdstmpl(27)=kpds(15)-kpds(14)
          ipdstmpl(28)=255
          ipdstmpl(29)=0
        elseif (kpds(16).eq.51) then
          ipdsnum=8
          !  get GRIB2 parameter category and number from GRIB1
          !  parameter number
          if (ecmwf) then       ! treat ecmwf data conversion seperately
             call param_ecmwf_g1_to_g2(kpds(5),kpds(19),idum,
     &            ipdstmpl(1),ipdstmpl(2))
          else
             call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1),
     &                        ipdstmpl(2))
          endif
          ipdstmpl(3)=2
          ipdstmpl(4)=0
          ipdstmpl(5)=kpds(2)
          ipdstmpl(6)=0
          ipdstmpl(7)=0
          ipdstmpl(8)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(8)=13
          ipdstmpl(9)=kpds(14)
          call cnvlevel(kpds(6),kpds(7),ipdstmpl)
          !  calculate ending time using initial ref-time, idat,
          !  and increment rinc.
          idat=0
          idat(1)=((kpds(21)-1)*100)+kpds(8)
          idat(2)=kpds(9)
          idat(3)=kpds(10)
          idat(4)=-500     ! EST
          idat(5)=kpds(11)
          idat(6)=kpds(12)
          rinc=0.0
          if ( ipdstmpl(8).eq.0 ) then
             rinc(3)=kpds(15)
          elseif ( ipdstmpl(8).eq.1 ) then
             rinc(2)=kpds(15)
          elseif ( ipdstmpl(8).eq.2 ) then
             rinc(1)=kpds(15)
          elseif ( ipdstmpl(8).eq.10 ) then
             rinc(2)=kpds(15) * 3
          elseif ( ipdstmpl(8).eq.11 ) then
             rinc(2)=kpds(15) * 6
          elseif ( ipdstmpl(8).eq.12 ) then
             rinc(2)=kpds(15) * 12
          elseif ( ipdstmpl(8).eq.13 ) then
             rinc(4)=kpds(15)
          endif
          call w3movdat(rinc,idat,jdat)     ! calculate end date/time
          ipdstmpl(16)=jdat(1)                       ! year of end time
          ipdstmpl(17)=jdat(2)                       ! month of end time
          ipdstmpl(18)=jdat(3)                       ! day of end time
          ipdstmpl(19)=jdat(5)                       ! hour of end time
          ipdstmpl(20)=jdat(6)                       ! minute of end time
          ipdstmpl(21)=jdat(7)                       ! second of end time
          ipdstmpl(22)=1                             ! # of time ranges
          ipdstmpl(23)=kpds(20)                      ! # of values missing
          ipdstmpl(24)=51                            ! Climatological Mean
          ipdstmpl(25)=2
          ipdstmpl(26)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(26)=13
          ipdstmpl(27)=kpds(15)-kpds(14)
          ipdstmpl(28)=255
          ipdstmpl(29)=0
        else
           Print *,' Unrecognized Time Range Indicator = ',kpds(16)
           Print *,'pds2pdt: Couldn:t construct  PDS Template '
           iret=1
        endif

        return
        end


        subroutine cnvlevel(ltype,lval,ipdstmpl)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    cnvlevel
C   PRGMMR: Gilbert        ORG: W/NP11     DATE: 2003-06-12
C
C ABSTRACT: this routine converts a GRIB1 Level type and Level value
C   to GRIB2 values and fills in the appropriate PDT values for the 
C   level/layer information.
C
C PROGRAM HISTORY LOG:
C 2003-06-12  Gilbert
C
C USAGE:    CALL cnvlevel(ltype,lval,ipdstmpl)
C   INPUT ARGUMENT LIST:
C     ltype    - GRIB1 level type (PDS octet 10)
C     lval     - GRIB1 level/layer value(s) (PDS octets 11 and 12)
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     ipdstmpl() - GRIB2 Product Definition Template values
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C$$$

        integer,intent(in) :: ltype,lval
        integer,intent(inout) :: ipdstmpl(*)

        ipdstmpl(10)=ltype
        ipdstmpl(11)=0
        ipdstmpl(12)=0
        ipdstmpl(13)=255
        ipdstmpl(14)=0
        ipdstmpl(15)=0

        if (ltype.eq.100) then
           ipdstmpl(12)=lval*100
        elseif (ltype.eq.101) then
           ipdstmpl(10)=100
           ipdstmpl(12)=(lval/256)*1000
           ipdstmpl(13)=100
           ipdstmpl(15)=mod(lval,256)*1000
        elseif (ltype.eq.102) then
           ipdstmpl(10)=101
        elseif (ltype.eq.103) then
           ipdstmpl(10)=102
           ipdstmpl(12)=lval
        elseif (ltype.eq.104) then
           ipdstmpl(10)=102
           ipdstmpl(12)=lval/256
           ipdstmpl(13)=102
           ipdstmpl(15)=mod(lval,256)
        elseif (ltype.eq.105) then
           ipdstmpl(10)=103
           ipdstmpl(12)=lval
        elseif (ltype.eq.106) then
           ipdstmpl(10)=103
           ipdstmpl(12)=(lval/256)*100
           ipdstmpl(13)=103
           ipdstmpl(15)=mod(lval,256)*100
        elseif (ltype.eq.107) then
           ipdstmpl(10)=104
           ipdstmpl(11)=4
           ipdstmpl(12)=lval
        elseif (ltype.eq.108) then
           ipdstmpl(10)=104
           ipdstmpl(11)=2
           ipdstmpl(12)=lval/256
           ipdstmpl(13)=104
           ipdstmpl(14)=2
           ipdstmpl(15)=mod(lval,256)
        elseif (ltype.eq.109) then
           ipdstmpl(10)=105
           ipdstmpl(12)=lval
        elseif (ltype.eq.110) then
           ipdstmpl(10)=105
           ipdstmpl(12)=lval/256
           ipdstmpl(13)=105
           ipdstmpl(15)=mod(lval,256)
        elseif (ltype.eq.111) then
           ipdstmpl(10)=106
           ipdstmpl(11)=2
           ipdstmpl(12)=lval
        elseif (ltype.eq.112) then
           ipdstmpl(10)=106
           ipdstmpl(11)=2
           ipdstmpl(12)=lval/256
           ipdstmpl(13)=106
           ipdstmpl(14)=2
           ipdstmpl(15)=mod(lval,256)
        elseif (ltype.eq.113) then
           ipdstmpl(10)=107
           ipdstmpl(12)=lval
        elseif (ltype.eq.114) then
           ipdstmpl(10)=107
           ipdstmpl(12)=475+(lval/256)
           ipdstmpl(13)=107
           ipdstmpl(15)=475+mod(lval,256)
        elseif (ltype.eq.115) then
           ipdstmpl(10)=108
           ipdstmpl(12)=lval*100
        elseif (ltype.eq.116) then
           ipdstmpl(10)=108
           ipdstmpl(12)=(lval/256)*100
           ipdstmpl(13)=108
           ipdstmpl(15)=mod(lval,256)*100
        elseif (ltype.eq.117) then
           ipdstmpl(10)=109
           ipdstmpl(11)=9
           ipdstmpl(12)=lval
           if ( btest(lval,15) ) then
              ipdstmpl(12)=-1*mod(lval,32768)
           endif
        elseif (ltype.eq.119) then
           ipdstmpl(10)=111
           ipdstmpl(11)=4
           ipdstmpl(12)=lval
        elseif (ltype.eq.120) then
           ipdstmpl(10)=111
           ipdstmpl(11)=2
           ipdstmpl(12)=lval/256
           ipdstmpl(13)=111
           ipdstmpl(14)=2
           ipdstmpl(15)=mod(lval,256)
        elseif (ltype.eq.121) then
           ipdstmpl(10)=100
           ipdstmpl(12)=(1100+(lval/256))*100
           ipdstmpl(13)=100
           ipdstmpl(15)=(1100+mod(lval,256))*100
        elseif (ltype.eq.125) then
           ipdstmpl(10)=103
           ipdstmpl(11)=2
           ipdstmpl(12)=lval
        elseif (ltype.eq.128) then
           ipdstmpl(10)=104
           ipdstmpl(11)=3
           ipdstmpl(12)=1100+(lval/256)
           ipdstmpl(13)=104
           ipdstmpl(14)=3
           ipdstmpl(15)=1100+mod(lval,256)
        elseif (ltype.eq.141) then
           ipdstmpl(10)=100
           ipdstmpl(12)=(lval/256)*100
           ipdstmpl(13)=100
           ipdstmpl(15)=(1100+mod(lval,256))*100
        elseif (ltype.eq.160) then
           ipdstmpl(10)=160
           ipdstmpl(12)=lval
        elseif (ltype.gt.99.AND.ltype.lt.200) then
           print *,'cnvlevel: GRIB1 Level ',ltype,' not recognized.'
           ipdstmpl(10)=255
        endif

        return
        end
