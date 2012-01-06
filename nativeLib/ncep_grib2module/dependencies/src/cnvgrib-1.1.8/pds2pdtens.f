        subroutine pds2pdtens(kpds,kens,kprob,xprob,kclust,kmember,
     &                     ipdsnum,ipdstmpl,numcoord,coordlist,
     &                     iret)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    pds2pdtens
C   PRGMMR: Gilbert        ORG: W/NP11     DATE: 2003-06-12
C
C ABSTRACT: This routine converts a GRIB1 PDS (Section 1) that includes
C   NCEP ensemble PDS extensions
C   to a GRIB2 PDS (Section 4) info with appropriate Product Definition
C   Template.
C
C PROGRAM HISTORY LOG:
C 2003-06-12  Gilbert
C 2007-02-07  Gilbert    - fixed end date calculation
C 2007-05-14  Boi Vuong  - Added Time Range Indicator 51 (Climatological 
C                          Mean Value)
C
C USAGE:    CALL pds2pdtens(kpds,kens,kprob,xprob,kclust,kmember,
C                           ipdsnum,ipdstmpl,numcoord,coordlist,iret)
C   INPUT ARGUMENT LIST:
C     kpds()   - GRIB1 PDS info as specified in W3FI63.
C     kens()   - Ensemble identification from PDS octets 41-45
C     kprob()  - Ensemble probability info from PDS octets 46 & 47
C     xprob()  - Ensemble probability info from PDS octets 48-55
C     kclust() - Ensemble cluster info from PDS octets 61-76
C     kmember()- Ensemble membership info from PDS octest 77-86
C
C   OUTPUT ARGUMENT LIST:
C     ipdsnum    - GRIB2 Product Definition Template Number
C     ipdstmpl() - GRIB2 Product Definition Template entries for PDT 4.ipdsnum
C     numcoord   - number of vertical discretisation values ( not implemented )
C     coordlist()- vertical discretisation values ( not implemented )
C     iret       - Error return value:
C                  0  = Successful
C                  1  = Unrecognized GRIB1 Time Range Indicator for ensembles
C                  2  = Unrecognized GRIB1 Ensemble type
C                  10 = Unrecognized GRIB1 Time Range Indicator for probabilities
C
C REMARKS:  Use routine pds2pdt for non ensemble related PDS.
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C$$$
        
        use params

        integer,intent(in) :: kpds(*),kens(*),kprob(*),kclust(*)
        integer,intent(in) :: kmember(*)
        real,intent(in) :: xprob(*)
        integer,intent(out) :: ipdstmpl(*)
        real,intent(out) :: coordlist(*)
        integer,intent(out) :: ipdsnum,numcoord,iret

        integer :: idat(8),jdat(8)
        real :: rinc(5)

        iret=0
        numcoord=0
        if (kens(2).eq.1.or.kens(2).eq.2.or.kens(2).eq.3) then
           !     individual ensemble fcst...
           if (kpds(16).eq.0.or.kpds(16).eq.1.or.kpds(16).eq.10) then
             !   At specific point in time...
             ipdsnum=1
             !  get GRIB2 parameter category and number from GRIB1 
             !  parameter number
             call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1),
     &                           ipdstmpl(2))
             ipdstmpl(3)=4
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
             if (kens(2).eq.1) then
!               if (kens(3).eq.1) ipdstmpl(16)=0
!               if (kens(3).eq.2) ipdstmpl(16)=1
               ipdstmpl(16)=kens(3)-1
               ipdstmpl(17)=0
             elseif (kens(2).eq.2) then
               ipdstmpl(16)=2
               ipdstmpl(17)=kens(3)
             elseif (kens(2).eq.3) then
               ipdstmpl(16)=3
               ipdstmpl(17)=kens(3)
             endif
             ipdstmpl(18)=10
           elseif (kpds(16).ge.2.AND.kpds(16).le.5) then
              !    over time range...
              ipdsnum=11
             !  get GRIB2 parameter category and number from GRIB1 
             !  parameter number
              call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1),
     &                           ipdstmpl(2))
              ipdstmpl(3)=4
              ipdstmpl(4)=0
              ipdstmpl(5)=kpds(2)
              ipdstmpl(6)=0
              ipdstmpl(7)=0
              ipdstmpl(8)=kpds(13)
              if (kpds(13).eq.254) ipdstmpl(8)=13
              ipdstmpl(9)=kpds(14)
              call cnvlevel(kpds(6),kpds(7),ipdstmpl)
              !ipdstmpl(9)=kpds(15)
              if (kens(2).eq.1) then
!                if (kens(3).eq.1) ipdstmpl(16)=0
!                if (kens(3).eq.2) ipdstmpl(16)=1
                ipdstmpl(16)=kens(3)-1
                ipdstmpl(17)=0
              elseif (kens(2).eq.2) then
                ipdstmpl(16)=2
                ipdstmpl(17)=kens(3)
              elseif (kens(2).eq.3) then
                ipdstmpl(16)=3
                ipdstmpl(17)=kens(3)
              endif
              ipdstmpl(18)=10
              !  calculate ending time using initial ref-time, idat,
              !  and increment rinc.
              idat=0
              idat(1)=((kpds(21)-1)*100)+kpds(8)
              idat(2)=kpds(9)
              idat(3)=kpds(10)
              idat(4)=-500     ! EST
              idat(5)=kpds(11)
              idat(6)=kpds(12)
              rinc=0
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
              ipdstmpl(19)=jdat(1)              ! year of end time
              ipdstmpl(20)=jdat(2)              ! month of end time
              ipdstmpl(21)=jdat(3)              ! day of end time
              ipdstmpl(22)=jdat(5)              ! hour of end time
              ipdstmpl(23)=jdat(6)              ! minute of end time
              ipdstmpl(24)=jdat(7)              ! second of end time
              ipdstmpl(25)=1
              ipdstmpl(26)=0
              if (kpds(16).eq.2) then
                 ipdstmpl(27)=255
                 if (kpds(5).eq.15) ipdstmpl(27)=2
                 if (kpds(5).eq.16) ipdstmpl(27)=3
              elseif (kpds(16).eq.3) then
                 ipdstmpl(27)=0
              elseif (kpds(16).eq.4) then
                 ipdstmpl(27)=1
              elseif (kpds(16).eq.5) then
                 ipdstmpl(27)=4
              endif
              ipdstmpl(28)=2
              ipdstmpl(29)=kpds(13)
              if (kpds(13).eq.254) ipdstmpl(29)=13
              ipdstmpl(30)=kpds(15)-kpds(14)
              ipdstmpl(31)=255
              ipdstmpl(32)=0
           elseif (kpds(16).eq.51) then
              !    over time range...
              ipdsnum=11
             !  get GRIB2 parameter category and number from GRIB1
             !  parameter number
              call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1),
     &                           ipdstmpl(2))
              ipdstmpl(3)=4
              ipdstmpl(4)=0
              ipdstmpl(5)=kpds(2)
              ipdstmpl(6)=0
              ipdstmpl(7)=0
              ipdstmpl(8)=kpds(13)
              if (kpds(13).eq.254) ipdstmpl(8)=13
              ipdstmpl(9)=kpds(14)
              call cnvlevel(kpds(6),kpds(7),ipdstmpl)
              !ipdstmpl(9)=kpds(15)
              if (kens(2).eq.1) then
!                if (kens(3).eq.1) ipdstmpl(16)=0
!                if (kens(3).eq.2) ipdstmpl(16)=1
                ipdstmpl(16)=kens(3)-1
                ipdstmpl(17)=0
              elseif (kens(2).eq.2) then
                ipdstmpl(16)=2
                ipdstmpl(17)=kens(3)
              elseif (kens(2).eq.3) then
                ipdstmpl(16)=3
                ipdstmpl(17)=kens(3)
              endif
              ipdstmpl(18)=10
              !  calculate ending time using initial ref-time, idat,
              !  and increment rinc.
              idat=0
              idat(1)=((kpds(21)-1)*100)+kpds(8)
              idat(2)=kpds(9)
              idat(3)=kpds(10)
              idat(4)=-500     ! EST
              idat(5)=kpds(11)
              idat(6)=kpds(12)
              rinc=0
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
              ipdstmpl(19)=jdat(1)              ! year of end time
              ipdstmpl(20)=jdat(2)              ! month of end time
              ipdstmpl(21)=jdat(3)              ! day of end time
              ipdstmpl(22)=jdat(5)              ! hour of end time
              ipdstmpl(23)=jdat(6)              ! minute of end time
              ipdstmpl(24)=jdat(7)              ! second of end time
              ipdstmpl(25)=1
              ipdstmpl(26)=0
              ipdstmpl(27)=51
              ipdstmpl(28)=2
              ipdstmpl(29)=kpds(13)
              if (kpds(13).eq.254) ipdstmpl(29)=13
              ipdstmpl(30)=kpds(15)-kpds(14)
              ipdstmpl(31)=255
              ipdstmpl(32)=0
           else
              Print *,' Unrecognized Time Range Ind for ensembles = ',
     &                  kpds(16),kens(2)
              Print *,'pds2pdtens: Couldn:t construct  PDS Template '
              iret=1
           endif

        elseif (kens(2).eq.5) then         ! WHOLE or CLUSTERENSEMBLE type
           if (kpds(5).eq.191.OR.kpds(5).eq.192) then   ! probs
              if (kpds(16).eq.0.or.kpds(16).eq.1.or.kpds(16).eq.10) then
                 !   At specific point in time...
                 ipdsnum=5
                 !  get GRIB2 parameter category and number from GRIB1 
                 !  parameter number
                 call param_g1_to_g2(kprob(1),kpds(19),idum,ipdstmpl(1),
     &                               ipdstmpl(2))
                 ipdstmpl(3)=5
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
                 ipdstmpl(16)=0   !?
                 ipdstmpl(17)=kclust(1)   !?
                 ipdstmpl(18)=kprob(2)-1
                 if (ipdstmpl(18).eq.0.OR.ipdstmpl(18).eq.2) then
                   ipdstmpl(19)=3
                   ipdstmpl(20)=nint(xprob(1)*1000.0)
                 else
                   ipdstmpl(19)=0
                   ipdstmpl(20)=0
                 endif
                 if (ipdstmpl(18).eq.1.OR.ipdstmpl(18).eq.2) then
                   ipdstmpl(21)=3
                   ipdstmpl(22)=nint(xprob(2)*1000.0)
                 else
                   ipdstmpl(21)=0
                   ipdstmpl(22)=0
                 endif
              elseif (kpds(16).ge.2.AND.kpds(16).le.5) then
                 !    over time range...
                 ipdsnum=9
                 !  get GRIB2 parameter category and number from GRIB1 
                 !  parameter number
                 call param_g1_to_g2(kprob(1),kpds(19),idum,ipdstmpl(1),
     &                               ipdstmpl(2))
                 ipdstmpl(3)=5
                 ipdstmpl(4)=0
                 ipdstmpl(5)=kpds(2)
                 ipdstmpl(6)=0
                 ipdstmpl(7)=0
                 ipdstmpl(8)=kpds(13)
                 if (kpds(13).eq.254) ipdstmpl(8)=13
                 ipdstmpl(9)=kpds(14)
                 call cnvlevel(kpds(6),kpds(7),ipdstmpl)
                 !ipdstmpl(9)=kpds(15)
                 ipdstmpl(16)=0   !?
                 ipdstmpl(17)=kclust(1)   !?
                 ipdstmpl(18)=kprob(2)-1
                 if (ipdstmpl(18).eq.0.OR.ipdstmpl(18).eq.2) then
                   ipdstmpl(19)=3
                   ipdstmpl(20)=nint(xprob(1)*1000.0)
                 else
                   ipdstmpl(19)=0
                   ipdstmpl(20)=0
                 endif
                 if (ipdstmpl(18).eq.1.OR.ipdstmpl(18).eq.2) then
                   ipdstmpl(21)=3
                   ipdstmpl(22)=nint(xprob(2)*1000.0)
                 else
                   ipdstmpl(21)=0
                   ipdstmpl(22)=0
                 endif
                 !  calculate ending time using initial ref-time, idat,
                 !  and increment rinc.
                 idat=0
                 idat(1)=((kpds(21)-1)*100)+kpds(8)
                 idat(2)=kpds(9)
                 idat(3)=kpds(10)
                 idat(4)=-500     ! EST
                 idat(5)=kpds(11)
                 idat(6)=kpds(12)
                 rinc=0
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
                 ipdstmpl(23)=jdat(1)              ! year of end time
                 ipdstmpl(24)=jdat(2)              ! month of end time
                 ipdstmpl(25)=jdat(3)              ! day of end time
                 ipdstmpl(26)=jdat(5)              ! hour of end time
                 ipdstmpl(27)=jdat(6)              ! minute of end time
                 ipdstmpl(28)=jdat(7)              ! second of end time
                 ipdstmpl(29)=1
                 ipdstmpl(30)=0
                 if (kpds(16).eq.2) then
                    ipdstmpl(31)=255
                    if (kpds(5).eq.15) ipdstmpl(31)=2
                    if (kpds(5).eq.16) ipdstmpl(31)=3
                 elseif (kpds(16).eq.3) then
                    ipdstmpl(31)=0
                 elseif (kpds(16).eq.4) then
                    ipdstmpl(31)=1
                 elseif (kpds(16).eq.5) then
                    ipdstmpl(31)=4
                 endif
                 ipdstmpl(32)=2
                 ipdstmpl(33)=kpds(13)
                 if (kpds(13).eq.254) ipdstmpl(33)=13
                 ipdstmpl(34)=kpds(15)-kpds(14)
                 ipdstmpl(35)=255
                 ipdstmpl(36)=0
              elseif (kpds(16).eq.51) then
                 !    over time range...
                 ipdsnum=9
                 !  get GRIB2 parameter category and number from GRIB1
                 !  parameter number
                 call param_g1_to_g2(kprob(1),kpds(19),idum,ipdstmpl(1),
     &                               ipdstmpl(2))
                 ipdstmpl(3)=5
                 ipdstmpl(4)=0
                 ipdstmpl(5)=kpds(2)
                 ipdstmpl(6)=0
                 ipdstmpl(7)=0
                 ipdstmpl(8)=kpds(13)
                 if (kpds(13).eq.254) ipdstmpl(8)=13
                 ipdstmpl(9)=kpds(14)
                 call cnvlevel(kpds(6),kpds(7),ipdstmpl)
                 !ipdstmpl(9)=kpds(15)
                 ipdstmpl(16)=0   !?
                 ipdstmpl(17)=kclust(1)   !?
                 ipdstmpl(18)=kprob(2)-1
                 if (ipdstmpl(18).eq.0.OR.ipdstmpl(18).eq.2) then
                   ipdstmpl(19)=3
                   ipdstmpl(20)=nint(xprob(1)*1000.0)
                 else
                   ipdstmpl(19)=0
                   ipdstmpl(20)=0
                 endif
                 if (ipdstmpl(18).eq.1.OR.ipdstmpl(18).eq.2) then
                   ipdstmpl(21)=3
                   ipdstmpl(22)=nint(xprob(2)*1000.0)
                 else
                   ipdstmpl(21)=0
                   ipdstmpl(22)=0
                 endif
                 !  calculate ending time using initial ref-time, idat,
                 !  and increment rinc.
                 idat=0
                 idat(1)=((kpds(21)-1)*100)+kpds(8)
                 idat(2)=kpds(9)
                 idat(3)=kpds(10)
                 idat(4)=-500     ! EST
                 idat(5)=kpds(11)
                 idat(6)=kpds(12)
                 rinc=0
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
                 ipdstmpl(23)=jdat(1)              ! year of end time
                 ipdstmpl(24)=jdat(2)              ! month of end time
                 ipdstmpl(25)=jdat(3)              ! day of end time
                 ipdstmpl(26)=jdat(5)              ! hour of end time
                 ipdstmpl(27)=jdat(6)              ! minute of end time
                 ipdstmpl(28)=jdat(7)              ! second of end time
                 ipdstmpl(29)=1
                 ipdstmpl(30)=0
                 ipdstmpl(31)=51
                 ipdstmpl(32)=2
                 ipdstmpl(33)=kpds(13)
                 if (kpds(13).eq.254) ipdstmpl(33)=13
                 ipdstmpl(34)=kpds(15)-kpds(14)
                 ipdstmpl(35)=255
                 ipdstmpl(36)=0
              else
                 Print *,' Unrecognized Time Range Ind for Probs = ',
     &                     kpds(16),kens(2)
                 Print *,'pds2pdtens: Couldn:t construct  PDS Template '
                 iret=10
              endif
           else      ! Non-probablility Whole Ens Fcst
              if (kpds(16).eq.0.or.kpds(16).eq.1.or.kpds(16).eq.10) then
                !   At specific point in time...
                ipdsnum=2
                !  get GRIB2 parameter category and number from GRIB1 
                !  parameter number
                call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1),
     &                              ipdstmpl(2))
                ipdstmpl(3)=4
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
                if (kens(4).eq.1) then
                  ipdstmpl(16)=0
                elseif (kens(4).eq.2) then
                  ipdstmpl(16)=1
                elseif (kens(4).eq.11) then
                  ipdstmpl(16)=2
                elseif (kens(4).eq.12) then
                  ipdstmpl(16)=3
                endif
                ipdstmpl(17)=kclust(1)
              elseif (kpds(16).ge.2.AND.kpds(16).le.5) then
                 !    over time range...
                 ipdsnum=12
                !  get GRIB2 parameter category and number from GRIB1 
                !  parameter number
                 call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1),
     &                               ipdstmpl(2))
                 ipdstmpl(3)=4
                 ipdstmpl(4)=0
                 ipdstmpl(5)=kpds(2)
                 ipdstmpl(6)=0
                 ipdstmpl(7)=0
                 ipdstmpl(8)=kpds(13)
                 if (kpds(13).eq.254) ipdstmpl(8)=13
                 ipdstmpl(9)=kpds(14)
                 call cnvlevel(kpds(6),kpds(7),ipdstmpl)
                 !ipdstmpl(9)=kpds(15)
                 if (kens(4).eq.1) then
                   ipdstmpl(16)=0
                 elseif (kens(4).eq.2) then
                   ipdstmpl(16)=1
                 elseif (kens(4).eq.11) then
                   ipdstmpl(16)=2
                 elseif (kens(4).eq.12) then
                   ipdstmpl(16)=3
                 endif
                 ipdstmpl(17)=kclust(1)
                 !  calculate ending time using initial ref-time, idat,
                 !  and increment rinc.
                 idat=0
                 idat(1)=((kpds(21)-1)*100)+kpds(8)
                 idat(2)=kpds(9)
                 idat(3)=kpds(10)
                 idat(4)=-500     ! EST
                 idat(5)=kpds(11)
                 idat(6)=kpds(12)
                 rinc=0
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
                 ipdstmpl(18)=jdat(1)              ! year of end time
                 ipdstmpl(19)=jdat(2)              ! month of end time
                 ipdstmpl(20)=jdat(3)              ! day of end time
                 ipdstmpl(21)=jdat(5)              ! hour of end time
                 ipdstmpl(22)=jdat(6)              ! minute of end time
                 ipdstmpl(23)=jdat(7)              ! second of end time
                 ipdstmpl(24)=1
                 ipdstmpl(25)=0
                 if (kpds(16).eq.2) then
                    ipdstmpl(26)=255
                    if (kpds(5).eq.15) ipdstmpl(26)=2
                    if (kpds(5).eq.16) ipdstmpl(26)=3
                 elseif (kpds(16).eq.3) then
                    ipdstmpl(26)=0
                 elseif (kpds(16).eq.4) then
                    ipdstmpl(26)=1
                 elseif (kpds(16).eq.5) then
                    ipdstmpl(26)=4
                 endif
                 ipdstmpl(27)=2
                 ipdstmpl(28)=kpds(13)
                 if (kpds(13).eq.254) ipdstmpl(28)=13
                 ipdstmpl(29)=kpds(15)-kpds(14)
                 ipdstmpl(30)=255
                 ipdstmpl(31)=0
              elseif (kpds(16).eq.51) then
                 !    over time range...
                 ipdsnum=12
                !  get GRIB2 parameter category and number from GRIB1
                !  parameter number
                 call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1),
     &                               ipdstmpl(2))
                 ipdstmpl(3)=4
                 ipdstmpl(4)=0
                 ipdstmpl(5)=kpds(2)
                 ipdstmpl(6)=0
                 ipdstmpl(7)=0
                 ipdstmpl(8)=kpds(13)
                 if (kpds(13).eq.254) ipdstmpl(8)=13
                 ipdstmpl(9)=kpds(14)
                 call cnvlevel(kpds(6),kpds(7),ipdstmpl)
                 !ipdstmpl(9)=kpds(15)
                 if (kens(4).eq.1) then
                   ipdstmpl(16)=0
                 elseif (kens(4).eq.2) then
                   ipdstmpl(16)=1
                 elseif (kens(4).eq.11) then
                   ipdstmpl(16)=2
                 elseif (kens(4).eq.12) then
                   ipdstmpl(16)=3
                 endif
                 ipdstmpl(17)=kclust(1)
                 !  calculate ending time using initial ref-time, idat,
                 !  and increment rinc.
                 idat=0
                 idat(1)=((kpds(21)-1)*100)+kpds(8)
                 idat(2)=kpds(9)
                 idat(3)=kpds(10)
                 idat(4)=-500     ! EST
                 idat(5)=kpds(11)
                 idat(6)=kpds(12)
                 rinc=0
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
                 ipdstmpl(18)=jdat(1)              ! year of end time
                 ipdstmpl(19)=jdat(2)              ! month of end time
                 ipdstmpl(20)=jdat(3)              ! day of end time
                 ipdstmpl(21)=jdat(5)              ! hour of end time
                 ipdstmpl(22)=jdat(6)              ! minute of end time
                 ipdstmpl(23)=jdat(7)              ! second of end time
                 ipdstmpl(24)=1
                 ipdstmpl(25)=0
                 ipdstmpl(26)=51
                 ipdstmpl(27)=2
                 ipdstmpl(28)=kpds(13)
                 if (kpds(13).eq.254) ipdstmpl(28)=13
                 ipdstmpl(29)=kpds(15)-kpds(14)
                 ipdstmpl(30)=255
                 ipdstmpl(31)=0
              endif
           endif
        else
           Print *,' Unrecognized Ensemble type = ',kens(2)
           Print *,'pds2pdtens: Couldn:t construct  PDS Template '
           iret=2
        endif

        return
        end

