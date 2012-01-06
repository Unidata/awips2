        subroutine makepdsens(ipdsnum,ipdstmpl,kpds,kens,kprob,
     &                     xprob,kclust,kmembr,iret)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    makepdsens
C   PRGMMR: Gilbert        ORG: W/NP11     DATE: 2003-06-12
C
C ABSTRACT: This routine creates the GRIB1 NCEP Ensemble PDS
C   extension information from appropriate information from a GRIB2 
C   Product Definition Template.
C
C PROGRAM HISTORY LOG:
C 2003-06-12  Gilbert
C 2007-05-14  Boi Vuong  -Corrected scale factor probabilities 
C
C USAGE:    CALL makepdsens(ipdsnum,ipdstmpl,kpds,kens,kprob,
C                        xprob,kclust,kmembr,iret)
C   INPUT ARGUMENT LIST:
C     ipdsnum    - GRIB2 Product Definition Template Number
C     ipdstmpl() - GRIB2 Product Definition Template entries for PDT 4.ipdsnum
C     kpds()     - GRIB1 PDS info as specified in W3FI63.
C          (1)   - ID OF CENTER
C          (2)   - GENERATING PROCESS ID NUMBER
C          (3)   - GRID DEFINITION
C          (4)   - GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
C          (5)   - INDICATOR OF PARAMETER
C          (6)   - TYPE OF LEVEL
C          (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
C          (8)   - YEAR INCLUDING (CENTURY-1)
C          (9)   - MONTH OF YEAR
C          (10)  - DAY OF MONTH
C          (11)  - HOUR OF DAY
C          (12)  - MINUTE OF HOUR
C          (13)  - INDICATOR OF FORECAST TIME UNIT
C          (14)  - TIME RANGE 1
C          (15)  - TIME RANGE 2
C          (16)  - TIME RANGE FLAG
C          (17)  - NUMBER INCLUDED IN AVERAGE
C          (18)  - VERSION NR OF GRIB SPECIFICATION
C          (19)  - VERSION NR OF PARAMETER TABLE
C          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
C          (21)  - CENTURY OF REFERENCE TIME OF DATA
C          (22)  - UNITS DECIMAL SCALE FACTOR
C          (23)  - SUBCENTER NUMBER
C
C   OUTPUT ARGUMENT LIST:
C     kpds()     - GRIB1 PDS info as specified in W3FI63.
C          (1)   - ID OF CENTER
C          (2)   - GENERATING PROCESS ID NUMBER
C          (3)   - GRID DEFINITION
C          (4)   - GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
C          (5)   - INDICATOR OF PARAMETER
C          (6)   - TYPE OF LEVEL
C          (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
C          (8)   - YEAR INCLUDING (CENTURY-1)
C          (9)   - MONTH OF YEAR
C          (10)  - DAY OF MONTH
C          (11)  - HOUR OF DAY
C          (12)  - MINUTE OF HOUR
C          (13)  - INDICATOR OF FORECAST TIME UNIT
C          (14)  - TIME RANGE 1
C          (15)  - TIME RANGE 2
C          (16)  - TIME RANGE FLAG
C          (17)  - NUMBER INCLUDED IN AVERAGE
C          (18)  - VERSION NR OF GRIB SPECIFICATION
C          (19)  - VERSION NR OF PARAMETER TABLE
C          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
C          (21)  - CENTURY OF REFERENCE TIME OF DATA
C          (22)  - UNITS DECIMAL SCALE FACTOR
C          (23)  - SUBCENTER NUMBER
C     kens()     - Ensemble identification for PDS octets 41-45
C     kprob()    - Ensemble probability info for PDS octets 46 & 47
C     xprob()    - Ensemble probability info for PDS octets 48-55
C     kclust()   - Ensemble cluster info for PDS octets 61-76
C     kmembr()   - Ensemble membership info for PDS octest 77-86
C     iret       - Error return value:
C                  0  = Successful
C                  2  = Unrecognized GRIB2 PDT 4.ipdsnum
C
C REMARKS:  Use pds2pdtens for ensemble related PDS
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C$$$
        
        use params

        integer,intent(in) :: ipdstmpl(*)
        integer,intent(in) :: ipdsnum
        integer,intent(inout) :: kpds(*)
        integer,intent(out) :: kens(5),kprob(2)
        integer,intent(out) :: kclust(16),kmembr(80)
        real,intent(out) :: xprob(2)
        integer,intent(out) :: iret

        iret=0
        kpds(23)=2          !  subcenter = ensemble

        kens(1:5)=0
        kprob(1:2)=0
        xprob(1:2)=0.
        kclust(1:16)=0
        kmembr(1:80)=0
        !
        !  Individual Ensemble Fcst
        !
        if ( ipdsnum.eq.1.OR.ipdsnum.eq.11 ) then
           kens(1)=1
           selectcase ( ipdstmpl(16) )
             case(0)
                kens(2)=1
                kens(3)=1
             case(1)
                kens(2)=1
                kens(3)=2
             case(2)
                kens(2)=2
                kens(3)=ipdstmpl(17)
             case(3)
                kens(2)=3
                kens(3)=ipdstmpl(17)
           end select
           kens(4)=1
           kens(5)=255

        !
        !  Probability Fcst
        !
        elseif ( ipdsnum.eq.5.OR.ipdsnum.eq.9 ) then
           kens(1)=1
           kens(2)=5
           kens(3)=0
           kens(4)=0
           kens(5)=255
           kprob(1)=kpds(5)
           kpds(5)=191
           kprob(2)=ipdstmpl(18)+1
           if ( kprob(2).eq.1 ) then
              rscale=10.**ipdstmpl(19)
              xprob(1)=real(ipdstmpl(20))/rscale
              xprob(2)=0.0
           elseif ( kprob(2).eq.2 ) then
              xprob(1)=0.0
              rscale=10.**ipdstmpl(21)
              xprob(2)=real(ipdstmpl(22))/rscale
           elseif ( kprob(2).eq.3 ) then
              rscale=10.**ipdstmpl(19)
              xprob(1)=real(ipdstmpl(20))/rscale
              rscale=10.**ipdstmpl(21)
              xprob(2)=real(ipdstmpl(22))/rscale
           endif
           kclust(1)=ipdstmpl(17)
        !
        !  Derived Ensemble Fcst
        !
        elseif ( ipdsnum.eq.2.OR.ipdsnum.eq.12 ) then
           kens(1)=1
           kens(2)=5
           kens(3)=0
           selectcase ( ipdstmpl(16) )
             case(0)
                kens(4)=1
             case(1)
                kens(4)=2
             case(2)
                kens(4)=11
             case(3)
                kens(4)=12
           end select
           !kens(5)=89
           kens(5)=0
           kclust(1)=ipdstmpl(17)
        else
           print *,'makepdsens: Don:t know GRIB2 PDT 4.',ipdsnum
           iret=2
        endif

        return
        end

