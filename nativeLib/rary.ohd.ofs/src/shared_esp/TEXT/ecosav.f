cew subroutine to update the carryover date so for ESP Verification
cew carryover date saving.
C
      subroutine ecosav(ncstor,icday,ichour,iecday,iechour,
     +                    ijdls,ihls,idloop,ljdls,lhls,ierr)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fprog'
      INCLUDE 'common/fcio'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/etime'
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fcfgs'
cfan
      common /leapyearcarryover/ileapyear,icday0229
cfan
C
      dimension icday(1),ichour(1),iecday(1),iechour(1)
      dimension sbname(2),oldopn(2)
      integer ieday_lst(20), iehour_lst(20), idte
      character*128 newnam
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ecosav.f,v $
     . $',                                                             '
     .$Id: ecosav.f,v 1.6 2006/05/22 12:15:10 xfan Exp $
     . $' /
C    ===================================================================
C
       DATA SBNAME / 4hECOS,4hAV   /
C
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
      OPNAME(I)=SBNAME(I)
   10 CONTINUE
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,14HECOSAV ENTERED)
C

       ierr=0

cew   compute difference between the year in the requested carryover date (icday) and
cew   and the startesp date year (ijdls).  Then new carryover date year
cew   is the current starting histroical year (idloop) plus the difference .
cew   Do computations on the year only so that the day is the same and does
cew   not get shifted by leap years.  Use mdyh1 to compute the years from the
cew   julian day variables and then use julda1 to go back to julian day from
cew   m/d/y/h/tzc

cew icday(i) and ichour(i) are the dates that will get saved
cew iecday(i) and iechour(i) are the orignal dates before they get shifted.

cew set idseg in common fcio to be equal to idsegn in fcsegn.
cew fcwtco uses these two id's to determine if a segment is new.
cew by setting them equal fcwtco will not write out any header info.
cew all header info will be written by ecosav

      DO 42 I=1,2
   42 IDSEG(I) = IDSEGN(I)

cew just check for a valid value of ncstor
cew ncstor is the number of carryover days to be saved.

       if(ncstor.gt.10) then

cew the system is way out of whack if ncstor gt 10

           write(ipr,*)'** ERROR ** SYSTEM INDICATES ATTEMPT TO SAVE MOR
     +E THAN 10 CARRYOVER DAYS.'

            ierr=1

            call error

cew jump out of loop to end of subroutine
            goto 400

        endif

cew loop through all carryover days checking dates and updating them.

       do 200  i=1,ncstor

cew convert the julian days into m/d/y/h
cew do it here so you only have to do it one time

          call mdyh1(ijdls,ihls,ijmonth,ijday,ijyear,ijhour,nlstz,
     +     0,icode)

          call mdyh1(idloop,ihls,idmonth,idday,idyear,idhour,nlstz,
     +     0,icode)

cew convert to LST not internal
          call efclst(iecday(i),iechour(i),ieday_lst(i),iehour_lst(i))

cew shift the start date
C
C   CALCULATE SHIFT NECESSARY TO MAKE START TIME COMPATIBLE
C   WITH MINDT.
C
cew  always shift ona 24 hour mindt, removed several lines of code
       idte=24
C
      ieday_shft=ieday_lst(i)
      iehour_shft=iehour_lst(i)
      CALL ESHFT(ieday_shft,iehour_shft,IDTE,IMOVE)

cew In EWTINF you have to do what is described below.
cew Because ESP runs with internal time = LST, must shift dates
cew from LST to internal time and then use the MDYH1 function
cew to compute mdyh because mdyh1 expects internal time not lst
cew this is just for printing

cew but if you do not reset LOCAL then you do not need to
cew do this step.

          call mdyh1(ieday_shft,iehour_shft,icmonth,iccday,icyear,
     +     icchour,nlstz,0,icode)

          call mdyh1(ieday_lst,iehour_lst,icmonth_orig,iccday_orig,
     +        icyear_orig,icchour_orig,nlstz,0,icode)

        if(imove .ne. 0) then
	 write(ipr,199) icmonth_orig,iccday_orig,icyear_orig,
     +         icchour_orig,icode, icmonth,iccday,icyear,icchour,
     +          icode
	endif

 199   format('**NOTE**  CARRYOVER DATE, ',
     +         i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4,
     +         'HAS BEEN SHIFTED TO '
     +        i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4)

cew Do a bunch of date checks here and at the end of the checks update the years

cew check to be sure that the carryover days are within the run period
cew that is that the carryover days are between the startesp value
cew and the latest day in the window technique
cew that is that the carryover dates are between ijdls, ihls and ljdls, lhls

         if(ieday_shft .lt. ijdls) then
                write(ipr,201)icmonth,iccday,icyear,icchour,icode,
     +            ijmonth,ijday,ijyear,ijhour,icode
                write(ipr,225)icmonth,iccday,icyear,icchour,icode

201             format('** WARNING ** CARRYOVER DATE, ',
     +            i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4,
     +            'BEFORE STARTESP DATE, ',
     +            i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4)

                ncstor=i-1
                call warn
cew jump out of loop to end of subroutine
              goto 400


         elseif(ieday_shft .eq. ijdls .and. iehour_shft .lt. ihls) then
                write(ipr,201)icmonth,iccday,icyear,icchour,icode,
     +            ijmonth,ijday,ijyear,ijhour,icode
                write(ipr,225)icmonth,iccday,icyear,icchour,icode

                ncstor=i-1
                call warn
cew jump out of loop to end of subroutine
              goto 400


         elseif(ieday_shft .gt. ljdls) then
                call mdyh1(ljdls,lhls,ilmonth,ilday,ilyear,ilhour,
     +            nlstz,0,icode)

                write(ipr,202)icmonth,iccday,icyear,icchour,icode,
     +            ilmonth,ilday,ilyear,ilhour,icode
                write(ipr,225)icmonth,iccday,icyear,icchour,icode

202             format('** WARNING ** CARRYOVER DATE, ',
     +            i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4,
     +            'AFTER STARTESP DATE, ',
     +            i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4)

                ncstor=i-1
                call warn
cew jump out of loop to end of subroutine
              goto 400

         elseif(ieday_shft .eq. ljdls .and. iehour_shft .gt. lhls) then
                call mdyh1(ljdls,lhls,ilmonth,ilday,ilyear,ilhour,
     +            nlstz,0,icode)

                write(ipr,201)icmonth,iccday,icyear,icchour,icode,
     +            ilmonth,ilday,ilyear,ilhour,icode
                write(ipr,225)icmonth,iccday,icyear,icchour,icode

                ncstor=i-1
                call warn
cew jump out of loop to end of subroutine
              goto 400

         endif


cew make these checks only if there is more than 1 carryover day

         if(i .gt.1) then

c check to be sure that carryover dates are entered sequentially.
            if(ieday_lst(i) .lt. ieday_lst(i-1)) then

              write(ipr,205) i-1, ncstor

205           format('** WARNING ** CARRYOVER DATES MUST BE ',
     +                     'ENTERED SEQUENTIALLY.  ONLY ',i2,
     +                     ' DATES WILL BE SAVED, NOT ',i2,'.')

cew convert previous day from julian to m/d/y/h

              call mdyh1(ieday_lst(i-1),iehour_lst(i-1),iimonth,iiday,iiyear,
     +                iihour,nlstz,0,iicode)


              write(ipr,210) icmonth_orig,iccday_orig,icyear_orig,
     +                        icchour_orig,icode,
     +                       iimonth,iiday,iiyear,iihour,iicode

210           format(5x,'THE DATE ',
     +             i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4,
     +             ' SHOULD BE ENTERED BEFORE THE DATE ',
     +             i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4)

cew reduce the number of dates being stored
              ncstor=i-1

              call warn

cew jump out of loop to end of subroutine
              goto 400

cew check in case just hour is wrong

            else if(ieday_lst(i) .eq. ieday_lst(i-1) .and.
     +              iehour_lst(i) .lt. iehour_lst(i-1)) then

              write(ipr,205) i-1, ncstor


              call mdyh1(ieday_lst(i-1),iehour_lst(i-1),iimonth,iiday,
     +               iiyear, iihour,nlstz,0,iicode)

              write(ipr,210) icmonth_orig,iccday_orig,icyear_orig,
     +                        icchour_orig,icode,
     +                       iimonth,iiday,iiyear,iihour,iicode

              ncstor=i-1

              call warn

cew jump out of loop to end of subroutine
              goto 400

cew check if dates are the same

            else if (ieday_lst(i) .eq. ieday_lst(i-1) .and.
     +                iehour_lst(i)  .eq. iehour_lst(i-1)) then

              write(ipr,215)i-1,ncstor

215           format('** WARNING ** CARRYOVER DATES MUST BE ',
     +                     'UNIQUE.  ONLY ',i2,' DATES WILL BE',
     +                     ' SAVED, NOT ',i2,'.')


              call mdyh1(ieday_lst(i-1),iehour_lst(i-1),iimonth,iiday,
     +                    iiyear,iihour,nlstz,0,iicode)

              write(ipr,210) icmonth_orig,iccday_orig,icyear_orig,
     +                        icchour_orig,icode,
     +                       iimonth,iiday,iiyear,iihour,iicode

220           format(5x,'THE DATE ',
     +             i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4,
     +             ' IS THE SAME AS THE DATE ',
     +             i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4)


              ncstor=i-1

              call warn

cew jump out of loop to end of subroutine
              goto 400



cew check to be sure that all of the carryover days are in the same month.

         else if( ifirst_month .ne. icmonth) then

              write(ipr,'(20x,a)') '** WARNING **  All CARRYOVER DATES M
     +UST BE IN THE SAME MONTH.'

              write(ipr,225)icmonth,iccday,icyear,icchour,icode

225           format(20x,'THE DATE ',
     +             i2.2,'/',i2.2,'/',i4.4,'/',i2.2,1x,a4,
     +             'AND SUBSEQUENT REQUESTED DATES WILL NOT BE SAVED.')

              ncstor=i-1

              call warn

cew jump out of loop to end of subroutine
              goto 400

cew end of if ... else if ... else if
            endif

cew end of if (i gt 1)
           endif

cew only get here if all checks above are ok

          idiff=ijyear-icyear
          inewyear=idyear-idiff

c hsd bug r26-26   01/12/2006 xfan
c When running etsgen with a SaveCODate = 2005-03-01
c the program will come up with an error every 4 years
c (referrence to 2/29).
c Fix: when SaveCODate is March 01, then carryover date
c should be shifted to Feburary 29 if it is a leap year,
c otherwise to Feburary 28.

cfan check leap year
          ileap=1
          if (mod(inewyear,4).ne.0) ileap=0
          if (mod(inewyear,100).eq.0 .and. mod(inewyear,400).ne.0)
     &                              ileap=0
          if (ileap .eq. 1) then
           if (icmonth .eq. 2 .and. iccday .eq. 28) then
              iccday=29
           endif
          endif
cfan

          inewyear=mod(inewyear,100)

          call fctzc(nlstz,0,icode)
          call julda(icday(i),ichour(i),icmonth, iccday,inewyear,
     +     icchour,nlstz,0,icode)
cfan
          if (ileap .eq.1) then
           ileapyear=1
           icday0229=icday(1)
          endif
cfan
          write(ipr,230) icmonth,iccday,inewyear,icchour,icode
230       format(20x,'**NOTE**',2x,'CARRYOVER WILL BE SAVED FOR: ',
     +             i2.2,'/',i2.2,'/',i2.2,'/',i2.2,1x,a4)

cew open the files temporary files TEMP.70 - TEMP.79
cew one for each day of carryover to be saved.

cew KOD=8 to force unformatted files, plus 1 to create or open
        KOD = 8+1
        CALL UDOPEN(icfnum(i),KOD,NEWNAM,LRECL,ISTAT)

cew write out header since fcwtco will think it has already been written
cew each date will have a header written out for it

         WRITE (icfnum(i)) IDSEG,IWOCRY,ICDAY(I),ICHOUR(I),NC,NCOPS

cew save first month for making checks later
        if(i.eq.1) ifirst_month=icmonth

200    continue

cew this is where you arrive when you fail one of the date checks
400   IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)

      RETURN
      END
