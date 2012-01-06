C$PRAGMA C (checksystem)
C
C23456---1---------2---------3---------4---------5---------6---------712
C
      subroutine cvtgriddb
C
C-----------------------------------------------------------------------
C     Find more detail information about XMRG files
C     /fs/awips/rfc/users/xfan/devl/bin/fcst_dir/PTPE/ptpe.ipt
C
C     There are 4 kind of XMRG file formats:
C
C (1) The 1st format:  no record 2
C
C     Record Field# Dim. Type  Description
C     1      1      1    I*4   XOR, western most HRAP column in the area
C            2      1    I*4   YOR, southern most HRAP row in the area
C            3      1    I*4   MAXX, number of columns
C            4      1    I*4   MAXY, number of rows
C   2-MAXY   1     MAXX  I*2   IVAL, gridded values
C
C (2) pre AWIPS Bld4.2 format:  recoed 2 has 37 or 38 characters
C
C     Record Field# Dim. Type  Description
C     1      1      1    I*4   XOR, western most HRAP column in the area
C            2      1    I*4   YOR, southern most HRAP row in the area
C            3      1    I*4   MAXX, number of columns
C            4      1    I*4   MAXY, number of rows
C     2      1      1    C*10  USER, office generating file
C            2      1    C*20  NDATIM, saved date & time: ccyy-mm-dd hh:nn:ss
C            3      1    C*7/8 P_FLAG, process flag
C   3-MAXY   1     MAXX  I*2   IVAL, gridded values
C
C (3) post AWIPS Bld4.2 format:  recoed 2 has 66 characters
C
C     Record Field# Dim. Type  Description
C     1      1      1    I*4   XOR, western most HRAP column in the area
C            2      1    I*4   YOR, southern most HRAP row in the area
C            3      1    I*4   MAXX, number of columns
C            4      1    I*4   MAXY, number of rows
C     2      1      1    C*10  USER, office generating file
C            2      1    C*20  NDATIM, saved date & time: ccyy-mm-dd hh:nn:ss
C            3      1    C*8   P_FLAG, process flag
C            4      1    C*20  VDATIM, valid date & time: ccyy-mm-dd hh:nn:ss
C            5      1    I*4   LEX, not used
C            6      1    R*4   VERS, version number of file
C   3-MAXY   1     MAXX  I*2   IVAL, gridded values
C
C (4) post AWIPS Bld5.2.2 format:  recoed 2 has 66 characters
C
C     Record Field# Dim. Type  Description
C     1      1      1    I*4   XOR, western most HRAP column in the area
C            2      1    I*4   YOR, southern most HRAP row in the area
C            3      1    I*4   MAXX, number of columns
C            4      1    I*4   MAXY, number of rows
C     2      0      1    C*2   Oper Sys (HP or LX)
C            1      1    C*8   User ID
C            2      1    C*20  NDATIM, saved date & time: ccyy-mm-dd hh:nn:ss
C            3      1    C*8   P_FLAG, process flag
C            4      1    C*20  VDATIM, valid date & time: ccyy-mm-dd hh:nn:ss
C            5      1    I*4   Maximum value (unit=mm)
C            6      1    R*4   VERS, version number of file
C   3-MAXY   1     MAXX  I*2   IVAL, gridded values
C
C
C     XMRG files are written row by row from within a "do_loop" using
C     a FORTRAN unformatted write statement. FORTRAN unformatted records
C     have a 4 byte integer at the beginning and end of ench record that
C     is equal to the number of 4 byte words contained in the record.
C
C-----------------------------------------------------------------------
C
C    Variables:
C
C     bc:    XOR(HRAP-X coordinate of sw corner of grid)
C     br:    YOR(HRAP-Y coordinate of sw corner of grid)
C     nc:    MAXX(Num of HRAP grid boxes in X direction)
C     nr:    MAXY(Num of HRAP grid boxes in Y direction)
C     nflag: 1 for UNIX and 0 for LINUX
C     num1:  record length of 1st record
C     num2:  record length of 2nd record
C     num3:  record length of 3rd record
C
C---------------------------------------------------------------------
      integer   unitin, unitout, num0, num1, num2, num3, nflag
      integer*4 bc, br, nc, nr
      integer*2 mapx(5000)
      character file1*100, file2*100
      character*66 info66
      character*38 info38
      character*37 info37
C
      character*256 SourceFile
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/cvtgriddb/RCS/cvtgriddb.f,v $
     . $',                                                             '
     .$Id: cvtgriddb.f,v 1.2 2003/06/03 14:01:31 xfan Exp $
     . $' /
C    ===================================================================
C
      no_of_arguments = iargc()
C
      if ( no_of_arguments .ne. 1 ) then
         write(*,*) '--------------------------------------------------'
         write(*,*) 'Usage: cvt_griddb <file name>        '
         write(*,*) 'Where: <file name> is the the original XMRG file'
         write(*,*) 'Output: <file name>.out'
         write(*,*) '--------------------------------------------------'
         call exit(1)
      else
         num0 = 1
         call getarg( num0, SourceFile)
      endif

C     Set the name of the file to convert and its target.

      file1 = trim(SourceFile)
      file2 = trim(SourceFile) // ".out"

      UNITIN  = 33
      UNITOUT = 34

      open (UNITIN, file=file1,access='direct',recl=1)
      open (UNITOUT, file=file2,form='unformatted')

C     Check the System

      call checksystem(nflag)
      if (nflag.eq.1) then
c         write(*,*)'The system is UNIX'
      else
c         write(*,*)'The system is LINUX'
      endif

C     Check the endian format of the input xmrg file

      call readBigInt4( UNITIN, 1, 4, num1 )

      if (num1 .eq. 16) then

c        write(*,11)
        if (nflag.eq.1) then
          write(*,333) trim(SourceFile)
          STOP 'NO CONVERSION.'
        endif

C--1--  Read the first row

        call readBigInt4( UNITIN, 5, 8, bc )
        call readBigInt4( UNITIN, 9, 12, br )
        call readBigInt4( UNITIN, 13, 16, nc )
        call readBigInt4( UNITIN, 17, 20, nr )

c        write(*,111) bc,br,nc,nr
        write( UNITOUT ) bc,br,nc,nr

        call readBigInt4( UNITIN, 25, 28, num2 )

C--2--  Read the second line

        if (num2 .EQ. 66) then                    !------------------num2=66

           call readChr1( UNITIN, 29, 29+num2-1, info66 )
           write( UNITOUT ) info66

           call readBigInt4( UNITIN, 29+num2+4, 29+num2+7, num3)

        elseif (num2 .EQ. 38) then                !------------------num2=38

           call readChr1( UNITIN, 29, 29+num2-1, info38 )
           write( UNITOUT ) info38

           call readBigInt4( UNITIN, 29+num2+4, 29+num2+7, num3)

        elseif (num2 .EQ. 37) then               !-------------------num2=37

           call readChr1( UNITIN, 29, 29+num2-1, info37 )
           write( UNITOUT ) info37

           call readBigInt4( UNITIN, 29+num2+4, 29+num2+7, num3)

        else                                     !-------------------num2=others

          num3 = num2
          num2 = -8

        endif                                    !-------------------endif

c        write(*,*)'num1,num2,num3====',num1,num2,num3

        if (num3 .eq. 2*nc) then

C--3--  Read the sunsequent lines

          do i = 1, nr
            i1 = (i-1)*(nc+4)*2 + 29+num2+8
            i2 = i1 + nc*2 - 1
            call readBigInt2( UNITIN, i1, i2, mapx )
            write( UNITOUT ) (mapx(j), j=1, nc )
          end do

        else

          STOP 'Wrong XMRG Formati for UNIX'

        endif

      else

        if (nflag.eq.0) then
          write(*,444) trim(SourceFile)
          STOP 'NO CONVERSION.'
        endif

C--1--  Read the first row

        call readLitInt4( UNITIN, 5, 8, bc )
        call readLitInt4( UNITIN, 9, 12, br )
        call readLitInt4( UNITIN, 13, 16, nc )
        call readLitInt4( UNITIN, 17, 20, nr )

c        write(*,111) bc,br,nc,nr
        write( UNITOUT ) bc,br,nc,nr

        call readLitInt4( UNITIN, 25, 28, num2 )

C--2--  Read the second line

        if (num2 .EQ. 66) then                    !------------------num2=66

           call readChr1( UNITIN, 29, 29+num2-1, info66 )
           write( UNITOUT ) info66

           call readLitInt4( UNITIN, 29+num2+4, 29+num2+7, num3)

        elseif (num2 .EQ. 38) then                !------------------num2=38

           call readChr1( UNITIN, 29, 29+num2-1, info38 )
           write( UNITOUT ) info38

           call readLitInt4( UNITIN, 29+num2+4, 29+num2+7, num3)

        elseif (num2 .EQ. 37) then               !-------------------num2=37

           call readChr1( UNITIN, 29, 29+num2-1, info37 )
           write( UNITOUT ) info37

           call readLitInt4( UNITIN, 29+num2+4, 29+num2+7, num3)

        else                                     !-------------------num2=others

          num3 = num2
          num2 = -8

        endif                                    !-------------------endif

c        write(*,*)'num1,num2,num3====',num1,num2,num3

        if (num3 .eq. 2*nc) then

C--3--  Read the sunsequent lines

          do i = 1, nr
            i1 = (i-1)*(nc+4)*2 + 29+num2+8
            i2 = i1 + nc*2 - 1
            call readLitInt2( UNITIN, i1, i2, mapx )
            write( UNITOUT ) (mapx(j), j=1, nc )
          end do

        else

          STOP 'Wrong XMRG Formati for LINUX'

        endif

      endif

      write(*,*) trim(SourceFile), ' conversion successful!'

      close (UNITIN)
      close (UNITOUT)
      close (35)
  11  format(39HThe input file is in Big_Endian format.,
     &       39HThis converter should work on LINUX in ,
     &       39Horder to get Lit_Endian format output. )
  22  format(39HThe input file is in Lit_Endian format.,
     &       39HThis converter should work on UNIX  in ,
     &       39Horder to get Big_Endian format output. )
 111  format(44HXOR(HRAP-X coordinate of sw corner of grid):,I4/,
     &       44HYOR(HRAP-Y coordinate of sw corner of grid):,I4/,
     &       44HMAXX(Num of HRAP grid boxes in X direction):,I4/,
     &       44HMAXY(Num of HRAP grid boxes in Y direction):,I4)
 222  format(A66)
 333    format(A,25h conversion unsuccessful!,2x, 10hThe system,
     &  50h is UNIX, the input file is in Big_Endian format. )
 444    format(A,25h conversion unsuccessful!,2x, 10hThe system,
     &  50h is LINUX, the input file is in Lit_Endian format.)

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Subroutines
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine checkendian( iunit, i1, i2, iout )
        character*1 x
        iout = 0
        do i = i1, i2
            read( iunit, rec=i ) x
            iout = iout*2**8 + ichar(x)
        end do
      end

      subroutine readBigInt4( iunit, i1, i2, iout )
        character*1 x(4)
        iout = 0
        do i=i1,i2
            read( iunit, rec=i ) x(i-i1+1)
        enddo
            iout = ichar(x(1))*2**24 + ichar(x(2))*2**16
     &           + ichar(x(3))*2**8  + ichar(x(4))
      end

      subroutine readLitInt4( iunit, i1, i2, iout )
        character*1 x(4)
        iout = 0
        do i=i1,i2
            read( iunit, rec=i ) x(i-i1+1)
        enddo
            iout = ichar(x(4))*2**24 + ichar(x(3))*2**16
     &           + ichar(x(2))*2**8  + ichar(x(1))
      end

      subroutine readBigInt2( iunit, i1, i2, iout )
        integer*2 iout(*)
        character*1 x
        do i = i1, i2
            read( iunit, rec=i ) x
          k = (i-i1)/2 + 1
          if ( mod(i-i1,2) .eq. 0 ) then
              iout(k) = ichar(x)
          else
              iout(k) = iout(k)*2**8 + ichar(x)
          end if
        end do
      end

      subroutine readLitInt2( iunit, i1, i2, iout )
        integer*2 iout(*)
        character*1 x
        do i = i1, i2
            read( iunit, rec=i ) x
          k = (i-i1)/2 + 1
          if ( mod(i-i1,2) .eq. 0 ) then
              iout(k) = ichar(x)
          else
              iout(k) = iout(k) + ichar(x)*2**8
          end if
        end do
      end

      subroutine readChr1( iunit, i1, i2, iout )
      character*1 x
      character*(*) iout
        j = 0
        do i = i1, i2
            read( iunit, rec=i ) x
            j = j + 1
            iout(j:j) = x
        end do
      end
