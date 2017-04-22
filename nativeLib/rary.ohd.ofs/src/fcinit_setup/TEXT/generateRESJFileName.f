C$PRAGMA C (GET_APPS_DEFAULTS)
C generateRESJFileName.f
C
C generate file name for RES-J use
C
C History:
C 11/24/97      Eric Markstrom, RTi Created Routine
C
C PO    	I       a real array, internally dimensioned PO(1)
C          		  	PO(1) == 0 if TEMP FILE
C				PO(1) == 1 if not TEMP FILE
C fileName 	O    name of RESJ file for processed database
C iseg		I    name or segment
C ierr     	O    ierr = 0; return without error, 1 otherwise
C
      subroutine generateRESJFileName( po, fileName, iseg ,ierr )
C  
C      implicit none

C    PASSED VARIABLES
      REAL 		po(*)		! real array containing whether the RES-J
					! fs5file is a permanent or temporary one
      CHARACTER*128 	fileName 	! name of RES-J file, includes directory
      INTEGER 		iseg(2)		! name of segment
      INTEGER 		ierr		! 0 - no error, otherwise error

C    LOCAL VARIABLES
      CHARACTER*10 	cseg		! character string to hold name of segment
      CHARACTER*10 	cname		! character string to hold name of operation
      CHARACTER*128 	fs5_dir  	! character string to hold name of fs5file dirctories 
      CHARACTER*128 	tempFileName 	! character string to hold temporary file name
      INTEGER 		len_fs5_dir    	! length of string containing fs4_dir
      INTEGER 		i		! counter
      INTEGER		j 		! counter
      INTEGER           icount12        ! number 12
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/generateRESJFileName.f,v $
     . $',                                                             '
     .$Id: generateRESJFileName.f,v 1.3 2004/05/03 21:41:25 hank Exp $
     . $' /
C    ===================================================================
C
	
C    GET THE FS5FILE DIRECTORY 
      icount12 = 12
      ierr = get_apps_defaults ( 'ofs_fs5files', icount12, fs5_dir, 
     + len_fs5_dir )
      if ( ierr .GT. 0 ) then	    	!!! return error 
        ierr = 1
        return
      end if

      write ( cseg, 100 ) ( iseg(j), j= 1,2 )
      write( cname, 100 ) ( po(j), j = -4, -3 )
100   format ( 2A4 )

C   CONCATENATE THE DIRECTORY AND FILE NAME 
      if ( po(1) .LT. 0.1 ) then   		!!! temp file 
        tempFileName = fs5_dir(1 : len_fs5_dir) //
     +                 '/TEMP.RESJ.' // cseg // '.' // cname        	

      else                                		!!! permanent fs5file 
        tempFileName = fs5_dir(1 : len_fs5_dir) //
     +                 '/RESJ.' // cseg // '.' // cname            	
      end if

C     REMOVE SPACES AND TRANSFER tempFileName TO fileName
      fileName = ' '
      j = 1
      do i = 1, 128
        if ( (tempFileName( i:i ) .ge. '#') .and. 
     + (tempFileName( i:i ) .le. 'z') ) then
           fileName( j:j ) = tempFileName( i:i )
           j = j + 1
        end if
      end do

chdh  Added by Hank to add the process id to the name if it is a TEMP.
chdh  This will move the file name to be in the directory /tmp if
chdh  it is a TEMP.* file.
      call upnofi(fileName,1)  

      end
		
