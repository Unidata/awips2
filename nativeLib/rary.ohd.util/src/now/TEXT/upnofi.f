C$PRAGMA C (GETPID2)
C$PRAGMA C (GETHOSTID2)
C$PRAGMA C (GET_APPS_DEFAULTS)

C  =====================================================================
C  pgm: UPNOFI(PATH,IMVTMP) .. set pathname to blanks if it is a temp 
C  pgm:                        file name
C
C  input: IMVTMP ..... If 1, then a TEMP file will have its name moved
C  input:              to /tmp before being returned.
C  i/o: PATH ......... file pathname as character data - CHAR*(*)
C  i/o:                ( if the basename of the input pathname begins
C  i/o:                  with:
C  out:                      TEMP.
C  out:                      
C  out:                  then the filename will
C  out:                  be considered a temporary file and the input
C  out:                  pathname "PATH" will be made /tmp )
C  =====================================================================
      SUBROUTINE UPNOFI(PATH,IMVTMP)

      INTRINSIC      LEN
      CHARACTER*(*)  PATH
      CHARACTER      BLANK,SLASH
      INTEGER        IMVTMP
      
CHDH Added by hank to get the process id for later use.
      INTEGER       IPROCID
      CHARACTER*5   ZPROCID
      INTEGER       IHOSTID
      CHARACTER*10  ZHOSTID
      CHARACTER*16  ZSUFFIX
      
      character*256 fs5_dir
      integer       len_fs5_dir
      integer       igeterr
      
      INTEGER        LEN,IBBB,IEEE,ISSS,IMMM
      PARAMETER      ( BLANK=' ' )
      PARAMETER      ( SLASH='/' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upnofi.f,v $
     . $',                                                             '
     .$Id: upnofi.f,v 1.2 2004/10/08 19:22:59 hank Exp $
     . $' /
C    ===================================================================
C

C         Important variables:
C          IBBB ..... first non-blank character in PATH
C          IEEE ..... last non-blank character in PATH
C          ILLL ..... last character
C          ISSS ..... first character of file basename
C          IMMM ..... last character of concern in the basename

C         Get the length of the input pathname, PATH, as ILLL; then
C          work backwards to get the last non-blank character (IEEE).
        ILLL = LEN(PATH)
        IEEE = ILLL+1
  100   IEEE = IEEE-1
        IF( IEEE .LE. 1 ) GO TO 110
        IF( PATH(IEEE:IEEE) .EQ. BLANK ) GO TO 100
  110   CONTINUE

C         Now get the first non-blank character in PATH as IBBB.

        IBBB = 0
  120   IBBB = IBBB+1
        IF( IBBB .GE. IEEE ) GO TO 130
        IF( PATH(IBBB:IBBB) .EQ. BLANK ) GO TO 120
  130   CONTINUE

C         Working backwards from the last character, look for a
C          slash "/" and if found set ISSS to the next character
C          which is the start of the basename (else ISSS will be
C          the same as IBBB if no slash is found).

        ISSS = IEEE+1
  140   ISSS = ISSS-1
        IF( ISSS .LT. IBBB ) GO TO 150
        IF( PATH(ISSS:ISSS) .NE. SLASH ) GO TO 140
  150   ISSS=ISSS+1

chdh  Added in order to acquire process id and add it to file name.        
        CALL GETPID2(IPROCID)
        CALL UFI2AZ (ZPROCID,1,5,1,IPROCID)

chdh  Added in order to acquire host id and add it to file name.        
        CALL GETHOSTID2(IHOSTID)
        CALL UFI2AZ (ZHOSTID,1,10,1,IHOSTID)

chdh  Stores the full suffix to be added to the file name, unique
chdh  to this processor and this process.
        ZSUFFIX = ZPROCID // '.' // ZHOSTID
        
C        write (*,*) 'UPNOFI called... IN: ',PATH

C         Check to see if the PATH given already includes the 
C         process id.  If yes, then do not do the if's involving
C         the process id.  Instead, skip to the return at 990.

        IPPP=IEEE-15
        IF( IPPP .GT. 0 ) THEN
C          write (*,*) 'COMPARING... ',PATH(IPPP:IEEE),' -- ',ZPROCID
          IF( PATH(IPPP:IEEE) .EQ. ZSUFFIX) GOTO 990
        ENDIF

C         I do not yet have the process id.  So, continue...
        
C         Got the basename from character ISSS thru IEEE; check if
C          the basename is four characters and is "TEMP" or "temp".
C         If not then check if it is long enough to be one of the
C          staring sequences.
C         If one of the key strings is found, set the pathname PATH
C          to be the current PATH plus the process id. 

c         If IMVTMP equals 1, then the program will force TEMP files
C         to be placed in the /tmp directory if the path is the same as the ofs_fs5files
        
        IF( IMVTMP .EQ. 1) THEN
        
cew       now check to see if the path is the same as the ofs_fs5files token
            
cav             igeterr=get_apps_defaults ( 'ofs_fs5files', 12, 
           call get_apps_defaults ( 'ofs_fs5files', 12,
     +        fs5_dir, len_fs5_dir )
cew check to be sure the string fits
c  should never encounter these errors as we have already used this token elsewhere, but ....
          
cav              if(len_fs5_dir .gt. 256) then
cav                 CALL ERROR
cav              endif
              
cav              if (igeterr .ne. 0)then
cav                 CALL ERROR
cav              endif

C  is it the same without the slash
              if(path(IBBB:(ISSS-2)) .eq. fs5_dir(1:len_fs5_dir)) then

c  if it is then check on file name and change path if filename starts with TEMP.               
                 IF ( IEEE-ISSS .GT. 3 ) THEN
                   IMMM = ISSS+4
                   IF( PATH(ISSS:IMMM) .EQ. 'TEMP.' ) PATH = 
     +                '/tmp/' // PATH(ISSS:IEEE) // '.' // ZSUFFIX
                 ENDIF
                 
C  is it the same with the slash    Stupid line continue because of 72 char limit                
               elseif(path(IBBB:(ISSS-1)) .eq. fs5_dir(1:len_fs5_dir))
     +           then
               
c  if it is then check on file name and change path if filename starts with TEMP.                             
                 IF ( IEEE-ISSS .GT. 3 ) THEN
                   IMMM = ISSS+4
                   IF( PATH(ISSS:IMMM) .EQ. 'TEMP.' ) PATH = 
     +                '/tmp/' // PATH(ISSS:IEEE) // '.' // ZSUFFIX
                 ENDIF

              endif  !!end of if on path name        

        ELSE
                
C         Otherwise... use the files base path and add the suffix.
          IF( IEEE-ISSS .EQ. 3 ) THEN
            IF( PATH(ISSS:IEEE) .EQ. 'TEMP' ) PATH = 
     +        PATH(IBBB:IEEE) // '.' // ZSUFFIX
            IF( PATH(ISSS:IEEE) .EQ. 'temp' ) PATH = 
     +        PATH(IBBB:IEEE) // '.' // ZSUFFIX 
          ELSEIF ( IEEE-ISSS .GT. 3 ) THEN
            IMMM = ISSS+4
            IF( PATH(ISSS:IMMM) .EQ. 'TEMP.' ) PATH = 
     +        PATH(IBBB:IEEE) // '.' // ZSUFFIX 
            IF( PATH(ISSS:IMMM) .EQ. 'temp.' ) PATH = 
     +        PATH(IBBB:IEEE) // '.' // ZSUFFIX 
            IF( PATH(ISSS:IMMM) .EQ. 'TEMP_' ) PATH = 
     +        PATH(IBBB:IEEE) // '.' // ZSUFFIX 
            IF( PATH(ISSS:IMMM) .EQ. 'temp_' ) PATH = 
     +        PATH(IBBB:IEEE) // '.' // ZSUFFIX
          ENDIF
          
        ENDIF
        
C        write (*,*) 'UPNOFI called... OUT: ',PATH
CHDH      write (*,*) 'HERE... ',PATH
  990   RETURN
        END
