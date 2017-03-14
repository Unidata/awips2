C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: SHCLIT .. Close files for shefpars driver used as a filter
C
C  use:     CALL SHCLIT(LUIN,LUOT,LUSH,LUER,LUCP)
C
C  i/o: LUIN ....... logical unit num opened for "shefin" file - INT
C  i/o:              (if neg, then cannot set up i/o files)
C  i/o: LUOT ....... logical unit num opened for "shefout" file - INT
C  i/o: LUSH ....... logical unit num opened for "shefparm" file - INT
C  i/o: LUER ....... logical unit num opened for shef error msgs - INT
C  i/o: LUCP ....... logical unit num opened for all shef msgs - INT
C  out: (stderr) ... output messages may be sent to stderr
C
C  rqd: GET_APPS_DEFAULTS,SHCLWR,SHCLBL
C  =====================================================================
      SUBROUTINE SHCLIT(LUIN,LUOT,LUSH,LUER,LUCP)

C     EXTERNAL       GET_APPS_DEFAULTS
      EXTERNAL       SHCLWR,SHCLBL
      INTRINSIC      LEN

      INTEGER        LUIN,LUOT,LUSH,LUER,LUCP,IU
      INTEGER        IE,IEND
      CHARACTER*1    KHUN

C                   If input unit never existed, skip this routine

        IF (LUIN .GE. 0) THEN

C                   Set error unit (default 7 for HPUX)
 
          IU = 7
          CALL GET_APPS_DEFAULTS('fortran_stderr',14,KHUN,IEND)
            IF (IEND .EQ. 1) READ(KHUN,'(I1)',IOSTAT=IE) IU
            IF (IEND .EQ. 2) READ(KHUN,'(I2)',IOSTAT=IE) IU
 

C                   Write a blank line if needed
C                   Write out name of SHEFPARM file
C                   Write out name of input file
C                   Write out name of output file if one is created
C                   Write out name of error file if one is created
C                   Write a blank line if needed

          IF (IU .GE. 0) THEN
            CALL SHCLBL(IU,LUER)
            CALL SHCLWR(IU,LUER,LUSH,10,'  shefparm file used:  ')
            CALL SHCLWR(IU,LUER,LUIN,11,'     Input file used:  ')
            CALL SHCLWR(IU,LUER,LUOT,12,' Output file created:  ')
            CALL SHCLWR(IU,LUER,LUER,13,'  Error file created:  ')
            CALL SHCLBL(IU,LUER)
          ENDIF

C                   Now close files

          IF (LUSH .EQ. 10) CLOSE(LUSH,IOSTAT=IE)
          IF (LUIN .EQ. 11) CLOSE(LUIN,IOSTAT=IE)
          IF (LUOT .EQ. 12) CLOSE(LUOT,IOSTAT=IE)
          IF (LUER .EQ. 13) CLOSE(LUER,IOSTAT=IE)
          IF (LUCP .GT.  0) CLOSE(LUCP,IOSTAT=IE)

C                   Reset i/o units

          LUIN = -1
          LUOT = -1
          LUSH = -1
          LUER = -1
          LUCP = -1

        ENDIF

      RETURN
      END
