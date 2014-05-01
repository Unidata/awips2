C MODULE GGPXHD
C
      SUBROUTINE GGPXHD(ISTRT, LENGTH, ITYPE, ISTATF, IPCODE, IERROR)
C
C.....THIS SUBROUTINE PROCESSES A FIELD RETURNED FROM UFIEL2. THE FIELD
C.....IS TO BE THE GRID POINT PRECIPITATION MOD HEADER.
C
C.....ARGUMENT LIST.
C
C.....ISTRT  - START OF FIELD (FROM UFIEL2).
C.....LENGTH - LENGTH OF FIELD (FROM UFIEL2).
C.....ITYPE  - TYPE OF FIELD (FROM UFIEL2).
C.....ISTATF - STATUS CODE (FROM UFIEL2).
C.....IPCODE - PROCESS CODE
C.....          = 1   CONTINUE PROCESSING THE FIELD.
C.....          = 0   DO NOT PROCESS THE FIELD ANY FURTHER.
C.....IERROR - ERROR CODE.
C
C.....ORIGINALLY WRITTEN BY
C
C.....JERRY M. NUNN       WGRFC FT. WORTH, TEXAS       SEPTEMBER 1986
C
C
      DIMENSION SNAME(2)
C
      INCLUDE 'common/where'
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/ggpxhd.f,v $
     . $',                                                             '
     .$Id: ggpxhd.f,v 1.2 1998/07/02 20:34:11 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA GPXP, SNAME /4hGPXP, 4hGGPX, 4hHD  /
C
  900 FORMAT(1H0, '*** GGPXHD ENTERED -- PROCESS CODE IS ', I2, ' ***')
  901 FORMAT(1H0, '*** EXIT GGPXHD -- ERROR RETURN CODE = ', I2,
     * ' ***')
  902 FORMAT(1H0, '*** RETURN CODE FROM ''.GRIDPX'' SEARCH ROUTINE IS ',
     * I2, ' ***')
C
      INCLUDE 'gcommon/setwhere'
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,900) IPCODE
      IBUG = IPBUG(GPXP)
      IERROR = 0
C
C.....CHECK FIELD RETURNED FROM UFIEL2. IT MUST BE A CHARACTER STRING
C.....18300 AND START IN COL. 1. OF COURSE...THERE MUST BE SUFFICIENT
C.....SPACE IN CHAR TO HOLD THE FIELD.
C
      IF(ISTATF .NE. 0) GOTO 100
      IF(ISTRT  .NE. 1) GOTO 100
      IF(ITYPE  .NE. 2) GOTO 100
      IF(LENGTH .NE. 7) GOTO 100
C
C.....CHECK THE HEADING FOR THE PROPER NAME.
C
      NP = 1
      CALL GSTCHK(NP, ICDBUF, LENGTH, ISTATH)
      IF(IBUG .EQ. 1) WRITE(IOPDBG,901) ISTATH
      IF(ISTATH .EQ. 0) GOTO 200
C
  100 IERROR = 1
C
  200 IF(IPTRCE .GE. 1) WRITE(IOPDBG,901) IERROR
      RETURN
      END
