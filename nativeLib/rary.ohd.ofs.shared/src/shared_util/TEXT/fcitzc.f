C MODULE FCITZC
C-----------------------------------------------------------------------
C
      SUBROUTINE FCITZC (ITZ,IDSAV,CODE)
C
C  ROUTINE FCITZC CONVERTS A FOUR CHARACTER TIME ZONE CODE (CODE) INTO 
C  THE TIME ZONE NUMBER (ITZ) AND DAYLIGHT SAVINGS CODE (IDSAV).
C
C  VALUES OF ITZ, IDSAV AND CODE:
C
C      CODE  ITZ  IDSAV
C      ----  ---  -----
C      EST    -5    0
C      EDT    -5    1
C      CST    -6    0
C      CDT    -6    1
C      MST    -7    0
C      MDT    -7    1
C      PST    -8    0
C      PDT    -8    1
C      AST    -9    0
C      ADT    -9    1
C      HST   -10    0
C      HDT   -10    1
C      NST   -11    0
C      NDT   -11    1
C      Z       0    0
C      Z0      0    0
C      Z+1     1    0
C      Z+2     2    0
C       .      .    .
C       .      .    .
C       .      .    .
C      Z+12   12    0
C      Z-1    -1    0
C      Z-2    -2    0
C       .      .    .
C       .      .    .
C      Z-12  -12    0
C      INTL  100    0
C
C NOTES:
C   1. VALUES OF IDSAV OTHER THAN IDSAV=1 ARE TREATED
C      LIKE IDSAV=0
C   2. VALUES OF ITZ OUTSIDE THE RANGE -12 TO +12 ARE TREATED
C      LIKE ITZ=100
C   3. MISSPELLED TIME ZONE CODES ARE TREATED
C      LIKE CODE='INTL'
C
C
      CHARACTER*4 CODE,DTIMES(7),STIMES(7),ZTPL(12),ZTMIN(12),Z(2)
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2 
      DATA             RCSKW1,RCSKW2 /                                '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fcitzc.f,v $
     . $',                                                             '
     .$Id: fcitzc.f,v 1.2 2001/06/13 09:56:13 mgm Exp $
     . $' /
C    ===================================================================
C
      DATA DTIMES/'EDT ','CDT ','MDT ','PDT ','ADT ','HDT ','NDT '/
      DATA STIMES/'EST ','CST ','MST ','PST ','AST ','HST ','NST '/
      DATA Z/'Z   ','Z0   '/
      DATA ZTPL/'Z+1 ','Z+2 ','Z+3 ','Z+4 ','Z+5 ','Z+6 ',
     *          'Z+7 ','Z+8 ','Z+9 ','Z+10','Z+11','Z+12'/
      DATA ZTMIN/'Z-1 ','Z-2 ','Z-3 ','Z-4 ','Z-5 ','Z-6 ',
     *           'Z-7 ','Z-8 ','Z-9 ','Z-10','Z-11','Z-12'/
C
C
      IF (ITRACE.GT.2) WRITE (IODBUG,*) 'ENTER FCITZC'
C
      IDSAV=1
      DO 60 I=1,7
         ITZ=-4-I
         IF (CODE.EQ.DTIMES(I)) GO TO 90
60       CONTINUE
      IDSAV=0
      DO 70 I=1,7
         ITZ=-4-I
         IF (CODE.EQ.STIMES(I)) GO TO 90
70       CONTINUE
      ITZ=0
      IF (CODE.EQ.Z(1).OR.CODE.EQ.Z(2)) GO TO 90
      DO 80 I=1,12
         ITZ=I
         IF (CODE.EQ.ZTPL(I)) GO TO 90
         ITZ=-I
         IF (CODE.EQ.ZTMIN(I)) GO TO 90
80       CONTINUE
      ITZ=100
C
90    IF (ITRACE.GT.2) WRITE (IODBUG,*) 'EXIT FCITZC'
C
      RETURN
C
      END
