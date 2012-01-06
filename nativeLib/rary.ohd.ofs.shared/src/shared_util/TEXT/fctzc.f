C MODULE FCTZC
C-----------------------------------------------------------------------
C  
      SUBROUTINE FCTZC (ITZ,IDSAV,CODE)
C
C  ROUTINE FCTZC CONVERTS TIME ZONE NUMBER (ITZ) AND DAYLIGHT SAVINGS 
C  CODE (IDSAV) INTO A FOUR CHARACTER TIME ZONE CODE (CODE).
C
C  VALUES OF ITZ,IDSAV, AND CODE:
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
      CHARACTER*4 CODE,DTIMES(7),STIMES(7),ZTPL(12),ZTMIN(12),Z(2)
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fctzc.f,v $
     . $',                                                             '
     .$Id: fctzc.f,v 1.3 2001/06/13 09:56:28 mgm Exp $
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
      IF (ITRACE.GT.2) WRITE (IODBUG,*) 'ENTER FCTZC'
C
      CODE='INTL'
C
C  INTERNAL TIME IF ITZ OUT OF RANGE
      IF (ITZ.GT.12.OR.ITZ.LT.-12) GO TO 30
C
C  DO NOT RETURN Z MNEMONICS UNLESS NECESSARY
      IF (ITZ.GT.-5.OR.ITZ.LT.-11) GO TO 20
C
      I=-4-ITZ
      CODE=STIMES(I)
      IF (IDSAV.EQ.1) CODE=DTIMES(I)
      GO TO 30
C
20    I=ITZ
      IF (IDSAV.EQ.1) I=I+1
      IF (I.EQ.13) I=-12
      IF (I.EQ.0) CODE=Z(1)
      IF (I.LT.0) CODE=ZTMIN(IABS(I))
      IF (I.GT.0) CODE=ZTPL(I)
C
30    IF (ITRACE.GT.2) WRITE (IODBUG,*) 'EXIT FCTZC'
C
      RETURN
C
      END
