C MEMBER EPRD01
C  (from old member EEPRD01)
C
      SUBROUTINE EPRD01(DSP,LPESP)
C
C ......................................................................
C
C        PRINT DISPLAY 1 INFO
C
C THIS SUBROUTINE PRINTS DISPLAY INFORMATION STORED IN THE ESP
C    P ARRAY (PESP). IT IS IS ONE OF THE PRINT DISPLAY SUBROUTINES
C    AND PRINTS INFO ABOUT THE DISPLAY TYPE : SUMMARY TABLE.
C
C THIS ROUTINE IS PART OF THE ESP INITILIZATION PROGRAM
C   AND PRINT SEGMENT ROUTINE, AND MORE SPECIFICALLY
C   THE PRINT P ARRAY ROUTINE (EPRP).
C
C .....................................................................
C
C   ORIGINALLY WRITTEN BY
C     ED VANBLARGAN - HRL -JUNE,1981
C
C .....................................................................
C
      DIMENSION DSP(1),SBNAME(2),OLDOPN(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/espinit/RCS/eprd01.f,v $
     . $',                                                             '
     .$Id: eprd01.f,v 1.1 1995/09/17 18:46:26 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NAME,SBNAME/4HEPRT,4HEPRD,4H01  /
C
C SET ERROR TRACES
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
10    OPNAME(I)=SBNAME(I)
C
C GET TRACE LEVEL AND DEBUG
C
      IF (ITRACE.GE.1) WRITE(IODBUG,100)
100   FORMAT(1H0,14HEPRD01 ENTERED)
      IBUG=IFBUG(NAME)
C
C SET LOCATIONS IN ARRAY. ITYP= DISPLAY TYPE
C NUM= NUMBER OF DISPLAY PARAMETERS + 2
C NDSP= NUMBER OF DISPLAY PARAMETERS
C
      ITYP=DSP(1)
      NUM=DSP(2)
      NDSP=NUM-2
      WRITE (IPR,200)
200   FORMAT(/ 21X,28HDISPLAY TYPE : SUMMARY TABLE)
C
C DEBUG TIME
C
      IF (IBUG.GE.1) WRITE(IODBUG,300) ITYP,NDSP
300   FORMAT(/ 21X,I4,31H = NUMBER DEFINING DISPLAY TYPE
     */ 21X,I4,31H = NUMBER OF DISPLAY PARAMETERS)
C
C
C RESET ERROR TRACES IN CB/WHERE/
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
      END
