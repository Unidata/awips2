C MEMBER PRTMDR
C  (from old member PDPRTOBS)
C-----------------------------------------------------------------------
C
      SUBROUTINE PRTMDR (XDTYPE,JULST,JULSTP,LIDATA,IDATA)
C
C          ROUTINE:  PRTMDR
C
C             VERSION:  1.0.0
C
C                DATE:  8-31-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE PRINTS THE MDR6 OBSERVATION DATA FOR THE
C    SPECIFIED DATES.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       XDTYPE     A    I      1    DATA TYPE
C       JULST      I    I      1    FIRST DATE FOR PRINT (JULIAN DAY)
C       JULSTP     I    I      1    LAST DATE FOR PRINT (JULIAN DAY)
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udatas'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pddtdr'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (LPNTRS=32)
      PARAMETER (LDATES=64)
C
      CHARACTER*(*) XDTYPE
C
      INTEGER*2 IDATA(LIDATA)
      INTEGER*2 IPNTRS(LPNTRS)
      INTEGER*2 MSNG
C
      DIMENSION IDATES(LDATES)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/prtmdr.f,v $
     . $',                                                             '
     .$Id: prtmdr.f,v 1.2 1996/03/21 16:35:39 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,*) '*** ENTER PRTMDR'
C
      JULNXT=JULST
      IDSPLY=0
C
C  FIND TYPE IN DIRECTORY
      IDX=IPDCKD(XDTYPE)
      IF (IDX.NE.0) GO TO 10
         WRITE (LP,220) XDTYPE(1:LENSTR(XDTYPE))
         GO TO 210
C
10    IF (JULNXT.GT.JULSTP) GO TO 170
C
C  GET DATA FOR ONE DAY
      IRETRN=0
      CALL RPDDLY (XDTYPE,JULNXT,IRETRN,LPNTRS,IPNTRS,LPFILL,
     *   LIDATA,IDATA,LDFILL,NUMSTA,MSNG,LDATES,IDATES,IERR)
      IF (IERR.EQ.0) GO TO 20
      GO TO (20,90,90,110,130,190),IERR
C
20    IF (IPDDB.GT.1) WRITE (IOGDB,*) 'NUMSTA=',NUMSTA
C
      IF (NUMSTA.EQ.0) GO TO 150
C
C  CONVERT DATE FOR PLOT
      JULDA=JULNXT-1
      IOFF=1
      IPER=6
      DO 60 K=1,4
         CALL MDYH2 (JULDA,IPER,IMO,IDAY,IYR,IHR,ITZ,IDSAV,TIME(3))
         IF (LDFILL.LE.0) GO TO 80
         N=IOFF+IDDTDR(16,IDX)-1
         DO 30 I=IOFF,N
            IF (IDATA(I).NE.MSNG) GO TO 40
30          CONTINUE
         WRITE (LP,230) K,IMO,IDAY,IYR,IHR,TIME(3)
         GO TO 50
40       CALL PDPLOT (K,IDATA(IOFF),MSNG,IMO,IDAY,IYR,IHR,TIME(3))
         IDSPLY=1
50       IOFF=N+1
C     INCREMENT PERIOD
         IPER=IPER+6
60       CONTINUE
C
C  INCREMENT TO NEXT DAY
70    JULNXT=JULNXT+1
      GO TO 10
C
C  NO MDR DATA FOR THIS DAY
80    WRITE (LP,240) IMO,IDAY,IYR,IHR,TIME(3)
      GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    WRITE (LP,100)
100   FORMAT ('0**ERROR** WORK ARRAYS TOO SMALL.')
      GO TO 210
C
110   WRITE (LP,120) XDTYPE(1:LENSTR(XDTYPE)),JULNXT
120   FORMAT (' **WARNING** ',A,' DATA NOT FOUND FOR JULIAN DATE ',I6,
     *   '.')
      GO TO 70
C
130   WRITE (LP,140) XDTYPE(1:LENSTR(XDTYPE))
140   FORMAT ('0**ERROR** DATA TYPE ',A,' NOT FOUND.')
      GO TO 210
C
150   WRITE (LP,160) XDTYPE(1:LENSTR(XDTYPE))
160   FORMAT ('0**NOTE** NO STATIONS DEFINED FOR DATA TYPE ',A,'.')
      GO TO 210
C
170   IF (IDSPLY.EQ.1) GO TO 210
      WRITE (LP,180)
180   FORMAT ('0**WARNING** NO MDR DATA PRINTED.')
      GO TO 210
C
190   WRITE (LP,200) IERR
200   FORMAT ('0**ERROR** IN PRTMDR - SYSTEM ERROR - IERR=',I2)
      GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
210   IF (IPDTR.GT.0) WRITE (IOGDB,*) '*** EXIT PRTMDR'
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
220   FORMAT ('0**ERROR** DATA TYPE ',A,' NOT FOUND.')
230   FORMAT (' **NOTE** DATA IS ALL MISSING FOR PERIOD ',I2,' ON ',
     *   2(I2.2,'/'),I4,' AT ',I2.2,A4,'.')
240   FORMAT (' **NOTE** NO MDR DATA FOUND FOR ',
     *   2(I2.2,'/'),I4,' AT ',I2.2,A4,'.')
C
      END
