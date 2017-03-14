C MODULE HCOMPT
C-----------------------------------------------------------------------
C
C  MAIN ROUTINE FOR THE COMPUTE FUNCTION
C
      SUBROUTINE HCOMPT (ISTAT)
C
C  ARGUMENT LIST:
C
C       NAME      TYPE   I/O   DIM   DESCRIPTION
C       ------    ----   ---   ---   -----------
C       ISTAT     I*4     O     1    STATUS:
C                                      0=OK
C                                      1=ERROR
C
      CHARACTER*8 FUNNAM
      CHARACTER*8 RTNNAM,RTNOLD
      CHARACTER*8 TECHNAME
      INTEGER KOND
C
      INCLUDE 'uiox'
      INCLUDE 'hclcommon/hcurfc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/hcompt.f,v $
     . $',                                                             '
     .$Id: hcompt.f,v 1.6 2004/09/28 18:00:28 hank Exp $
     . $' /
C    ===================================================================
C
C   

      RTNNAM='HCOMPT'
      IOPNUM=-1
      CALL FSTWHR (RTNNAM,IOPNUM,RTNOLD,IOLDOP)
C
C  SET UP FUNCTION
      FUNNAM=' '
      CALL HCMPSU (FUNNAM,ISTAT)
      IF (ISTAT.NE.0) GO TO 210

C  Change made by Hank Herr (2004-06-15).  
C  At this point, the variable IHCFUN is known after calling HCMPSU.
C  So, lock the files needed by calling hfcstlock(IHCFUN).
C  It is assumed that the calling routine will free all locks before
C  this routine is called.
      CALL HFCSTLOCK(IHCFUN,KOND)
      IF (KOND > 0) THEN
          STOP 16
      ENDIF
C
C  CHECK IF GLOBAL OR LOCAL FUNCTION
      IGOTO=IHCFUN/500+1
CHDH  RPDBCI is called all the time, even if SKIPBLND is turned on.
CHDH  By turning SKIPBLND on, the data read in by rpdbci will go 
CHDH  unused, but it doesn't cause any harm to read it anyway.  
CHDH  (This will make it so that the no-technique defined message for
CHDH  SKIPBLND will only appear if ESP is being run!)
      CALL RPDBCI (IERR)
      GO TO (30,190),IGOTO
C
10    WRITE (LP,20) IGOTO
20    FORMAT ('0**ERROR** IN HCOMPT - FUNCTION NUMBER ',I4,
     *   ' NOT RECOGNIZED.')
      CALL ERROR
      GO TO 210
      
      
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  HCL GLOBAL FUNCTIONS
C
30    GO TO (40,50,60,70,80,
     *       90,100,110,120,130,
     *       140,150,160,170,180),IHCFUN
      GO TO 10
C
C  FCEXEC FUNCTION
40    CALL HCKLST (FUNNAM,IERR)
      IF (IERR.EQ.0) CALL FUN001
      GO TO 210
C
C  SAVEDATE FUNCTION
50    CALL FUN002
      GO TO 210
C
C  FREEDATE FUNCTION
60    CALL FUN003
      GO TO 210
C
C  ESP FUNCTION
70    CALL FUN004
      GO TO 210
C
C  CPUCHECK FUNCTION
80    CALL FUN005
      GO TO 210
C
C  CGSTATUS FUNCTION
90    CALL FUN006
      GO TO 210
C
C  PRINTOPS FUNCTION
100   CALL FUN007
      GO TO 210
C
C  MAT FUNCTION
110   CALL HCKLST (FUNNAM,IERR)
      IF (IERR.EQ.0) CALL FUN008
      GO TO 210
C
C  MAP FUNCTION
120   CALL HCKLST (FUNNAM,IERR)
      IF (IERR.EQ.0) CALL FUN009
      GO TO 210
C
C  MAPE FUNCTION
130   CALL HCKLST (FUNNAM,IERR)
      IF (IERR.EQ.0) CALL FUN010
      GO TO 210
C
C  RRS FUNCTION
140   CALL HCKLST (FUNNAM,IERR)
      IF (IERR.EQ.0) CALL FUN011
      GO TO 210
C
C  FMAP FUNCTION
150   CALL HCKLST (FUNNAM,IERR)
      IF (IERR.EQ.0) CALL FUN012
      GO TO 210
C
C  SETDEBUG FUNCTION
160   CALL FUN013
      GO TO 210
C
C  MARO FUNCTION
170   CALL HCKLST (FUNNAM,IERR)
      IF (IERR.EQ.0) CALL FUN014
      GO TO 210
C
C  MAPX FUNCTION
180   CALL HCKLST (FUNNAM,IERR)
      IF (IERR.EQ.0) CALL FUN015
      GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  HCL LOCAL FUNCTIONS
C
190   IGOTO=IHCFUN-500+1
      GO TO (200),IGOTO
      GO TO 10
C
200   CONTINUE
CCC      CALL FUN500
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
210   CALL FSTWHR (RTNOLD,IOLDOP,RTNOLD,IOLDOP)
C

C  Change made by Hank Herr (2004-06-15).  
C  Now, unlock the files before returning to the calling routine.
      CALL HFCSTUNLOCK(IHCFUN,KOND)
      IF (KOND > 0) THEN
          STOP 16
      ENDIF
      
      RETURN
C
      END
