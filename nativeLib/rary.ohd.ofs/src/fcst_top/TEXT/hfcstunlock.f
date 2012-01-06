C  =====================================================================
C  pgm: HFCSTUNLOCK .. Free the OFS locks necessary for the fcst 
C  pgm:                function given by the passed in fun number. 
C
C  use:     CALL HFCSTUNLOCK(FUNNUM,KOND)
C
C   in: FUNNUM ........ The number of the function. This is the same
C   in:                 as the IHCFUN variable in hcompt.f.
C  out: KOND .......... condition return of HUNLOCKFILES call - INT
C  out:                   = 0 ... lock files opened
C  out:                   = 1 ... could not open all lock files after
C  out:                           a max number of passes.
C  out:                   = 2 ... invalid arguments passed in.  Check
C  out:                           the number of types and ids... they
C  out:                           must be identical.
C
C  rqd: 
C  =====================================================================
      SUBROUTINE HFCSTUNLOCK(FUNNUM,KOND)
     
      INTEGER  FUNNUM
      INTEGER  KOND
      CHARACTER*8 TECHNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/hfcstunlock.f,v $
     . $',                                                             '
     .$Id: hfcstunlock.f,v 1.4 2004/08/11 13:08:49 hank Exp $
     . $' /
C    ===================================================================
C

C  The values of IHCFUN (passed in as FUNNUM herein) are as follows:
C
C  1 -- fcexec; 2 -- savedate; 3 -- freedate; 4 -- esp; 5 -- cpucheck;
C  6 -- cgstatus; 7 -- printops; 8 -- mat; 9 -- map; 10 -- map3; 
C  11 -- rrs; 12 -- fmap; 13 -- setdebug; 14 -- maro; 15 -- mapx;
C
C  If any of these numbers change, this routine must be edited!!!

30    GO TO (40,50,50,60,50,
     *       50,50,70,70,70,
     *       70,70,50,80,70),FUNNUM
     
C
C  FCEXEC FUNCTION
40    IASSIM = 0
      IFFG = 0
      
C     Load the ffg and assim techniques, if they are defined.
      TECHNAME='FFG'
      CALL HPAST (TECHNAME,IFFG,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
      TECHNAME='ASSIM'
      CALL HPAST (TECHNAME,IASSIM,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)

C     Free the appropriate locks based on ffg and assim.      
      IF (IASSIM.NE.0.OR.IFFG.NE.0) THEN
          CALL HUNLOCKFILES('FCEXEC_FA',KOND)
      ELSE
          CALL HUNLOCKFILES('FCEXEC_NOFA',KOND)
      ENDIF
      
C     Check for errors. 
      IF (KOND.GT.0) THEN
          STOP 16
      ENDIF
      GO TO 210
C
C  SAVEDATE, FREEDATE, CPUCHECK, CGSTATUS, PRINTOPS 
C  SETDEBUG, FUNCTIONS
50    CALL HUNLOCKFILES('NONFCST',KOND)
      IF (KOND.GT.0) THEN
          STOP 16
      ENDIF
      GO TO 210
C
C  ESP FUNCTION
60    ISKIPBLND = 0
      
C     Load the ffg and assim techniques, if they are defined.
C     Note that if the technique is not found, HPAST will 
C     return an ISKIPBLND value of 0 (it initializes it to 0
C     and then does not change it before returning).  
      TECHNAME='SKIPBLND'
      CALL HPAST (TECHNAME,ISKIPBLND,ISTAT)
      IF (ISTAT.GT.0) THEN
          IF (ISTAT.NE.2) THEN
              CALL FPHPWN (ISTAT,TECHNAME)
          ENDIF
      ENDIF

C     Request the appropriate locks based on ffg and assim.      
      IF (ISKIPBLND.EQ.1) THEN
          CALL HUNLOCKFILES('ESP_NOBLEND',KOND)
      ELSE
          CALL HUNLOCKFILES('ESP_BLEND',KOND)
      ENDIF
      
C     Check for errors. 
      IF (KOND.GT.0) THEN
          STOP 16
      ENDIF
      
      GO TO 210
C
C  MAT, MAP, MAPE, MAPX, RRS FUNCTION
70    CALL HUNLOCKFILES('PREPROC',KOND)
      IF (KOND.GT.0) THEN
          STOP 16
      ENDIF
      GO TO 210
C
C  MARO FUNCTION -- NOT SURE ABOUT THIS LOCK!!!
80    CALL HUNLOCKFILES('GENERAL',KOND)
      IF (KOND.GT.0) THEN
          STOP 16
      ENDIF
      GO TO 210

210   RETURN
      END
