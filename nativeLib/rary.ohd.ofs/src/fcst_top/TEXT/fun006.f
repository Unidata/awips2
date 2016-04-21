C MODULE FUN006
C-----------------------------------------------------------------------
C
C  THIS FUNCTION PRINTS THE CARRYOVER GROUP STATUS
C
C.......................................................................
C
C     FUNCTION ORIGINALLY WRITTEN BY
C        GEORGE F. SMITH - NOVEMBER 1982 - HRL
C.......................................................................
C
      SUBROUTINE FUN006
C
C     THREE TECHNIQUES FOR THIS FUNCTION
C
C      1. PRINT    - SET THE I/O UNIT NUMBER FOR PRINTOUT
C      2. NOUTZ    - SET THE OUTPUT TIME ZONE CODE
C      3. NOUTDS   - SET THE DAYLIGHT SAVINGS TIME SWITCH
C
      COMMON/HDFLTS/DX1(3),DFLTNM(2),DX2(16),LCLHCL,LTZHCL,DX3(2)
      INCLUDE 'common/fcrfc'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/where'
C
C     SET VALUES IN WHERE COMMON BLOCK
C
      DIMENSION SUBNAM(2),OLDOPN(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fun006.f,v $
     . $',                                                             '
     .$Id: fun006.f,v 1.2 2002/02/11 20:40:25 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SUBNAM/4HFUN0,4H06  /
C
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SUBNAM(I)
      IOLDOP=IOPNUM
      IOPNUM=0
C
      IOUNT=0
      IF (IOUNT.EQ.1) THEN
C
C  GET VALUE FOR IPR FROM TECHNIQUE 'PRINT'
      CALL HPAST (8HPRINT   ,IPR,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,8HPRINT   )
      CALL FTEKCK (IPR,8HPRINT   ,6,IPR,1,9)
C
      ENDIF
C
C  GET VALUE FOR NOUTZ FROM TECHNIQUE 'NOUTZ'
      CALL HPAST (8HNOUTZ   ,NOUTZ,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,8HNOUTZ   )
      CALL FTEKCK (NOUTZ,8HNOUTZ   ,-6,NOUTZ,-12,12)
C
C  GET VALUE FOR NOUTDS FROM TECHNIQUE 'NOUTDS'
      CALL HPAST (8HNOUTDS  ,NOUTDS,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,8HNOUTDS  )
      CALL FTEKCK (NOUTDS,8HNOUTDS  ,0,NOUTDS,0,1)
C
C     SET VALUES IN COMMON BLOCK FDBUG
C
      IODBUG=IPR
      ITRACE=0
      IDBALL=0
      NDEBUG=0
C
C     SET VALUES IN COMMON BLOCK SYSBUG
C
      NDEBGS=0
      IALL=0
C
C     SET VALUE OF NOW IN FCTIME COMMON BLOCK
C
      CALL FSETNW
C
C     SET VALUE OF RFCNAM IN FCRFC COMMON BLOCK
C
      RFCNAM(1)=DFLTNM(1)
      RFCNAM(2)=DFLTNM(2)
C
C     SET VALUES FOR LOCAL AND NLSTZ IN COMMON BLOCK FCTIME
C
      LOCAL=LCLHCL
      NLSTZ=LTZHCL
C.......................................................................
C
C   PRINT CARRYOVER GROUP STATUS
C
      CALL FSTACG
C.......................................................................
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
C
      END
