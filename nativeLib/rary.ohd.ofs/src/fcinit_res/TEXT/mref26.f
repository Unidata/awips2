C MEMBER MREF26
C  (from old member FCMREF26)
C
C DESC CHECK ON REFERENCE TO A POTENTIALLY MULTIPLY DEFINED CURVE
C---------------------------------------------------------------------
      SUBROUTINE MREF26(MNUM,REFCDE,LOCWK,LOCPO,IERM)
C---------------------------------------------------------------------
C  ROUTINE TO CHECK ON VALID REFERENCE FOR MULTIPLY DEFINED PARMS
C  BY CHECKING ON VALID NAME, AND PREDEFINED PARM FOR REFERENCE.
C---------------------------------------------------------------------
C
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
      INCLUDE 'common/mult26'
      INCLUDE 'common/suid26'
C
      DIMENSION TNAME(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/mref26.f,v $
     . $',                                                             '
     .$Id: mref26.f,v 1.1 1995/09/17 18:51:53 dws Exp $
     . $' /
C    ===================================================================
C
C
      IERM = 1
C
C  SEE IF ID IS VALID
C
      CALL IDWP26(TNAME,3,INAME,ILEVEL,IERID)
      IF (IERID.EQ.0) GO TO 20
      IF (IERID.EQ.1) CALL STER26(7,1)
      GO TO 9999
C
C  SEE IF PARAMETER HAS BEEN DEFINED AT THE REFERENCE LEVEL
C
  20  CONTINUE
      IF (NMDEF(MNUM).GT.0) GO TO 30
C
      CALL STRN26(66,1,MULTNM(1,MNUM),3)
      GO TO 9999
C
  30  CONTINUE
      NTIMES = NMDEF(MNUM)
      REFCDE = SUCODE(INAME) + ILEVEL + 0.01
      DO 40 I=1,NTIMES
      INUM = I
      IF (ABS(REFCDE-DMCODE(I,MNUM)).LT.0.1) GO TO 50
  40  CONTINUE
C
C  REFERENCE NEVER DEFINED
C
      CALL STRN26(66,1,MULTNM(1,MNUM),3)
      GO TO 9999
C
C  EVERYTHING IS OK
C
  50  CONTINUE
      LOCPO = IPOWD(INUM,MNUM)
      LOCWK = IWKWD(INUM,MNUM)
      IERM = 0
C
 9999 CONTINUE
      RETURN
      END
