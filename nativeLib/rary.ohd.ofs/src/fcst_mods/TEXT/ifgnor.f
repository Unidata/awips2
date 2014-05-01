C MEMBER IFGNOR
C  (from old member FCIFGNOR)
C
C  DESC - FUNCTION TO DETERMINE IF 'IGNORETS' MOD IS IN EFFECT FOR
C  DESC - A TYPE TS AND A PARTICULAR DATE.
C----------------------------------------------------------------
C
      FUNCTION IFGNOR(ITYPE,JULHR,ISUTYP)
C
C----------------------------------------------------------------
C  THIS FUNCTION CHECKS IN /FIGNOR/ TO SEE IF
C   1) THE APPROPRIATE TIME-SERIES IS TO BE IGNORED,
C   2) THE DATE PASSED (JULIAN HR) IS WITHIN THE PERIOD THAT THE
C      MOD IS IN EFFECT
C   3) (FOR FUTURE USE) FOR THE RES-SNGL OPERATION, THE SCHEME/
C      UTILITY NO. PASSED IS ONE THAT THE MOD IS IN EFFECT FOR.
C----------------------------------------------------------------
C
      COMMON/FIGNOR/NIGNOR,IGNOPT(10),IGSDAT(10),IGEDAT(10)
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/ifgnor.f,v $
     . $',                                                             '
     .$Id: ifgnor.f,v 1.1 1995/09/17 19:03:19 dws Exp $
     . $' /
C    ===================================================================
C
C
C----------------------------------------------------------------
C
      IBUG = IFBUG(4HIGNO)
      IF (IBUG.GE.1) WRITE(IODBUG,1610) NIGNOR,(IGNOPT(K),IGSDAT(K),
     .      IGEDAT(K),K=1,NIGNOR)
 1610 FORMAT('  NIGNOR = ',I5/(' OPTION, START, END =',3I10/))
      IFGNOR = 0
      IF (NIGNOR .EQ. 0) GO TO 999
C
C  IF THE TYPE OF TIME-SERIES TO CHECK HAS A VALUE OF ZERO, DON'T
C   MAKE ANY CHECKS
C
      IF (ITYPE .EQ. 0) GO TO 999
C
C  LOOP THROUGH ALL DEFINED USES OF THE MOD, LOOKING FOR A COMBINATION
C  OF TIME-SERIES THAT INCLUDES THE TYPE PASSED TO THE FUNCTION AND
C  SEEING IF THE DATE PASSED IS WITHIN THE RANGE OF DATES THAT EACH
C  MOD DEFINITION IS IN EFFECT.
C
      DO 100 I=1,NIGNOR
      IF (MOD(IGNOPT(I),2**(1+ITYPE/2)) .LT. ITYPE) GO TO 100
      IF (JULHR.LT.IGSDAT(I) .OR. JULHR.GT.IGEDAT(I)) GO TO 100
C
C  ALL CHECKS ARE SUCCESSFUL. MOD IS IN EFFECT
C
      IFGNOR = 1
      GO TO 999
C
  100 CONTINUE
C
  999 CONTINUE
      IF (IBUG.GE.1) WRITE(IODBUG,1690) ITYPE,JULHR,ISUTYP,IFGNOR
 1690 FORMAT(' EXIT FROM IFGNOR - TYPE,JULHR,SUTYPE AND FUNC VALUE =',
     .   4I10)
      RETURN
      END
