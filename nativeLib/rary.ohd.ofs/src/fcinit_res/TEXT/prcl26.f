C MEMBER PRCL26
C  (from old member FCSPRP26)
C---------------------------------------------------------------
C  SUBROUTINE TO OUTPUT A LINE OF RCL IF WE'VE FILLED THE ARRAY
C---------------------------------------------------------------
C
      SUBROUTINE PRCL26(IOUNIT,RCL,LORCL,LCOL,IBUG)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      DIMENSION RCL(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/prcl26.f,v $
     . $',                                                             '
     .$Id: prcl26.f,v 1.1 1995/09/17 18:52:28 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA BLANK/4H    /,AMP/4H   &/
C
C  FIRST CHECK. IF WE'RE NOT PAST THE SPECIFIED COLUMN, DON'T
C  OUTPUT THE RCL, JUST RETURN.
C
C
      IF (IBUG.GT.0) WRITE(IODBUG,1610) LORCL,LCOL,IOUNIT
 1610 FORMAT('  CURRENT COLUMN =',I3,' CRITERIA COLUMN =',I3,
     .  ' OUTPUT DEVICE NO. =',I3)
C
      IF (LORCL .LT. LCOL) GO TO 99
C
C  WE'RE BEYOND THE SPEC'D COLUMN, SO SET THE LAST CHARACTER AS A
C  CONTINUATION (THAT'S AN &), OUTPUT THE LINE TO THE PROPER UNIT,
C  SET THE LINE TO ALL BLANKS AND SET THE COLUMN POINTER TO ONE.
C
      RCL(18) = AMP
      IF (IOUNIT.EQ.IPR) WRITE(IOUNIT,900) RCL
      IF (IOUNIT.NE.IPR) WRITE(IOUNIT,901) (RCL(M),M=1,18)
C
      CALL UMEMST(BLANK,RCL,20)
C
      LORCL = 1
C
   99 CONTINUE
  900 FORMAT(1H ,10X,20A4)
  901 FORMAT(18A4)
      RETURN
      END
