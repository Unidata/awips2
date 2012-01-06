C MEMBER POINT
C  (from old member OARSCH)
C*********************************************************************
      SUBROUTINE POINT(N,XOPT,XN,ISEED,K,BL,BU,VAR)
C
C      DIMENSION BL(16),BU(16),XN(16),XOPT(16)
C      DIMENSION VAR(16,5)
C
      integer, parameter::MaxNopt=100
	  integer, parameter::MaxNpg=2*MaxNopt+1,MaxNpt=MaxNopt*MaxNpg
      DIMENSION BL(MaxNopt),BU(MaxNopt),XN(MaxNopt),XOPT(MaxNopt)
      DIMENSION VAR(MaxNopt,5)
C
      INCLUDE 'common/ionum'
      SAVE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/point.f,v $
     . $',                                                             '
     .$Id: point.f,v 1.4 2003/08/26 17:46:10 wkwock Exp $
     . $' /
C    ===================================================================
C
C
      DATA IBUG/4HARS /
C
      DO 50 I=1,N
        ZP=VAR(I,K)
        XMINVL=BL(I)
C
C       SEARCHING THE ENTIRE PARAMETER SPACE FOR THE FIRST RANGE
C
        IF(K .GT. 1) GO TO 10
        RANGE=ZP
        GO TO 40
C
   10   ZP1=ZP*0.5
C
C       CHECK IF POINT CENTERED PARAMETER SPACE FOR LATER RANGE IS BELOW
C       BL.  IF SO, RANGE IS ADJUSTED.
C
        XOPT1=XOPT(I)-ZP1
        IF(XOPT1 .GE. BL(I)) GO TO 20
        ZPA=XOPT(I)-BL(I)
        ZPB=ZP1-ZPA
        RANGE=ZP-ZPB
        GO TO 40
C
C       CHECK IF POINT CENTERED PARAMETER SPACE FOR LATER RANGE IS ABOVE
C       BU.  IF SO, RANGE IS ADJUSTED.
C
   20   XOPT2=XOPT(I)+ZP1
        IF(XOPT2 .LE. BU(I)) GO TO 30
        ZPA=BU(I)-XOPT(I)
        ZPB=ZP1-ZPA
        RANGE=ZP-ZPB
        XMINVL=XOPT(I)-ZP1
        GO TO 40
C
C       POINT CENTERED PARAMETER SPACE FOR LATER RANGE IS WITHIN THE
C       PARAMETER SPACE OF THE FIRST RANGE.  NO RANGE ADJUSTMENT IS MADE
C
   30   RANGE=ZP
        XMINVL=XOPT(I)-ZP1
C
   40   CALL RANDU(ISEED,IY,Z)
        ISEED=IY
        XNEW1=XMINVL+Z*RANGE
        XN(I)=XNEW1
   50 CONTINUE
C
C     DEBUG OUTPUT FOR SUBROUTINE POINT; CURRENT PARAMETER, LOWER AND
C     UPPER LIMITS.
C
      IF(IFBUG(IBUG) .NE. 1) GOTO 70
      WRITE(IPR,900) K
      WRITE(IPR,910)
      DO 60 I=1,N
        WRITE(IPR,920) XN(I),BL(I),BU(I)
   60 CONTINUE
  900 FORMAT(/,5X,'DEBUG OUTPUT FOR SUBROUTINE POINT; RANGE LEVEL=',I3)
  910 FORMAT(/,10X,'PARAMETER',10X,'LOWER BOUND',10X,'UPPER BOUND')
  920 FORMAT(11X,F7.3,14X,F7.3,15X,F7.3,/)
C
   70 RETURN
      END
