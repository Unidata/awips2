C MODULE JULDA
C-----------------------------------------------------------------------
C  ROUTINE JULDA CONVERTS FROM MONTH, DAY, YEAR, HOUR FOR A SPECIFIED
C  TIME ZONE TO INTERNAL CLOCK TIME
C          (JULIAN DAY RELATIVE TO JAN 1, 1900)
C-----------------------------------------------------------------------
      SUBROUTINE JULDA (JDAY,INTHR,M,D,Y,H,ITZ,IDSAV,CODE)

      EXTERNAL    DDYCDL,DDGCDM,DDGCD2,WARN

      INTEGER D,Y,H,CODE

      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/julda.f,v $
     . $',                                                             '
     .$Id: julda.f,v 1.1 1998/07/02 20:04:15 page Exp $
     . $' /
C    ===================================================================
C

C  J1 IS JULIAN DAY OF DEC 31,1899
      DATA INTL / 4hINTL /

      IF (ITRACE.GT.2) WRITE (IODBUG,*) ' **JULDA ENTERED'

C  CAN ONLY CONVERT TO INTERNAL TIME WHEN NLSTZ IS DEFINED
      IF ( (NLSTZ.LT.-12 .OR.  NLSTZ.GT.12)  .AND.
     *     (ITZ.GE.-12   .AND. ITZ.LE.12  )        ) THEN
        WRITE (IPR,40) CODE
40      FORMAT (1H0,10X,'**WARNING** JULDA UNABLE TO CONVERT ',
     *    'FROM INTERMAL TIME TO REQUESTED TIME ZONE ',A4,' BECAUSE' /
     *    11X,'VARIABLE NLSTZ IN COMMON BLOCK FCTIME IS OUTSIDE ',
     *    'THE RANGE -12 TO 12.')
        CALL WARN()
        ITZ=100
        IDSAV=0
        CODE=INTL
      ENDIF

C  REPLACE ARGUMENTS M,D,Y,H WITH IM,ID,IY, AND IH
      IY=Y
      IM=M
      ID=D
      IH=H

      IF (IM.LT.1)  IM=1
      IF (IM.GT.12) IM=12
      IF (ID.LT.1)  ID=1

C         Make sure the year is four digits using the 90/10 year rule
C         Get number of days in month, NODIM

            CALL DDYCDL(IY,IM,ID)
            CALL DDGCDM(IY,IM,NODIM)

      IF (ID.GT.NODIM) ID=NODIM
      IF (IH.GT.24) IH=24
      IF (IH.LT.0)  IH=0

C  COMPUTE JULIAN DAY
      CALL DDGCD2(JDAY,IY,IM,ID)

C  CONVERT IH TO INTERNAL TIME

C  IH IS IN TIME ZONE ITZ
C  INTERNAL CLOCK IS IN TIME ZONE (NLSTZ-LOCAL)
C  TIME ZONE DIFFERENCE BETWEEN THEM IS (NLSTZ-LOCAL)-ITZ
C  THEREFORE INTHR=IH+TIME ZONE DIFFERENCE
C                 =IH+NLSTZ-LOCAL-ITZ
C  FOR EXAMPLE, PROCESSED DATA FILE HOUR 1 IS 13Z
C      IN EST TIME ZONE, NLSTZ=-5
C      AND 8 AM EST IS HOUR 1 OF THE INTERNAL CLOCK
C      SO LOCAL=7
C      (NLSTZ-LOCAL)=-12 FOR THIS CASE WHICH IS THE TIME
C                        ZONE NUMBER OF THE TIME ZONE WHERE 13Z
C                        IS 1 O'CLOCK AM

      INTHR=IH
      IF (ITZ.GE.-12 .AND. ITZ.LE.12) THEN
        INTHR=IH+NLSTZ-LOCAL-ITZ

C  DAYLIGHT SAVINGS TIME CORRECTION
        IF (IDSAV.EQ.1) INTHR=INTHR-1
      ENDIF

C  DAY CORRECTION TO PUT INTHR IN THE RANGE 1-24
      NDOFF=(INTHR-24)/24
      IF (INTHR.GT.0) NDOFF=INTHR/24
      IF (NDOFF.GT.0.AND.MOD(INTHR,24).EQ.0) NDOFF=NDOFF-1
      JDAY=JDAY+NDOFF
      INTHR=INTHR-NDOFF*24

C  CHECK IF ARGUMENTS WERE OUT OF RANGE AND RETURN
      IF (M.EQ.IM.AND.D.EQ.ID.AND.Y.EQ.IY.AND.H.EQ.IH) GO TO 80
      IF (M.EQ.IM.AND.D.EQ.ID.AND.IY-Y.EQ.1900.AND.H.EQ.IH) GO TO 80
      IF (M.EQ.IM.AND.D.EQ.ID.AND.IY-Y.EQ.2000.AND.H.EQ.IH) GO TO 80
        WRITE (IPR,70) M,D,Y,H,IM,ID,IY,IH
70      FORMAT (1H0,10X,'**WARNING** JULDA CALLED WITH ',
     *    'ARGUMENTS OUT OF RANGE WERE RESET TO INDICATED VALUES.' /
     *    1H ,20X,5X,5HMONTH,7X,3HDAY,6X,4HYEAR,6X,4HHOUR /
     *    1H ,11X,9HAS CALLED,4I10/
     *    1H ,11X,9HRESET TO ,4I10/)
        CALL WARN()

80    IF (ITRACE.GT.2) WRITE (IODBUG,*) ' **EXIT JULDA'

      RETURN
      END
