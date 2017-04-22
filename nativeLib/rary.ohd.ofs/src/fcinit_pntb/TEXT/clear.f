C MODULE CLEAR
C-----------------------------------------------------------------------
C
      SUBROUTINE CLEAR (TSID,TYPE,IDT,LOCD,LOCTS,TC,LEFT,IUSET,
     *   NXT,TS,MTS)
C
C  THIS SUBROUTINE INSERTS CLEAR-TS OPERATION ENTRIES INTO THE TS ARRAY.
C
C  WRITTEN BY ERIC ANDERSON - HRL - 9/1979
C
      DIMENSION TS(MTS)
      INTEGER TC(*)
      DIMENSION TSID(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
      COMMON /CLROPS/ NUMCLR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/clear.f,v $
     . $',                                                             '
     .$Id: clear.f,v 1.3 2001/06/13 09:34:16 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER CLEAR'
C
      IF (IDT.GT.0) THEN
         WRITE (IPR,904) TSID,TYPE,IDT
  904 FORMAT ('0**NOTE** A CLEAR-TS OPERATION AUTOMATICALLY INSERTED ',
     * 'FOR TIME SERIES ID=',2A4,' TYPE=',A4,' IDT=',I2,' HOURS.')
         NUMCLR=NUMCLR+1
         GO TO 100
         ENDIF
C
      IDT=-IDT
C
  100 IBUG=0
      IF (IDBALL.GT.0) IBUG=1
      IF (NDEBUG.GT.0) THEN
         DO 10 I=1,NDEBUG
            IF (IDEBUG(I).EQ.4) GO TO 11
   10       CONTINUE
         ENDIF
      GO TO 101
   11 IBUG=1
C
C  CHECK IF 5 SPACES ARE AVAILABLE IN T()
  101 IF (LEFT.GE.5) GO TO 105
         WRITE (IPR,905)
  905 FORMAT ('0**ERROR** THIS OPERATION NEEDS MORE SPACE THAN IS ',
     * 'AVAILABLE IN THE T ARRAY.')
         CALL ERROR
         IUSET=0
         GO TO 199
C
C  SPACE IS AVAILABLE --MAKE ENTRIES INTO TC()
  105 LEFT=LEFT-5
      TC(1)=4
      TC(2)=NXT+5
      TC(3)=LOCTS
      TC(4)=LOCD
      NPDT=TS(LOCTS+6)
      LENGTH=NDD*(24/IDT)*NPDT
      TC(5)=LENGTH
      IUSET=5
C
C  MAKE SURE FLAG IN TS() IS SET TO INDICATE THAT THE TIME SERIES HAS
C  BEEN ASSIGNED VALUES
      TS(LOCTS+8)=1.01
C
      IF (IBUG.EQ.1) THEN
         WRITE (IODBUG,906) (TC(I),I=1,5)
  906 FORMAT (' IN CLEAR - CONTENTS OF TC=',5I6)
         ENDIF
C
C  TC ENTRIES FOR THE CLEAR OPERATION ARE:
C     POSITION   CONTENTS
C     --------   --------
C        1       ID NUMBER FOR THE OPERATION=3
C        2       LOCATION OF THE NEXT OPERATION IN THE T ARRAY
C        3       LOCATION OF THE TIME SERIES INFORMATION IN THE TS ARRAY
C        4       LOCATION OF THE TIME SERIES DATA IN THE D ARRAY
C        5       LENGTH OF THE TIME SERIES IN THE D ARRAY
C
  199 IF (ITRACE.GE.1) WRITE(IODBUG,*) 'EXIT CLEAR'
C
      RETURN
C
      END
