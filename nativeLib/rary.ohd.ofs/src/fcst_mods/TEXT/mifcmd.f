C MEMBER MIFCMD
C  (from old member MIFCMD)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 07/07/95.11:30:54 BY $WC21DT
C
C @PROCESS LVL(77)
C
      INTEGER FUNCTION MIFCMD(IFIELD,LEN,NFLD)
C
C     THIS FUNCTION CHECKS A FIELD TO SEE IF IT IS A MOD COMMAND
C     FUNCTION VALUE IS ZERO IF NOT A COMMAND
C     AND ONE THROUGH NUMCMD IF IT IS
C
      CHARACTER*8 FIELD,BLANK8,CMDNAM,DOT
C
      INCLUDE 'common/fdbug'
      COMMON/MFG/ISFG,NDTS
C
      DIMENSION IFIELD(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/mifcmd.f,v $
     . $',                                                             '
     .$Id: mifcmd.f,v 1.2 1995/11/14 21:24:09 erb Exp $
     . $' /
C    ===================================================================
C
C
      DATA BLANK8 /'        '/,DOT/'.       '/
C
      IBUG=IFBUG(4HMODS)
      IF(IBUG.EQ.1)WRITE(IODBUG,900)IFIELD,LEN,NFLD
  900 FORMAT(1H0,10X,'** ENTERING MIFCMD **'/11X,'IFIELD=',3A4,
     1 ', LEN=',I3,', NFLD=',I3)
C
      MIFCMD=0
      ISFG=0
      NDTS=0
C
C     IS FIELD LONG ENOUGH?
C
      IF(LEN.LT.2)GO TO 30
C
C     IS FIELD TOO LONG?
C
      IF(LEN.GT.9)GO TO 30
C
C     DOES FIELD START WITH A PERIOD?
C
      FIELD=BLANK8
      CALL SUBSTR(IFIELD,1,1,FIELD,1)
C
      IF(FIELD.NE.DOT)GO TO 30
C
C     FIELD STARTS WITH A PERIOD - MOVE REST OF FIELD
C     INTO VARIABLE FIELD AND CHECK AGAINST LIST OF COMMANDS
C
      NUM=LEN-1
      FIELD=BLANK8
      CALL SUBSTR(IFIELD,2,NUM,FIELD,1)
C
      IF(IBUG.EQ.1)WRITE(IODBUG,901)NUM,FIELD
  901 FORMAT(1H0,10X,'AFTER CALL TO SUBSTR, NUM=',I3,', FIELD=',A8)
C
      ICMND=-1
      CALL MCOMND(ICMND,FIELD,ISFG,NDTS)
C
      MIFCMD=ICMND
C
   30 IF(IBUG.EQ.1)WRITE(IODBUG,902)MIFCMD,ISFG,NDTS
  902 FORMAT(1H0,10X,'** LEAVING MIFCMD ** MIFCMD=',I3,
     1 ', ISFG=',I3,', NDTS=',I3)
C
      RETURN
      END
