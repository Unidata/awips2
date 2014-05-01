C MEMBER XOLIST
C  (from old member PPXOLIST
C
      SUBROUTINE XOLIST(ARRAY,NX,MX,IERR)
C.......................................
C     THIS SUBROUTINE READS THE ALPHABETICAL ORDER LISTS FOR
C      THE MAP FUNCTION.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON, HRL -- APRIL 1984
C.......................................
      DIMENSION ARRAY(1),SNAME(2),BLNKID(2),AID(2)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/IONUM/IN,IPR,IPU
      COMMON/WHERE/IA(2),IND,SUB(2)
      COMMON/XALPHA/IOP24,NOP24,LOP24,IOPVR,NOPVR,LOPVR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xolist.f,v $
     . $',                                                             '
     .$Id: xolist.f,v 1.1 1995/09/17 19:00:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HXOLI,4HST  /
      DATA OP24,OPVR/4HOP24,4HOPVR/
      DATA XORD/4HXORD/
      DATA BLNKID/4H    ,4H    /
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.1) WRITE(IOPDBG,900)
 900  FORMAT(1H0,17H** XOLIST ENTERED)
C.......................................
C     FILL WHERE COMMON BLOCK
      SUB(1)=SNAME(1)
      SUB(2)=SNAME(2)
C.......................................
C     CHECK IF DEBUG ON.
      IBUG=0
      IF(IPBUG(XORD).EQ.1) IBUG=1
C.......................................
C     INITIAL VALUES
      IOP24=0
      NOP24=0
      LOP24=0
      IOPVR=0
      NOPVR=0
      LOPVR=0
      IERR=0
C.......................................
C     READ OP24 PARAMETRIC RECORD.
      TYPE=OP24
      AID(1)=BLNKID(1)
      AID(2)=BLNKID(2)
      IPTR=0
      K=MX-NX+1
      CALL RPPREC(AID,TYPE,IPTR,K,ARRAY,NFILL,INX,ISTAT)
      IF(ISTAT.EQ.0) GO TO 106
      IF ((ISTAT.EQ.2).OR.(ISTAT.EQ.4)) GO TO 105
      CALL PSTRDC(ISTAT,TYPE,AID,IPTR,K,NFILL)
      WRITE(IPR,901)
 901  FORMAT(1H0,40H**FATAL ERROR** THE ABOVE ERROR IS FATAL)
      CALL KILLFN(8HMAP     )
      IERR=1
      GO TO 199
 105  WRITE(IPR,902) TYPE
 902  FORMAT(1H0,35H**WARNING** ALPHABETICAL ORDER LIST,1X,
     1A4,1X,35HDOES NOT EXIST FOR THE MAP FUNCTION,/6X,
     271HSTATIONS WILL BE PROCESSED AND DISPLAYED IN THE ORDER THEY WERE
     3 DEFINED)
      CALL WARN
      NUM=0
      GO TO 109
 106  IOP24=1
      NOP24=ARRAY(5)
      LOP24=NX
      NUM=((NOP24-1)/2)+1
      DO 107 I=1,NUM
 107  ARRAY(I)=ARRAY(5+I)
      NX=NX+NUM
 109  LOC=NUM+1
      IF(IBUG.EQ.0) GO TO 110
      WRITE(IOPDBG,903)TYPE,IOP24,NOP24,LOP24,NUM,NX,LOC
 903  FORMAT(1H0,14HXOLIST DEBUG--,A4,6I10)
      IF(NUM.EQ.0) GO TO 110
      CALL XCHAR(ARRAY,NOP24)
C.......................................
C     READ OPVR PARAMETRIC RECORD.
 110  TYPE=OPVR
      AID(1)=BLNKID(1)
      AID(2)=BLNKID(2)
      IPTR=0
      K=MX-NX+1
      CALL RPPREC(AID,TYPE,IPTR,K,ARRAY(LOC),NFILL,INX,ISTAT)
      IF(ISTAT.EQ.0) GO TO 116
      IF ((ISTAT.EQ.2).OR.(ISTAT.EQ.4)) GO TO 115
      CALL PSTRDC(ISTAT,TYPE,AID,IPTR,K,NFILL)
      WRITE(IPR,901)
      CALL KILLFN(8HMAP     )
      IERR=1
      GO TO 199
 115  WRITE(IPR,902)TYPE
      CALL WARN
      NUM=0
      GO TO 119
 116  IOPVR=1
      NOPVR=ARRAY(LOC+4)
      LOPVR=NX
      NUM=((NOPVR-1)/2)+1
      J=LOC-1
      DO 117 I=1,NUM
 117  ARRAY(J+I)=ARRAY(J+5+I)
      NX=NX+NUM
 119  IF(IBUG.EQ.0) GO TO 199
      WRITE(IOPDBG,903)TYPE,IOPVR,NOPVR,LOPVR,NUM,NX
      IF(NUM.EQ.0) GO TO 199
      CALL XCHAR(ARRAY(LOC),NOPVR)
C.......................................
C     EXIT SUBROUTINE
 199  IF(IPTRCE.GE.1) WRITE(IOPDBG,904)
 904  FORMAT(1H0,14H** EXIT XOLIST)
      RETURN
      END
