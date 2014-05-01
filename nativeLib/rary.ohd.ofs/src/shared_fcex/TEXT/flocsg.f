C MEMBER FLOCSG
C  (from old member FCFLOCSG)
C
C DESC 'LOCATES SEGMENT ON FILE FCSEGSTS'
C.......................................................................
      SUBROUTINE FLOCSG(INID,IROUT)
C
C  RETURNS IROUT = RECORD NUMBER ON FILE FCSEGSTS OF SEGMENT INID
C
C  ROUTINE ORIGINALLY WRITTEN BY --
C    ED JOHNSON -- HRL -- 5 NOV 1979
C.......................................................................
      DIMENSION INID(2),ITEMP(3)
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fsglst'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/flocsg.f,v $
     . $',                                                             '
     .$Id: flocsg.f,v 1.2 1999/01/19 20:21:25 page Exp $
     . $' /
C    ===================================================================
C
C
C  TRACE LEVEL = 2
C
      IF(ITRACE.GE.2)WRITE(IODBUG,900)
 900  FORMAT(19H *** FLOCSG ENTERED)
C
C SEARCH COMMON BLOCK FSGLST FIRST(IF DEFINED)
C
      IROUT=0
      IF(NS.EQ.0)RETURN
      IF(NLIST.LE.0)GO TO 200
      DO 100 I=1,NLIST
      IF(INID(1).NE.IDZ(1,I))GO TO 100
      IF(INID(2).NE.IDZ(2,I))GO TO 100
      IROUT=IDZ(3,I)
      RETURN
 100  CONTINUE
      IF(NLIST.EQ.NS)RETURN
C
C MUST SEARCH FILE FCSEGPTR DIRECTLY
C       REDEFINE COMMON BLOCKS FCSEGP AND FSGLST AT THE SAME TIME
C       USING VALUES DERECTLY FROM FILE.
C
 200  CALL UREADT(KFSGPT,1,NS,ISTAT)
      CALL UREADT(KFSGPT,2,NRP,ISTAT)
C
C  FIRST READ PART OF FILE THAT FITS IN COMMON BLOCK FSGLST
C
      NLIST=NS
      IF(NLIST.GT.MLIST)NLIST=MLIST
      DO 210 I=1,NLIST
      J=I+2
      CALL UREADT(KFSGPT,J,IDZ(1,I),ISTAT)
      IF(INID(1).NE.IDZ(1,I))GO TO 210
      IF(INID(2).NE.IDZ(2,I)) GO TO 210
      IROUT=IDZ(3,I)
 210  CONTINUE
C
C  HAVE NOW FILLED BOTH COMMON BLOCKS, IF SEGMENT FOUND, JUST RETURN
C
      IF(IROUT.NE.0)GO TO 1000
      IF(NLIST.GE.NS)GO TO 1000
      I2=NLIST+1
      DO 220 I=I2,NS
      J=I+2
      CALL UREADT(KFSGPT,J,ITEMP,ISTAT)
      IF(INID(1).NE.ITEMP(1))GO TO 220
      IF(INID(2).NE.ITEMP(2))GO TO 220
      IROUT=ITEMP(3)
      GO TO 1000
 220  CONTINUE
C
C  REWIND FILE AND RETURN
C
 1000 RETURN
      END
