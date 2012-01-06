C MEMBER XS2626
C
      SUBROUTINE XS2626(SUNUM,PO,W,D,LOCWS,IDPT)
C
C----------------------------------------------------------------------
C  SUBROUTINE TO EXTRACT ALL INFORMATION FROM PO ARRAY AND DATA FROM D
C  ARRAY AND PASS THESE TO THE ACTUAL COMPUTATION ROUTINE FOR THE SETDQ
C  SCHEME, PRDQ26.
C----------------------------------------------------------------------
C  WRITTEN BY - KUANG HSU - HRL NOVEMBER 1998
C----------------------------------------------------------------------
C
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/exg26'
      INCLUDE 'common/pres26'
C
      DIMENSION PO(1),W(1),D(1),LOCWS(1),IDPT(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xs2626.f,v $
     . $',                                                             '
     .$Id: xs2626.f,v 1.2 2003/08/13 08:21:07 hsu Exp $
     . $' /
C    ===================================================================
C
      DATA DL3T/4HL3/T/
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XS2626 ***')
C
C  GET POINTER INFO FOR THIS SCHEME
C
      CALL XPTR26(SUNUM,PO,IORD,IBASE,LEVEL,LOCPM,LOCTS,LOCCO)
C
C  SET EXECUTION POINTER FOR THIS SCHEME
C
      LOCEX = IORD*3
      W(LOCEX) = 1.01
C
C  NOW SET THE OUTFLOW VALUE AND ITS TYPE
C
      QOX=QO1
      QOMX=QOM
      ITBL = PO(LOCPM)
      ITBTYP = PO(LOCPM+1)
      IQO = PO(LOCPM+2)
C
C  DISCHARGE CHANGE RATE IS SET BY USER AND IS STORED IN PO ARRAY
C  GET THE TYPE OF DATA FOR PROPER ASSIGNMENT
C
      IF (ITBL.LE.0) THEN
         DQO = PO(LOCPM+1)/NTIM24
         GO TO 200
      ENDIF
C
C---------------------------------------------
C  DISCHARGE CHANGE RATE IS TO COME FROM TABLE
C
      QHX=QO1
      IF(ITBTYP.EQ.2 .AND. QOM.GE.0.0) QHX=QOM
      IF(ITBTYP.EQ.3) QHX=ELEV1
      LOCPM=LOCPM+3
C
      NQH=PO(LOCPM)
      LOCPM=LOCPM+1
      CALL TERP26(QHX,DQDAY,PO(LOCPM),PO(LOCPM+NQH),NQH,IFLAG,IBUG)
C
      DQO=DQDAY/NTIM24
 200  CONTINUE
      IF (IQO.EQ.1) THEN
         QO2 = QO1+DQO
         IF(QO2.LE.0.0) QO2=0.0
         QOM = 0.5*(QO1+QO2)
      ELSE
         IF(QOMX.GE.0.0) THEN
            QOM=QOMX+DQO
            IF(QOM.LE.0.0) QOM=0.0
            QO2=2.0*QOM-QO1
         ELSE
            QOM=QO1+DQO
            IF(QOM.LE.0.0) QOM=0.0
            QO2 =QOM
         ENDIF
      ENDIF
C
C--------------------------------------------------------------
C  COMPUTE OUTPUT FOR THIS SCHEME
C
      CALL PRDQ26(PO(LESSTO),PO(LESELV))
C
C----------------------------------------------
C  ALL DONE WITH 'SETDQ'
C
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XS2626 ***')
      RETURN
      END
