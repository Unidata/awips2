C MEMBER XS2726
C
      SUBROUTINE XS2726(SUNUM,PO,W,D,LOCWS,IDPT)
C
C----------------------------------------------------------------------
C  SUBROUTINE TO EXTRACT ALL INFORMATION FROM PO ARRAY AND DATA FROM D
C  ARRAY AND PASS THESE TO THE ACTUAL COMPUTATION ROUTINE FOR THE SETDH
C  SCHEME, PRDH26.
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xs2726.f,v $
     . $',                                                             '
     .$Id: xs2726.f,v 1.1 2000/07/20 12:35:25 page Exp $
     . $' /
C    ===================================================================
C
      DATA DL3T/4HL3/T/
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XS2726 ***')
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
C  NOW SET TABLE TYPE
C
      ITBL = PO(LOCPM)
      ITBTYP = PO(LOCPM+1)
C
C  POOL ELEV CHANGE RATE IS SET BY USER AND IS STORED IN PO ARRAY
C  GET THE TYPE OF DATA FOR PROPER ASSIGNMENT
C
      IF (ITBL.LE.0) THEN
         DHPRD = PO(LOCPM+1)/NTIM24
         GO TO 200
      ENDIF
C
C---------------------------------------------
C  POOL ELEV CHANGE RATE IS TO COME FROM TABLE
C
      QHX=ELEV1
      IF(ITBTYP.EQ.1) QHX=QO1
      IF(ITBTYP.EQ.2 .AND. QOM.GE.0.0) QHX=QOM
      LOCPM=LOCPM+2
C
      NQH=PO(LOCPM)
      LOCPM=LOCPM+1
      CALL TERP26(QHX,DHDAY,PO(LOCPM),PO(LOCPM+NQH),NQH,IFLAG,IBUG)
C
 200  DHPRD=DHDAY/NTIM24
      ELEV2=ELEV1+DHPRD
C
C--------------------------------------------------------------
C  COMPUTE OUTPUT FOR THIS SCHEME
C
      CALL PRDH26(PO(LESSTO),PO(LESELV))
C
C----------------------------------------------
C  ALL DONE WITH 'SETDH'
C
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XS2726 ***')
      RETURN
      END
