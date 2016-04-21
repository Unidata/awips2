c  =====================================================================
c  pgm:  ror43 (len,rline,rndx,rnri2)
c
c  out: len    .... length of table
c  out: rline  .... precipitation curve lines
c  out: rndx   .... final runoff index
c  out: rnri2  .... runoff values at precip line inflection point
c  =====================================================================
      subroutine ror43 (len,rline,rndx,rnri2)
c.......................................................................
c  This routine initializes rainfall/runoff arrays for the Buffalo area
c  as used in the Northeast RFC rainfall/runoff relationship.

c     len -  ENDING POSITION OF TABLE
c.......................................................................
c  Initially written by
c     Tim Sweeney  HRL                              Feb 1995
c     Ken Mack   NERFC                               5/18/95
c.......................................................................
c
c      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
c      COMMON /IONUM/ IN,IPR,IPU
      include 'common/fdbug'
      include 'common/ionum'
c
      INTEGER*4 rline(13),rndx(15,13),rnri2(15,13)
      DIMENSION NLINE(13),INDX(15,10),IRI2(15,10)
c  INDICIES FOR THE GENESEE AREA
      DIMENSION I1(15),I2(15),I3(15),I4(15),I5(15),I6(15),I7(15),
     1I8(15),I9(15),I10(15)
      DIMENSION IR1(15),IR2(15),IR3(15),IR4(15),IR5(15),IR6(15),IR7(15),
     1IR8(15),IR9(15),IR10(15)
      DIMENSION SUBNAM(2)
c
      EQUIVALENCE
     1  (I1(1),INDX(1,1)),(I2(1),INDX(1,2)),(I3(1),INDX(1,3)),
     2  (I4(1),INDX(1,4)),(I5(1),INDX(1,5)),(I6(1),INDX(1,6)),
     3  (I7(1),INDX(1,7)),(I8(1),INDX(1,8)),(I9(1),INDX(1,9)),
     4  (I10(1),INDX(1,10))

      EQUIVALENCE
     1  (IR1(1),IRI2(1,1)),(IR2(1),IRI2(1,2)),(IR3(1),IRI2(1,3)),
     2  (IR4(1),IRI2(1,4)),(IR5(1),IRI2(1,5)),(IR6(1),IRI2(1,6)),
     3  (IR7(1),IRI2(1,7)),(IR8(1),IRI2(1,8)),(IR9(1),IRI2(1,9)),
     4  (IR10(1),IRI2(1,10))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/ror43.f,v $
     . $',                                                             '
     .$Id: ror43.f,v 1.1 1996/03/21 15:54:28 page Exp $
     . $' /
C    ===================================================================
C

      DATA NLINE / 0, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900,
     1             1000,1100 /

c  RI2 INDEX FOR THE GENESEE
      DATA I1/10 ,80 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I2/10 ,14 ,20 ,25 ,36 ,62 ,80 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I3/10 ,14 ,20 ,30 ,39 ,49 ,70 ,80 ,0 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I4/10 ,19 ,23 ,29 ,34 ,39 ,43 ,49 ,59 ,80 ,0 ,0 ,0 ,0 ,0 /
      DATA I5/10 ,23 ,29 ,34 ,37 ,40 ,46 ,55 ,67 ,80 ,0 ,0 ,0 ,0 ,0 /
      DATA I6/10 ,30 ,32 ,36 ,40 ,44 ,56 ,70 ,80 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I7/10 ,31 ,34 ,37 ,40 ,45 ,55 ,67 ,80 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I8/10 ,33 ,37 ,40 ,43 ,50 ,60 ,80 ,0 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I9/10 ,33 ,37 ,40 ,45 ,50 ,65 ,80 ,0 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I10/10 ,33 ,37 ,40 ,45 ,50 ,65 ,80 ,0 ,0 ,0 ,0 ,0 ,0 ,0 /
C RUNOFF FOR THE GENESEE
      DATA IR1/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA IR2/ 25, 16, 09, 06, 02, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA IR3/ 62, 48, 33, 17, 09, 05, 01, 01, 0, 0, 0, 0, 0,
     1          0, 0/
      DATA IR4/162,106, 85, 58, 40, 25, 18, 13, 08, 05, 0, 0,
     1          0, 0, 0/
      DATA IR5/262,155,110, 78, 65, 54, 41, 30, 20, 11, 0, 0,
     1          0, 0, 0/
      DATA IR6/362,164,145,118,100, 87, 61, 40, 25, 0, 0,
     1          0, 0, 0, 0/
      DATA IR7/462,236,207,182,164,143,111, 77, 43, 0, 0,
     1          0, 0, 0, 0/
      DATA IR8/562,311,275,251,233,200,158, 85, 0, 0, 0,
     1          0, 0, 0, 0/
      DATA IR9/662,411,372,345,313,287,215,150, 0, 0, 0,
     1          0, 0, 0, 0/
      DATA IR10/762,511,472,445,413,387,315,250, 0, 0, 0,
     1          0, 0, 0, 0/
c
      DATA SUBNAM /4hROR4,4h3   /,NOP/43/,IVERS/1/
      CALL FPRBUG(SUBNAM,2,NOP,IBUG)
      len = 10
c
 1000 do 1020 i = 1,len
      rline(i) = nline(i)
 1020 continue
c
      do 1030 i = 1,15
      do 1035 j = 1,len
      rndx(i,j) = indx(i,j)
      rnri2(i,j) = iri2(i,j)
 1035 continue
 1030 continue
c
      RETURN
      END
