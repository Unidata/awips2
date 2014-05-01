c  =====================================================================
c  pgm:  ron43 (len,rline,rndx,rnri2)
c
c  out: len    .... length of table
c  out: rline  .... precipitation curve lines
c  out: rndx   .... final runoff index
c  out: rnri2  .... runoff values at precip line inflection point
c  =====================================================================
      subroutine ron43 (len,rline,rndx,rnri2)
c.......................................................................
c  This routine initializes rainfall/runoff arrays with values for the
c  New England, Eastern NY areas as used in the Northeast RFC
c  rainfall/runoff relationship.
c
c     len -  ENDING POSITION OF TABLE
c.......................................................................
c  Initially written by
c     Tim Sweeney    HRL                           Feb 1995
c.......................................................................
c
      INTEGER*4 rline(13),rndx(15,13),rnri2(15,13)

      DIMENSION NLINE(13),INDX(15,13),IRI2(15,13)
c  INDICIES FOR NEW ENGLAND, EASTERN NY AREA
      DIMENSION I1(15),I2(15),I3(15),I4(15),I5(15),I6(15),I7(15),
     1I8(15),I9(15),I10(15),I11(15),I12(15),I13(15)
      DIMENSION IR1(15),IR2(15),IR3(15),IR4(15),IR5(15),IR6(15),IR7(15),
     1IR8(15),IR9(15),IR10(15),IR11(15),IR12(15),IR13(15)
      DIMENSION SUBNAM(2)
c
c      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
c      COMMON /IONUM/ IN,IPR,IPU
c
      include 'common/fdbug'
      include 'common/ionum'
c
      EQUIVALENCE
     1  (I1(1),INDX(1,1)),(I2(1),INDX(1,2)),(I3(1),INDX(1,3)),
     2  (I4(1),INDX(1,4)),(I5(1),INDX(1,5)),(I6(1),INDX(1,6)),
     3  (I7(1),INDX(1,7)),(I8(1),INDX(1,8)),(I9(1),INDX(1,9)),
     4  (I10(1),INDX(1,10)),(I11(1),INDX(1,11)),(I12(1),INDX(1,12)),
     5  (I13(1),INDX(1,13))

      EQUIVALENCE
     1  (IR1(1),IRI2(1,1)),(IR2(1),IRI2(1,2)),(IR3(1),IRI2(1,3)),
     2  (IR4(1),IRI2(1,4)),(IR5(1),IRI2(1,5)),(IR6(1),IRI2(1,6)),
     3  (IR7(1),IRI2(1,7)),(IR8(1),IRI2(1,8)),(IR9(1),IRI2(1,9)),
     4  (IR10(1),IRI2(1,10)),(IR11(1),IRI2(1,11)),(IR12(1),IRI2(1,12)),
     5  (IR13(1),IRI2(1,13))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/ron43.f,v $
     . $',                                                             '
     .$Id: ron43.f,v 1.1 1996/03/21 15:54:12 page Exp $
     . $' /
C    ===================================================================
C

      DATA NLINE / 0, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900,
     1             1000,1100 /

c  RI2 INDEX FOR NEW ENGLAND, EASTERN NY AREA
      DATA I1 /10,80,0,0,0,0,0,0,0,0,0,0,0,0,0/
      DATA I2 /10,15,21,28,36,61,80,0,0,0,0,0,0,0,0/
      DATA I3 /10,15,22,29,37,54,74,80,0,0,0,0,0,0,0/
      DATA I4 /10,16,21,26,32,40,52,64,80,0,0,0,0,0,0/
      DATA I5 /10,15,20,25,30,36,44,56,71,80,0,0,0,0,0/
      DATA I6 /10,16,20,25,30,36,43,56,71,80,0,0,0,0,0/
      DATA I7 /10,17,22,27,31,37,43,52,67,80,0,0,0,0,0/
      DATA I8 /10,18,24,29,33,37,43,50,65,80,0,0,0,0,0/
      DATA I9 /10,20,25,30,33,37,43,51,60,70,80,0,0,0,0/
      DATA I10/10,23,29,34,39,46,53,61,71,80,0,0,0,0,0/
      DATA I11/10,23,29,35,40,47,53,62,70,80,0,0,0,0,0/
      DATA I12/10,23,29,34,41,46,55,70,80,0,0,0,0,0,0/
      DATA I13/10,23,29,35,42,48,56,67,80,0,0,0,0,0,0/
c  RUNOFF VALUES FOR NEW ENGLAND, EASTERN NY AREAS
      DATA IR1 /0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      DATA IR2 /0034,0022,0013,0007,0004,0,0,0,0,0,0,0,0,0,0/
      DATA IR3 /0069,0043,0028,0016,0010,0004,0,0,0,0,0,0,0,0,0/
      DATA IR4 /0145,0099,0069,0048,0032,0021,0013,0010,0009,0,0,0,0,0,
     10/
      DATA IR5 /0243,0183,0137,0102,0077,0059,0043,0030,0020,0015,0,0,0,
     10,0/
      DATA IR6 /0342,0252,0204,0158,0123,0095,0075,0050,0030,0021,0,0,0,
     10,0/
      DATA IR7 /0442,0323,0255,0200,0165,0131,0108,0081,0050,0029,0,0,0,
     10,0/
      DATA IR8 /0542,0397,0305,0243,0204,0177,0146,0118,0073,0038,0,0,0,
     10,0/
      DATA IR9 /0642,0459,0376,0306,0272,0237,0198,0155,0117,0081,0052,
     10,0,0,0/
      DATA IR10/0742,0505,0411,0347,0297,0240,0193,0150,0105,0069,0,0,0,
     10,0/
      DATA IR11/0842,0605,0511,0434,0381,0318,0270,0210,0163,0107,0,0,0,
     10,0/
      DATA IR12/0942,0705,0611,0546,0468,0419,0347,0239,0172,0,0,0,0,0,
     10/
      DATA IR13/1042,0805,0711,0634,0558,0502,0440,0360,0272,0,0,0,0,0,
     10/
c
      DATA SUBNAM /4HRON4,4H3   /,NOP/43/,IVERS/1/
      CALL FPRBUG(SUBNAM,2,NOP,IBUG)
      len = 13
c
 1000 do 1020 i = 1,len
      rline(i) = nline(i)
 1020 continue
      do 1030 i = 1,15
      do 1035 j = 1,len
      rndx(i,j) = indx(i,j)
      rnri2(i,j) = iri2(i,j)
 1035 continue
 1030 continue
c
      RETURN
      END
