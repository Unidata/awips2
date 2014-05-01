c  =====================================================================
c  pgm:  rob43 (len,rline,rndx,rnri2)
c
c  out: len    .... length of table
c  out: rline  .... precipitation curve lines
c  out: rndx   .... final runoff index
c  out: rnri2  .... runoff values at precip line inflection point
c  =====================================================================
      subroutine rob43 (len,rline,rndx,rnri2)
c.......................................................................
c  This routine initializes rainfall/runoff arrays for the Buffalo area
c  as used in the Northeast RFC rainfall/runoff relationship.
c
c     len -  ENDING POSITION OF TABLE
c.......................................................................
c  Initially written by
c     Tim Sweeney  HRL                                  Feb 1995
c     Ken Mack   NERFC                                   5/18/95
c.......................................................................
c
c      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
c      COMMON /IONUM/ IN,IPR,IPU
c
      include 'common/fdbug'
      include 'common/ionum'
c
      INTEGER*4 rline(13),rndx(15,13),rnri2(15,13)

      DIMENSION NLINE(13),INDX(15,10),IRI2(15,10)
c  INDICIES FOR THE BUFFALO AREA
      DIMENSION I1(15),I2(15),I3(15),I4(15),I5(15),I6(15),I7(15),
     1  I8(15),I9(15),I10(15)
      DIMENSION IR1(15),IR2(15),IR3(15),IR4(15),IR5(15),IR6(15),IR7(15),
     1  IR8(15),IR9(15),IR10(15)
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/rob43.f,v $
     . $',                                                             '
     .$Id: rob43.f,v 1.1 1996/03/21 15:53:21 page Exp $
     . $' /
C    ===================================================================
C

      DATA NLINE / 0, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900,
     1             1000,1100 /

c  RI2 INDEX FOR THE BUFFALO AREA
      DATA I1/10 ,80 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I2/10 ,14 ,20 ,26 ,43 ,62 ,80 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I3/10 ,15 ,20 ,26 ,37 ,45 ,67 ,80 ,0 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I4/10 ,14 ,19 ,23 ,29 ,34 ,42 ,50 ,80 ,0 ,0 ,0 ,0 ,0 ,0 /
      DATA I5/10 ,15 ,18 ,21 ,25 ,29 ,35 ,43 ,52 ,70 ,80 ,0 ,0 ,
     1         0 ,0 /
      DATA I6/10 ,16 ,21 ,25 ,28 ,33 ,38 ,46 ,57 ,70 ,80 ,0 ,0 ,
     1         0 ,0 /
      DATA I7/10 ,16 ,24 ,27 ,31 ,36 ,43 ,52 ,70 ,80 ,0 ,0 ,0 ,0 ,0 /
      DATA I8/10 ,20 ,25 ,29 ,33 ,38 ,45 ,52 ,65 ,80 ,0 ,0 ,0 ,0 ,0 /
      DATA I9/10 ,20 ,25 ,29 ,34 ,40 ,46 ,55 ,65 ,80 ,0 ,0 ,0 ,0 ,0 /
      DATA I10/10 ,20 ,25 ,29 ,34 ,40 ,46 ,55 ,65 ,80 ,0 ,0 ,0 ,0 ,0 /
C RUNOFF VALUES FOR THE BUFFALO AREA
      DATA IR1/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA IR2/ 25, 16, 09, 06, 01, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA IR3/ 63, 47, 35, 24, 11, 06, 01, 01, 0, 0, 0, 0, 0, 0, 0/
      DATA IR4/147,115, 87, 69, 48, 34, 20, 17, 05, 0, 0, 0, 0,
     1          0, 0/
      DATA IR5/247,187,155,130,105, 86, 67, 50, 37, 21, 13, 0,
     1          0, 0, 0/
      DATA IR6/347,266,209,171,150,124,105, 83, 61, 40, 25,
     1          0, 0, 0, 0/
      DATA IR7/447,363,267,238,208,180,150,120, 69, 43, 0,
     1          0, 0, 0, 0/
      DATA IR8/547,412,354,315,285,257,222,191,140, 84,
     1          0, 0, 0, 0, 0/
      DATA IR9/647,512,454,415,380,342,310,266,222,165,
     1          0, 0, 0, 0, 0/
      DATA IR10/747,612,554,515,480,442,410,366,322,265,
     1          0, 0, 0, 0, 0/
c
      DATA SUBNAM /4hROB4,4h3   /,NOP/43/,IVERS/1/
c
      CALL FPRBUG(SUBNAM,2,NOP,IBUG)
      len = 10
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
