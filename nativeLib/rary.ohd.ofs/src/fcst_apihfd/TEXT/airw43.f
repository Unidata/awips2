c  =====================================================================
c  pgm:  airw43 (len,line,andx,anati)
c
c  out: len    .... length of table
c  out: line   .... ati curve lines
c  out: andx   .... antecedent precipitation index
c  out: anati  .... antecedent index
c  =====================================================================
      subroutine airw43 (len,line,andx,anati)
c.......................................................................
c  This routine initializes antecedent index/ai arrays with winter 
c  values for the Genessee area as used in the Northeast RFC api/ai
c  relationship.
c
c     len -  ENDING POSITION OF TABLE
c.......................................................................
c  Initially written by
c     Ken Mack    NERFC                              5/18/95
c.......................................................................
c
      include 'common/fdbug'
      include 'common/ionum'
c
      DIMENSION andx(15,5),anati(15,5)
      DIMENSION NLINE(5),INDX(15,5),IATI(15,5),LINE(5)
c  INDICIES FOR THE BUFFALO AREA
      DIMENSION I1(15),I2(15),I3(15),I4(15),I5(15)
      DIMENSION IR1(15),IR2(15),IR3(15),IR4(15),IR5(15)
      DIMENSION SUBNAM(2)
c
      EQUIVALENCE
     1  (I1(1),INDX(1,1)),  (I2(1),INDX(1,2)),  (I3(1),INDX(1,3)),
     2  (I4(1),INDX(1,4)),  (I5(1),INDX(1,5))

      EQUIVALENCE
     1  (IR1(1),IATI(1,1)),  (IR2(1),IATI(1,2)),  (IR3(1),IATI(1,3)),
     2  (IR4(1),IATI(1,4)),  (IR5(1),IATI(1,5))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/airw43.f,v $
     . $',                                                             '
     .$Id: airw43.f,v 1.1 1996/03/21 15:52:23 page Exp $
     . $' /
C    ===================================================================
C

      DATA NLINE / 33, 50, 55, 58, 60/

c  API INDEX FOR THE GENESSEE - ROCHESTER AREA
      DATA I1 /0,12,35,60,85,120,150,195,245,290,320,500,0,0,0/
      DATA I2 /0,50,90,105,130,160,190,230,280,365,423,500,0,0,0/
      DATA I3 /0,33,70,105,130,155,175,205,235,280,350,410,485,500,0/
      DATA I4 /0,48,84,120,145,175,195,215,250,290,350,440,500,0,0/
      DATA I5 /0,71,115,155,185,205,240,265,295,330,380,450,500,0,0/
c  ANTECEDENT INDEX FOR THE GENESSEE - ROCHESTER AREA
      DATA IR1 /3830,3450,2865,2395,2045,1690,1470,1250,1100,1020,1000,
     11000,0,0,0/
      DATA IR2 /5350,4000,3085,2800,2445,2115,1875,1635,1415,1145,1000,
     11000,0,0,0/
      DATA IR3 /6040,5000,4000,3250,2835,2500,2295,2045,1850,1630,1370,
     11195,1000,1000,0/
      DATA IR4 /7155,6100,5050,4160,3660,3165,2900,2695,2400,2140,1830,
     11465,1250,0,0/
      DATA IR5 /8000,7900,6540,5360,4640,4240,3670,3350,3055,2795,2480,
     12110,1880,0,0/
c
      DATA SUBNAM /4HAIRW,4H43  /,NOP/43/,IVERS/1/
c
      CALL FPRBUG(SUBNAM,2,NOP,IBUG)
      len = 5
 1000 do 1020 i = 1,len
      line(i) = nline(i)
 1020 continue
c
      do 1030 i = 1,15
      do 1035 j = 1,len
      andx(i,j) = indx(i,j) / 100.
      anati(i,j) = iati(i,j)
 1035 continue
 1030 continue
c
      RETURN
      END
