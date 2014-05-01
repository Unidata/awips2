c  =====================================================================
c  pgm:  airs43 (len,line,andx,anati)
c
c  out: len    .... length of table
c  out: line   .... ati curve lines
c  out: andx   .... antecedent precipitation index
c  out: anati  .... antecedent index
c  =====================================================================
      subroutine airs43 (len,line,andx,anati)
c.......................................................................
c  This routine initializes antecedent index/ai arrays with summer
c  values for the Genessee area as used in the Northeast RFC api/ai
c  relationship.
c
c     len -  ENDING POSITION OF TABLE
c.......................................................................
c  Initially written by
c     Ken Mack   NERFC                                    4/14/95
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/airs43.f,v $
     . $',                                                             '
     .$Id: airs43.f,v 1.1 1996/03/21 15:52:12 page Exp $
     . $' /
C    ===================================================================
C

      DATA NLINE / 33, 40, 45, 55, 60/

c  API INDEX FOR THE GENESSEE - ROCHESTER AREA
      DATA I1 /0,12,35,60,85,120,150,195,245,290,320,500,0,0,0/
      DATA I2 /0,15,35,50,65,90,115,140,180,210,250,300,350,420,500/
      DATA I3 /0,45,80,120,150,180,205,240,300,360,450,500,0,0,0/
      DATA I4 /0,11,50,85,115,140,170,195,215,235,260,290,330,385,500/
      DATA I5 /0,66,110,140,165,190,210,235,265,305,340,390,440,500,0/
c  ANTECEDENT INDEX FOR THE GENESSEE - ROCHESTER AREA
      DATA IR1 /3830,3450,2865,2395,2045,1690,1470,1250,1100,1020,1000,
     11000,0,0,0/
      DATA IR2 /4900,4500,4000,3680,3380,2955,2605,2330,2000,1810,1635,
     11480,1365,1260,1190/
      DATA IR3 /6210,5050,4310,3645,3215,2880,2655,2405,2160,1960,1740,
     11640,0,0,0/
      DATA IR4 /8000,7900,6770,5860,5200,4715,4205,3850,3600,3400,3190,
     12985,2750,2500,2055/
      DATA IR5 /8000,7900,6740,5950,5350,4850,4510,4190,3865,3500,3250,
     12960,2710,2440,0/
c
      DATA SUBNAM /4HAIRS,4H43  /,NOP/43/,IVERS/1/
      len = 5
      CALL FPRBUG(SUBNAM,2,NOP,IBUG)
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
