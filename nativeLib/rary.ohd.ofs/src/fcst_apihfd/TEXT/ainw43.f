c  =====================================================================
c  pgm:  ainw43 (len,line,andx,anati)
c
c  out: len    .... length of table
c  out: line   .... ati curve lines
c  out: andx   .... antecedent precipitation index
c  out: anati  .... antecedent index
c  =====================================================================
      subroutine ainw43 (len,line,andx,anati)
c.......................................................................
c  This routine initializes antecedent index/ai arrays with winter 
c  values for the New England, Eastern NY areas as used in the 
c  Northeast RFC api/ai relationship.
c
c     LEN -  ENDING POSITION OF TABLE
c.......................................................................
c  Initially written by
c     Ken Mack - NERFC                             09/21/95
c.......................................................................
c
      include 'common/fdbug'
      include 'common/ionum'
c
      DIMENSION ANDX(15,5),ANATI(15,5)
      DIMENSION NLINE(4),INDX(15,4),IATI(15,4),LINE(4)
c  INDICIES FOR NEW ENGLAND, EASTERN NY AREA
      DIMENSION I1(15),I2(15),I3(15),I4(15)
      DIMENSION IR1(15),IR2(15),IR3(15),IR4(15)
      DIMENSION SUBNAM(2)
c
      EQUIVALENCE
     1  (I1(1),INDX(1,1)),  (I2(1),INDX(1,2)),  (I3(1),INDX(1,3)),
     2  (I4(1),INDX(1,4))

      EQUIVALENCE
     1  (IR1(1),IATI(1,1)),  (IR2(1),IATI(1,2)),  (IR3(1),IATI(1,3)),
     2  (IR4(1),IATI(1,4))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/ainw43.f,v $
     . $',                                                             '
     .$Id: ainw43.f,v 1.1 1996/03/21 15:51:54 page Exp $
     . $' /
C    ===================================================================
C

      DATA NLINE / 33, 45, 55, 60/

c  API INDEX FOR NEW ENGLAND, EASTERN NY AREA
      DATA I1 /0,50,100,150,205,275,350,500,0,0,0,0,0,0,0/
      DATA I2 /0,50,100,150,200,260,375,500,0,0,0,0,0,0,0/
      DATA I3 /0,25,50,75,100,130,175,225,300,500,0,0,0,0,0/
      DATA I4 /0,60,100,125,160,200,250,300,360,500,0,0,0,0,0/
c  ANTECEDENT INDEX FOR NEW ENGLAND, EASTERN NY AREAS
      DATA IR1 /2250,1930,1700,1550,1420,1300,1210,1130,0,0,0,0,0,0,0/
      DATA IR2 /3340,2710,2250,1950,1750,1600,1430,1310,0,0,0,0,0,0,0/
      DATA IR3 /4800,4170,3660,3270,2950,2650,2330,2100,1870,1630,0,0,0,
     10,0/
      DATA IR4 /6200,4750,4000,3610,3210,2900,2600,2420,2270,2070,0,0,0,
     10,0/
c
      DATA SUBNAM /4HAINW,4H43  /,NOP/43/,IVERS/1/
c
      CALL FPRBUG(SUBNAM,2,NOP,IBUG)
      len = 4
 1000 DO 1020 I = 1,len
      LINE(I) = NLINE(I)
 1020 continue
c
      do 1030 I = 1,15
      do 1035 J = 1,len
      andx(i,j) = indx(i,j) / 100.
      anati(i,j) = iati(i,j)
 1035 continue
 1030 continue
c
      RETURN
      END
