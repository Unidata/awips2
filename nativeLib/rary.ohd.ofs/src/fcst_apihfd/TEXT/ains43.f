c  =====================================================================
c  pgm   ains43 (len,line,andx,anati)
c
c  out: len    .... length of table
c  out: line   .... ati curve lines
c  out: andx   .... antecedent precipitation index
c  out: anati  .... antecedent index
c  =====================================================================
      subroutine ains43 (len,line,andx,anati)
c.......................................................................
c  routine initializes antecedent index/ai arrays with values for the
c  New England, Eastern NY areas as used in the Northeast RFC
c  api/ai relationship.
c
c     len -  ENDING POSITION OF TABLE
c.......................................................................
c  Initially written by
c     Ken Mack - NERFC                            5/18/95
c.......................................................................
c
      include 'common/fdbug'
      include 'common/ionum'
c
      DIMENSION andx(15,5),anati(15,5)
      DIMENSION NLINE(5),INDX(15,5),IATI(15,5),LINE(5)
c  INDICIES FOR NEW ENGLAND, EASTERN NY AREA
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/ains43.f,v $
     . $',                                                             '
     .$Id: ains43.f,v 1.1 1996/03/21 15:51:40 page Exp $
     . $' /
C    ===================================================================
C

      DATA NLINE / 33, 40, 50, 55, 60/

c  API INDEX FOR NEW ENGLAND, EASTERN NY AREA
      DATA I1 /0,50,100,150,225,350,500,0,0,0,0,0,0,0,0/
      DATA I2 /0,38,75,105,150,200,250,350,500,0,0,0,0,0,0/
      DATA I3 /0,40,75,100,125,160,210,270,350,500,0,0,0,0,0/
      DATA I4 /0,50,85,110,150,200,250,300,360,500,0,0,0,0,0/
      DATA I5 /0,80,115,150,175,205,250,300,350,425,500,0,0,0,0/
c  ANTECEDENT INDEX FOR NEW ENGLAND, EASTERN NY AREAS
      DATA IR1 /2250,1930,1720,1550,1370,1210,1130,0,0,0,0,0,0,0,0/
      DATA IR2 /3550,3000,2610,2370,2100,1920,1780,1610,1440,0,0,0,0,0,
     10/
      DATA IR3 /5300,4350,3690,3340,3050,2750,2450,2240,2050,1860,0,0,0,
     10,0/
      DATA IR4 /6550,5150,4350,3910,3400,2960,2700,2510,2360,2150,0,0,0,
     10,0/
      DATA IR5 /7800,5500,4670,4100,3750,3450,3100,2870,2710,2560,2460,
     10,0,0,0/
      DATA SUBNAM /4HAINS,4H43  /,NOP/43/,IVERS/1/
c
      CALL FPRBUG(SUBNAM,2,NOP,IFDEB)
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
