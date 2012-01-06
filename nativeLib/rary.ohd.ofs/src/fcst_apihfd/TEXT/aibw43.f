c  =====================================================================
c  pgm: aibw43 (len,line,andx,anati)
c
c  out: len    .... length of table
c  out: line   .... ati curve lines
c  out: andx   .... antecedent precipitation index
c  out: anati  .... antecedent index
c  =====================================================================
      subroutine aibw43 (len,line,andx,anati)
c.......................................................................
c  This routine initializes antecedent index arrays with winter values
c  for the Buffalo area as used in the Northeast RFC api relationship
c
c     len -  ENDING POSITION OF TABLE
c.......................................................................
c  Initially written by
c     Ken Mack    NERFC                                 5/18/95
c.......................................................................
c
c
      include 'common/fdbug'
      include 'common/ionum'
c
      DIMENSION andx(15,5),anati(15,5)
      DIMENSION NLINE(4),INDX(15,4),IATI(15,4),LINE(5)
c  INDICIES FOR THE BUFFALO AREA
      DIMENSION I1(15),I2(15),I3(15),I4(15),I5(15)
      DIMENSION IR1(15),IR2(15),IR3(15),IR4(15),IR5(15)
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/aibw43.f,v $
     . $',                                                             '
     .$Id: aibw43.f,v 1.1 1996/03/21 15:51:29 page Exp $
     . $' /
C    ===================================================================
C

      DATA NLINE / 33, 55, 58, 60/

c  API INDEX FOR THE BUFFALO AREA
      DATA I1 /0,50,70,90,120,190,290,375,500,0,0,0,0,0,0/
      DATA I2 /0,45,65,90,120,160,210,275,330,402,500,0,0,0,0/
      DATA I3 /0,50,80,100,130,170,205,245,280,325,375,440,500,0,0/
      DATA I4 /0,51,110,150,210,235,260,290,335,375,425,500,0,0,0/
c  ANTECEDENT INDEX FOR THE BUFFALO AREA
      DATA IR1 /2910,2060,1825,1650,1490,1275,1095,1000,1000,0,0,0,0,0,
     10/
      DATA IR2 /3295,2450,2120,1855,1655,1470,1305,1155,1080,1000,1000,
     10,0,0,0/
      DATA IR3 /6920,5010,4065,3560,2965,2360,1955,1620,1410,1240,1115,
     11040,1020,0,0/
      DATA IR4 /8000,7900,5965,4840,3470,2995,2680,2410,2130,1950,1800,
     11665,0,0,0/
c
      DATA SUBNAM /4HAIBW,4H43  /,NOP/43/,IVERS/1/
      CALL FPRBUG(SUBNAM,2,NOP,IBUG)
      len = 4
 1000 do 1020 i = 1,len
      line(i) = nline(i)
 1020 continue

      do 1030 i = 1,15
      do 1035 j = 1,len
      andx(i,j) = indx(i,j) / 100.
      anati(i,j) = iati(i,j)
 1035 continue
 1030 continue
c
      RETURN
      END
