//
// SINUnav.java
//

/*
This source file is part of the edu.wisc.ssec.mcidas package and is
Copyright (C) 1998 - 2009 by Tom Whittaker, Tommy Jasmin, Tom Rink,
Don Murray, James Kelly, Bill Hibbard, Dave Glowacki, Curtis Rueden
and others.
 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA
*/

package edu.wisc.ssec.mcidas;

public final class SINUnav extends AREAnav {
  double drad, decc, r;
  double xrow, xcol, xlat, xlon;
  double xblat, xblon, xspace, yspace;
  int itype, iwest;
  boolean isEastPositive = true;


  public SINUnav(int[] iparms) throws IllegalArgumentException {
    
     if (iparms[0] != SIN )
        throw new IllegalArgumentException("Invalid navigation type "+
                iparms[0]);

     itype = 2;
     xrow = iparms[1];
     xcol = iparms[2];

     xlat = McIDASUtil.integerLatLonToDouble(iparms[3]);
     xlon = McIDASUtil.integerLatLonToDouble(iparms[4]);

     xspace = iparms[5]/1000.;
     yspace = xspace;

     drad = iparms[6]/1000.;
     r = drad;
     decc = iparms[7]/1.e6;
     iwest = iparms[9];
     if (iwest >= 0) iwest = 1;
     xblat = r*DEGREES_TO_RADIANS/xspace;
     xblon = DEGREES_TO_RADIANS*r/yspace;

     /*
     for (int i=0; i<10; i++) {
       System.out.println("####   i="+i+"  val = "+iparms[i]);
     }
     */
   }

   public double[][] toLatLon(double[][] linele) {
     
     double xlin, xele, xldif, xedif, xdis, ylat, ylon;

     int number = linele[0].length;

     double[][] latlon = new double[2][number];
     double[][] imglinele = areaCoordToImageCoord(linele);

     for (int point=0; point < number; point++ ){

       xlin = imglinele[indexLine][point];
       xele = imglinele[indexEle][point];

       xldif = xrow - xlin;
       xedif = xcol - xele;
       xdis = Math.sqrt(xldif*xldif + xedif*xedif);
       if (xdis > .001) {
         double xangl = Math.atan2(xldif, xedif) - 90.*DEGREES_TO_RADIANS;
         double xange = Math.atan2(xldif,xedif) + 90.*DEGREES_TO_RADIANS;
         xldif = xdis*Math.cos(xangl);
         xedif = xdis*Math.sin(xange);
       }

       ylat = xlat + xldif/xblat;
       ylon = iwest * xedif/xblon/Math.cos(ylat*DEGREES_TO_RADIANS);

       if (Math.abs(ylon) > 180.0) {
         latlon[indexLat][point] = Double.NaN;
         latlon[indexLon][point] = Double.NaN;

       } else {
       
         ylon = xlon + ylon;
         if (ylon < -180.0) {
           ylon = ylon + 360.0;
         } else if (ylon > 180.0) {
           ylon = ylon - 360.0;
         }

         if (Math.abs(ylat) > 90.0 || Math.abs(ylon) > 180.0) {
           latlon[indexLat][point] = Double.NaN;
           latlon[indexLon][point] = Double.NaN;
         }

         if (isEastPositive) ylon = -ylon;

         latlon[indexLat][point] = ylat;
         latlon[indexLon][point] = ylon;
       }

     }

     return latlon;

   }


   public double[][] toLinEle(double[][] latlon) {
     double xdis, xele, xlin, zlat, zlon, xrlon, xrlat, xldif, xedif;

     int number = latlon[0].length;
     double[][] linele = new double[2][number];

     for (int point=0; point<number; point++) {
       zlat = latlon[indexLat][point];
       zlon = isEastPositive
              ?  -latlon[indexLon][point]
              :   latlon[indexLon][point];
       if (Double.isNaN(zlat) || Double.isNaN(zlon) ||
          (Math.abs(zlat) > 90.) ) {
            xele = Double.NaN;
            xlin = Double.NaN;
       } else {
          xrlon = iwest*(zlon - xlon);
          if (xrlon > 180.) xrlon = xrlon - 360.;
          if (xrlon < -180.) xrlon = xrlon + 360.;
          xrlat = zlat - xlat;
          xldif = xblat*xrlat;
          xedif = xrlon*xblon*Math.cos(zlat * DEGREES_TO_RADIANS);
          xdis = Math.sqrt(xldif*xldif + xedif*xedif);
          if (xdis > .001) {
            double xangl = Math.atan2(xldif, xedif) - 90.*DEGREES_TO_RADIANS;
            double xange = Math.atan2(xldif, xedif) + 90.*DEGREES_TO_RADIANS;
            xldif = xdis * Math.cos(xangl);
            xedif = xdis * Math.sin(xange);
          }

          xlin = xrow - xldif;
          xele = xcol - xedif;
        }

        linele[indexLine][point] = xlin;
        linele[indexEle][point] = xele;
      }

      return imageCoordToAreaCoord(linele, linele);
   }

   public float[][] toLatLon(float[][] linele) {
     
     double xlin, xele, xldif, xedif, xdis, ylat, ylon;

     int number = linele[0].length;
     float[][] latlon = new float[2][number];
     float[][] imglinele = areaCoordToImageCoord(linele);

     for (int point=0; point < number; point++ ){

       xlin = imglinele[indexLine][point];
       xele = imglinele[indexEle][point];

       xldif = xrow - xlin;
       xedif = xcol - xele;
       xdis = Math.sqrt(xldif*xldif + xedif*xedif);

       if (xdis > .001) {
         double xangl = Math.atan2(xldif, xedif) - 90.*DEGREES_TO_RADIANS;
         double xange = Math.atan2(xldif,xedif) + 90.*DEGREES_TO_RADIANS;
         xldif = xdis*Math.cos(xangl);
         xedif = xdis*Math.sin(xange);
       }

       ylat = xlat + xldif/xblat;
       ylon = iwest * xedif/xblon/Math.cos(ylat*DEGREES_TO_RADIANS);

       if (Math.abs(ylon) > 180.0) {
         latlon[indexLat][point] = Float.NaN;
         latlon[indexLon][point] = Float.NaN;

       } else {
       
         ylon = xlon + ylon;
         if (ylon < -180.0) {
           ylon = ylon + 360.0;
         } else if (ylon > 180.0) {
           ylon = ylon - 360.0;
         }

         if (Math.abs(ylat) > 90.0 || Math.abs(ylon) > 180.0) {
           latlon[indexLat][point] = Float.NaN;
           latlon[indexLon][point] = Float.NaN;
         }

         if (isEastPositive) ylon = -ylon;

         latlon[indexLat][point] = (float) ylat;
         latlon[indexLon][point] = (float) ylon;

       }

     }

     return latlon;

   }


   public float[][] toLinEle(float[][] latlon) {
     double xdis, xele, xlin, zlat, zlon, xrlon, xrlat, xldif, xedif;

     int number = latlon[0].length;
     float[][] linele = new float[2][number];

     for (int point=0; point<number; point++) {
       zlat = latlon[indexLat][point];
       zlon = isEastPositive
              ?  -latlon[indexLon][point]
              :   latlon[indexLon][point];
       if (Double.isNaN(zlat) || Double.isNaN(zlon) ||
          (Math.abs(zlat) > 90.) ) {
            xele = Double.NaN;
            xlin = Float.NaN;
       } else {
          xrlon = iwest*(zlon - xlon);
          if (xrlon > 180.) xrlon = xrlon - 360.;
          if (xrlon < -180.) xrlon = xrlon + 360.;
          xrlat = zlat - xlat;
          xldif = xblat*xrlat;
          xedif = xrlon*xblon*Math.cos(zlat * DEGREES_TO_RADIANS);
          xdis = Math.sqrt(xldif*xldif + xedif*xedif);
          if (xdis > .001) {
            double xangl = Math.atan2(xldif, xedif) - 90.*DEGREES_TO_RADIANS;
            double xange = Math.atan2(xldif, xedif) + 90.*DEGREES_TO_RADIANS;
            xldif = xdis * Math.cos(xangl);
            xedif = xdis * Math.sin(xange);
          }

          xlin = xrow - xldif;
          xele = xcol - xedif;
        }

        linele[indexLine][point] = (float)xlin;
        linele[indexEle][point] = (float)xele;
      }

      return imageCoordToAreaCoord(linele, linele);
   }

  public static void main(String[] a) {
    int[] p = {1397313056, 10000, 10000, 421353,831951,1000,6378388,81992,0,0};
    SINUnav sn = new SINUnav(p);
    double [][] latlon = new double[2][1];
    latlon[0][0] = 43.;
    latlon[1][0] = -89.;
    System.out.println("####  Doing double");
    System.out.println("####  Original latlon="+latlon[0][0]+"  "+latlon[1][0]);
    double[][] pix = sn.toLinEle(latlon);
    System.out.println("####  pix="+pix[0][0]+"  "+pix[1][0]);

    latlon = sn.toLatLon(pix);
    System.out.println("####  latlon="+latlon[0][0]+"  "+latlon[1][0]);

    System.out.println("####  Now doing float....");
    float [][] flatlon = new float[2][1];
    flatlon[0][0] = 43.f;
    flatlon[1][0] = -89.f;
    System.out.println("####  Original flatlon="+flatlon[0][0]+"  "+flatlon[1][0]);
    float[][] fpix = sn.toLinEle(flatlon);
    System.out.println("####  fpix="+fpix[0][0]+"  "+fpix[1][0]);

    flatlon = sn.toLatLon(fpix);
    System.out.println("####  flatlon="+flatlon[0][0]+"  "+flatlon[1][0]);

  }
}
