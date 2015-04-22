//
// MSGTnav.java
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

/**
 * Navigation class for MSG type nav. This code was modified
 * from the original FORTRAN code (nvxmsgt.dlm) on the McIDAS system. It
 * only supports latitude/longitude to line/element transformations (LL) 
 * and vice/versa. Transform to 'XYZ' not implemented.
 * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
 *      McIDAS Programmer's Manual</A>
 *
 * @author  Tom Whittaker
 */
public final class MSGTnav extends AREAnav 
{

    private boolean isEastPositive = true;

    final double NOMORB=42164.;   // nominal radial distance of satellite (km)
    final double EARTH_RADIUS=6378.155; // earth equatorial radius (km)

    int itype;
    double h;
    double a;
    double rp;
    double cdr;
    double crd;
    double rs, yk;
    double deltax;
    double deltay;
    int LOFF, COFF, LFAC, CFAC;
    int[] ioff = new int[3];

    boolean first = true;
    int count = 0;
    double sublon = 0.0;

    /**
     * Set up for the real math work.  Must pass in the int array
     * of the MSGT nav 'codicil'.
     *
     * @param iparms  the nav block from the image file
     * @throws IllegalArgumentException
     *           if the nav block is not a MSGT type.
     */
    public MSGTnav (int[] iparms) throws IllegalArgumentException {

        if (iparms[0] != MSGT ) 
            throw new IllegalArgumentException("Invalid navigation type" + 
                                                iparms[0]);
        itype = 2;

        LOFF = iparms[1];
        COFF = iparms[2];
        LFAC = iparms[3];
        CFAC = iparms[4];
        sublon = iparms[5]/10000.;
        if (isEastPositive) sublon = -sublon; 

        h = NOMORB - EARTH_RADIUS;
        rs = EARTH_RADIUS + h;
        yk = rs/EARTH_RADIUS;
        a = 1./297.;
        rp = EARTH_RADIUS / (1. + a);
        crd = 180. / Math.PI;
        cdr = Math.PI / 180.;

        deltax = 1.0/(CFAC/1000000.);
        deltay = 1.0/(LFAC/1000000.);
    }

    /** converts from satellite coordinates to latitude/longitude
     *
     * @param  linele[][]  array of line/element pairs.  Where 
     *                     linele[indexLine][] is a 'line' and 
     *                     linele[indexEle][] is an element. These are in 
     *                     'file' coordinates (not "image" coordinates.)
     *
     * @return latlon[][]  array of lat/long pairs. Output array is 
     *                     latlon[indexLat][] of latitudes and 
     *                     latlon[indexLon][] of longitudes.
     *
     */
    public double[][] toLatLon(double[][] linele) {


        int number = linele[0].length;

        // Convert array to Image coordinates for computations
        double[][] imglinele = areaCoordToImageCoord(linele);
        double[][] latlon = imglinele;

        double xlin, xele, xr, yr, tanx, tany, v1, v2;
        double vmu, xt, yt, zt, teta, xlat, xlon;

        for (int point=0; point < number; point++) 
        {

            if (Double.isNaN(imglinele[indexLine][point]) || 
                Double.isNaN(imglinele[indexEle][point])) {
                continue;
            }

            xlin = 11136.0 - imglinele[indexLine][point] + 1.0;
            xele = 11136.0 - imglinele[indexEle][point] + 1.0;

            xlin = (xlin + 2.0) / 3.0;
            xele = (xele + 2.0) / 3.0;


            xr = xele - (COFF/10.);
            yr = xlin - (LOFF/10.);
            xr = xr*deltax*cdr;
            yr = yr*deltay*cdr;
            tanx = Math.tan(xr);
            tany = Math.tan(yr);

            v1 = 1. + tanx*tanx;
            v2 = 1. + (tany*tany)*((1.+a)*(1.+a));

            if (v1*v2 > ((yk*yk)/(yk*yk-1))) {
               xlat = Double.NaN; 
               xlon =  Double.NaN;
            } else {

               vmu = (rs - EARTH_RADIUS*Math.sqrt(yk*yk-(yk*yk-1)*v1*v2))/(v1*v2);
               xt = rs - vmu;
               yt = - vmu*tanx;
               zt = vmu * tany/Math.cos(xr);
               teta = Math.asin(zt/rp);

               xlat = Math.atan(Math.tan(teta)*EARTH_RADIUS/rp) * crd;
               xlon = Math.atan(yt/xt) * crd;

            }  

            //  put longitude into East Positive (form)
            xlon = xlon + sublon;
            if (!isEastPositive) xlon = -xlon;

            latlon[indexLat][point] = xlat;
            latlon[indexLon][point] = xlon;

        } // end point for loop

        return latlon;

    }

    /**
     * toLinEle converts lat/long to satellite line/element
     *
     * @param  latlon[][] array of lat/long pairs. Where latlon[indexLat][]
     *                    are latitudes and latlon[indexLon][] are longitudes.
     *
     * @return linele[][] array of line/element pairs.  Where
     *                    linele[indexLine][] is a line and linele[indexEle][]
     *                    is an element.  These are in 'file' coordinates
     *                    (not "image" coordinates);
     */
    public double[][] toLinEle(double[][] latlon) {
       
      int number = latlon[0].length;
      double[][] linele = new double[2][number];
      double xfi, xla, rom, y, r1, r2, teta, xt, yt, zt;
      double px, py, xr, yr, xele, xlin;
      double xlat, xlon;

      if (first) {
       //System.out.println("####   first time...");
       first = false;
      }

      for (int point=0; point < number; point++) 
      {

          if (Double.isNaN(latlon[indexLat][point]) || 
              Double.isNaN(latlon[indexLon][point])) {
              linele[indexLine][point] = Double.NaN;
              linele[indexEle][point] = Double.NaN;
              continue;
          }

          xlat = latlon[indexLat][point];

          // expects positive East Longitude.
          xlon = isEastPositive 
                   ?  latlon[indexLon][point]
                   : -latlon[indexLon][point];
          xlon = xlon - sublon;


          xfi = xlat*cdr;
          xla = xlon*cdr;
          rom = EARTH_RADIUS*rp/Math.sqrt(rp*rp*Math.cos(xfi) * 
              Math.cos(xfi)+EARTH_RADIUS*EARTH_RADIUS * 
              Math.sin(xfi)*Math.sin(xfi));

          y = Math.sqrt(h*h + rom*rom - 2.*h*rom*Math.cos(xfi)*Math.cos(xla));
          r1 = y*y + rom*rom;
          r2 = h*h;
    
          if (r1 > r2) {
            xlin = Double.NaN; 
            xele =  Double.NaN;
            linele[indexLine][point] = Double.NaN;
            linele[indexEle][point] = Double.NaN;

          } else {

            teta = Math.atan((rp/EARTH_RADIUS) * Math.tan(xfi));
            xt = EARTH_RADIUS * Math.cos(teta) * Math.cos(xla);
            yt = EARTH_RADIUS * Math.cos(teta) * Math.sin(xla);
            zt = rp * Math.sin(teta);

            px = Math.atan(yt/(xt-rs));
            py = Math.atan(-zt/(xt-rs)*Math.cos(px));
            px = px*crd;
            py = py*crd;
            xr = px/deltax;
            yr = py/deltay;
            xele = (COFF/10.) + xr;
            xlin = (LOFF/10.) + yr;

            xlin = xlin * 3.0 - 2.0;
            xele = xele * 3.0 - 2.0;
            linele[indexLine][point] = 11136.0 - xlin + 1.0;
            linele[indexEle][point] = 11136.0 - xele + 1.0;

          }  // end calculations

      } // end point loop

        // Return in 'File' coordinates
        return imageCoordToAreaCoord(linele, linele);
    }

    /** converts from satellite coordinates to latitude/longitude
     *
     * @param  linele[][]  array of line/element pairs.  Where 
     *                     linele[indexLine][] is a 'line' and 
     *                     linele[indexEle][] is an element. These are in 
     *                     'file' coordinates (not "image" coordinates.)
     *
     * @return latlon[][]  array of lat/long pairs. Output array is 
     *                     latlon[indexLat][] of latitudes and 
     *                     latlon[indexLon][] of longitudes.
     *
     */
    public float[][] toLatLon(float[][] linele) {


        int number = linele[0].length;

        // Convert array to Image coordinates for computations
        float[][] imglinele = areaCoordToImageCoord(linele);
        float[][] latlon = imglinele;

        double xlin, xele, xr, yr, tanx, tany, v1, v2;
        double vmu, xt, yt, zt, teta, xlat, xlon;

        for (int point=0; point < number; point++) 
        {

            if (Float.isNaN(imglinele[indexLine][point]) || 
                Float.isNaN(imglinele[indexEle][point])) {
                continue;
            }

            xlin = 11136.0 - imglinele[indexLine][point] + 1.0;
            xele = 11136.0 - imglinele[indexEle][point] + 1.0;

            xlin = (xlin + 2.0) / 3.0;
            xele = (xele + 2.0) / 3.0;

            xr = xele - (COFF/10.);
            yr = xlin - (LOFF/10.);
            xr = xr*deltax*cdr;
            yr = yr*deltay*cdr;
            tanx = Math.tan(xr);
            tany = Math.tan(yr);

            v1 = 1. + tanx*tanx;
            v2 = 1. + (tany*tany)*((1.+a)*(1.+a));

            if (v1*v2 > ((yk*yk)/(yk*yk-1))) {
               xlat = Float.NaN; 
               xlon =  Float.NaN;
            } else {

               vmu = (rs - EARTH_RADIUS*Math.sqrt(yk*yk-(yk*yk-1)*v1*v2))/(v1*v2);
               xt = rs - vmu;
               yt = - vmu*tanx;
               zt = vmu * tany/Math.cos(xr);
               teta = Math.asin(zt/rp);

               xlat = Math.atan(Math.tan(teta)*EARTH_RADIUS/rp) * crd;
               xlon = Math.atan(yt/xt) * crd;

            }  

            //  put longitude into East Positive (form)
            xlon = xlon + sublon;
            if (!isEastPositive) xlon = -xlon;

            latlon[indexLat][point] = (float) xlat;
            latlon[indexLon][point] = (float) xlon;

        } // end point for loop

        return latlon;

    }

    /**
     * toLinEle converts lat/long to satellite line/element
     *
     * @param  latlon[][] array of lat/long pairs. Where latlon[indexLat][]
     *                    are latitudes and latlon[indexLon][] are longitudes.
     *
     * @return linele[][] array of line/element pairs.  Where
     *                    linele[indexLine][] is a line and linele[indexEle][]
     *                    is an element.  These are in 'file' coordinates
     *                    (not "image" coordinates);
     */
    public float[][] toLinEle(float[][] latlon) {
       
      int number = latlon[0].length;
      float[][] linele = new float[2][number];
      double xfi, xla, rom, y, r1, r2, teta, xt, yt, zt;
      double px, py, xr, yr, xele, xlin;
      double xlat, xlon;

      if (first) {
       //System.out.println("####   first time...");
       first = false;
      }

      for (int point=0; point < number; point++) 
      {

          if (Float.isNaN(latlon[indexLat][point]) || 
              Float.isNaN(latlon[indexLon][point])) {
              linele[indexLine][point] = Float.NaN;
              linele[indexEle][point] = Float.NaN;
              continue;
          }

          xlat = latlon[indexLat][point];

          // expects positive East Longitude.
          xlon = isEastPositive 
                   ?  latlon[indexLon][point]
                   : -latlon[indexLon][point];
          xlon = xlon - sublon;


          xfi = xlat*cdr;
          xla = xlon*cdr;
          rom = EARTH_RADIUS*rp/Math.sqrt(rp*rp*Math.cos(xfi) * 
              Math.cos(xfi)+EARTH_RADIUS*EARTH_RADIUS * 
              Math.sin(xfi)*Math.sin(xfi));

          y = Math.sqrt(h*h + rom*rom - 2.*h*rom*Math.cos(xfi)*Math.cos(xla));
          r1 = y*y + rom*rom;
          r2 = h*h;
    
          if (r1 > r2) {
            xlin = Float.NaN; 
            xele =  Float.NaN;
            linele[indexLine][point] = Float.NaN;
            linele[indexEle][point] = Float.NaN;

          } else {

            teta = Math.atan((rp/EARTH_RADIUS) * Math.tan(xfi));
            xt = EARTH_RADIUS * Math.cos(teta) * Math.cos(xla);
            yt = EARTH_RADIUS * Math.cos(teta) * Math.sin(xla);
            zt = rp * Math.sin(teta);

            px = Math.atan(yt/(xt-rs));
            py = Math.atan(-zt/(xt-rs)*Math.cos(px));
            px = px*crd;
            py = py*crd;
            xr = px/deltax;
            yr = py/deltay;
            xele = (COFF/10.) + xr;
            xlin = (LOFF/10.) + yr;

            xlin = xlin * 3.0 - 2.0;
            xele = xele * 3.0 - 2.0;
            linele[indexLine][point] = (float)(11136.0 - xlin + 1.0);
            linele[indexEle][point] = (float)(11136.0 - xele + 1.0);

          }  // end calculations

      } // end point loop

      // Return in 'File' coordinates
      return imageCoordToAreaCoord(linele, linele);
    }
}
