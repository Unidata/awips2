//
// MOLLnav.java
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
 * Navigation class for Mollweide (MOLL) type nav. This code was modified
 * from the original FORTRAN code (nvxmoll.dlm) on the McIDAS system. It
 * only supports latitude/longitude to line/element transformations (LL) 
 * and vice/versa. Transform to 'XYZ' not implemented.
 * @see edu.wisc.ssec.mcidas.AREAnav
 * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
 *      McIDAS Programmer's Manual</A>
 *
 * @author  Don Murray
 */
public final class MOLLnav extends AREAnav 
{

    private boolean isEastPositive = true;

    private final double EARTH_RADIUS=6378.155; // earth equatorial radius (km)

    private double drad;
    private double decc;
    private double[] tlat = new double[101];
    private double[] t = new double[101];
    private double[][] coef = new double[4][101];
    private double[] lattbl = new double[91];

    private double xrow, xcol, rpix, xqlon;
    private int itype, ihem, iwest, icord;
    private double asq = 40683833.48;
    private double bsq = 40410330.18;
    private double ab = 40546851.22;
    private double ecc = .081992e0;
    private double eccsqr = 6.72265e-3;
    private int kwest = -1;
    private int kcord = 0;

    private final int KMPP = 1263358032;
    private final int PPMK = 1347439947;

    /**
     * Set up for the real math work.  Must pass in the int array
     * of the MOLL nav 'codicil'.
     *
     * @param iparms  the nav block from the image file
     * @throws IllegalArgumentException
     *           if the nav block is not a MOLL type.
     */
    public MOLLnav (int[] iparms) 
        throws IllegalArgumentException
    {
        double res;
        double x;
        int i;

/*  No longer needed.  Here for consistency with nvxmoll.dlm
        if (ifunc != 1) 
        {
            if (iparms[0] == XY ) itype = 1;
            if (iparms[0] == LL ) itype = 2;
            return;
        }
*/

        if (iparms[0] != MOLL ) 
             throw new IllegalArgumentException("Invalid navigation type" + 
                                                 iparms[0]);

        itype = 2;

        xrow = iparms[1];
        xcol = iparms[2];
        xqlon = iparms[4];
        drad = iparms[6]/1000.e0;
        double r = drad;

        if (iparms[14] == KMPP || iparms[14] == PPMK)
        {
            res = iparms[3];
            rpix = (0.7071*r)/res;
        }
        else
        {
            rpix = iparms[3];
        }

        decc = iparms[7]/1.e6;
        iwest = iparms[9];
        if (iwest >= 0 ) iwest = 1;
        icord = iparms[8];

        // set up coefficients;
        //     CALL LLOPT(DRAD,DECC,IWEST,IPARMS(9))
        asq = drad*drad;
        ecc = decc;
        eccsqr = ecc*ecc;
        double dpole = Math.sqrt(asq*(1.e0-eccsqr));
        bsq = dpole*dpole;
        ab = drad*dpole;
        if(iwest < 0) kwest=1;
        if(icord < 0) kcord=-1;

        for (i = 0; i < tlat.length; i++)
        {
            x = i/100.;
            if (x >= 1.)
            {
                t[i] = 1.;
                tlat[i] = 1.57080/DEGREES_TO_RADIANS;
            }
            else
            {
                t[i] = x;
                tlat[i] = Math.asin((Math.asin(x)
                           +x*Math.sqrt(1.0-x*x))/1.57080)/DEGREES_TO_RADIANS;
            }
        }

        // create cubic spline table - from asspl2.for
        int n = 100;
        int m = n - 1;
        double w, z, t1, s, u, v, zs, zq, ws, wq, aa, ba, ca, da;
        for (i = 0; i < n; i++)
        {
            if (i != 0)
                t1 = (t[i+1]-t[i-1])/(tlat[i+1]-tlat[i-1]);
            else
            {
                w  = (t[1]+t[2])/2.0;
                z  = (tlat[1]+tlat[2])/2.0;
                t1 = (w-t[0])/(z-tlat[0]);
                t1 = 2.0*(t[1]-t[0])/(tlat[1]-tlat[0])-t1;
            }
            if (i != m)
                s = (t[i+2]-t[i])/(tlat[i+2]-tlat[i]);
            else
            {
                w = (t[n-1]+t[n-2])/2.0;
                z = (tlat[n-1]+tlat[n-2])/2.0;
                s = (t[n]-w)/(tlat[n]-z);
                s = 2.0*(t[n]-t[n-1])/(tlat[n]-tlat[n-1])-s;
            }
            u=t[i+1];
            v=t[i];
            w=(tlat[i+1]+tlat[i])/2.0;
            z=(tlat[i+1]-tlat[i])/2.0;
            zs=z*z;
            zq=z*zs;
            ws=w*w;
            wq=w*ws;
            aa=.5*(u+v)-.25*z*(s-t1);
            ba=.75*(u-v)/z-.25*(s+t1);
            ca=.25*(s-t1)/z;
            da=.25*(s+t1)/zs-.25*(u-v)/zq;
            coef[0][i]=aa-ba*w+ca*ws-da*wq;
            coef[1][i]=ba-2.0*ca*w+3.0*da*ws;
            coef[2][i]=ca-3.0*da*w;
            coef[3][i]=da;
        }
        for (int j = 0; j < 4; j++)
        {
            coef[j][n] = coef[j][n-1];
        }

        i = 0;
        int j, k;
        for (int l = 0; l < 91; l++)
        {
            u = l;
            if (i >= n-1) i=0;
            if (u < tlat[i] || u > tlat[i+1])
            {
                i = 0;
                j = n;
                do 
                {
                    k = (i+j)/2;
                    if (u < tlat[k])  j=k;
                    if (u >= tlat[k]) i=k;
                } while (j > i+1);
            }
            lattbl[l] = coef[0][i]+u*(coef[1][i]+u*(coef[2][i]+u*coef[3][i]));
        }

    }

    /** 
     * Converts from satellite coordinates to latitude/longitude
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
    public double[][] toLatLon(double[][] linele) 
    {

        double xlin, xele;
        double xldif, xedif;
        double w;
        double xlat, xlon;
        double ylat, ylon;
        double snlt, cslt, snln, csln, r, tnlt, z;

        int number = linele[0].length;
        double[][] latlon = new double[2][number];
        // Convert from file to Image coordinates for calculations
        double[][] imglinele = areaCoordToImageCoord(linele);

        for (int point=0; point < number; point++) 
        {
            xlin = imglinele[indexLine][point];
            xele = imglinele[indexEle][point];

            xldif = Math.abs(xlin - xrow)/rpix;
            xedif = (xcol - xele)/rpix;

            // WLH 8 March 2000
            // if (xldif > 1.0)
            if (xlin != xlin || xele != xele || xldif > 1.0)
            {
                xlat = Double.NaN;
                xlon = Double.NaN;
            }
            else
            {
                w = Math.sqrt(1.0 - xldif*xldif);
                if (w == 0.0 || Math.abs(xedif/w) > 2.0)
                {
                    xlat = Double.NaN;
                    xlon = Double.NaN;
                }
                else
                {
                    xlat = Math.asin((Math.asin(xldif)+
                                 xldif*w)/1.57080)/DEGREES_TO_RADIANS;
                    if (xlin > xrow) xlat = -xlat;

                    // Compute angular displacement from std longitude (XQLON)
                    xlon = -90.*(xedif/w);
                    xlon = xqlon - xlon;

                    // Force angles to (-180 < XLON < 180)
                    if (xlon > 180.) xlon = xlon - 360.;
                    if (xlon < -180.) xlon = xlon + 360.;

                    // Convert to cartesian? coordinates
                    if (itype == 1)
                    {
                       // LLCART(YLAT,YLON,XLAT,XLON,Z)
                       ylat=DEGREES_TO_RADIANS*xlat;
                       if (kcord >= 0) 
                           ylat = Math.atan2(bsq*Math.sin(ylat),
                                             asq*Math.cos(ylat));
                       ylon = kwest*DEGREES_TO_RADIANS*xlon;
                       snlt = Math.sin(ylat);
                       cslt = Math.cos(ylat);
                       csln = Math.cos(ylon);
                       snln = Math.sin(ylon);
                       tnlt = Math.pow((snlt/cslt),2.0);
                       r = ab*Math.sqrt((1.0+tnlt)/(bsq+asq*tnlt));
                       xlat = r*cslt*csln;
                       xlon = r*cslt*snln;
                       z = r*snlt;
                    }
                }
            }
            // transform from McIDAS (west positive longitude) coordinates
            if (isEastPositive) xlon = -xlon;
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
    public double[][] toLinEle(double[][] latlon) 
    {
        double x, y, z;
        double xlin, xele;
        double xlat, xlon;
        double flat, t, t2, w, diff_lon, xedif;
        int isign, ilat;

        int number = latlon[0].length;
        double[][] linele = new double[2][number];

        for (int point=0; point < number; point++) 
        {

            xlat = latlon[indexLat][point];

            // transform to McIDAS (west longitude positive) coordinates
            xlon = isEastPositive 
                     ? - latlon[indexLon][point]
                     :  latlon[indexLon][point];

            // WLH 8 March 2000
            if (Double.isNaN(xlat) || Double.isNaN(xlon) ||
                (Math.abs(xlat) > 90.) ) {   // DRM 10 June 2003
              xele = Double.NaN;
              xlin = Double.NaN;
            }
            else {
              // if in cartesian coords, transform to lat/lon
              if (itype == 1)
              {
                  x = xlat;
                  y = xlon;
                  // CALL CARTLL(X,Y,Z,XLAT,XLON)
              }
  
              isign = -1;
              if (xlat < 0.0) isign = 1;
              ilat = (int) (Math.abs(xlat));
              flat = Math.abs(xlat) - ilat;
              t = lattbl[ilat];
              if (ilat != 90) t = t + flat*(lattbl[ilat+1] - lattbl[ilat]);
              t2 = Math.max(0.0, 1.0-t*t);
              w = Math.sqrt(t2);
         
              //** Here we need to handle the problem of computing
              //** angular differences across the dateline.
  
              diff_lon = xlon - xqlon;
  
              if (diff_lon < -180.0) diff_lon = diff_lon  + 360.;
              if (diff_lon >  180.0) diff_lon = diff_lon  - 360.;
       
              xedif = w * (diff_lon)/90.;
       
              if (Math.abs(xedif) > 2.0) 
              {
                 xele = Double.NaN;
                 xlin = Double.NaN;
              }
              else
              {
                 xele = xcol - xedif*rpix;
                 xlin = isign*t*rpix + xrow;
              }
            } // end if (xlat == xlat && xlon == xlon)
            linele[indexLine][point] = xlin;
            linele[indexEle][point] = xele;

        } // end point loop

        // Return in 'File' coordinates
        return imageCoordToAreaCoord(linele, linele);
    }

    /** 
     * Converts from satellite coordinates to latitude/longitude
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
    public float[][] toLatLon(float[][] linele) 
    {

        double xlin, xele;
        double xldif, xedif;
        double w;
        double xlat, xlon;
        double ylat, ylon;
        double snlt, cslt, snln, csln, r, tnlt, z;

        int number = linele[0].length;
        float[][] latlon = new float[2][number];
        // Convert from file to Image coordinates for calculations
        float[][] imglinele = areaCoordToImageCoord(linele);

        for (int point=0; point < number; point++) 
        {
            xlin = imglinele[indexLine][point];
            xele = imglinele[indexEle][point];

            xldif = Math.abs(xlin - xrow)/rpix;
            xedif = (xcol - xele)/rpix;

            // WLH 8 March 2000
            // if (xldif > 1.0)
            if (xlin != xlin || xele != xele || xldif > 1.0)
            {
                xlat = Double.NaN;
                xlon = Double.NaN;
            }
            else
            {
                w = Math.sqrt(1.0 - xldif*xldif);
                if (w == 0.0 || Math.abs(xedif/w) > 2.0)
                {
                    xlat = Double.NaN;
                    xlon = Double.NaN;
                }
                else
                {
                    xlat = Math.asin((Math.asin(xldif)+
                                 xldif*w)/1.57080)/DEGREES_TO_RADIANS;
                    if (xlin > xrow) xlat = -xlat;

                    // Compute angular displacement from std longitude (XQLON)
                    xlon = -90.*(xedif/w);
                    xlon = xqlon - xlon;

                    // Force angles to (-180 < XLON < 180)
                    if (xlon > 180.) xlon = xlon - 360.;
                    if (xlon < -180.) xlon = xlon + 360.;

                    // Convert to cartesian? coordinates
                    if (itype == 1)
                    {
                       // LLCART(YLAT,YLON,XLAT,XLON,Z)
                       ylat=DEGREES_TO_RADIANS*xlat;
                       if (kcord >= 0) 
                           ylat = Math.atan2(bsq*Math.sin(ylat),
                                             asq*Math.cos(ylat));
                       ylon = kwest*DEGREES_TO_RADIANS*xlon;
                       snlt = Math.sin(ylat);
                       cslt = Math.cos(ylat);
                       csln = Math.cos(ylon);
                       snln = Math.sin(ylon);
                       tnlt = Math.pow((snlt/cslt),2.0);
                       r = ab*Math.sqrt((1.0+tnlt)/(bsq+asq*tnlt));
                       xlat = r*cslt*csln;
                       xlon = r*cslt*snln;
                       z = r*snlt;
                    }
                }
            }
            // transform from McIDAS (west positive longitude) coordinates
            if (isEastPositive) xlon = -xlon;
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
    public float[][] toLinEle(float[][] latlon) 
    {
        double x, y, z;
        double xlin, xele;
        double xlat, xlon;
        double flat, t, t2, w, diff_lon, xedif;
        int isign, ilat;

        int number = latlon[0].length;
        float[][] linele = new float[2][number];

        for (int point=0; point < number; point++) 
        {

            xlat = latlon[indexLat][point];

            // transform to McIDAS (west longitude positive) coordinates
            xlon = isEastPositive 
                     ? - latlon[indexLon][point]
                     :  latlon[indexLon][point];

            // WLH 8 March 2000
            if (Double.isNaN(xlat) || Double.isNaN(xlon) ||
                (Math.abs(xlat) > 90.) ) {   // DRM 10 June 2003
              xele = Double.NaN;
              xlin = Double.NaN;
            }
            else {
              // if in cartesian coords, transform to lat/lon
              if (itype == 1)
              {
                  x = xlat;
                  y = xlon;
                  // CALL CARTLL(X,Y,Z,XLAT,XLON)
              }
  
              isign = -1;
              if (xlat < 0.0) isign = 1;
              ilat = (int) (Math.abs(xlat));
              flat = Math.abs(xlat) - ilat;
              t = lattbl[ilat];
              if (ilat != 90) t = t + flat*(lattbl[ilat+1] - lattbl[ilat]);
              t2 = Math.max(0.0, 1.0-t*t);
              w = Math.sqrt(t2);
         
              //** Here we need to handle the problem of computing
              //** angular differences across the dateline.
  
              diff_lon = xlon - xqlon;
  
              if (diff_lon < -180.0) diff_lon = diff_lon  + 360.;
              if (diff_lon >  180.0) diff_lon = diff_lon  - 360.;
       
              xedif = w * (diff_lon)/90.;
       
              if (Math.abs(xedif) > 2.0) 
              {
                 xele = Double.NaN;
                 xlin = Double.NaN;
              }
              else
              {
                 xele = xcol - xedif*rpix;
                 xlin = isign*t*rpix + xrow;
              }
            } // end if (xlat == xlat && xlon == xlon)
            linele[indexLine][point] = (float) xlin;
            linele[indexEle][point] = (float) xele;

        } // end point loop

        // Return in 'File' coordinates
        return imageCoordToAreaCoord(linele, linele);
    }

}

