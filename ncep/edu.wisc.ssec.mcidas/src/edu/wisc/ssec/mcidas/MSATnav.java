//
// MSATnav.java
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
 * Navigation class for Meteosat (MSAT) type nav. This code was modified
 * from the original FORTRAN code (nvxmsat.dlm) on the McIDAS system. It
 * only supports latitude/longitude to line/element transformations (LL) 
 * and vice/versa. Transform to 'XYZ' not implemented.
 * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
 *      McIDAS Programmer's Manual</A>
 *
 * @author  Don Murray
 */
public final class MSATnav extends AREAnav 
{

    private boolean isEastPositive = true;

    final double NOMORB=42164.;   // nominal radial distance of satellite (km)
    final double EARTH_RADIUS=6378.155; // earth equatorial radius (km)

    int itype;
    double h;
    double a;
    double rp;
    double lpsi2;
    double deltax;
    double deltay;
    double rflon;                  // reference longitude;
    double sublon;
    int[] ioff = new int[3];

    /**
     * Set up for the real math work.  Must pass in the int array
     * of the MSAT nav 'codicil'.
     *
     * @param iparms  the nav block from the image file
     * @throws IllegalArgumentException
     *           if the nav block is not a MSAT type.
     */
    public MSATnav (int[] iparms) 
        throws IllegalArgumentException
    {

/* No longer needed.  Kept for consistency with nvxmsat.dlm
        if (ifunc != 1) 
        {
            if (iparms[0] == XY ) itype = 1;
            if (iparms[0] == LL ) itype = 2;
            return;
        }
*/

        if (iparms[0] != MSAT ) 
            throw new IllegalArgumentException("Invalid navigation type" + 
                                                iparms[0]);
        itype = 2;

        System.arraycopy(iparms, 3, ioff, 0, 3);
        h = (double) NOMORB - EARTH_RADIUS;
        a = 1./297.;
        rp = EARTH_RADIUS / (1. + a);
        lpsi2=1;
        deltax=18./2500.;
        deltay=18./2500.;
        rflon=0.0;     
        sublon = McIDASUtil.integerLatLonToDouble(iparms[6]);
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
    public double[][] toLatLon(double[][] linele) 
    {

        double xele, xlin;
        double ylat, ylon;
        double xele2, xlin2;
        double xfi, xla, z;
        double x, y;
        double xr, yr;
        double tanx, tany;
        double val1, val2;
        double yk;
        double vmu;
        double cosrf, sinrf;
        double teta;
        double xt, yt, zt;
        double rs;

        int number = linele[0].length;
        double[][] latlon = new double[2][number];

        // Convert array to Image coordinates for computations
        double[][] imglinele = areaCoordToImageCoord(linele);

        for (int point=0; point < number; point++) 
        {
            xlin = imglinele[indexLine][point];
            xele = imglinele[indexEle][point];

            xele2 = xele/2.;
            xlin2 = xlin/2.;
            x = 1250.5 - xele2;
            y = ioff[2] - (xlin2 + ioff[1] - ioff[0]);
            xr = x;
            yr = y;
            x = xr*lpsi2*deltax*DEGREES_TO_RADIANS;
            y = yr*lpsi2*deltay*DEGREES_TO_RADIANS;
            rs = EARTH_RADIUS + h;
            tanx = Math.tan(x);
            tany = Math.tan(y);
            val1=1.+tanx*tanx;
            val2=1.+(tany*tany)*((1.+a)*(1.+a));
            yk=rs/EARTH_RADIUS;
            if ((val1*val2) > ((yk*yk)/(yk*yk-1)))
            {
                latlon[indexLat][point] = Double.NaN;
                latlon[indexLon][point] = Double.NaN;
            }
            else
            {
                vmu = (rs-(EARTH_RADIUS*(Math.sqrt((yk*yk)-
                                  (yk*yk-1)*val1*val2))))/(val1*val2); 
                cosrf = Math.cos(rflon*DEGREES_TO_RADIANS);
                sinrf = Math.sin(rflon*DEGREES_TO_RADIANS);
                xt = (rs*cosrf) + (vmu*(tanx*sinrf - cosrf));
                yt = (rs*sinrf) - (vmu*(tanx*cosrf + sinrf));
                zt = vmu*tany/Math.cos(x);
                teta = Math.asin(zt/rp);
                xfi = (Math.atan(((Math.tan(teta))*EARTH_RADIUS)/rp))*
                         RADIANS_TO_DEGREES;
                xla=-Math.atan(yt/xt)*RADIANS_TO_DEGREES;
                
                // change longitude for correct subpoint
                xla = xla + sublon;
    
                // see if we have to convert to x, y, z
                if (itype == 1) 
                {
                    ylat = xfi;
                    ylon = xla;
                    // NLLXYZ(YLAT,YLON,XFI,XLA,Z)
                }

                //  put longitude into East Positive (form)
                if (isEastPositive) xla = -xla;
    
                latlon[indexLat][point] = xfi;
                latlon[indexLon][point] = xla;
            }  // end lat/lon point calculation 
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
        double x1, y1;
        double xlat, xlon;
        double xfi, xla;
        double rom;
        double r1, r2;
        double coslo, sinlo;
        double teta;
        double xt, yt, zt;
        double px, py;
        double rs;
        double reph, rpph;
        double xr, yr;

        int number = latlon[0].length;
        double[][] linele = new double[2][number];

        for (int point=0; point < number; point++) 
        {

            x1 = latlon[indexLat][point];

            // expects positive East Longitude.
            y1 = isEastPositive 
                     ?  latlon[indexLon][point]
                     : -latlon[indexLon][point];

            // if in cartesian coords, transform to lat/lon
            if (itype == 1)
            {
                x = latlon[indexLat][point];
                y = latlon[indexLon][point];
                // NXYZLL(x,y,z,zlat,zlon);
                y1 = -y1;
            }

            // correct for sublon
            y1 = y1 + sublon;
            xfi = x1*DEGREES_TO_RADIANS;
            xla = y1*DEGREES_TO_RADIANS;
            rom = 
                (EARTH_RADIUS*rp)/
                    Math.sqrt(
                        rp*rp*Math.cos(xfi)*Math.cos(xfi)+
                        EARTH_RADIUS*EARTH_RADIUS*Math.sin(xfi)*Math.sin(xfi));
            y = Math.sqrt(h*h+rom*rom-2*h*rom*Math.cos(xfi)*Math.cos(xla));
            r1 = y*y + rom*rom;
            r2 = h*h;
            if (r1 > r2)  // invalid point
            {
                linele[indexLine][point] = Double.NaN;
                linele[indexEle][point] = Double.NaN;
            }
            else          // calculate line an element
            {
                rs    = EARTH_RADIUS + h;
                reph  = EARTH_RADIUS;
                rpph  = rp;
                coslo = Math.cos(rflon*DEGREES_TO_RADIANS);
                sinlo = Math.sin(rflon*DEGREES_TO_RADIANS);
                teta  = Math.atan((rpph/reph)*Math.tan(xfi));
                xt    = reph*Math.cos(teta)*Math.cos(xla);
                yt    = reph*Math.cos(teta)*Math.sin(xla);
                zt    = rpph*Math.sin(teta);

                px    = Math.atan((coslo*(yt-rs*sinlo)-(xt-rs*coslo)*sinlo)/
                               (sinlo*(yt-rs*sinlo)+(xt-rs*coslo)*coslo));
                py    = Math.atan(zt*((Math.tan(px)*sinlo-
                                    coslo)/(xt-rs*coslo))*Math.cos(px));
                px = px*RADIANS_TO_DEGREES;
                py = py*RADIANS_TO_DEGREES;
                xr = px/(deltax*lpsi2);
                yr = py/(deltay*lpsi2);
                xr = 1250.5-xr;
                yr = yr + ioff[2] + ioff[1] - ioff[0];
                xr = xr*2;
                yr = 5000-yr*2;
                linele[indexLine][point] = yr;
                linele[indexEle][point] = xr;

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
    public float[][] toLatLon(float[][] linele) 
    {

        double xele, xlin;
        double ylat, ylon;
        double xele2, xlin2;
        double xfi, xla, z;
        double x, y;
        double xr, yr;
        double tanx, tany;
        double val1, val2;
        double yk;
        double vmu;
        double cosrf, sinrf;
        double teta;
        double xt, yt, zt;
        double rs;

        int number = linele[0].length;
        float[][] latlon = new float[2][number];

        // Convert array to Image coordinates for computations
        float[][] imglinele = areaCoordToImageCoord(linele);

        for (int point=0; point < number; point++) 
        {
            xlin = imglinele[indexLine][point];
            xele = imglinele[indexEle][point];

            xele2 = xele/2.;
            xlin2 = xlin/2.;
            x = 1250.5 - xele2;
            y = ioff[2] - (xlin2 + ioff[1] - ioff[0]);
            xr = x;
            yr = y;
            x = xr*lpsi2*deltax*DEGREES_TO_RADIANS;
            y = yr*lpsi2*deltay*DEGREES_TO_RADIANS;
            rs = EARTH_RADIUS + h;
            tanx = Math.tan(x);
            tany = Math.tan(y);
            val1=1.+tanx*tanx;
            val2=1.+(tany*tany)*((1.+a)*(1.+a));
            yk=rs/EARTH_RADIUS;
            if ((val1*val2) > ((yk*yk)/(yk*yk-1)))
            {
                latlon[indexLat][point] = Float.NaN;
                latlon[indexLon][point] = Float.NaN;
            }
            else
            {
                vmu = (rs-(EARTH_RADIUS*(Math.sqrt((yk*yk)-
                                  (yk*yk-1)*val1*val2))))/(val1*val2); 
                cosrf = Math.cos(rflon*DEGREES_TO_RADIANS);
                sinrf = Math.sin(rflon*DEGREES_TO_RADIANS);
                xt = (rs*cosrf) + (vmu*(tanx*sinrf - cosrf));
                yt = (rs*sinrf) - (vmu*(tanx*cosrf + sinrf));
                zt = vmu*tany/Math.cos(x);
                teta = Math.asin(zt/rp);
                xfi = (Math.atan(((Math.tan(teta))*EARTH_RADIUS)/rp))*
                         RADIANS_TO_DEGREES;
                xla=-Math.atan(yt/xt)*RADIANS_TO_DEGREES;
                
                // change longitude for correct subpoint
                xla = xla + sublon;
    
                // see if we have to convert to x, y, z
                if (itype == 1) 
                {
                    ylat = xfi;
                    ylon = xla;
                    // NLLXYZ(YLAT,YLON,XFI,XLA,Z)
                }

                //  put longitude into East Positive (form)
                if (isEastPositive) xla = -xla;
    
                latlon[indexLat][point] = (float) xfi;
                latlon[indexLon][point] = (float) xla;
            }  // end lat/lon point calculation 
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
        double x1, y1;
        double xlat, xlon;
        double xfi, xla;
        double rom;
        double r1, r2;
        double coslo, sinlo;
        double teta;
        double xt, yt, zt;
        double px, py;
        double rs;
        double reph, rpph;
        double xr, yr;

        int number = latlon[0].length;
        float[][] linele = new float[2][number];

        for (int point=0; point < number; point++) 
        {

            x1 = latlon[indexLat][point];

            // expects positive East Longitude.
            y1 = isEastPositive 
                     ?  latlon[indexLon][point]
                     : -latlon[indexLon][point];

            // if in cartesian coords, transform to lat/lon
            if (itype == 1)
            {
                x = latlon[indexLat][point];
                y = latlon[indexLon][point];
                // NXYZLL(x,y,z,zlat,zlon);
                y1 = -y1;
            }

            // correct for sublon
            y1 = y1 + sublon;
            xfi = x1*DEGREES_TO_RADIANS;
            xla = y1*DEGREES_TO_RADIANS;
            rom = 
                (EARTH_RADIUS*rp)/
                    Math.sqrt(
                        rp*rp*Math.cos(xfi)*Math.cos(xfi)+
                        EARTH_RADIUS*EARTH_RADIUS*Math.sin(xfi)*Math.sin(xfi));
            y = Math.sqrt(h*h+rom*rom-2*h*rom*Math.cos(xfi)*Math.cos(xla));
            r1 = y*y + rom*rom;
            r2 = h*h;
            if (r1 > r2)  // invalid point
            {
                linele[indexLine][point] = Float.NaN;
                linele[indexEle][point] = Float.NaN;
            }
            else          // calculate line an element
            {
                rs    = EARTH_RADIUS + h;
                reph  = EARTH_RADIUS;
                rpph  = rp;
                coslo = Math.cos(rflon*DEGREES_TO_RADIANS);
                sinlo = Math.sin(rflon*DEGREES_TO_RADIANS);
                teta  = Math.atan((rpph/reph)*Math.tan(xfi));
                xt    = reph*Math.cos(teta)*Math.cos(xla);
                yt    = reph*Math.cos(teta)*Math.sin(xla);
                zt    = rpph*Math.sin(teta);

                px    = Math.atan((coslo*(yt-rs*sinlo)-(xt-rs*coslo)*sinlo)/
                               (sinlo*(yt-rs*sinlo)+(xt-rs*coslo)*coslo));
                py    = Math.atan(zt*((Math.tan(px)*sinlo-
                                    coslo)/(xt-rs*coslo))*Math.cos(px));
                px = px*RADIANS_TO_DEGREES;
                py = py*RADIANS_TO_DEGREES;
                xr = px/(deltax*lpsi2);
                yr = py/(deltay*lpsi2);
                xr = 1250.5-xr;
                yr = yr + ioff[2] + ioff[1] - ioff[0];
                xr = xr*2;
                yr = 5000-yr*2;
                linele[indexLine][point] = (float) yr;
                linele[indexEle][point] = (float) xr;

            }  // end calculations
        } // end point loop

        // Return in 'File' coordinates
        return imageCoordToAreaCoord(linele, linele);
    }
}
