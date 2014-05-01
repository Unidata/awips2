//
// MERCnav.java
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
 * Navigation class for Mercator (MERC) type nav. This code was modified 
 * from the original FORTRAN code (nvxmerc.dlm) on the McIDAS system. 
 * It only supports latitude/longitude to line/element transformations (LL) 
 * and vice/versa. Transform to 'XYZ' not implemented.
 * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
 *      McIDAS Programmer's Manual</A>
 *
 * @author  Don Murray
 */
public final class MERCnav extends AREAnav 
{

    private boolean isEastPositive = true;

    int iwest;
    int leftlon;
    double xrow;
    double xcol;
    double xlat1;
    double xspace;
    double xqlon;
    double xblat;
    double xblon;

    /**
     * Set up for the real math work.  Must pass in the int array
     * of the MERC nav 'codicil'.
     *
     * @param iparms  the nav block from the image file
     * @throws IllegalArgumentException
     *           if the nav block is not a MERC type.
     */
    public MERCnav (int[] iparms) 
        throws IllegalArgumentException
    {

        if (iparms[0] != MERC ) 
            throw new IllegalArgumentException("Invalid navigation type" + 
                                                iparms[0]);
        xrow = iparms[1];
        xcol = iparms[2];
        xlat1 = McIDASUtil.integerLatLonToDouble(iparms[3]);
        xspace = iparms[4]/1000.;
        xqlon = McIDASUtil.integerLatLonToDouble(iparms[5]);
        double r = iparms[6]/1000.;
        iwest = iparms[9];
        if (iwest >= 0) iwest = 1;
        xblat = r * Math.cos(xlat1*DEGREES_TO_RADIANS)/xspace;
        xblon = DEGREES_TO_RADIANS*r/xspace;
        leftlon = (int) xqlon-180*iwest;
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

        double xldif;
        double xedif;
        double xlon;
        double xlat;
        double xrlat, xrlon;

        int number = linele[0].length;
        double[][] latlon = new double[2][number];

        // Convert array to Image coordinates for computations
        double[][] imglinele = areaCoordToImageCoord(linele);

        for (int point=0; point < number; point++) 
        {
            xldif = xrow - imglinele[indexLine][point];
            xedif = xcol - imglinele[indexEle][point];
            xrlon = iwest*xedif/xblon;
            xlon = xrlon+xqlon;
            xrlat = Math.atan(Math.exp(xldif/xblat));
            xlat = (xrlat/DEGREES_TO_RADIANS - 45.)*2.+xlat1;
            if (xlon > (360.+leftlon) || xlon < leftlon) 
            {
                latlon[indexLat][point] = Double.NaN;
                latlon[indexLon][point] = Double.NaN;
            }
            else
            {
                latlon[indexLat][point] = xlat;
                latlon[indexLon][point] = (iwest == 1) ? -xlon  : xlon;
            }
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
     
     *                    is an element.  These are in 'file' coordinates
     *                    (not "image" coordinates);
     */
    public double[][] toLinEle(double[][] latlon) 
    {
        double xlon;
        double xlat;
        double xrlon, xrlat;

        int number = latlon[0].length;
        double[][] linele = new double[2][number];

        for (int point=0; point < number; point++) 
        {

            xlat = latlon[indexLat][point];
            // transform to McIDAS (west positive longitude) coordinates
            xlon = (iwest == 1) 
                   ? -latlon[indexLon][point]
                   : latlon[indexLon][point];

            xrlon = iwest*(xlon-xqlon);
            if (xrlon > 180.) xrlon -= 360.;
            if (xrlon < -180.) xrlon += 360.;
            if (xlat >= 90.) xlat = 89.99;
            if (xlat <= -90.) xlat = -89.99;
            xrlat = ((xlat-xlat1)/2 + 45.)*DEGREES_TO_RADIANS;
            if (xrlat <= 0.0)
            {
                linele[indexLine][point] = Double.NaN;
                linele[indexEle][point] = Double.NaN;
            }
            else
            {
                linele[indexLine][point] = 
                    xrow - xblat*Math.log(Math.tan(xrlat));
                linele[indexEle][point] = xcol - xrlon*xblon;
            }
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

        double xldif;
        double xedif;
        double xlon;
        double xlat;
        double xrlon, xrlat;

        int number = linele[0].length;
        float[][] latlon = new float[2][number];

        // Convert array to Image coordinates for computations
        float[][] imglinele = areaCoordToImageCoord(linele);

        for (int point=0; point < number; point++) 
        {
            xldif = xrow - imglinele[indexLine][point];
            xedif = xcol - imglinele[indexEle][point];
            xrlon = iwest*xedif/xblon;
            xlon = xrlon+xqlon;
            xrlat = Math.atan(Math.exp(xldif/xblat));
            xlat = (xrlat/DEGREES_TO_RADIANS - 45.)*2.+xlat1;
            if (xlon > (360.+leftlon) || xlon < leftlon) 
            {
                latlon[indexLat][point] = Float.NaN;
                latlon[indexLon][point] = Float.NaN;
            }
            else
            {
                latlon[indexLat][point] = (float) xlat;
                latlon[indexLon][point] = (float) ((iwest == 1) ? -xlon  : xlon);
            }
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
     
     *                    is an element.  These are in 'file' coordinates
     *                    (not "image" coordinates);
     */
    public float[][] toLinEle(float[][] latlon) 
    {
        double xlon;
        double xlat;
        double xrlon, xrlat;

        int number = latlon[0].length;
        float[][] linele = new float[2][number];

        for (int point=0; point < number; point++) 
        {

            xlat = latlon[indexLat][point];
            // transform to McIDAS (west positive longitude) coordinates
            xlon = (iwest == 1) 
                   ? -latlon[indexLon][point]
                   : latlon[indexLon][point];

            xrlon = iwest*(xlon-xqlon);
            if (xrlon > 180.) xrlon -= 360.;
            if (xrlon < -180.) xrlon += 360.;
            if (xlat >= 90.) xlat = 89.99;
            if (xlat <= -90.) xlat = -89.99;
            xrlat = ((xlat-xlat1)/2 + 45.)*DEGREES_TO_RADIANS;
            if (xrlat <= 0.0)
            {
                linele[indexLine][point] = Float.NaN;
                linele[indexEle][point] = Float.NaN;
            }
            else
            {
                linele[indexLine][point] =  (float)
                    (xrow - xblat*Math.log(Math.tan(xrlat)));
                linele[indexEle][point] = (float) (xcol - xrlon*xblon);
            }
        } // end point loop

        // Return in 'File' coordinates
        return imageCoordToAreaCoord(linele, linele);
    }
}
