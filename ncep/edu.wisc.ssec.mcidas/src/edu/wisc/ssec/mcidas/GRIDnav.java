//
// GRIDnav.java
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
 * GRIDnav is the class for handling the navigation of McIDAS grids.
 * It is basically a Java version of GRDDEF.FOR.
 * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
 *      McIDAS Programmer's Manual</A>
 *
 * @author Don Murray
 * 
 */
public class GRIDnav
    implements java.io.Serializable
{

  static final long serialVersionUID = 8741895066356394200L;

  /** Navigation type for pseudo-mercator grids */
  final int PSEUDO_MERCATOR = 1;
  /** Navigation type for polar stero or lambert conformal conic grids */
  final int PS_OR_LAMBERT_CONIC = 2;
  /** Navigation type for equidistant grids */
  final int EQUIDISTANT = 3;
  /** Navigation type for pseudo-mercator (general case) grids */
  final int PSEUDO_MERCATOR_GENERAL = 4;
  final int NO_NAV = 5;
  /** Navigation type for lambert conformal tangent grids */
  final int LAMBERT_CONFORMAL_TANGENT = 6;
  final double EARTH_RADIUS = 6371.23;
  final double xrad = Math.PI/180.;
  /** "Row" index in row/column array */
  public final int indexRow=1;
  /** "Column" index in row/column array */
  public final int indexCol=0;
  /** "Latitude" index in latitude/longitude array */
  public final int indexLat=0;
  /** "Longitude" index in latitude/longitude array */
  public final int indexLon=1;

  /* Default start for grid calculations in McIDAS is (1,1) */
  private int startRow = 1;
  private int startColumn = 1;

  /* (1,1) is located in the upper left.  If lower left, it's flipped */
  private boolean isRowFlipped = false;
  private double rowOffset = 0.0;

  /* common calculation variables */
  private int navType;
  private double xnr;             // number of rows
  private double xnc;             // number of columns
  private double xnrow;           // number of rows for calculations
  private double xncol;           // number of columns for calculations
  private boolean wierd = false;  // JTYP = 1, navType > 10

  /* Merc and pseudo_merc parameters */
  private double glamx;           // max latitude
  private double glomx;           // max longitude
  private double ginct;           // grid increment in latitude
  private double gincn;           // grid increment in longitude

  /* PS and CONF projection parameters */
  private double xrowi;           // row # of North Pole*10000
  private double xcoli;           // column # of North Pole* 10000
  private double xqlon;           // longitude parallel to columns
  private double xspace;          // column spacing at standard latitude
  private double xh;              // 
  private double xfac;            //
  private double xblat;           //

  /* Equidistant params */
  private double xrot;            // rotation angle
  private double yspace;
  private double xblon; 

  /**
   *  Construct a new GRIDnav from a grid directory block
   *  @param gridDirBlock  grid header block
   *  @throws McIDASException  illegal grid header
   */
  public GRIDnav(int[] gridDirBlock)
    throws McIDASException
  {
    if (gridDirBlock.length != GridDirectory.DIRSIZE)
      throw new McIDASException("Directory is not the right size");
    int gridType = gridDirBlock[GridDirectory.NAV_BLOCK_INDEX];
    navType = gridType%10;
    wierd = gridType/10 == 1;
    xnr = gridDirBlock[GridDirectory.ROWS_INDEX];
    xnc = gridDirBlock[GridDirectory.COLS_INDEX];
    xnrow = xnr;
    xncol = xnc;
    switch(navType)
    {
      case PSEUDO_MERCATOR:
      case PSEUDO_MERCATOR_GENERAL:
        glamx=gridDirBlock[34]/10000.;
        glomx=gridDirBlock[35]/10000.;
        ginct=gridDirBlock[38]/10000.;
        gincn= 
          (navType == PSEUDO_MERCATOR_GENERAL) 
             ? gridDirBlock[39]/10000. : ginct;
        if (wierd) {
          double x = xnr;
          xnr = xnc;
          xnc = x;
        }
        break;
      case PS_OR_LAMBERT_CONIC:
        xrowi  = gridDirBlock[34]/10000.;  // row # of the North pole*10000
        xcoli  = gridDirBlock[35]/10000.;  // col # of the North pole*10000
        xspace = gridDirBlock[36]/1000.;   // column spacing at standard lat (m)
        xqlon  = gridDirBlock[37]/10000.;  // lon parallel to cols (deg*10000)
        double xt1 = gridDirBlock[38]/10000.;  // first standard lat
        double xt2 = gridDirBlock[39]/10000.;  // second standard lat
        xh = (xt1 >= 0) ? 1. : -1.;
        xt1 =(90.-xh*xt1)*xrad;
        xt2 =(90.-xh*xt2)*xrad;
        xfac =1.0;
        if (xt1 != xt2) 
           xfac = (Math.log(Math.sin(xt1))-Math.log(Math.sin(xt2)))/
                  (Math.log(Math.tan(.5*xt1))-Math.log(Math.tan(.5*xt2)));
        xfac = 1.0/xfac;
        xblat = 6370. * Math.sin(xt1)/
                 (xspace*xfac*(Math.pow(Math.tan(xt1*.5),xfac)));
        if (wierd) {
           double x=xnr;
           xnr=xnc;
           xnc=x;
           x=xcoli;
           xcoli=xrowi;
           xrowi=xnr-x+1.0;
           xqlon=xqlon+90.;
        }

        break;
      case EQUIDISTANT:
        xrowi = 1.;
        xcoli = 1.;
        glamx = gridDirBlock[34]/10000.;       // lat of (1,1) degrees*10000
        glomx = gridDirBlock[35]/10000.;       // lon of (1,1) degrees*10000
        xrot  = -xrad*gridDirBlock[36]/10000.; // clockwise rotation of col 1
        xspace = gridDirBlock[37]/1000.;       // column spacing
        yspace = gridDirBlock[38]/1000.;       // row spacing
        xblat = EARTH_RADIUS*xrad/yspace;
        xblon = EARTH_RADIUS*xrad/xspace;

        if (wierd) {
          double x = xnr;
          xnr = xnc;
          xnc = x;
        }

        break;
      case LAMBERT_CONFORMAL_TANGENT:
        xrowi  = gridDirBlock[34]/10000.;  // row # of the North pole*10000
        xcoli  = gridDirBlock[35]/10000.;  // col # of the North pole*10000
        xspace = gridDirBlock[36]/1000.;   // column spacing at standard lat (m)
        xqlon  = gridDirBlock[37]/10000.;  // lon parallel to cols (deg*10000)
        double xtl = gridDirBlock[38]/10000.; // standard lat
        xh = (xtl >= 0) ? 1. : -1.;
        xtl = (90. - xh * xtl) * xrad;
        xfac = Math.cos(xtl);
        xblat = EARTH_RADIUS * Math.tan(xtl) / 
                   (xspace * Math.pow(Math.tan(xtl*.5), xfac));

        break;
      default:  
        break;
    }
  }

  /** 
   * converts from grid coordinates (x,y) or (col, row) to latitude/longitude
   *
   * @param  rowcol[][]  array of row/col pairs.  Where 
   *                     rowcol[indexRow][] is a row and 
   *                     rowcol[indexCol][] is a column. 
   *
   * @return latlon[][]  array of lat/long pairs. Output array is 
   *                     latlon[indexLat][] of latitudes and 
   *                     latlon[indexLon][] of longitudes.
   *
   */
  public double[][] toLatLon(double[][] rowcol)
  {
    double[][] latlon = new double[2][rowcol[0].length];
    double xlat = Double.NaN; // temp variable for lat
    double xlon = Double.NaN; // temp variable for lon
    double xrlon = 0.;
    double xldif, xedif;
    double radius;

    for (int i = 0; i < rowcol[0].length; i++)
    {
      
      // account for flipped coordinates
      double xrow = isRowFlipped ? rowOffset - rowcol[indexRow][i] + 1
                                 : rowcol[indexRow][i];
      // adjust row/col based on startRow/startCol
      xrow = xrow + (startRow - 1);
      double xcol = rowcol[indexCol][i] - (startColumn - 1);

      if (xrow > xnrow || xrow < 1.0 ||
          xcol > xncol || xcol < 1.0) {
        xlat = Double.NaN;
        xlon = Double.NaN;
      } else {
        switch (navType)
        {
          case PSEUDO_MERCATOR:
          case PSEUDO_MERCATOR_GENERAL:
            if (wierd) {
              double x = xrow;
              xcol = xrow;
              xrow = xnr - x + 1.0;
            }
            xlat = glamx-((xrow-1.0)*ginct);
            xlon = glomx-((xcol-1.0)*gincn);
            break;

          case EQUIDISTANT:
            break;

          case PS_OR_LAMBERT_CONIC:
          case LAMBERT_CONFORMAL_TANGENT:

            xldif = xh * (xrow - xrowi) / xblat;
            xedif =      (xcoli - xcol) / xblat;

            xrlon = 0.;
            if( !(xldif == 0 && xedif == 0)) xrlon = Math.atan2( xedif,xldif);
      
            xlon = xrlon / xfac / xrad + xqlon;
            if(xlon > 180.) xlon = xlon - 360.;
      
            radius = Math.sqrt( xldif * xldif + xedif * xedif);
            if( radius < 1.E-5 ) {
               xlat = xh * 90.;
            } else {
               xlat = xh * (90. - 2. * Math.atan( 
                          Math.exp( Math.log(radius) / xfac)) /xrad);
            }
            break;

          default:
            xlat=1.0-(xrow-1.0)/(xnr-1.0);
            xlon=1.0-(xcol-1.0)/(xnc-1.0);
            break;
        }
      }
      latlon[indexLat][i] = xlat;
      latlon[indexLon][i] = -xlon; // convert to east positive
    }
    return latlon;
  }

  /**
   * toRowCol converts latitude/longitude to grid row/col
   *
   * @param  latlon[][] array of lat/long pairs. Where latlon[indexLat][]
   *                    are latitudes and latlon[indexLon][] are longitudes.
   *
   * @return rowcol[][] array of row/col pairs.  Where
   *                    rowcol[indexRow][] is a row and rowcol[indexCol][]
   *                    is an column.  These are in 'grid' coordinates
   */
  public double[][] toRowCol(double[][] latlon)
  {
    double[][] rowcol = new double[2][latlon[0].length];
    double xrow, xcol, xlat, xlon;
    double xrlon, xclat, xrlat;
    double glomx1;
    double xldif, xedif, xdis, xangl, xange;

    for (int i = 0; i < latlon[0].length; i++)
    {
      xrow = Double.NaN;
      xcol = Double.NaN;
      xlat = latlon[indexLat][i];
      xlon = -latlon[indexLon][i];  // convert to McIDAS (west pos)
      switch(navType)
      {
        case PSEUDO_MERCATOR:
        case PSEUDO_MERCATOR_GENERAL:
          glomx1 = glomx;
          if (glomx < 0 && glomx*xlon < 0)
            glomx1 = glomx + 360;
          xrow = (glamx-xlat)/ginct + 1.0;
          xcol = (glomx1-xlon)/gincn + 1.0;
          break;
        case EQUIDISTANT:
          xrlon = xlon-glomx;
          xrlat = xlat-glamx;
          xldif = xblat*xrlat;
          xedif = xrlon*xblon*Math.cos(xlat*xrad);
          xdis  = Math.sqrt(xldif*xldif+xedif*xedif);
          if( xdis > .001) {
             xangl = Math.atan2(xldif,xedif)-90.*xrad;
             xange = Math.atan2(xldif,xedif)+90.*xrad;
             xldif = xdis*Math.cos(-xrot+xangl);
             xedif = xdis*Math.sin(-xrot+xange);
          }
          xrow = xrowi-xldif;
          xcol = xcoli-xedif;

          break;
        case PS_OR_LAMBERT_CONIC:
        case LAMBERT_CONFORMAL_TANGENT:

          xrlon = xlon - xqlon;
          if(xrlon > 180.) xrlon = xrlon - 360.;
          xrlon = xrlon * xfac * xrad;
     
          xclat = (90. - xh * xlat) * xrad * .5;
          xrlat = xblat * Math.pow(Math.tan(xclat), xfac);
     
          xrow = xh * xrlat * Math.cos(xrlon) + xrowi;
          xcol = -xrlat * Math.sin(xrlon) + xcoli;
      
          break;
        default:
          xrow = (1.0 - xlat)*(xnr-1.0)+1;
          xcol = (1.0 - xlon)*(xnc-1.0)+1;
          break;
      }

      if (xrow > xnrow || xrow < 1.0 ||
          xcol > xncol || xcol < 1.0) {

        xrow = Double.NaN;
        xcol = Double.NaN;

      } else {
     
        // account for non (1,1) origin
        xrow = xrow - (startRow - 1);
        xcol = xcol + (startColumn - 1);
        // account for flipped coordinates
        if (isRowFlipped) xrow = rowOffset - xrow + 1;

      }

      rowcol[indexRow][i] = xrow;
      rowcol[indexCol][i] = xcol;
    }
    return rowcol;
  }


  /**
   * Determines whether or not the <code>Object</code> in question is
   * the same as this <code>AREAnav</code>.   Right now, this returns
   * false until we can figure out when two navigations are equal.  
   * Subclasses could override if desired.
   *
   * @param obj the AREAnav in question
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof GRIDnav)) return false;
    GRIDnav that = (GRIDnav) obj;
    return (Double.doubleToLongBits(this.xnr) == 
              Double.doubleToLongBits(that.xnr) &&
            Double.doubleToLongBits(this.xnc) == 
              Double.doubleToLongBits(that.xnc) &&
            this.navType == that.navType);
  }

  /** 
   * define the starting row and column of another coordinate system -- 
   * for example (0,0)
   *
   * @param  startRow      the starting row number in another 
   *                       coordinate system
   *
   * @param  startColumn   the starting column number in another 
   *                       coordinate system
   *
   */
  public void setStart(int startRow, int startColumn) 
  {
      this.startRow = startRow;
      this.startColumn = startColumn;
  }
    
  /** 
   * specify whether the row coordinates are inverted and the row
   * offset.
   *
   * @param  row  ending row number
   *
   */
  public void setFlipRowCoordinates(int row) 
  {
    isRowFlipped = true;
    rowOffset = (double) row;
  }

  /**
   * Determine if navigation is using flipped coordinates.  This would
   * mean that the start of the coordinate system is in the lower left
   * instead of the upper left.
   *
   * @return  true if using flipped row coordinates, otherwise false
   */
  public boolean isFlippedRowCoordinates()
  {
    return isRowFlipped;
  }

  /**
   * Get the row offset for flipped coordinates
   *
   * @return  row offset
   */
  public double getRowOffset()
  {
    return rowOffset;
  }

  /**
   * Get the row index
   * @return the row index in the returned arrays
   */
  public int getRowIndex() { return indexRow; }

  /**
   * Get the column index
   * @return the column index in the returned arrays
   */
  public int getColumnIndex() { return indexCol; }

}
