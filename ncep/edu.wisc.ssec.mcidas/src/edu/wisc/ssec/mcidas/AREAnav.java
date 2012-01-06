//
// AREAnav.java
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
 * The AREAnav is the superclass for AREA file navigation modules.
 * When used with AreaFile class, set up like this:
 *
 * <pre><code>
 *  AreaFile af;
 *  try {
 *    af = new AreaFile("/home/user/mcidas/data/AREA0001");
 *  } catch (AreaFileException e) {
 *    System.out.println(e);
 *    return;
 *  }
 *  int[] dir;
 *  try { dir=af.getDir();
 *  } catch (AreaFileException e){
 *    System.out.println(e);
 *    return;
 *  }
 *  int[] nav;
 *  try { nav=af.getNav();
 *  } catch (AreaFileException e){
 *    System.out.println(e);
 *    return;
 *  }
 *  try { 
 *    AREAnav ng = new XXXXnav(nav);  // XXXXnav is the specific implementation
 *  } catch (IllegalArgumentException excp) {
 *    System.out.println(excp);
 *    return;
 *  }
 *  ng.setImageStart(dir[5], dir[6]);
 *  ng.setRes(dir[11], dir[12]);
 *  ng.setStart(0,0);
 *  ng.setMag(1,1);
 *  ......................
 * </code></pre>
 *
 * By convention, latitudes are positive North (negative South), and
 * longitudes are positive East (negative West).
 *
 * @author Tom Whittaker/Don Murray
 * 
 */
public abstract class AREAnav 
    implements java.io.Serializable
{
    
    static final long serialVersionUID = 2334637524537406773L;
    
    /** Constant for radians to degrees conversion */
    public final static double RADIANS_TO_DEGREES = 180./Math.PI;

    /** Constant for degrees to radians conversion */
    public final static double DEGREES_TO_RADIANS = Math.PI/180.;   

    /** Code value in AREA files used to designate DMSP navigation */
    public static final int DMSP =  0x444D5250;    

    /** Code value in AREA files used to designate GMSX (GMS) navigation */
    public static final int GMSX =  0x474D5358;    

    /** Code value in AREA files used to designate GOES (GOES D-H) navigation */
    public static final int GOES =  0x474F4553;    

    /** Code value in AREA files used to designate GEOS navigation */
    public static final int GEOS =  0x47454F53;    


    /** Code value in AREA files used to designate GVAR (GOES I-M) navigation */
    public static final int GVAR =  0x47564152;    

    /** Code value in AREA files used to designate MOLL (Mollweide) 
        navigation */
    public static final int MOLL =  0x4D4F4C4C;

    /** Code value in AREA files used to designate MSAT (Meteosat) 
        navigation */
    public static final int MSAT =  0x4D534154;

    /** Code value in AREA files used to designate MSGT  navigation */
    public static final int MSGT = 0x4D534754;

    /** Code value in AREA files used to designate MSG navigation */
    public static final int MSG  = 0x4D534720;

    /** Code value in AREA files used to designate POES navigation */
    public static final int POES =  0x5449524F;    

    /** Code value in AREA files used to designate RADR (radar) navigation */
    public static final int RADR =  0x52414452;

    /** Code value in AREA files used to designate RECT (rectilinear) 
        navigation */
    public static final int RECT =  0x52454354;

    /** Code value in AREA files used to designate PS (polar stereographic) 
        navigation */
    public static final int PS   =  0x50532020;

    /** Code value in AREA files used to designate MERC (mercator) navigation */
    public static final int MERC =  0x4D455243;

    /** Code value in AREA files used to designate TANC (tangent cone) 
        navigation */
    public static final int TANC =  0x54414E43;

    /** Code value in AREA files used to designate SIN (sinusoidal cone) 
        navigation */
    public static final int SIN =  0x53494E20;

    /** Code value in AREA files used to designate LAMB (lambert conformal) 
        navigation */
    public static final int LAMB =  0x4C414D42;

    /** Code value in AREA files used to designate Lat/Lon */
    public static final int LALO = 0x4C414C4F;

    /** Code value in AREA files used to designate Lat/Lon */
    public static final int KALP = 0x4B414C50;

    /** Code value in AREA files used to designate ABIS */
    public static final int ABIS = 0x41424953;

    /** Code value for specifying Latitude/Longitude transformations */
    public static final int LL = 123;

    /** Code value for specifying Cartesian (X/Y) transformations */
    public static final int XY = 234;

    /** "Line" index in line/element array */
    public final int indexLine=1;
    /** "Element" index in line/element array */
    public final int indexEle=0;
    /** "Latitude" index in latitude/longitude array */
    public final int indexLat=0;
    /** "Longitude" index in latitude/longitude array */
    public final int indexLon=1;

    private boolean isLineFlipped = false;
    private float lineOffset = 0.0f;

    // the following are ancillary info set by the set/get
    // public methods Res, Mag, and Start
    private float resLine = 1.f;
    private float resElement = 1.f;
    private float magLine = 1.f;
    private float magElement = 1.f;
    private float startLine = 0.f;
    private float startElement = 0.f;
    private float startImageLine = 0.f;
    private float startImageElement = 0.f;

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
    public abstract double[][] toLatLon(double[][] linele);

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
    public abstract double[][] toLinEle(double[][] latlon);

    /** converts from satellite coordinates to latitude/longitude.
     * This implementation converts the input array to doubles
     * and calls the double signature of {@link toLatLon(double[][])}.
     * Subclasses should implement a real float version for better
     * performance.
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
       return doubleToFloat(toLatLon(floatToDouble(linele)));
    }

    /**
     * toLinEle converts lat/long to satellite line/element
     * This implementation converts the input array to doubles
     * and calls the double signature of {@link toLineEle(double[][])}.
     * Subclasses should implement a real float version for better
     * performance.
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
       return doubleToFloat(toLinEle(floatToDouble(latlon)));
    }

    /** 
     * Define the resolution of the image.
     * values range from 1 (highest) to n (lowest). Note
     * that when an image is blown down during display, this
     * value is changed in the frame object to reflect this
     * (rather than changing the magnification).
     *
     * @param resLine     is the resolution in the 'line' direction.
     *                    This value is always > 0.
     *
     * @param resElement  is the resolution in the 'element'
     *                    direction.  The value is always >0.
     *
     */
    public void setRes(int resLine, int resElement) 
    {
        this.resLine = (float)resLine;
        this.resElement = (float)resElement;
    }

    /** 
     * Define the resolution of the image.
     * values range from 1 (highest) to n (lowest). Note
     * that when an image is blown down during display, this
     * value is changed in the frame object to reflect this
     * (rather than changing the magnification).
     *
     * @param resLine     is the resolution in the 'line' direction.
     *                    This value is always > 0.
     *
     * @param resElement  is the resolution in the 'element'
     *                    direction.  The value is always >0.
     *
     */
    public void setRes(float resLine, float resElement) 
    {
        this.resLine = resLine;
        this.resElement = resElement;
    }


    /** 
     * define the magnification factor (in case an image
     * was blown up when displayed).  This value is always > 0.
     *
     * @param  magLine   is the (line) magnification factor that might have
     *                   been used when the image was displayed.
     *
     * @param  magElment is the (element) magnification factor
     *                   that might have been used when the image was displayed.
     *
     */
    public void setMag(int magLine, int magElement) 
    {
        this.magLine = (float)magLine;
        this.magElement = (float)magElement;
    }

    /** define the magnification factor (in case an image
     * was blown up when displayed).  This value is always > 0.
     *
     * @param  magLine   is the (line) magnification factor that might have
     *                   been used when the image was displayed.
     *
     * @param  magElment is the (element) magnification factor
     *                   that might have been used when the image was displayed.
     *
     */
    public void setMag(float magLine, float magElement) 
    {
        this.magLine = magLine;
        this.magElement = magElement;
    }

    /** define the starting line and element of another
     * coordinate system -- usually a TV (note that the TV
     * coordinates start at (1,1).
     *
     * @param  startLine     the starting line number in another 
     *                       coordinate system
     *
     * @param  startElement  the starting element number in another 
     *                       coordinate system
     *
     */
    public void setStart(int startLine, int startElement) 
    {
        this.startLine = (float)startLine;
        this.startElement = (float)startElement;
    }
    
    /** define the coordinate in the [0][0] position of the image.
     *
     * @param  startImageLine     redefines the starting image line number
     *                            (may be different than the signal indicated)
     *
     * @param  startImageElement  redefines the starting image element number
     *                            (may be different than the signal indicated)
     *
     */
    public void setImageStart(int startImageLine, int startImageElement) 
    {
        this.startImageLine = (float)startImageLine;
        this.startImageElement = (float)startImageElement;
    }

    /** 
     * specify whether the line coordinates are inverted and the line
     * offset.
     *
     * @param  line  ending line number
     *
     */
    public void setFlipLineCoordinates(int line) 
    {
        isLineFlipped = true;
        lineOffset = (float) line;
    }

    /**
     * Determine if navigation is using flipped coordinates
     *
     * @return  true if using flipped line coordinates, otherwise false
     */
    public boolean isFlippedLineCoordinates()
    {
        return isLineFlipped;
    }

    /** Get the lat,lon of the subpoint if available
    *
    * @return double[2] {lat, lon}
    *
    */
    
    public double[] getSubpoint() {
      return new double[] {Double.NaN, Double.NaN};
    }


    /**
     * Get the line offset for flipped coordinates
     *
     * @return  line offset
     */
    public double getLineOffset()
    {
        return (double) lineOffset;
    }

    /**
     * Converts line/element array values from AREA (file) to Image 
     * coordinates.  Creates new array instead of mucking with input.
     *
     * @param   linele  input line/element array in AREA coordinates
     * @return  array in Image coordinates
     */
    public double[][] areaCoordToImageCoord(double[][] linele)
    {
        return areaCoordToImageCoord(linele, null);
    }

    /**
     * Converts line/element array values from AREA (file) to Image 
     * coordinates.  Creates new array if newvals is null.
     *
     * @param   linele  input line/element array in AREA coordinates
     * @param   newvals return array - create a new array if null 
     * @return  array in Image coordinates
     */
    public double[][] areaCoordToImageCoord(double[][] linele, double[][] newvals)
    {
        if (newvals == null) newvals = new double[2][linele[0].length];
        double line;
        for (int i = 0; i < linele[0].length; i++)
        {
          if (linele[indexLine][i] == linele[indexLine][i]) {
            // account for flipped coordinates
            line = isLineFlipped ? lineOffset - linele[indexLine][i]
                                          : linele[indexLine][i];
            newvals[indexLine][i] = 
               startImageLine + (resLine * (line - startLine)) / magLine;
          }
          if (linele[indexEle][i] == linele[indexEle][i]) {
            newvals[indexEle][i] = 
               startImageElement + (resElement * (linele[indexEle][i] -
               startElement))/magElement;
          }
        }
        return newvals;
    }

    /**
     * Converts line/element array values from Image to AREA (File) 
     * coordinates.  Creates new array instead of mucking with input.
     *
     * @param   linele  input line/element array Image coordinates
     * @return  array in AREA coordinates
     */
    public double[][] imageCoordToAreaCoord(double[][] linele)
    {
        return imageCoordToAreaCoord(linele, null);
    } 

    /**
     * Converts line/element array values from Image to AREA (File) 
     * coordinates.  Creates new array if newvals is null.
     *
     * @param   linele  input line/element array Image coordinates
     * @param   newvals return array - create a new array if null 
     * @return  array in AREA coordinates
     */
    public double[][] imageCoordToAreaCoord(double[][] linele, double[][] newvals)
    {
        if (newvals == null) newvals = new double[2][linele[0].length];
        for (int i = 0; i < linele[0].length; i++)
        {
          if (linele[indexLine][i] == linele[indexLine][i]) {
            newvals[indexLine][i] = startLine + 
                ( magLine * (linele[indexLine][i] -
                  startImageLine)) / resLine;
            // account for flipped coordinates
            if (isLineFlipped) newvals[indexLine][i] = 
                         lineOffset - newvals[indexLine][i];
          }
          if (linele[indexEle][i] == linele[indexEle][i]) {
            newvals[indexEle][i] = startElement + 
                ( magElement * (linele[indexEle][i] -
                  startImageElement)) / resElement;
          }
        }
        return newvals;
    }

    /**
     * Converts line/element array values from AREA (file) to Image 
     * coordinates.  Creates new array instead of mucking with input.
     *
     * @param   linele  input line/element array in AREA coordinates
     * @return  array in Image coordinates
     */
    public float[][] areaCoordToImageCoord(float[][] linele)
    {
        return areaCoordToImageCoord(linele, null);
    } 

    /**
     * Converts line/element array values from AREA (file) to Image 
     * coordinates.  Creates new array if newvals is null.
     *
     * @param   linele  input line/element array in AREA coordinates
     * @param   newvals return array - create a new array if null 
     * @return  array in Image coordinates
     */
    public float[][] areaCoordToImageCoord(float[][] linele, float[][] newvals)
    {
        if (newvals == null) newvals = new float[2][linele[0].length];
        float line;
        for (int i = 0; i < linele[0].length; i++)
        {
           // account for flipped coordinates
           if (linele[indexLine][i] == linele[indexLine][i]) {
             line = isLineFlipped ? lineOffset - linele[indexLine][i]
                                         : linele[indexLine][i];
             newvals[indexLine][i] = 
               startImageLine + (resLine * (line - startLine)) / magLine;
           }
           if (linele[indexEle][i] == linele[indexEle][i]) {
             newvals[indexEle][i] = 
               startImageElement + (resElement * (linele[indexEle][i] -
               startElement))/magElement;
           }
        }
        return newvals;
    }

    /**
     * Converts line/element array values from Image to AREA (File) 
     * coordinates.  Creates new array instead of mucking with input.
     *
     * @param   linele  input line/element array Image coordinates
     * @return  array in AREA coordinates
     */
    public float[][] imageCoordToAreaCoord(float[][] linele)
    {
        return imageCoordToAreaCoord(linele, null);
    }

    /**
     * Converts line/element array values from Image to AREA (File) 
     * coordinates.  Creates new array if newvals is null.
     *
     * @param   linele  input line/element array Image coordinates
     * @param   newvals return array - create a new array if null 
     * @return  array in AREA coordinates
     */
    public float[][] imageCoordToAreaCoord(float[][] linele, float[][] newvals)
    {
        if (newvals == null) newvals = new float[2][linele[0].length];
        for (int i = 0; i < linele[0].length; i++)
        {
           if (linele[indexLine][i] == linele[indexLine][i]) {
             newvals[indexLine][i] = startLine + 
               ( magLine * (linele[indexLine][i] -
                 startImageLine)) / resLine;
             // account for flipped coordinates
             if (isLineFlipped) newvals[indexLine][i] = 
                          lineOffset - newvals[indexLine][i];
           }
           if (linele[indexEle][i] == linele[indexEle][i]) {
             newvals[indexEle][i] = startElement + 
               ( magElement * (linele[indexEle][i] -
                  startImageElement)) / resElement;
           }
        }
        return newvals;
    }

  /**
   * Return an AREAnav based on the input nav block.
   * @param navBlock  block to use
   * @return corresponding navigation routine.
   */
  public static AREAnav makeAreaNav(int[] navBlock) throws McIDASException {
     return (makeAreaNav(navBlock, null) ); 
  }

  public static AREAnav makeAreaNav(int[] navBlock, int[] auxBlock) 
                       throws McIDASException {
    AREAnav anav = null;
    //System.out.println("nav = " + McIDASUtil.intBitsToString(navBlock[0]));
    try
    {
        switch (navBlock[0]) {
            case GVAR:
                anav = new GVARnav(navBlock);
                break;
            case MOLL:
                anav = new MOLLnav(navBlock);
                break;
            case MSAT:
                anav = new MSATnav(navBlock);
                break;
            case MSG :
                anav = new MSGnav(navBlock);
                break;
            case MSGT:
                anav = new MSGTnav(navBlock);
                break;
            case RADR:
                anav = new RADRnav(navBlock);
                break;
            case RECT:
                anav = new RECTnav(navBlock);
                break;
            case GMSX:
                anav = new GMSXnav(navBlock);
                break;
            case GOES:
                anav = new GOESnav(navBlock);
                break;
            case GEOS:
                anav = new GEOSnav(navBlock);
                break;
            case PS:
                anav = new PSnav(navBlock);
                break;
            case MERC:
                anav = new MERCnav(navBlock);
                break;
            case LAMB:
                anav = new LAMBnav(navBlock);
                break;
            case TANC:
                anav = new TANCnav(navBlock);
                break;
            case SIN:
                anav = new SINUnav(navBlock);
                break;
            case LALO:
      //SAG          anav = new LALOnav(navBlock, auxBlock);
                break;
            case KALP:
                anav = new KALPnav(navBlock);
                break;
            case ABIS:
                anav = new ABISnav(navBlock);
                break;
            default:
                throw new McIDASException(
                     "makeAreaNav: Unknown navigation type" + navBlock[0]);
        }
    }
    catch (IllegalArgumentException excp)
    {
        throw new McIDASException( "Wrong nav block passed to AREAnav module");
    }
    return anav;
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
    // return false; WLH 13 April 2000, this broke visad.data.mcidas.TestArea
    if (obj instanceof AREAnav)
    {
        // this should really be done in the subclasses, but that will
        // have to wait for another day.
        AREAnav nav = (AREAnav) obj;
        return (resLine == nav.resLine &&
                resElement == nav.resElement &&
                magLine == nav.magLine &&
                magElement == nav.magElement &&
                startLine == nav.startLine &&
                startElement == nav.startElement &&
                startImageLine == nav.startImageLine &&
                startImageElement == nav.startImageElement &&
                isLineFlipped == nav.isLineFlipped &&
                lineOffset == nav.lineOffset);
    }
    else
    {
        return false;
    }
  }

  /**
   * Return a <code>String</code> representation of this nav module
   * @return wordy string.
   */
  public String toString() {
    String className = getClass().getName();
    int lastDot = className.lastIndexOf('.');
    className = className.substring(lastDot+1);
    return className.substring(0,className.indexOf("nav"));
  }

  /**
   * See if we can approximate by a spline.  Subclasses can override
   * @return true
   */
  public boolean canApproximateWithSpline() {
      return true;
  }

  /**
   * Convert arrays of floats to doubles
   * @param value  arrays of floats
   * @return value converted to arrays of doubles
   */
  public static double[][] floatToDouble(float[][] value) {
    if (value == null) return null;
    double[][] val = new double[value.length][];
    for (int i=0; i<value.length; i++) {
      if (value[i] == null) {
        val[i] = null;
      }
      else {
        val[i] = new double[value[i].length];
        for (int j=0; j<value[i].length; j++) {
          val[i][j] = value[i][j];
        }
      }
    }
    return val;
  }

  /**
   * Convert arrays of floats to doubles
   * @param value  arrays of floats
   * @return value converted to arrays of doubles
   */
  public static float[][] doubleToFloat(double[][] value) {
    if (value == null) return null;
    float[][] val = new float[value.length][];
    for (int i=0; i<value.length; i++) {
      if (value[i] == null) {
        val[i] = null;
      }
      else {
        val[i] = new float[value[i].length];
        for (int j=0; j<value[i].length; j++) {
          val[i][j] = (float) value[i][j];
        }
      }
    }
    return val;
  }
}
