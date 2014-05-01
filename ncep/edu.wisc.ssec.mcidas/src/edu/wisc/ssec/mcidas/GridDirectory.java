//
// GridDirectory.java
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

import java.util.Date;

/** 
 * Class for modelling a McIDAS grid header.
 * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
 *      McIDAS Programmer's Manual</A>
 *
 * @author Don Murray
 */
public class GridDirectory
{
    /** GridDirectory block size */
    public static final int DIRSIZE = 64;
    /** Grid size (rows*columns) */
    private static final int GRIDSIZE_INDEX = 0;
    /**  number of rows */
    public static final int ROWS_INDEX = 1;
    /**  number of columns */
    public static final int COLS_INDEX = 2;
    /**  ref date */
    public static final int REFDATE_INDEX = 3;
    /**  ref time */
    public static final int REFTIME_INDEX = 4;
    /**  forecast time */
    public static final int FTIME_INDEX = 5;
    /**  param name */
    public static final int PARAM_NAME_INDEX = 6;
    /**  param scale */
    public static final int PARAM_SCALE_INDEX = 7;
    /**  param units */
    public static final int PARAM_UNITS_INDEX = 8;
    /**  level value */
    public static final int LEVEL_VALUE_INDEX = 9;
    /**  level scale */
    public static final int LEVEL_SCALE_INDEX = 10;
    /**  level unit */
    public static final int LEVEL_UNITS_INDEX = 11;
    /**  param type */
    public static final int PARAM_TYPE_INDEX = 12;
    /**  second forecast time (for time diff or average) */
    public static final int SECOND_FTIME_INDEX = 13;
    /**  second level value */
    public static final int SECOND_LEVEL_VALUE_INDEX = 14;
    /**  navigation index */
    public static final int NAV_BLOCK_INDEX = 33;
    /**  navigation length */
    public static final int NAV_BLOCK_LENGTH = 8;
    /**  grid description */
    public static final int GRID_DESCR_INDEX = 52;
    /**  grid description length*/
    public static final int GRID_DESCR_LENGTH = 12;

    private int[] dir = new int[GridDirectory.DIRSIZE];   // single directory
    private String paramName;
    private String gridDescription;
    private String paramUnitName;
    private int forecastHour;
    private Date referenceTime;
    private Date validTime;
    private Date secondTime = null;
    private double paramScale;
    private double levelValue;
    private String levelUnitName;
    private double secondLevelValue;
    private int rows;
    private int columns;
    private int paramType;
    private int[] navBlock;
    private GRIDnav gridNav = null;

    /**
     * Construct a GridDirectory from the grid directory block.
     *
     * @param  dirblock  directory block from the McIDAS grid
     */
    public GridDirectory(int[] dirblock)
        throws McIDASException
    {
        if (dirblock.length != DIRSIZE)
            throw new McIDASException("Directory is not the right size");
        System.arraycopy(
            dirblock, 0, dir, 0, DIRSIZE);

        // March down the list of parameters
        rows = dirblock[ROWS_INDEX];
        columns = dirblock[COLS_INDEX];

        // date and time values
        int refDay = dirblock[REFDATE_INDEX];
        int refHMS = dirblock[REFTIME_INDEX];
        referenceTime = 
            new Date(McIDASUtil.mcDayTimeToSecs(refDay, refHMS) * 1000);
        forecastHour = dirblock[FTIME_INDEX];
        validTime = 
            new Date( (McIDASUtil.mcDayTimeToSecs(refDay,refHMS)+
                                    (forecastHour * 3600)) * 1000 );
        // parameter values
        paramName = 
            McIDASUtil.intBitsToString(dirblock[PARAM_NAME_INDEX]).trim();
        paramScale = Math.pow(10., dirblock[PARAM_SCALE_INDEX]);
        paramUnitName = 
            McIDASUtil.intBitsToString(dirblock[PARAM_UNITS_INDEX]).trim();
        paramType = dirblock[PARAM_TYPE_INDEX];

        // level values
        levelValue = 
            dirblock[LEVEL_VALUE_INDEX] *
                Math.pow(10., dirblock[LEVEL_SCALE_INDEX]); // level * scale
        levelUnitName = 
            McIDASUtil.intBitsToString(dirblock[LEVEL_UNITS_INDEX]).trim();
        // Special case for character levels
        if (Math.abs(levelValue) > 10000 && levelUnitName.equals(""))
        {
            levelUnitName = 
                McIDASUtil.intBitsToString(dirblock[LEVEL_VALUE_INDEX]).trim();
            levelValue = 999;
        }
        if (paramType == 4 || paramType == 8) // level difference or average
        {
            secondLevelValue =
                dirblock[SECOND_LEVEL_VALUE_INDEX] *
                    Math.pow(10., dirblock[LEVEL_SCALE_INDEX]); // 2nd * scale
        }
        if (paramType == 1 || paramType == 2) // time difference or average
        {
            secondTime = 
                new Date( (McIDASUtil.mcDayTimeToSecs(refDay,refHMS)+
                     ((dirblock[SECOND_FTIME_INDEX]/10000) * 3600)) * 1000 );
                     // NB: second time is 10000*time in hours to make HHMMSS
                     // so we need to divide it out
        }

        // make the nav block
        navBlock = new int[NAV_BLOCK_LENGTH];
        System.arraycopy(
            dirblock, NAV_BLOCK_INDEX, navBlock, 0, NAV_BLOCK_LENGTH);

        // get the grid description
        int[] nameBits = new int[GRID_DESCR_LENGTH];
        System.arraycopy(
            dirblock, GRID_DESCR_INDEX, nameBits, 0, nameBits.length);
        gridDescription = 
            McIDASUtil.intBitsToString(nameBits).trim();
    }

    /**
     * Get the raw directory block
     * @return  array of the raw parameters in int form
     * @deprecated  use getDirectoryBlock
     */
    public int[] getDirBlock()
    {
        return getDirectoryBlock();
    }

    /**
     * Get the raw directory block
     * @return  array of the raw parameters in int form
     */
    public int[] getDirectoryBlock()
    {
        return dir;
    }

    /**
     * Get the name of the parameter
     * @return parameter name
     */
    public String getParamName() {
        return paramName;
    }

    /**
     * Get the grid description
     * @return grid description
     */
    public String getGridDescription() {
        return gridDescription;
    }

    /**
     * Get the scale of the parameter values
     * @return parameter scale  (power of 10)
     */
    public double getParamScale() {
        return paramScale;
    }

    /**
     * Get the unit name of the parameter values
     * @return unit name for this parameter
     */
    public String getParamUnitName() {
        return paramUnitName;
    }

    /**
     * Get the reference time for this parameter
     * @return reference time
     */
    public Date getReferenceTime() {
        return referenceTime;
    }

    /**
     * Get the valid time for this parameter if it is a forecast
     * @return valid time
     */
    public Date getValidTime() {
        return validTime;
    }

    /**
     * Get the forecast hour for this parameter if it is a forecast
     * @return forecast hour
     */
    public int getForecastHour() {
        return forecastHour;
    }

    /**
     * Get the second time for this parameter if it is a time difference
     * @return second time (null if only one time associated with this)
     */
    public Date getSecondTime() {
        return secondTime;
    }

    /**
     * Get the vertical level value.  
     * @return level  By McIDAS conventions, 1013. == mean sea level,
     *                0. == tropopause, 1001. == surface.  Otherwise,
     *                value is what is returned.
     */
    public double getLevelValue() {
        return levelValue;
    }

    /**
     * Get the units of the vertical level.
     * @return unit name of the level
     */
    public String getLevelUnitName() {
        return levelUnitName;
    }

    /**
     * Get the second vertical level value if one exists.  
     * @return level  By McIDAS conventions, 1013. == mean sea level,
     *                0. == tropopause, 1001. == surface.  Otherwise,
     *                value is what is returned.
     */
    public double getSecondLevelValue() {
        return secondLevelValue;
    }

    /**
     * Get the number of rows in the grid
     * @return  number of rows
     */
    public int getRows()
    {
        return rows;
    }

    /**
     * Get the number of columns in the grid
     * @return  number of columns
     */
    public int getColumns()
    {
        return columns;
    }

    /**
     * Get the navigation parameters.
     * @return  array of nav parameters.  The first value is the grid type
     *          and subsequent values provide the parameters for that type.
     * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
     *      McIDAS Programmer's Manual</A> for a description
     * @see #getNavType()
     */
    public int[] getNavBlock()
    {
        return navBlock;
    }

    /**
     * Get the navigation 
     * @return  GRIDnav for this grid  (may be null)
     */
    public GRIDnav getNavigation()
    {
      if (gridNav == null) {
        // make the nav module
        try {
          gridNav = new GRIDnav(getDirectoryBlock());
        } catch (McIDASException excp) {
          gridNav = null;
        }
      }
      return gridNav;
    }

    /**
     * Get the navigation type.  Type is stored as first element of
     * the nav block.
     * @return  nav type
     * <pre>
     *          types are:
     *          1 = pseudo-Mercator
     *          2 = Polar Stereographic or Lambert Conformal
     *          3 = Equidistant
     *          4 = pseudo-Mercator (more general)
     *          5 = no navigation
     *          6 = Lambert Conformal Tangent Cone
     * </pre>
     * @see #getNavBlock()
     */
    public int getNavType()
    {
        return navBlock[0];
    }

    /**
     * Check the equality of the object in question with this.
     * @param o object in question
     */
    public boolean equals(Object o) {
       if (!(o instanceof GridDirectory)) return false;
       GridDirectory that = (GridDirectory) o;
       return (this == that ||
              java.util.Arrays.equals(
                   getDirectoryBlock(), that.getDirectoryBlock()));
    }

    /**
     * String representation of the GridDirectory
     * @return  human readable string
     */
    public String toString()
    {
        StringBuffer buff = new StringBuffer();
        buff.append("Grid Directory:");
        buff.append("\n");
        buff.append("\tParameter = ");
        buff.append(paramName + " [" + paramUnitName + "] (");
        buff.append(gridDescription +")");
        buff.append("\n");
        buff.append("\trefTime: ");
        buff.append(referenceTime.toGMTString());
        buff.append(" valid: "+validTime.toGMTString());
        buff.append("\n");
        buff.append("\tsecond Time: ");
        buff.append( (secondTime != null) ? secondTime.toGMTString() : "none");
        buff.append("\n");
        buff.append("\tLevel: ");
        buff.append(levelValue+" ["+levelUnitName+"] second: "+secondLevelValue);
        buff.append("\n");
        buff.append("\tNav Type: ");
        buff.append(getNavType());
        buff.append(" rows: " + rows);
        buff.append(" cols: " + columns);
        buff.append("\n");
        return buff.toString();
    }
}
