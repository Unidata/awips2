/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.dataaccess.geom;

import java.util.Set;

import javax.measure.converter.ConversionException;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataaccess.IData;
import com.vividsolutions.jts.geom.Geometry;

/**
 * An IGeometryData represents data of potentially multiple parameters
 * associated with a single geometry, typically a point or polygon, at a single
 * level.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface IGeometryData extends IData {

    public static enum Type {
        STRING, INT, LONG, FLOAT, DOUBLE;
    };

    /**
     * Gets the geometry associated with this instance of data. The geometry
     * coordinates are in Lat/Lon space.
     * 
     * @return the geometry of the data
     */
    public Geometry getGeometry();

    /**
     * Gets the list of parameters associated with this instance of data
     * 
     * @return the parameters on this instance of data
     */
    public Set<String> getParameters();

    /**
     * Gets the string value of a particular parameter.
     * 
     * @param param
     *            the parameter to get the value of
     * @return the string value of the parameter
     */
    public String getString(String param);

    /**
     * Gets the number value of a particular parameter
     * 
     * @param param
     *            the parameter to get the value of
     * @return the number value of the parameter
     */
    public Number getNumber(String param);

    /**
     * Gets the number value of a particular parameter converted to the
     * specified unit. Will throw ConversionException if the units are
     * incompatible or the data has no unit.
     * 
     * @param param
     *            the parameter to get the value of
     * @param unit
     *            the unit to get the value as
     * @return the number value of a parameter, converted to the specified unit
     * @throws ConversionException
     */
    public Number getNumber(String param, Unit<?> unit)
            throws ConversionException;

    /**
     * Gets the unit associated with a particular parameter. May be null.
     * 
     * @param param
     *            the parameter to get the unit of
     * @return the unit of the parameter
     */
    public Unit<?> getUnit(String param);

    /**
     * Gets the type of a particular parameter.
     * 
     * @param param
     *            the parameter to get the type of
     * @return the type as specified in IGeometryData.type
     */
    public Type getType(String param);

    /**
     * Gets the location name associated with this instance of IData. May be
     * null.
     * 
     * @return the location name or null
     */
    public String getLocationName();

}
