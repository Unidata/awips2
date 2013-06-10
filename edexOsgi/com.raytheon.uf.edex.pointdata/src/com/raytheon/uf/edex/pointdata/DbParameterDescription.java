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
package com.raytheon.uf.edex.pointdata;

import java.text.ParseException;
import java.text.ParsePosition;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Describes what parameters should be availbale to the point data api that can
 * be queried from the database rather than loaded from HDF5.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 27, 2011            bsteffen    Initial creation
 * May 09, 2013 1869       bsteffen    Modified D2D time series of point data to
 *                                     work without dataURI.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DbParameterDescription implements ISerializableObject {

    @XmlAttribute(name = "name", required = true)
    private String parameterName;

    @XmlAttribute(name = "queryName", required = true)
    private String queryName;

    @XmlAttribute(name = "type", required = true)
    private Type type;

    @XmlAttribute(name = "unit", required = false)
    private String unit;

    /**
     * If the units in the db are different from the units that we want the
     * point data container then set dbunit to what is in the database and unit
     * to what we want and conversion will occur.
     */
    @XmlAttribute(name = "dbunit", required = false)
    private String dbunit;

    @XmlAttribute(name = "fillValue", required = false)
    private String fillValue;

    private transient UnitConverter fromDbConverter;

    /**
     * @return the parameterName
     */
    public String getParameterName() {
        return parameterName;
    }

    /**
     * @param parameterName
     *            the parameterName to set
     */
    public void setParameterName(String parameterName) {
        this.parameterName = parameterName;
    }

    /**
     * @return the queryName
     */
    public String getQueryName() {
        return queryName;
    }

    /**
     * @param queryName
     *            the queryName to set
     */
    public void setQueryName(String queryName) {
        this.queryName = queryName;
    }

    /**
     * @return the type
     */
    public Type getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(Type type) {
        this.type = type;
    }

    /**
     * @return the unit
     */
    public String getUnit() {
        return unit;
    }

    /**
     * @param unit
     *            the unit to set
     */
    public void setUnit(String unit) {
        this.unit = unit;
    }

    public String getDbunit() {
        return dbunit;
    }

    public void setDbunit(String dbunit) {
        this.dbunit = dbunit;
    }

    /**
     * @return the fillValue
     */
    public String getFillValue() {
        return fillValue;
    }

    /**
     * @param fillValue
     *            the fillValue to set
     */
    public void setFillValue(String fillValue) {
        this.fillValue = fillValue;
    }

    /**
     * Get a converter for converting data from the units in the db to the units
     * we want in the point data container.
     * 
     * @return
     */
    public UnitConverter getUnitConverter() {
        if (fromDbConverter == null) {
            if (unit == null || dbunit == null || unit.equals(dbunit)) {
                fromDbConverter = UnitConverter.IDENTITY;
            } else {
                try {
                    Unit<?> dbunit = UnitFormat
                            .getUCUMInstance()
                            .parseProductUnit(this.dbunit, new ParsePosition(0));
                    Unit<?> unit = UnitFormat.getUCUMInstance()
                            .parseProductUnit(this.unit, new ParsePosition(0));
                    fromDbConverter = dbunit.getConverterTo(unit);
                } catch (ParseException e) {
                    fromDbConverter = UnitConverter.IDENTITY;
                }
            }
        }
        return fromDbConverter;
    }

}
