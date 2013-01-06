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
package com.raytheon.uf.viz.derivparam.data;

import java.util.Collections;
import java.util.List;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;

/**
 * AbstractRequestableData is the base metadata class for derived parameters. It
 * represents a single data value for a given source/parameter/level/time/space
 * combination. As well as representing the metadata it also provides a
 * getDataValue method which should return a dataValue that can be used in
 * derived parameters to calculate other parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractRequestableData {

    protected String source;

    protected String parameter;

    protected String parameterName;

    protected Unit<?> unit;

    protected Level level;

    protected DataTime dataTime = TimeAndSpace.TIME_AGNOSTIC;

    protected ISpatialObject space = TimeAndSpace.SPACE_AGNOSTIC;

    public AbstractRequestableData() {

    }

    public AbstractRequestableData(AbstractRequestableData that) {
        this.source = that.source;
        this.parameter = that.parameter;
        this.parameterName = that.parameterName;
        this.unit = that.unit;
        this.level = that.level;
        this.dataTime = that.dataTime;
        this.space = that.space;
    }

    // Object can be a plugin specific argument that will be passed through the
    // derived parameter implementation and reach the end data nodes. For
    // example grib uses the arg as a Request for subgridding. Point data may
    // use it to hold the pdc which serves as the base data for all base
    // parameters
    public abstract Object getDataValue(Object arg) throws VizException;

    /**
     * @return the source
     */
    public String getSource() {
        return source;
    }

    /**
     * @param source
     *            the source to set
     */
    public void setSource(String source) {
        this.source = source;
    }

    /**
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

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
     * @return the unit
     */
    public Unit<?> getUnit() {
        return unit;
    }

    /**
     * @param unit
     *            the unit to set
     */
    public void setUnit(Unit<?> unit) {
        this.unit = unit;
    }

    /**
     * @return the level
     */
    public Level getLevel() {
        return level;
    }

    /**
     * @param level
     *            the level to set
     */
    public void setLevel(Level level) {
        this.level = level;
    }

    public ISpatialObject getSpace() {
        return space;
    }

    public void setSpace(ISpatialObject space) {
        this.space = space;
    }

    /**
     * @return the dataTime
     */
    public DataTime getDataTime() {
        return dataTime;
    }

    /**
     * @param dataTime
     *            the dataTime to set
     */
    public void setDataTime(DataTime dataTime) {
        this.dataTime = dataTime;
    }

    public TimeAndSpace getTimeAndSpace() {
        return new TimeAndSpace(dataTime, space);
    }

    public List<AbstractRequestableData> getDependencies() {
        return Collections.emptyList();
    }

    public String toString() {
        return source + ":" + parameter + ":" + level + ":" + dataTime;
    }

}
