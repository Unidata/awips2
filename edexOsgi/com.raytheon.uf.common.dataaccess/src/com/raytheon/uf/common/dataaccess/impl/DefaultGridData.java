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
package com.raytheon.uf.common.dataaccess.impl;

import java.util.Map;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.geospatial.interpolation.data.DataDestination;
import com.raytheon.uf.common.geospatial.interpolation.data.DataSource;
import com.raytheon.uf.common.time.DataTime;

/**
 * A default grid data object if factory developers do not wish to create their
 * own IGridData implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DefaultGridData implements IGridData {

    protected DataSource data;

    protected DataTime time;

    protected String parameter;

    protected Level level;

    protected Unit<?> unit;

    protected GridGeometry2D gridGeometry;

    protected Map<String, Object> attributes;

    public DefaultGridData(DataSource data, GridGeometry2D gridGeometry) {
        this.data = data;
        this.gridGeometry = gridGeometry;
    }

    @Override
    public Object getAttribute(String key) {
        Object result = null;
        if (attributes != null) {
            result = attributes.get(key);
        }
        return result;
    }

    @Override
    public DataTime getDataTime() {
        return time;
    }

    @Override
    public String getParameter() {
        return parameter;
    }

    @Override
    public Level getLevel() {
        return level;
    }

    @Override
    public GridGeometry2D getGridGeometry() {
        return gridGeometry;
    }

    @Override
    public Unit<?> getUnit() {
        return unit;
    }

    @SuppressWarnings("unchecked")
    @Override
    public DataDestination populateData(DataDestination destination) {
        if (destination == null) {
            throw new IllegalArgumentException(
                    "Data destination must not be null");
        }

        for (int x = 0; x < gridGeometry.getGridRange2D().width; x++) {
            for (int y = 0; y < gridGeometry.getGridRange2D().height; y++) {
                destination.setDataValue(data.getDataValue(x, y), x, y);
            }
        }
        return destination;
    }

    public void setDataTime(DataTime time) {
        this.time = time;
    }

    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    public void setLevel(Level level) {
        this.level = level;
    }

    public void setUnit(Unit<?> unit) {
        this.unit = unit;
    }

    public void setAttributes(Map<String, Object> attrs) {
        this.attributes = attrs;
    }

}
