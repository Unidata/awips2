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
package com.raytheon.uf.viz.xy.timeseries.adapter;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResourceData;
import com.raytheon.viz.core.graphing.xy.XYDataList;

/**
 * Adapts data for rendering on time series displays.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 07, 2010            bsteffen    Initial creation
 * Jun 18, 2014 3242       njensen     Added getEnsembleId()
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractTimeSeriesAdapter<T extends PluginDataObject> {

    protected TimeSeriesResourceData resourceData;

    protected Set<T> records = new HashSet<T>();

    protected RGB color;

    protected DisplayType displayType;

    /**
     * @param resourceData
     *            the resourceData to set
     */
    public void setResourceData(TimeSeriesResourceData resourceData) {
        this.resourceData = resourceData;
    }

    /**
     * @param color
     *            the color to set
     */
    public void setColor(RGB color) {
        this.color = color;
    }

    /**
     * @param displayType
     *            the displayType to set
     */
    public void setDisplayType(DisplayType displayType) {
        this.displayType = displayType;
    }

    public abstract XYDataList loadData() throws VizException;

    public abstract XYDataList loadRecord(PluginDataObject pdo)
            throws VizException;

    public abstract SingleLevel getLevel();

    public abstract Unit<?> getDataUnit();

    public abstract String getParameterName();

    @SuppressWarnings("unchecked")
    public void addRecord(PluginDataObject pdo) {
        synchronized (records) {
            records.add((T) pdo);
        }
    }

    public boolean hasRecord(PluginDataObject pdo) {
        return records.contains(pdo);
    }

    public void remove(DataTime time) {
        synchronized (records) {
            Iterator<T> itr = records.iterator();
            while (itr.hasNext()) {
                if (itr.next().getDataTime().equals(time)) {
                    itr.remove();
                }
            }
        }
    }

    /**
     * Get the ensemble id if applicable, otherwise return null
     * 
     * @return
     */
    public String getEnsembleId() {
        return null;
    }

}
