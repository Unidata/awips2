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
package com.raytheon.uf.viz.xy.crosssection.adapter;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.uf.viz.xy.crosssection.rsc.CrossSectionResourceData;
import com.raytheon.viz.core.graphing.xy.XYData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 23, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractCrossSectionAdapter<T extends PluginDataObject>
        implements Serializable {

    private static final long serialVersionUID = 1L;

    protected CrossSectionResourceData resourceData;

    protected CrossSectionDescriptor descriptor;

    protected List<T> records = new ArrayList<T>();

    /**
     * @param resourceData
     *            the resourceData to set
     */
    public void setResourceData(CrossSectionResourceData resourceData) {
        this.resourceData = resourceData;
    }

    /**
     * @param descriptor
     *            the descriptor to set
     */
    public void setDescriptor(CrossSectionDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    public abstract List<float[]> loadData(DataTime currentTime,
            CrossSectionGraph graph, GridGeometry2D geometry)
            throws VizException;

    public abstract Unit<?> getUnit();

    public abstract String getParameterName();

    @SuppressWarnings("unchecked")
    public void addRecord(PluginDataObject pdo) {
        synchronized (records) {
            records.add((T) pdo);
        }
    }

    public void remove(DataTime time) {
        Iterator<T> itr = records.iterator();
        while (itr.hasNext()) {
            if (itr.next().getDataTime().equals(time)) {
                itr.remove();
            }
        }
    }

    public void sortData(List<XYData> data) {
        Collections.sort(data, new Comparator<XYData>() {

            @Override
            public int compare(XYData o1, XYData o2) {
                return Float.compare((Float) o1.getY(), (Float) o2.getY());
            }

        });
    }

}
