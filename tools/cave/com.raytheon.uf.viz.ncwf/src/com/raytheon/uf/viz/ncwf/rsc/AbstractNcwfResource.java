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
package com.raytheon.uf.viz.ncwf.rsc;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.ncwf.BUFRncwf;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.grid.rsc.DataMappedGridResource;

/**
 * 
 * Some logic shared by Movement and Polygon Resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @param <T>
 */
public abstract class AbstractNcwfResource<T extends AbstractResourceData>
        extends AbstractVizResource<T, MapDescriptor> {

    protected String resourceName;

    protected AbstractNcwfResource(T resourceData, LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(new IResourceDataChanged() {
            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    PluginDataObject[] pdo = (PluginDataObject[]) object;
                    for (PluginDataObject p : pdo) {
                        if (p instanceof BUFRncwf) {
                            addRecord((BUFRncwf) p);
                        }
                    }
                }
                issueRefresh();
            }

        });
        this.dataTimes = new ArrayList<DataTime>();
    }

    @Override
    public String getName() {
        DataMappedGridResource gridResource = getGridResource();
        if (gridResource == null) {
            return resourceName;
        }
        // If a grid Resource is available we need to add our own time since we
        // are time agnostic
        DataTime time = descriptor.getTimeForResource(this);
        if (time == null) {
            time = descriptor.getTimeForResource(gridResource);
        }
        if (time == null) {
            return "No Data Available";
        } else {
            return resourceName + " " + time.getLegendString();
        }
    }

    @Override
    public boolean isTimeAgnostic() {
        // If a grid resource is available then act Time Agnostic
        return getGridResource() != null;
    }

    @Override
    public DataTime[] getDataTimes() {
        if (this.dataTimes == null) {
            return new DataTime[0];
        }
        return this.dataTimes.toArray(new DataTime[this.dataTimes.size()]);
    }

    private DataMappedGridResource getGridResource() {
        List<DataMappedGridResource> gridResources = descriptor.getResourceList()
                .getResourcesByTypeAsType(DataMappedGridResource.class);
        if (gridResources.isEmpty()) {
            return null;
        } else {
            return gridResources.get(0);
        }
    }

    protected abstract void addRecord(BUFRncwf p);

}
