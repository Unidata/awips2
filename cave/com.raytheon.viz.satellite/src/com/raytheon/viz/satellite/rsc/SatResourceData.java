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
package com.raytheon.viz.satellite.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.satellite.inventory.SatelliteDataCubeAdapter;

/**
 * Resource data for satellite data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 17, 2009           njensen     Initial creation
 * Feb 20, 2000	 2032	  jsanchez	  Added @XmlAccessorType(XmlAccessType.NONE).
 * Apr 23, 2013  2947     bsteffen    Fix updates for derived products with
 *                                    multiple records per frame.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class SatResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatResourceData.class);

    public SatelliteRecord[] records;

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.comm.LoadProperties,
     * com.raytheon.edex.db.objects.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        records = new SatelliteRecord[objects.length];
        for (int i = 0; i < objects.length; i++) {
            records[i] = (SatelliteRecord) objects[i];
        }
        return new SatResource(this, loadProperties);
    }


    @Override
    public void update(Object updateData) {
        if (updateData instanceof PluginDataObject[]) {
            /*
             * This is here because derived updates will send us records that we
             * don't want, so filter them.
             */
            PluginDataObject[] pdos = (PluginDataObject[]) updateData;
            Set<DataTime> invalidTimes = new HashSet<DataTime>();
            for (PluginDataObject pdo : (PluginDataObject[]) updateData) {
                try {
                    Map<String, Object> pdoMap = RecordFactory.getInstance()
                            .loadMapFromUri(pdo.getDataURI());
                    for (Entry<String, RequestConstraint> entry : metadataMap
                            .entrySet()) {
                        if (entry.getKey().equals(
                                SatelliteDataCubeAdapter.DERIVED)) {
                            continue;
                        }
                        Object pdoItem = pdoMap.get(entry.getKey());
                        RequestConstraint rc = entry.getValue();
                        /*
                         * Record Factory automatically replaces space with
                         * underscore, but some derived parameters have
                         * underscore in them
                         */
                        String pdoItemStr = pdoItem.toString()
                                .replace(" ", "_");
                        if (!(rc.evaluate(pdoItem) || rc
                                        .evaluate(pdoItemStr))) {
                            DataTime time = pdo.getDataTime();
                            if (binOffset != null) {
                                time = binOffset.getNormalizedTime(time);
                            }
                            invalidTimes.add(time);
                            break;
                        }
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
            if (!invalidTimes.isEmpty()) {
                /* Next time query should requery */
                invalidateAvailableTimesCache();
                /* Remove times from resources where three is new derived data. */
                for (DataTime time : invalidTimes) {
                    fireChangeListeners(ChangeType.DATA_REMOVE, time);
                }
                /*
                 * Don't send updates for PDO's with invalidTimes, the time
                 * matcher will pull in all the records including derived
                 * records.
                 */
                List<PluginDataObject> pdoList = new ArrayList<PluginDataObject>(
                        Arrays.asList(pdos));
                Iterator<PluginDataObject> it = pdoList.iterator();
                while (it.hasNext()) {
                    DataTime t = it.next().getDataTime();
                    if (binOffset != null) {
                        t = binOffset.getNormalizedTime(t);
                    }
                    if (invalidTimes.contains(t)) {
                        it.remove();
                    }
                }
                if (pdoList.isEmpty()) {
                    return;
                } else {
                    updateData = pdoList.toArray(new PluginDataObject[0]);
                }
            }
        }
        super.update(updateData);
    }

    /**
     * @return the records
     */
    public SatelliteRecord[] getRecords() {
        return records;
    }

    /**
     * @param records
     *            the records to set
     */
    public void setRecords(SatelliteRecord[] records) {
        this.records = records;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        // under some circumstances obj might consider itself equal to this, so
        // just let it decide.
        if (obj instanceof SatBestResResourceData) {
            return obj.equals(this);
        }
        return super.equals(obj);
    }

}
