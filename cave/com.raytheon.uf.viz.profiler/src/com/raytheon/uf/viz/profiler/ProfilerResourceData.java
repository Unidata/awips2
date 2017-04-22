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
package com.raytheon.uf.viz.profiler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.profiler.ProfilerObs;
import com.raytheon.uf.common.dataplugin.profiler.dao.ProfilerDataTransform;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.datacube.DataCubeContainer;

/**
 * ProfilerResourceData
 * 
 * Implements drawing for profiler data
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    08Apr2009    2219        dhladky    Initial Creation.
 *    09Sep2013    2277        mschenke   Got rid of ScriptCreator references
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "profilerResourceData")
public class ProfilerResourceData extends AbstractRequestableResourceData {

    protected List<ProfilerObs> records;

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        records = new ArrayList<ProfilerObs>(objects.length);
        if (objects.length > 0) {

            for (PluginDataObject p : objects) {
                records.add((ProfilerObs) p);
            }
        }
        return new ProfilerResource(this, loadProperties);
    }

    @Override
    protected PluginDataObject[] requestPluginDataObjects(
            Collection<DataTime> loadSet) throws VizException {
        List<String> dataTimes = new ArrayList<String>(loadSet.size());
        for (DataTime time : loadSet) {
            dataTimes.add(time.toString());
        }
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>(
                getMetadataMap());
        constraints.put(PluginDataObject.DATATIME_ID, new RequestConstraint(
                dataTimes));
        PointDataContainer pdc;
        try {
            pdc = DataCubeContainer.getPointData(ProfilerObs.PLUGIN_NAME,
                    ProfilerDataTransform.MAN_PARAMS, constraints);
        } catch (DataCubeException e) {
            throw new VizException(e);
        }
        if (pdc != null) {
            return ProfilerDataTransform.toProfilerRecords(pdc);
        }
        return null;
    }

    @Override
    protected void update(AlertMessage... messages) {
        invalidateAvailableTimesCache();
    }

    /**
     * @return the records
     */
    public List<ProfilerObs> getRecords() {
        return records;
    }

    /**
     * @param records
     *            the records to set
     */
    public void setRecords(List<ProfilerObs> records) {
        this.records = records;
    }

}
