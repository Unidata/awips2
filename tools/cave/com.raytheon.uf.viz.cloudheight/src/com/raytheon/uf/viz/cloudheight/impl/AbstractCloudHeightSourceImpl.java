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
package com.raytheon.uf.viz.cloudheight.impl;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.cloudheight.CloudHeightAlgorithm;
import com.raytheon.uf.viz.cloudheight.CloudHeightAlgorithm.ICloudHeightSourceImplementation;
import com.raytheon.uf.viz.cloudheight.data.SoundingSource;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Abstract class for cloud height sounding implementation. Provides async PDO
 * requesting
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractCloudHeightSourceImpl implements
        ICloudHeightSourceImplementation {

    private DataTime[] ourTimes;

    private Map<DataTime, PluginDataObject[]> pdoMap = new HashMap<DataTime, PluginDataObject[]>();

    protected final AbstractRequestableResourceData resourceData;

    protected final SoundingSource source;

    private Job requestJob;

    private long timeInterval;

    protected AbstractCloudHeightSourceImpl(SoundingSource source) {
        this.source = source;
        resourceData = new AbstractRequestableResourceData() {
            @Override
            protected AbstractVizResource<?, ?> constructResource(
                    LoadProperties loadProperties, PluginDataObject[] objects)
                    throws VizException {
                // No resource
                return null;
            }
        };
        resourceData.setMetadataMap(constructMetadataMap());
        timeInterval = getValidTimeInterval();
    }

    protected abstract HashMap<String, RequestConstraint> constructMetadataMap();

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.cloudheight.CloudHeightAlgorithm.
     * ICloudHeightSourceImplementation#getDataTimes()
     */
    @Override
    public final DataTime[] getDataTimes() {
        try {
            DataTime[] ourTimes = resourceData.getAvailableTimes();
            Arrays.sort(ourTimes);
            this.ourTimes = ourTimes;
            return ourTimes;
        } catch (VizException e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    "Error requesting grib data times", e);
        }
        return new DataTime[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.cloudheight.CloudHeightAlgorithm.
     * ICloudHeightSourceImplementation
     * #createSounding(com.vividsolutions.jts.geom.Coordinate,
     * com.raytheon.uf.common.time.DataTime)
     */
    @Override
    public final VerticalSounding createSounding(Coordinate latLon,
            DataTime rscTime) {
        DataTime[] ourTimes = this.ourTimes;
        if (rscTime == null || ourTimes == null || ourTimes.length == 0) {
            return null;
        }

        // TODO: Can we do this on a
        // Find best time to use, from A1 SatPVImageDepict
        DataTime[] timeToUse = new DataTime[1];
        long minDiff = Long.MAX_VALUE;
        for (DataTime dt : ourTimes) {
            long diff = Math.abs(rscTime.getMatchValid() - dt.getMatchValid());
            if (diff < minDiff && diff <= timeInterval) {
                minDiff = diff;
                timeToUse[0] = dt;
            } else if (diff == minDiff
                    && dt.getMatchRef() > timeToUse[0].getMatchRef()) {
                minDiff = diff;
                timeToUse[0] = dt;
            }
        }
        final DataTime time = timeToUse[0];

        if (time == null) {
            return null;
        }

        PluginDataObject[] pdos = pdoMap.get(time);
        if (pdos == null && time != null) {
            if (requestJob == null) {
                requestJob = new Job("Requesting " + source.getName() + " Data") {
                    @Override
                    protected IStatus run(IProgressMonitor monitor) {
                        long t0 = System.currentTimeMillis();
                        try {
                            PluginDataObject[] reqPdos = resourceData
                                    .getLatestPluginDataObjects(
                                            new DataTime[] { time },
                                            new DataTime[0]);
                            pdoMap.put(time, reqPdos);
                        } catch (VizException e) {
                            // Avoid error every time
                            pdoMap.put(time, new PluginDataObject[0]);
                            UFStatus.getHandler()
                                    .handle(Priority.PROBLEM,
                                            "Error requesting metadata for grib sounding",
                                            e);
                        }
                        System.out.println("Time to request pdos: "
                                + (System.currentTimeMillis() - t0) + "ms");
                        requestJob = null;
                        return Status.OK_STATUS;
                    }
                };
                requestJob.schedule();
            } else {
                return CloudHeightAlgorithm.LOADING;
            }
        }

        if (pdos != null && pdos.length > 0) {
            return createSoundingInternal(latLon, time, pdos);
        }

        return null;
    }

    protected abstract VerticalSounding createSoundingInternal(
            Coordinate latLon, DataTime time, PluginDataObject[] pdos);

    protected abstract long getValidTimeInterval();
}
