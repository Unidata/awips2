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
package com.raytheon.viz.mpe.ui.displays;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.mpe.core.MPEDataManager;

/**
 * MPE time matcher, generates times in hourly increments from
 * {@link MPEDataManager}'s latest time to earliest and uses those times for all
 * resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MPETimeMatcher extends AbstractTimeMatcher {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractTimeMatcher#redoTimeMatching(com.raytheon
     * .uf.viz.core.rsc.AbstractVizResource)
     */
    @Override
    public void redoTimeMatching(AbstractVizResource<?, ?> resource) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractTimeMatcher#redoTimeMatching(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void redoTimeMatching(IDescriptor descriptor) throws VizException {
        MPEDataManager dm = MPEDataManager.getInstance();
        // Add all times from MPEDataManager.earliest to latest in hourly
        // increments
        Date latest = dm.getLatestDate();
        Date earliest = dm.getEarliestDate();
        Calendar cal = Calendar.getInstance();
        cal.setTime(earliest);
        List<DataTime> frameTimes = new ArrayList<DataTime>();
        Date time = cal.getTime();
        do {
            frameTimes.add(new DataTime(time));
            cal.add(Calendar.HOUR_OF_DAY, 1);
            time = cal.getTime();
        } while (latest.before(time) == false);
        // Get curr info and time
        FramesInfo currInfo = descriptor.getFramesInfo();
        DataTime currTime = currInfo.getCurrentFrame();

        DataTime[] times = frameTimes.toArray(new DataTime[0]);
        Map<AbstractVizResource<?, ?>, DataTime[]> rscTimeMap = new HashMap<AbstractVizResource<?, ?>, DataTime[]>();
        for (ResourcePair rp : descriptor.getResourceList()) {
            if (rp.getResource() != null) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc.isTimeAgnostic() == false) {
                    // for now, just put frame times of descriptor in each
                    // resource, maybe add time availability in MPE
                    rscTimeMap.put(rsc, Arrays.copyOf(times, times.length));
                }
            }
        }

        int newIdx = times.length - 1;
        if (currTime != null) {
            newIdx = Arrays.binarySearch(times, currTime);
            if (newIdx < 0) {
                // Fix Arrays.binarySearch returning -insertionIndex-1
                newIdx = -newIdx - 1;
            }

            // Cap index to ensure within bounds of times
            if (newIdx < 0) {
                newIdx = 0;
            } else if (newIdx >= times.length) {
                newIdx = times.length - 1;
            }
        }
        descriptor.setFramesInfo(new FramesInfo(times, newIdx, rscTimeMap));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractTimeMatcher#handleRemove(com.raytheon
     * .uf.viz.core.rsc.AbstractVizResource,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void handleRemove(AbstractVizResource<?, ?> resource,
            IDescriptor descriptor) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractTimeMatcher#initialLoad(com.raytheon
     * .uf.viz.core.rsc.LoadProperties, com.raytheon.uf.common.time.DataTime[],
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public DataTime[] initialLoad(LoadProperties loadProps,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException {
        return new DataTime[0];
    }

}
