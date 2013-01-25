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
package com.raytheon.viz.gfe.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.rsc.GFEResource;

/**
 * GFE Time matcher, uses the union of visible resources available times to
 * create set of descriptor times. Has notion of a selected time which is added
 * to set of descriptor times
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GFETimeMatcher extends AbstractTimeMatcher {

    /** The selected date, added as a extra frame if not null */
    private Date selectedDate;

    /** Set of resources used in calculating the descriptor times */
    private Set<GFEResource> tmbResources = new HashSet<GFEResource>();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractTimeMatcher#redoTimeMatching(com.raytheon
     * .uf.viz.core.rsc.AbstractVizResource)
     */
    @Override
    public void redoTimeMatching(AbstractVizResource<?, ?> resource) {
        // Noop
    }

    /**
     * Time matches a single resource against the set of descriptorTimes and
     * adds times into the timeMap
     * 
     * @param descriptorTimes
     * @param timeMap
     * @param resource
     */
    private void redoTimeMatching(DataTime[] descriptorTimes,
            Map<AbstractVizResource<?, ?>, DataTime[]> timeMap,
            AbstractVizResource<?, ?> resource) {
        if (resource instanceof GFEResource) {
            GFEResource rsc = (GFEResource) resource;
            Parm parm = rsc.getParm();
            DataTime[] rscTimes = new DataTime[descriptorTimes.length];
            for (int i = 0; i < descriptorTimes.length; ++i) {
                IGridData overlapping = parm.overlappingGrid(descriptorTimes[i]
                        .getRefTime());
                if (overlapping != null) {
                    TimeRange tr = overlapping.getGridTime();
                    rscTimes[i] = new DataTime(tr.getStart().getTime(),
                            new TimeRange(tr.getStart(), tr.getEnd()));
                }
            }
            if (timeMap != null) {
                timeMap.put(rsc, rscTimes);
            }
        }
    }

    protected DataTime[] calculateDescriptorTimes(IDescriptor descriptor,
            FramesInfo currInfo, Set<GFEResource> tmbResources) {
        DataTime currTime = currInfo.getCurrentFrame();
        List<GFEResource> rscs = descriptor.getResourceList()
                .getResourcesByTypeAsType(GFEResource.class);

        // Figure list of parms from visible GFEResources
        List<Parm> parms = new ArrayList<Parm>(rscs.size());
        for (GFEResource rsc : rscs) {
            if (rsc.getProperties().isVisible()) {
                parms.add(rsc.getParm());
                tmbResources.add(rsc);
            }
        }

        // calculate time steps from parms
        SortedSet<DataTime> dateSet = new TreeSet<DataTime>();
        for (Date stepTime : calcTimeSteps(parms)) {
            dateSet.add(new DataTime(stepTime));
        }

        if (currTime != null && selectedDate == null
                && dateSet.contains(currTime) == false) {
            selectedDate = currTime.getRefTime();
        }

        // Add special selected time
        if (selectedDate != null) {
            currTime = new DataTime(selectedDate);
            dateSet.add(currTime);
        }
        return dateSet.toArray(new DataTime[0]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractTimeMatcher#redoTimeMatching(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public synchronized void redoTimeMatching(IDescriptor descriptor)
            throws VizException {
        Set<GFEResource> tmbResources = new HashSet<GFEResource>();
        FramesInfo currInfo = descriptor.getFramesInfo();
        int currIdx = currInfo.getFrameIndex();
        DataTime currTime = currInfo.getCurrentFrame();

        // Create descriptor times, for each resource, time match against them
        DataTime[] descriptorTimes = calculateDescriptorTimes(descriptor,
                currInfo, tmbResources);
        Map<AbstractVizResource<?, ?>, DataTime[]> rscTimeMap = new HashMap<AbstractVizResource<?, ?>, DataTime[]>();
        for (ResourcePair rp : descriptor.getResourceList()) {
            redoTimeMatching(descriptorTimes, rscTimeMap, rp.getResource());
        }

        // Update current displayed index
        if (descriptorTimes.length == 0) {
            currIdx = -1;
        } else if (currTime != null) {
            currIdx = Arrays.binarySearch(descriptorTimes, currTime);
            if (currIdx < 0) {
                // Fix Arrays.binarySearch returning -insertionIndex-1
                currIdx = -currIdx - 1;
            }

            // Cap index to ensure within bounds of times
            if (currIdx < 0) {
                currIdx = 0;
            } else if (currIdx >= descriptorTimes.length) {
                currIdx = descriptorTimes.length - 1;
            }
        } else {
            currIdx = 0;
        }

        FramesInfo newInfo = new FramesInfo(descriptorTimes, currIdx,
                rscTimeMap);
        descriptor.setFramesInfo(newInfo);

        this.tmbResources = tmbResources;
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
        if (tmbResources.contains(resource)) {
            // This was a time match basis resource, need to redo time matching
            try {
                redoTimeMatching(resource.getDescriptor());
            } catch (VizException e) {
                // Do not like this handle, should throw exception?
                UFStatus.getHandler().handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
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
        // Should we support this? Would need to figure out a simple time
        // matching method (would be same as how non GFEResources are handled in
        // redoTimeMatching)
        return new DataTime[0];
    }

    /**
     * @param selectedDate
     *            the selectedDate to set
     */
    public void setSelectedDate(Date selectedDate) {
        this.selectedDate = selectedDate;
    }

    /**
     * Given a list of parms, calculate the time steps between them
     * 
     * @param parms
     * @return
     */
    public static List<Date> calcTimeSteps(List<Parm> parms) {
        SortedSet<Date> dateSet = new TreeSet<Date>();
        for (Parm pi : parms) {
            IGridData[] inv = pi.getGridInventory();
            for (IGridData grid : inv) {
                dateSet.add(grid.getGridTime().getStart());

                if (!dateSet.contains(grid.getGridTime().getEnd())) {
                    for (Parm pk : parms) {
                        if (pi != pk
                                && pk.overlappingGrid(grid.getGridTime()
                                        .getEnd()) != null) {
                            dateSet.add(grid.getGridTime().getEnd());
                            break;
                        }
                    }
                }
            }
        }
        return new ArrayList<Date>(dateSet);
    }
}
