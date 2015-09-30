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
package com.raytheon.viz.radar.frame;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.NavigableSet;
import java.util.TreeSet;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.FrameCoordinator;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.IFrameChangedListener;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.viz.radar.rsc.AbstractRadarResource;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

/**
 * 
 * A specialized frame coordinator which introduces specialized behavior when
 * the time match basis is a radar resource and that radar is in
 * SAILS(Supplemental Adaptive Intra-Volume Low-Level Scan) mode. Many of the
 * operations are the same as a normal {@link FrameCoordinator} except the
 * following:
 * 
 * <ul>
 * <li>When the last frame is displayed it will go to the frame with the highest
 * elevation number that is part of the last volume scan. The opposite is true
 * when going to the first frame.
 * 
 * <li>When traversing the vertical dimension all frames in the same volume scan
 * will be considered even if the reftimes are different. When choosing between
 * multiple frames with the same volume scan and elevation angle the frame that
 * was most recently viewed will be used.
 * 
 * </ul>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 13, 2015  4461     bsteffen    Initial creation
 * Sep 30, 2015  4902     bsteffen    Better determination of best frame.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SailsFrameCoordinator extends FrameCoordinator implements
        IFrameChangedListener, RemoveListener {

    private final RadarFrameCache cache;

    private final RadarDisplayManager displayManager;

    public SailsFrameCoordinator(final IDescriptor descriptor) {
        super(descriptor);
        cache = new RadarFrameCache();
        descriptor.addFrameChangedListener(this);
        this.displayManager = RadarDisplayManager.getInstance();
        IFrameCoordinator existing = descriptor.getFrameCoordinator();
        currentAnimationMode = existing.getAnimationMode();
        descriptor.getResourceList().addPostRemoveListener(this);
    }

    protected boolean isEnabled() {
        return displayManager.getCurrentSettings().isSailsFrameCoordinator();
    }

    @Override
    protected int getLastVerticalIndex(DataTime[] frames, int dataIndex,
            IFrameValidator validator) {
        if (isEnabled()) {
            List<RadarDataTime> volumeScan = getVolumeScan(frames, dataIndex);
            Collections.sort(volumeScan,
                    Collections.reverseOrder(ELEVATION_ANGLE_COMPARATOR));
            for (RadarDataTime time : volumeScan) {
                if (validator.isValid(time)) {
                    return getBestFrameIndex(frames, dataIndex, time, validator);
                }
            }
        }
        return super.getLastVerticalIndex(frames, dataIndex, validator);
    }

    @Override
    protected int getFirstVerticalIndex(DataTime[] frames, int dataIndex,
            IFrameValidator validator) {
        if (isEnabled()) {
            List<RadarDataTime> volumeScan = getVolumeScan(frames, dataIndex);
            Collections.sort(volumeScan, ELEVATION_ANGLE_COMPARATOR);
            for (RadarDataTime time : volumeScan) {
                if (validator.isValid(time)) {
                    return getBestFrameIndex(frames, dataIndex, time, validator);
                }
            }
        }
        return super.getLastVerticalIndex(frames, dataIndex, validator);
    }

    @Override
    protected int getNextVerticalIndex(DataTime[] frames, int dataIndex,
            FrameChangeOperation op, IFrameValidator validator) {
        if (isEnabled()) {
            if (op == FrameChangeOperation.NEXT) {
                List<RadarDataTime> volumeScan = getVolumeScan(frames,
                        dataIndex);
                Collections.sort(volumeScan, ELEVATION_ANGLE_COMPARATOR);
                int startIdx = volumeScan.indexOf(frames[dataIndex]);
                double currentLevel = volumeScan.get(startIdx).getLevelValue();
                int idx = (startIdx + 1) % volumeScan.size();
                while (idx != startIdx) {
                    RadarDataTime time = volumeScan.get(idx);
                    if (validator.isValid(time)
                            && time.getLevelValue() != currentLevel) {
                        return getBestFrameIndex(frames, dataIndex, time,
                                validator);
                    }
                    idx = (idx + 1) % volumeScan.size();
                }
            } else if (op == FrameChangeOperation.PREVIOUS) {
                List<RadarDataTime> volumeScan = getVolumeScan(frames,
                        dataIndex);
                Collections.sort(volumeScan, ELEVATION_ANGLE_COMPARATOR);
                int startIdx = volumeScan.indexOf(frames[dataIndex]);
                double currentLevel = volumeScan.get(startIdx).getLevelValue();
                int idx = (startIdx + volumeScan.size() - 1)
                        % volumeScan.size();
                while (idx != startIdx) {
                    RadarDataTime time = volumeScan.get(idx);
                    if (validator.isValid(time)
                            && time.getLevelValue() != currentLevel) {
                        return getBestFrameIndex(frames, dataIndex, time,
                                validator);
                    }
                    idx = (idx + volumeScan.size() - 1) % volumeScan.size();
                }
            }
        }
        return super.getNextVerticalIndex(frames, dataIndex, op, validator);
    }

    @Override
    protected int getLastDataTimeIndex(DataTime[] frames, int dataIndex,
            IFrameValidator validator) {
        int idx = super.getLastDataTimeIndex(frames, dataIndex, validator);
        if (isEnabled()) {
            List<RadarDataTime> volumeScan = getVolumeScan(frames, idx);
            Collections.sort(volumeScan,
                    Collections.reverseOrder(ELEVATION_NUMBER_COMPARATOR));
            for (RadarDataTime time : volumeScan) {
                if (validator.isValid(time)) {
                    return Arrays.asList(frames).indexOf(time);
                }
            }
        }
        return idx;
    }

    @Override
    protected int getFirstDataTimeIndex(DataTime[] frames, int dataIndex,
            IFrameValidator validator) {
        int idx = super.getFirstDataTimeIndex(frames, dataIndex, validator);
        if (isEnabled()) {
            List<RadarDataTime> volumeScan = getVolumeScan(frames, idx);
            Collections.sort(volumeScan, ELEVATION_NUMBER_COMPARATOR);
            for (RadarDataTime time : volumeScan) {
                if (validator.isValid(time)) {
                    return Arrays.asList(frames).indexOf(time);
                }
            }
        }
        return idx;
    }

    /**
     * Given an example time, find the best time in the frames that has the same
     * volume scan number and primary elevation angle. This attempts to maintain
     * the same time as the previously displayed time.
     * 
     * @param frames
     *            the frames that can be displayed
     * @param previousIndex
     *            the index of the previously displayed frame
     * @param example
     *            the RadarDataTime that was chosen from frames by one of the
     *            other methods based off of the frame change action. This
     *            method might return the index of this time or might select a
     *            better time with the same elevation angle and volume scan
     *            number.
     * @param validator
     *            object to use to ensure no invalid frames are returned.
     * @return the index of the frame that should displayed.
     */
    private int getBestFrameIndex(DataTime[] frames, int previousIndex,
            RadarDataTime example, IFrameValidator validator) {
        List<DataTime> framesList = Arrays.asList(frames);
        RadarDataTime previousDataTime = (RadarDataTime) frames[previousIndex];

        List<RadarDataTime> volumeScan = getVolumeScan(frames, previousIndex);

        NavigableSet<RadarDataTime> previousByLevel = filterByLevel(volumeScan,
                previousDataTime.getLevelValue());
        NavigableSet<RadarDataTime> nextByLevel = filterByLevel(volumeScan,
                example.getLevelValue());
        Iterator<RadarDataTime> validatingIterator = nextByLevel.iterator();
        while (validatingIterator.hasNext()) {
            if (!validator.isValid(validatingIterator.next())) {
                validatingIterator.remove();
            }

        }

        /*
         * If the previous elevation had multiple times available then ideally
         * the next elevation should choose a time between the previous time and
         * the next time at that same elevation.
         */
        RadarDataTime endRange = previousByLevel.higher(previousDataTime);
        NavigableSet<RadarDataTime> nextSet = null;
        if (endRange == null) {
            nextSet = nextByLevel.tailSet(previousDataTime, true);
        } else {
            nextSet = nextByLevel.subSet(previousDataTime, true, endRange,
                    false);
        }
        if (nextSet.isEmpty()) {
            if (endRange != null) {
                /*
                 * If there are no times within range then try getting all times
                 * after the previous time.
                 */
                nextSet = nextByLevel.tailSet(previousDataTime, true);
            }
            if (nextSet.isEmpty()) {
                /*
                 * If there is still nothing then all times at the new elevation
                 * can be considered.
                 */
                nextSet = nextByLevel;
            }
        }
        if (nextSet.isEmpty()) {
            /*
             * This is hopefully impossible, nextSet should at least contain
             * example.
             */
            return framesList.indexOf(example);
        } else if (nextSet.size() == 1) {
            /*
             * This should be very common for the case where elevations have the
             * same times or where the next elevation has fewer times than the
             * previous elevation.
             */
            return framesList.indexOf(nextSet.iterator().next());
        } else {
            /*
             * This is the case when the next elevation has more times then the
             * previous elevation.
             * 
             * Load the most recently viewed frame or if there are none then the
             * latest.
             */
            RadarDataTime time = cache.getLastFrameTime(nextSet);
            if (time == null) {
                return framesList.indexOf(nextSet.last());
            } else {
                return framesList.indexOf(time);
            }
        }
    }

    @Override
    public void frameChanged(IDescriptor descriptor, DataTime oldTime,
            DataTime newTime) {
        cache.setLastFrameTime(newTime);
    }

    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        List<AbstractVizResource<?, ?>> radarResources = descriptor
                .getResourceList().getResourcesByType(
                        AbstractRadarResource.class);
        if (radarResources.isEmpty()) {
            cache.clear();
        }
    }

    /**
     * Get a set of times sorted by reftime that all have the level value
     * provided.
     */
    private static <T extends DataTime> NavigableSet<T> filterByLevel(
            Collection<T> times, double level) {
        NavigableSet<T> filtered = new TreeSet<T>(REF_TIME_COMPARATOR);
        for (T time : times) {
            if (time.getLevelValue().doubleValue() == level) {
                filtered.add(time);
            }
        }
        return filtered;
    }

    /**
     * Get a list of all the {@link DataTime} in frames that are an instance of
     * {@link RadarDataTime} and have the same volume scan number as the
     * DataTime at the provided index.
     */
    private static List<RadarDataTime> getVolumeScan(DataTime[] frames,
            int dataIndex) {
        DataTime example = frames[dataIndex];
        if (example instanceof RadarDataTime) {
            int volumeScanNumber = ((RadarDataTime) example)
                    .getVolumeScanNumber();
            long exampleRef = example.getRefTime().getTime();
            List<RadarDataTime> result = new ArrayList<>();
            for (DataTime frame : frames) {
                if (frame instanceof RadarDataTime) {
                    RadarDataTime radarTime = (RadarDataTime) frame;
                    if (radarTime.getVolumeScanNumber() == volumeScanNumber) {
                        long timeRef = radarTime.getRefTime().getTime();
                        long diff = Math.abs(exampleRef - timeRef);
                        if (diff < TimeUtil.MILLIS_PER_HOUR) {
                            result.add(radarTime);
                        }
                    }
                }
            }
            return result;
        }
        return Collections.emptyList();
    }

    public static void addToDescriptor(IDescriptor descriptor) {
        IFrameCoordinator current = descriptor.getFrameCoordinator();
        if (current instanceof SailsFrameCoordinator) {
            return;
        }
        if (descriptor instanceof AbstractDescriptor) {
            current = new SailsFrameCoordinator(descriptor);
            ((AbstractDescriptor) descriptor).setFrameCoordinator(current);
        }
    }

    /**
     * Used for sorting times based off elevation angle.
     */
    private static Comparator<DataTime> ELEVATION_ANGLE_COMPARATOR = new Comparator<DataTime>() {

        @Override
        public int compare(DataTime time1, DataTime time2) {
            return Double.compare(time1.getLevelValue(), time2.getLevelValue());
        }

    };

    /**
     * Used for sorting times based off elevation number.
     */
    private static Comparator<RadarDataTime> ELEVATION_NUMBER_COMPARATOR = new Comparator<RadarDataTime>() {

        @Override
        public int compare(RadarDataTime time1, RadarDataTime time2) {
            return Integer.compare(time1.getElevationNumber(),
                    time2.getElevationNumber());
        }

    };

    private static Comparator<DataTime> REF_TIME_COMPARATOR = new Comparator<DataTime>() {

        @Override
        public int compare(DataTime time1, DataTime time2) {
            return time1.getRefTime().compareTo(time2.getRefTime());
        }

    };

}
