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
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.FrameCoordinator;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.IFrameChangedListener;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
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
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SailsFrameCoordinator extends FrameCoordinator implements
        IFrameChangedListener {

    private final RadarFrameCache cache;

    private final RadarDisplayManager displayManager;

    public SailsFrameCoordinator(IDescriptor descriptor) {
        super(descriptor);
        cache = new RadarFrameCache();
        descriptor.addFrameChangedListener(this);
        this.displayManager = RadarDisplayManager.getInstance();
        IFrameCoordinator existing = descriptor.getFrameCoordinator();
        currentAnimationMode = existing.getAnimationMode();
    }

    protected boolean isEnabled() {
        return displayManager.getCurrentSettings().isSailsFrameCoordinator();
    }

    @Override
    protected int getLastVerticalIndex(DataTime[] frames, int dataIndex,
            IFrameValidator validator) {
        if (isEnabled()) {
            List<RadarDataTime> volumeScan = getVolumeScan(frames, dataIndex);
            Collections.sort(volumeScan, new ElevationAngleComparator<>());
            Collections.reverse(volumeScan);
            for (RadarDataTime time : volumeScan) {
                if (validator.isValid(time)) {
                    return getBestFrameIndex(frames, time, validator);
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
            Collections.sort(volumeScan, new ElevationAngleComparator<>());
            for (RadarDataTime time : volumeScan) {
                if (validator.isValid(time)) {
                    return getBestFrameIndex(frames, time, validator);
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
                Collections.sort(volumeScan, new ElevationAngleComparator<>());
                int startIdx = volumeScan.indexOf(frames[dataIndex]);
                double currentLevel = volumeScan.get(startIdx).getLevelValue();
                int idx = (startIdx + 1) % volumeScan.size();
                while (idx != startIdx) {
                    RadarDataTime time = volumeScan.get(idx);
                    if (validator.isValid(time)
                            && time.getLevelValue() != currentLevel) {
                        return getBestFrameIndex(frames, time, validator);
                    }
                    idx = (idx + 1) % volumeScan.size();
                }
            } else if (op == FrameChangeOperation.PREVIOUS) {
                List<RadarDataTime> volumeScan = getVolumeScan(frames,
                        dataIndex);
                Collections.sort(volumeScan, new ElevationAngleComparator<>());
                int startIdx = volumeScan.indexOf(frames[dataIndex]);
                double currentLevel = volumeScan.get(startIdx).getLevelValue();
                int idx = (startIdx + volumeScan.size() - 1)
                        % volumeScan.size();
                while (idx != startIdx) {
                    RadarDataTime time = volumeScan.get(idx);
                    if (validator.isValid(time)
                            && time.getLevelValue() != currentLevel) {
                        return getBestFrameIndex(frames, time, validator);
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
            Collections.sort(volumeScan, new ElevationNumberComparator());
            Collections.reverse(volumeScan);
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
            Collections.sort(volumeScan, new ElevationNumberComparator());
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
     * volume scan number and primary elevation angle. This is done by
     * consulting the cache to see if there is a previously viewed frame that
     * can be used.
     */
    private int getBestFrameIndex(DataTime[] frames, RadarDataTime example,
            IFrameValidator validator) {
        List<DataTime> framesList = Arrays.asList(frames);
        DataTime cached = cache.getLastFrameTime(example);
        int index = framesList.indexOf(cached);
        if (index >= 0 && validator.isValid(frames[index])) {
            return index;
        } else {
            return framesList.indexOf(example);
        }
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
    private static class ElevationAngleComparator<T extends DataTime>
            implements Comparator<T> {

        @Override
        public int compare(T time1, T time2) {
            return Double.compare(time1.getLevelValue(), time2.getLevelValue());
        }

    }

    /**
     * Used for sorting times based off elevation number.
     */
    private static class ElevationNumberComparator implements
            Comparator<RadarDataTime> {

        @Override
        public int compare(RadarDataTime time1, RadarDataTime time2) {
            return Integer.compare(time1.getElevationNumber(),
                    time2.getElevationNumber());
        }

    }

    @Override
    public void frameChanged(IDescriptor descriptor, DataTime oldTime,
            DataTime newTime) {
        cache.setLastFrameTime(newTime);
    }

}
