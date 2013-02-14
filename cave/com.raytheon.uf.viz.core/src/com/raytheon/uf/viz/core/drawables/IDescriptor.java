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
package com.raytheon.uf.viz.core.drawables;

import java.util.Map;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;

/**
 * 
 * IDescriptor
 * 
 * General Interface that describes the interface to renderable displays
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Sep 4, 2007              chammack    Initial Creation.
 *    Oct 22, 2009   #3348     bsteffen    added ability to limit number of frames
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public interface IDescriptor extends IResourceGroup {

    public static class FramesInfo {

        boolean setIndex = false;

        boolean setFrames = false;

        boolean setMap = false;

        DataTime[] frameTimes = null;

        int frameIndex;

        Map<AbstractVizResource<?, ?>, DataTime[]> timeMap;

        /**
         * Constructor to use if wanting to change frames, index, and time map
         * 
         * @param frameTimes
         * @param frameIndex
         */
        public FramesInfo(DataTime[] frameTimes, int frameIndex,
                Map<AbstractVizResource<?, ?>, DataTime[]> timeMap) {
            this.frameTimes = frameTimes;
            this.frameIndex = frameIndex;
            this.timeMap = timeMap;
            setFrames = true;
            setIndex = true;
            setMap = true;
        }

        /**
         * Constructor to use if wanting to change both frames and index
         * 
         * @param frameTimes
         * @param frameIndex
         */
        public FramesInfo(DataTime[] frameTimes, int frameIndex) {
            this.frameTimes = frameTimes;
            this.frameIndex = frameIndex;
            setFrames = true;
            setIndex = true;
        }

        public FramesInfo(Map<AbstractVizResource<?, ?>, DataTime[]> timeMap) {
            this.timeMap = timeMap;
            this.setMap = true;
        }

        /**
         * Constructor to use if only wanting to change frames
         * 
         * @param frameTimes
         */
        public FramesInfo(DataTime[] frameTimes) {
            this.frameTimes = frameTimes;
            setFrames = true;
        }

        /**
         * Constructor to use if only wanted to change index
         * 
         * @param frameIndex
         */
        public FramesInfo(int frameIndex) {
            this.frameIndex = frameIndex;
            setIndex = true;
        }

        public DataTime[] getFrameTimes() {
            return frameTimes;
        }

        public int getFrameIndex() {
            return frameIndex;
        }

        public int getFrameCount() {
            return (frameTimes == null ? 0 : frameTimes.length);
        }

        public Map<AbstractVizResource<?, ?>, DataTime[]> getTimeMap() {
            return timeMap;
        }

        public DataTime getTimeForResource(AbstractVizResource<?, ?> rsc) {
            return getTimeForResource(rsc, getFrameIndex());
        }

        public DataTime getTimeForResource(AbstractVizResource<?, ?> rsc,
                int idx) {
            DataTime[] dt = timeMap.get(rsc);
            return getFrame(dt, idx);
        }

        public DataTime getCurrentFrame() {
            return getFrame(frameTimes, frameIndex);
        }

        private DataTime getFrame(DataTime[] frames, int idx) {
            if (frames == null
                    || frames.length <= idx
                    || idx < 0
                    || (frameTimes != null && frameTimes.length > idx
                            && frameTimes[idx] != null && !frameTimes[idx]
                            .isVisible())) {
                return null;
            }
            return frames[idx];
        }
    }

    /** The default width of the world in pixels */
    public static final int DEFAULT_WORLD_WIDTH = 20000;

    /** The default height of the world in pixels */
    public static final int DEFAULT_WORLD_HEIGHT = 10000;

    /**
     * DEPRECATED, use IFrameCoordinator.FrameChangeOperation and call
     * getFrameCoordinator().changeFrame(...)
     * 
     * Possible operations when changing frames:
     * 
     * FIRST - The first possible frame LAST - The last possible frame NEXT -
     * The next sequential frame PREVIOUS - The previous sequential frame
     */
    @Deprecated
    public static enum FrameChangeOperation {
        FIRST, LAST, NEXT, PREVIOUS
    };

    /**
     * DEPRECATED, use IFrameCoordinator.FrameChangeMode and call
     * getFrameCoordinator().changeFrame(...)
     * 
     * Possible modes for changing frames
     * 
     * TIME_ONLY - Advance only using time (ignore/stationary space) SPACE_ONLY
     * - Advance only in space (ignore/stationary time) TIME_AND_SPACE - Advance
     * in time and space (the highest spatial level
     */
    @Deprecated
    public static enum FrameChangeMode {
        TIME_ONLY, SPACE_ONLY, TIME_AND_SPACE
    };

    public static interface IFrameChangedListener {
        void frameChanged(IDescriptor descriptor, DataTime oldTime,
                DataTime newTime);
    }

    /**
     * Add a frame change listener to the descriptor
     * 
     * @param listener
     */
    public void addFrameChangedListener(IFrameChangedListener listener);

    /**
     * Remove a frame change listener from the descriptor
     * 
     * @param listener
     */
    public void removeFrameChangedListener(IFrameChangedListener listener);

    /**
     * Use getFramesInfo() then use getFrameCount() on FramesInfo
     * 
     * Get the number of time frames in the descriptor
     * 
     * @return the frame count
     */
    @Deprecated
    public abstract int getFrameCount();

    /**
     * Use getFramesInfo() for thread safe use!
     * 
     * Get the current frame of the map descriptor
     * 
     * @return the current frame
     */
    @Deprecated
    public abstract int getCurrentFrame();

    /**
     * Use setFramesInfo(...) for thread safe use!
     * 
     * Set the current frame of the map descriptor
     * 
     * @param frame
     *            the current frame number
     */
    @Deprecated
    public abstract void setFrame(int frame);

    /**
     * Use getFramesInfo() for thread safe use!
     * 
     * Return the times for frames
     * 
     * @return
     */
    @Deprecated
    public DataTime[] getFrames();

    /**
     * Get coordinate reference system
     * 
     * @return the coordinate referece system
     */
    public abstract CoordinateReferenceSystem getCRS();

    /**
     * Set the number of frames in the display
     * 
     * @param frameCount
     *            the number of frames
     */
    public abstract void setNumberOfFrames(int frameCount);

    /**
     * Return the number of frames in the display
     * 
     * @return the number of frames
     */
    public abstract int getNumberOfFrames();

    /**
     * Limit the number of frames that will actually be displayed to
     * min(frameCount, this.frameCount). This function should be used if so the
     * descriptor can remember the "real" number of frames when you call
     * unlimitNumberOfFrames. If you need to control numberOfFrames you should
     * probably be calling setNumberOfFrames instead
     * 
     * @param frameCount
     *            the maximum number of frames to displayed
     * @return true if this effects the number of frames displayed(you need to
     *         redo time matching)
     */
    public abstract boolean limitNumberOfFrames(int frameCount);

    /**
     * remove the limit on the number of frames displayed
     * 
     * @return true if this effects the number of frames displayed(you need to
     *         redo time matching)
     */
    public abstract boolean unlimitNumberOfFrames();

    /**
     * Use getFramesInfo() for thread safe use!
     * 
     * Get the DataTimes of all of the frames of the display
     * 
     * @return the dataTimes
     */
    @Deprecated
    public DataTime[] getDataTimes();

    /**
     * Use setFramesInfo(...) for thread safe use!
     * 
     * Set the data times
     * 
     * @param dataTimes
     */
    @Deprecated
    public void setDataTimes(DataTime[] dataTimes);

    /**
     * Return the grid geometry
     * 
     * @return the grid geometry
     */
    public abstract GeneralGridGeometry getGridGeometry();

    /**
     * Set the geometry for the descriptor
     * 
     * @param geometry
     * @throws VizException
     */
    public abstract void setGridGeometry(GeneralGridGeometry geometry)
            throws VizException;

    /**
     * DEPRECATED, use getFrameCoordinator().changeFrame(...) with
     * IFrameCoordinator.FrameChangeOperation/FrameChangeMode
     * 
     * Change a frame given a specified operation mode
     * 
     * @param operation
     *            the operation to perform (see FrameChangeOperation)
     * 
     * @param mode
     *            the mode to use (see FrameChangeMode)
     */
    @Deprecated
    public abstract void changeFrame(FrameChangeOperation operation,
            FrameChangeMode mode);

    /**
     * Convenience method to transform a set of pixel coordinates to world
     * coordinates
     * 
     * @param pixel
     *            the pixel coordinates (x, y)
     * @return the world coordinates (x, y)
     */
    public abstract double[] pixelToWorld(double[] pixel);

    /**
     * Convenience method to transform a set of world coordinates to pixel
     * coordinates
     * 
     * @param worldPixel
     *            an array of two of world coordinates (x, y)
     * @return the pixel coordinates (x, y)
     */
    public abstract double[] worldToPixel(double[] worldPixel);

    /**
     * @return the timeMatcher
     */
    public AbstractTimeMatcher getTimeMatcher();

    /**
     * @param timeMatcher
     *            the timeMatcher to set
     */
    public void setTimeMatcher(AbstractTimeMatcher timeMatcher);

    /**
     * Re-does time matching for the descriptor
     * 
     * @throws VizException
     */
    public void redoTimeMatching() throws VizException;

    /**
     * Synchronize time matching with the other descriptor
     * 
     * @param other
     */
    public void synchronizeTimeMatching(IDescriptor other);

    /**
     * Determine what time the resource should be drawn at
     * 
     * @param rsc
     * @return
     */
    public DataTime getTimeForResource(AbstractVizResource<?, ?> rsc);

    /**
     * Get the renderable display the descriptor is loaded to
     * 
     * @return
     */
    public IRenderableDisplay getRenderableDisplay();

    /**
     * Set the renderable display for the descriptor, NOTE: descriptor should be
     * == to descriptor.getRenderableDisplay().getDescriptor()
     * 
     * @param display
     *            display to set
     * 
     */
    public void setRenderableDisplay(IRenderableDisplay display);

    /**
     * determine if this descriptor can load resources from another descriptor
     * 
     * @param other
     * @return
     */
    public boolean isCompatible(IDescriptor other);

    /**
     * Thread safe method of setting the frame information including frame times
     * and/or frame index.
     * 
     * @param info
     */
    public void setFramesInfo(FramesInfo info);

    /**
     * Thread safe method of getting the frame information including frame times
     * and frame index
     * 
     * @return
     */
    public FramesInfo getFramesInfo();

    /**
     * Get the frame coordination object
     * 
     * @return the frame coordination object for the descriptor
     */
    public IFrameCoordinator getFrameCoordinator();
}