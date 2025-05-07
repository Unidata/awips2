/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.xy.crosssection.rsc;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.Future;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameData;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionImage;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;

/**
 * Resource for displaying cross sections as images. This enhances the parent
 * class' handling of data updates by continuing to display the previous image
 * while calculating the new image, rather than blanking the display during the
 * calculation.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr  2, 2024 2037091    mapeters    Initial creation
 * May 22, 2024 2037092    mapeters    Cleanup previous image when data is empty
 * Aug 20, 2024 2037631    mapeters    Wrap floats and images in new classes, undo 2037092 change
 *                                     since non-null, empty CrossSectionImage is now put in image
 *                                     map when no image can be displayed
 *
 * </pre>
 *
 * @author mapeters
 */
public class CrossSectionIntermediateImageResource
        extends CrossSectionImageResource {

    protected final Map<DataTime, CrossSectionImage> previousImageMap = new HashMap<>();

    protected final Map<DataTime, Future<CrossSectionFrameData>> previousSliceMap = new HashMap<>();

    public CrossSectionIntermediateImageResource(
            CrossSectionResourceData resourceData, LoadProperties props,
            AbstractCrossSectionAdapter<?> adapter) {
        super(resourceData, props, adapter);
        resourceData.addChangeListener((type, object) -> {
            if (type == ChangeType.DATA_UPDATE && object instanceof DataTime) {
                DataTime dt = (DataTime) object;
                dataUpdateArrived(dt);
            }
        });
        registerListener((IRefreshListener) () -> {
            cleanupPreviousMaps();
        });
    }

    /**
     * Remove entries in the previous maps that have corresponding entries in
     * the main maps.
     */
    protected void cleanupPreviousMaps() {
        synchronized (lock) {
            previousSliceMap.keySet()
                    .removeIf(time -> getSliceData(time) != null);

            Iterator<Entry<DataTime, CrossSectionImage>> imageIter = previousImageMap
                    .entrySet().iterator();
            while (imageIter.hasNext()) {
                Entry<DataTime, CrossSectionImage> entry = imageIter.next();
                if (imageMap.get(entry.getKey()) != null) {
                    imageIter.remove();
                    entry.getValue().dispose();
                }
            }
        }
    }

    @Override
    protected void disposeFrames() {
        super.disposeFrames();
        synchronized (lock) {
            previousSliceMap.clear();
            for (CrossSectionImage image : previousImageMap.values()) {
                image.dispose();
            }
            previousImageMap.clear();
        }
    }

    @Override
    protected CrossSectionImage getOrCreateImage(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        CrossSectionImage image = super.getOrCreateImage(target, paintProps);

        DataTime currentTime = paintProps.getDataTime();
        if (currentTime == null) {
            return null;
        }

        synchronized (lock) {
            if (image == null) {
                image = previousImageMap.get(currentTime);
                if (image == null) {
                    CrossSectionFrameData previousData = getSliceData(
                            currentTime, previousSliceMap);
                    if (previousData != null) {
                        image = constructImage(currentTime, previousData,
                                target);
                        previousImageMap.put(currentTime, image);
                    }
                }
            }
        }

        return image;
    }

    @Override
    public void disposeFrame(DataTime frameTime, boolean onUpdate) {
        super.disposeFrame(frameTime, onUpdate);

        if (!onUpdate) {
            synchronized (lock) {
                CrossSectionImage prevImage = previousImageMap
                        .remove(frameTime);
                if (prevImage != null) {
                    prevImage.dispose();
                }
                previousSliceMap.remove(frameTime);
            }
        }
    }

    /**
     * Handle a data update arriving for the given time.
     *
     * @param dataTime
     *            the time that's been updated
     */
    protected void dataUpdateArrived(DataTime dataTime) {
        synchronized (lock) {
            for (DataTime frameTime : resourceData
                    .getAffectedFrameTimes(dataTime)) {
                Future<CrossSectionFrameData> data = sliceMap.remove(frameTime);
                CrossSectionImage image = imageMap.remove(frameTime);

                if (data != null) {
                    previousSliceMap.put(frameTime, data);
                }
                if (image != null) {
                    CrossSectionImage previousImage = previousImageMap
                            .put(frameTime, image);
                    if (previousImage != null) {
                        previousImage.dispose();
                    }
                }

                remove(frameTime, true);
            }
        }

        issueRefresh();
    }

    @Override
    protected float[] getInspectData(DataTime time) {
        float[] data = super.getInspectData(time);
        if (data != null) {
            return data;
        }
        CrossSectionFrameData sliceData = getSliceData(time, previousSliceMap);
        if (sliceData == null || !sliceData.hasData()) {
            return null;
        }
        return sliceData.getData().get(0);
    }

    @Override
    protected CrossSectionImage getFrameRenderable(DataTime frameTime) {
        CrossSectionImage renderable = super.getFrameRenderable(frameTime);
        if (renderable == null) {
            synchronized (lock) {
                renderable = previousImageMap.get(frameTime);
            }
        }
        return renderable;
    }
}
