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
package com.raytheon.uf.viz.collaboration.pointdata.image;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.AbstractRemoteGraphicsEvent;
import com.raytheon.uf.viz.remote.graphics.events.IRenderEvent;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension.PointImage;

/**
 * Event for drawing a set of point images
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class DrawPointImagesEvent extends AbstractRemoteGraphicsEvent implements
        IRenderEvent {

    @DynamicSerializeElement
    private float alpha = 1.0f;

    @DynamicSerializeElement
    private Set<PointImageEvent> images = new HashSet<PointImageEvent>();

    @DynamicSerializeElement
    private Set<PointImageEvent> removals = null;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#createDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public IRenderEvent createDiffObject(IRenderEvent event) {
        DrawPointImagesEvent diffEvent = (DrawPointImagesEvent) event;
        Set<PointImageEvent> additions = new HashSet<PointImageEvent>(
                diffEvent.images);
        additions.removeAll(images);
        Set<PointImageEvent> removals = new HashSet<PointImageEvent>(images);
        removals.removeAll(diffEvent.images);
        DrawPointImagesEvent diffObject = new DrawPointImagesEvent();
        diffObject.setAlpha(diffEvent.getAlpha());
        if (additions.size() > 0) {
            diffObject.setImages(additions);
        }
        if (removals.size() > 0) {
            diffObject.setRemovals(removals);
        }
        return diffObject;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#applyDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public void applyDiffObject(IRenderEvent diffEvent) {
        DrawPointImagesEvent diffObject = (DrawPointImagesEvent) diffEvent;
        if (diffObject.removals != null) {
            images.removeAll(diffObject.removals);
        }
        if (diffObject.images != null) {
            images.addAll(diffObject.images);
        }
        alpha = diffObject.getAlpha();
    }

    public void setPointImages(Collection<PointImage> images) {
        for (PointImage image : images) {
            PointImageEvent event = new PointImageEvent();
            event.setPointImage(image);
            this.images.add(event);
        }
    }

    /**
     * @return the removals
     */
    public Set<PointImageEvent> getRemovals() {
        return removals;
    }

    /**
     * @param removals
     *            the removals to set
     */
    public void setRemovals(Set<PointImageEvent> removals) {
        this.removals = removals;
    }

    /**
     * @return the images
     */
    public Set<PointImageEvent> getImages() {
        return images;
    }

    /**
     * @param images
     *            the images to set
     */
    public void setImages(Set<PointImageEvent> images) {
        this.images = images;
    }

    /**
     * @return the alpha
     */
    public float getAlpha() {
        return alpha;
    }

    /**
     * @param alpha
     *            the alpha to set
     */
    public void setAlpha(float alpha) {
        this.alpha = alpha;
    }

}
