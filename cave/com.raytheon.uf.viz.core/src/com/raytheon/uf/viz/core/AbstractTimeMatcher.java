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
package com.raytheon.uf.viz.core;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Abstract time matching object, by default does nothing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractTimeMatcher implements ISerializableObject {

    /**
     * Trigger the time matcher to update time information on this resource the
     * next time redoTimeMatching is called for its descriptor. Anyone that
     * calls must call redoTimeMatcher for a descriptor before the changes take
     * affect.
     * 
     * @param resource
     */
    public abstract void redoTimeMatching(AbstractVizResource<?, ?> resource);

    /**
     * Redo the time matching
     * 
     * (Useful after changes to time matching parameters, etc)
     * 
     * @param the
     *            descriptor that contains the data
     */
    public abstract void redoTimeMatching(IDescriptor descriptor)
            throws VizException;

    /**
     * Handle removing a resource from a descriptor
     * 
     * @param resource
     * @param descriptor
     */
    public abstract void handleRemove(AbstractVizResource<?, ?> resource,
            IDescriptor descriptor);

    /**
     * Perform an initial load of PluginDataObjects utilizing the time matcher
     * 
     * @param loadProps
     * @param resourceData
     * @param descriptor
     * @return
     * @throws VizException
     */
    public abstract DataTime[] initialLoad(LoadProperties loadProps,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException;

    /**
     * Given a list of RenderableDisplays, determine the order to instantiate
     * them in to get the correct time match settings, should only be called
     * when there are no instantiated resources on any of the displays(like when
     * deserializing).
     * 
     * @param displays
     * @return
     */
    public List<AbstractRenderableDisplay> getDisplayLoadOrder(
            List<AbstractRenderableDisplay> displays) {
        // Just load them in the default order
        return displays;
    }

    /**
     * Given a list of ResourcePairs, determine the order to instantiate the
     * them in to get the correct time match settings, should only be called
     * when there are no instantiated resources in the list(like when
     * deserializing).
     * 
     * @param displays
     * @return
     */
    public List<ResourcePair> getResourceLoadOrder(List<ResourcePair> resources) {
        // Just load them in the default order
        return resources;
    }

    /**
     * Copy the time matcher data from timeMatcher
     * 
     * @param timeMatcher
     */
    public void copyFrom(AbstractTimeMatcher timeMatcher) {
        // default is do nothing
    }

    /**
     * When loading multiple resources a time matcher might try to share common
     * configuration elements between them. This function will clear any such
     * data if it exists so that the next load will create a fresh
     * configuration. This should be called whenever loading a new "batch" of
     * resources.
     */
    public void resetMultiload() {
        ;// Default does not store any multiload info.
    }

}
