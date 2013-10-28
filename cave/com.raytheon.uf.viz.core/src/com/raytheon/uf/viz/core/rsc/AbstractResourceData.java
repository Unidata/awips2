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
package com.raytheon.uf.viz.core.rsc;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * A base implementation of resource data and metadata. This class is used by
 * IVizResource, which provides rendering capability. This class, unlike
 * IVizResource, is serialized and stored to provide a description of a data
 * type visualization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 02, 2009           chammack    Initial creation
 * Oct 22, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractResourceData {

    /** The generator used to generate names for labels */
    protected AbstractNameGenerator nameGenerator = null;

    /**
     * Listeners used for notifying external sources (e.g. resources) that data
     * has changed
     */
    protected transient Set<IResourceDataChanged> dataChangedListeners;

    @XmlElement
    protected DataTime frozenTime;

    /**
     * Default constructor. Required for serialization
     */
    protected AbstractResourceData() {
        this.dataChangedListeners = new CopyOnWriteArraySet<IResourceDataChanged>();
    }

    /**
     * @return the generator
     */
    public AbstractNameGenerator getNameGenerator() {
        return nameGenerator;
    }

    /**
     * @param generator
     *            the generator to set
     */
    public void setNameGenerator(AbstractNameGenerator generator) {
        this.nameGenerator = generator;
    }

    /**
     * Constructs a resource from this resource data.
     * 
     * This class is typically implemented and provides any transformations
     * necessary to go between the raw data form and the input of the renderable
     * AbstractVizResource.
     * 
     * 
     * @param loadProperties
     *            the load properties
     * @param descriptor
     *            the descriptor that the resource will be loaded onto
     * @throws VizException
     *             if construction fails
     * @return the renderable capability
     */
    public abstract AbstractVizResource<?, ?> construct(
            LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException;

    /**
     * Update a resource with new data
     * 
     * This class is typically implemented and provides any transformations
     * necessary to go between the raw data form and the input of the renderable
     * AbstractVizResource.
     * 
     * 
     * @param updateData
     *            the data to update with
     */
    public abstract void update(Object updateData);

    public void addChangeListener(IResourceDataChanged listener) {
        this.dataChangedListeners.add(listener);
    }

    public void removeChangeListener(IResourceDataChanged listener) {
        this.dataChangedListeners.remove(listener);
    }

    public void fireChangeListeners(IResourceDataChanged.ChangeType type,
            Object data) {
        for (IResourceDataChanged listener : this.dataChangedListeners) {
            listener.resourceChanged(type, data);
        }
    }

    /**
     * The time the resource should be loaded at
     * 
     * @return
     */
    public DataTime getFrozenTime() {
        return frozenTime;
    }

    /**
     * Set the time the resource should be loaded at
     * 
     * @param frozenTime
     */
    public void setFrozenTime(DataTime frozenTime) {
        this.frozenTime = frozenTime;
    }

    /**
     * Is the resource frozen
     * 
     * @return
     */
    public boolean isFrozen() {
        return this.frozenTime != null;
    }

    protected void establishListeners() {
        // if (this.capabilities != null) {
        // Iterator<AbstractCapability> iterator = this.capabilities
        // .iterator();
        // while (iterator.hasNext()) {
        // iterator.next().setResourceData(this);
        // }
        // }

    }

    /**
     * This function allows a resource data to set some internal state based off
     * the descriptor before being added to the descriptor, most resources
     * should not need this.
     * 
     * @param loadProperties
     * @param descriptor
     * @throws VizException
     */
    public void configure(LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        ;// default do nothing
    }

    @Override
    public abstract boolean equals(Object obj);
}
