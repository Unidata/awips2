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
package com.raytheon.uf.viz.core.rsc.capabilities;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.viz.core.rsc.AbstractResourceData;

/**
 * A collection of persistable capabilities.
 * 
 * A capability is an inherent changeable property of the resource. This class
 * implements {@link Iterable} to easily move through the set of data.
 * 
 * Typically users interact with this class by calling getCapability(Class) and
 * hasCapability(Class). Note that getCapability(Class) should not be used to
 * test for the existance of a capability because it will automatically
 * instantiate a capability if it does not exist.
 * 
 * NOTE: This class operates as a set: only one capability of a specified type
 * may be contained at a time (e.g. only one {@link ColorableCapability}) may be
 * preset in Capabilities at a time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2009             chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class Capabilities implements Iterable<AbstractCapability> {

    private static final long serialVersionUID = 1L;

    protected HashMap<Class<? extends AbstractCapability>, AbstractCapability> backingMap;

    /**
     * Constructor
     */
    public Capabilities() {
        this.backingMap = new HashMap<Class<? extends AbstractCapability>, AbstractCapability>();
    }

    /**
     * Add a capability to the capability set given an instantiation of a
     * capability
     * 
     * @param capability
     */
    public void addCapability(AbstractCapability capability) {
        Validate.notNull(capability, "Capability must not be null");
        this.backingMap.put(capability.getClass(), capability);
        capability.capabilityChanged();
    }

    /**
     * Add a capability by instantiating the default constructor of the
     * capability
     * 
     * @param clazz
     *            the capability class to instantiate
     */
    public void addCapability(Class<? extends AbstractCapability> clazz) {
        Validate.notNull(clazz, "Capability class must not be null");
        AbstractCapability cap;
        try {
            cap = clazz.newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Error constructing capability: "
                    + clazz.getName());
        }
        this.addCapability(cap);
    }

    /**
     * Remove a capability by giving it's class
     * 
     * @param clazz
     */
    public void removeCapability(Class<? extends AbstractCapability> clazz) {
        Validate.notNull(clazz, "Capability class must not be null");
        this.backingMap.remove(clazz);
    }

    /**
     * Get a capability, and return the default instance if it does not already
     * exist.
     * 
     * End users may find AbstractVizResource.getCapability(Class) more
     * convenient.
     * 
     * @param <T>
     * @param rscData
     *            the resource data that the capability belongs to
     * @param capabilityClass
     *            the capability class
     * @return the capability object
     */
    @SuppressWarnings("unchecked")
    public <T extends AbstractCapability> T getCapability(
            AbstractResourceData rscData, Class<T> capabilityClass) {
        Validate.notNull(capabilityClass, "Capability class must not be null");

        AbstractCapability abstractCap = this.backingMap.get(capabilityClass);
        if (abstractCap != null) {
            if (abstractCap.getResourceData() == null) {
                abstractCap.setResourceData(rscData);
            }
            return (T) abstractCap;
        }

        // Construct the default case
        T capability;
        try {
            capability = capabilityClass.newInstance();
            capability.setResourceData(rscData);
            addCapability(capability);
        } catch (Exception e) {
            throw new RuntimeException(
                    "Unable to construct default instantiation of "
                            + capabilityClass.getName(), e);
        }

        return capability;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Iterable#iterator()
     */
    @Override
    public Iterator<AbstractCapability> iterator() {
        return this.backingMap.values().iterator();
    }

    /**
     * Return the collection of capabilities as an array
     * 
     * This is usually used by serialization, not end users.
     * 
     * @return
     */
    public AbstractCapability[] getCapabilityClassCollection() {
        return this.backingMap.values().toArray(
                new AbstractCapability[this.backingMap.size()]);
    }

    /**
     * Set the collection of capabilities as an array
     * 
     * This is usually used by serialization, not end users.
     * 
     * @param collection
     */
    @XmlElement(name = "capability")
    public void setCapabilityClassCollection(AbstractCapability[] collection) {
        Iterator<AbstractCapability> collectionIterator = Arrays.asList(
                collection).iterator();
        while (collectionIterator.hasNext()) {
            AbstractCapability cap = collectionIterator.next();
            this.backingMap.put(cap.getClass(), cap);
        }
    }

    /**
     * Check to see if a particular capability belongs to this capability set.
     * 
     * @param clz
     *            the capability class
     * @return true if the capability is present
     */
    public boolean hasCapability(Class<? extends AbstractCapability> clz) {
        return backingMap.containsKey(clz);
    }

    public Capabilities clone() {
        Capabilities cap = new Capabilities();
        Iterator<AbstractCapability> iter = iterator();
        while (iter.hasNext()) {
            AbstractCapability ac = iter.next().clone();
            cap.backingMap.put(ac.getClass(), ac);
        }
        return cap;
    }
}
