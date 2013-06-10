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
package com.raytheon.uf.viz.core.rsc.tools;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.ColorUtil;

/**
 * Generic tool resource data, constructs resource using reflection
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2013       1638 mschenke    Renamed to better represent purpose
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GenericToolsResourceData<T extends AbstractVizResource<?, ?>>
        extends AbstractResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GenericToolsResourceData.class);

    /**
     * The name of the tools resource layer.
     */
    @XmlElement
    private String name;

    /**
     * The Class type of T.
     */
    private Class<? extends T> classT;

    public GenericToolsResourceData() {
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return name;
            }
        };
    }

    /**
     * 
     */
    public GenericToolsResourceData(String aName, Class<? extends T> classT) {
        this();
        this.name = aName;
        this.classT = classT;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.comm.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public T construct(LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        try {
            Class<?>[] ctorParams = new Class<?>[] { this.getClass(),
                    LoadProperties.class };
            Object[] ctorArgs = new Object[] { this, loadProperties };

            Constructor<? extends T> ctor = null;
            try {
                ctor = classT.getConstructor(ctorParams);
            } catch (NoSuchMethodException e) {
                // check for ctor with additional descriptor parameter
                ctorParams = new Class<?>[] { this.getClass(),
                        LoadProperties.class, MapDescriptor.class };
                ctorArgs = new Object[] { this, loadProperties, descriptor };
                ctor = classT.getConstructor(ctorParams);
            }
            T rsc = ctor.newInstance(ctorArgs);

            if (rsc.hasCapability(ColorableCapability.class) == false) {
                rsc.getCapability(ColorableCapability.class).setColor(
                        ColorUtil.getNewColor(descriptor));
            }

            return rsc;
        } catch (SecurityException e) {
            throw new VizException("Failed to initialize a Tool Resource", e);
        } catch (NoSuchMethodException e) {
            throw new VizException("Failed to initialize a Tool Resource", e);
        } catch (IllegalArgumentException e) {
            throw new VizException("Failed to initialize a Tool Resource", e);
        } catch (InstantiationException e) {
            throw new VizException("Failed to initialize a Tool Resource", e);
        } catch (IllegalAccessException e) {
            throw new VizException("Failed to initialize a Tool Resource", e);
        } catch (InvocationTargetException e) {
            throw new VizException("Failed to initialize a Tool Resource", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {
        // Nothing to update
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || obj instanceof GenericToolsResourceData == false) {
            return false;
        }

        GenericToolsResourceData<?> other = (GenericToolsResourceData<?>) obj;

        if (this.name != null && other.name == null) {
            return false;
        } else if (this.name == null && other.name != null) {
            return false;
        } else if (this.name != null && this.name.equals(other.name) == false) {
            return false;
        }

        if (this.classT != null && other.classT == null) {
            return false;
        } else if (this.classT == null && other.classT != null) {
            return false;
        } else if (this.classT != null
                && this.classT.equals(other.classT) == false) {
            return false;
        }

        return true;
    }

    /**
     * @return the classT
     */
    public String getClassT() {
        return classT.getCanonicalName();
    }

    @SuppressWarnings("unchecked")
    @XmlElement
    public void setClassT(String classT) {
        try {
            this.classT = (Class<? extends T>) Class.forName(classT);
        } catch (ClassNotFoundException e) {
            statusHandler
                    .handle(Priority.PROBLEM, "Error gettings resource class: "
                            + e.getLocalizedMessage(), e);
        }
    }

}
