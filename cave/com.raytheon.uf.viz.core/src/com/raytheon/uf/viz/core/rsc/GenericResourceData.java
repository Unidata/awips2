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

import java.lang.reflect.Constructor;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.status.StatusConstants;

/**
 * Generic resource data for resources to use which don't actually require
 * resource data but only one of the resource type per resource list should be
 * added
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 22, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public final class GenericResourceData extends AbstractResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(GenericResourceData.class);

    private String clazz;

    private Class<? extends AbstractVizResource<GenericResourceData, ? extends IDescriptor>> rscClass;

    /**
     * @param rscClazz
     */
    public GenericResourceData(
            Class<? extends AbstractVizResource<GenericResourceData, ? extends IDescriptor>> rscClass) {
        this.rscClass = rscClass;
        this.clazz = rscClass.getName();
    }

    public GenericResourceData() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        if (rscClass == null) {
            throw new VizException(
                    "Resource class is null, nothing to construct");
        }

        try {
            Constructor<? extends AbstractVizResource<GenericResourceData, ? extends IDescriptor>> c = rscClass
                    .getConstructor(GenericResourceData.class,
                            LoadProperties.class);
            return c.newInstance(this, loadProperties);
        } catch (Exception e) {
            throw new VizException(
                    "Error creating instance for resource class: "
                            + rscClass.getCanonicalName(), e);
        }
    }

    public String getClazz() {
        return clazz;
    }

    @SuppressWarnings("unchecked")
    @XmlAttribute
    public void setClazz(String clazz) {
        this.clazz = clazz;
        try {
            this.rscClass = (Class<? extends AbstractVizResource<GenericResourceData, ? extends IDescriptor>>) Class
                    .forName(clazz);
        } catch (ClassNotFoundException e) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Error gettings resource class: " + e.getLocalizedMessage(),
                    e);
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

    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        GenericResourceData other = (GenericResourceData) obj;
        if (rscClass == null) {
            if (other.rscClass != null)
                return false;
        } else if (!rscClass.equals(other.rscClass))
            return false;
        return true;
    }

}
