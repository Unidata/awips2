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
package com.raytheon.viz.core.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * Represents resource data for blended resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 5, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlRootElement(name = "blendedResource")
@XmlAccessorType(XmlAccessType.NONE)
public class BlendedResourceData extends AbstractResourceData implements
        IResourceGroup {

    @XmlElement(name = "resource")
    protected ResourceList resourceList;

    public BlendedResourceData() {
        this.resourceList = new ResourceList();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                String s = resourceList.get(0).getResource().getName();
                for (int i = 1; i < resourceList.size(); i++) {
                    s += " + " + resourceList.get(i).getResource().getName();
                }
                return s;
            }

        };
    }

    public BlendedResourceData(BlendedResourceData brd) {
        this.resourceList = brd.resourceList;
        this.nameGenerator = brd.nameGenerator;
    }

    @Override
    public BlendedResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        BlendedResource rsc = new BlendedResource(this, loadProperties);
        return rsc;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#configure(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void configure(LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        for (ResourcePair rp : resourceList) {
            rp.getResourceData().configure(rp.getLoadProperties(), descriptor);
        }
    }

    @Override
    public void update(Object updateData) {
        // Updating should be modified to update the children directly
    }

    /**
     * @return the resourceList
     */
    public ResourceList getResourceList() {
        return resourceList;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || obj instanceof BlendedResourceData == false) {
            return false;
        }
        BlendedResourceData other = (BlendedResourceData) obj;

        if (this.resourceList != null && other.resourceList == null) {
            return false;
        } else if (this.resourceList == null && other.resourceList != null) {
            return false;
        } else if (this.resourceList != null
                && this.resourceList.equals(other.resourceList) == false) {
            return false;
        }

        return true;
    }

}
