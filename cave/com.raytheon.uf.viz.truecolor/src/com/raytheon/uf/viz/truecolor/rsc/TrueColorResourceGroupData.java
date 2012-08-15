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
package com.raytheon.uf.viz.truecolor.rsc;

import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.Channel;

/**
 * {@link TrueColorResourceGroup} resource data. Contains a red/blue/green
 * channel resource and a name. Sub resources that .equal each other will be
 * replaced with the first reference to save time
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TrueColorResourceGroupData extends AbstractResourceData implements
        IResourceGroup {

    private ResourceList resourceList;

    @XmlElement
    private String groupName;

    @XmlElement
    private AbstractResourceData redChannelResource;

    @XmlElement
    private AbstractResourceData greenChannelResource;

    @XmlElement
    private AbstractResourceData blueChannelResource;

    public TrueColorResourceGroupData() {
        nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return groupName;
            }
        };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IResourceGroup#getResourceList()
     */
    @Override
    public ResourceList getResourceList() {
        if (resourceList == null) {
            resourceList = new ResourceList();
            // Initialize the resource list, if any of the resources equal each
            // other, replace with reference instead of copy to save memory
            if (redChannelResource != null) {
                addResource(redChannelResource);
            }
            if (greenChannelResource != null) {
                if (greenChannelResource.equals(redChannelResource)) {
                    greenChannelResource = redChannelResource;
                } else {
                    addResource(greenChannelResource);
                }
            }
            if (blueChannelResource != null) {
                if (blueChannelResource.equals(redChannelResource)) {
                    blueChannelResource = redChannelResource;
                } else if (blueChannelResource.equals(greenChannelResource)) {
                    blueChannelResource = greenChannelResource;
                } else {
                    addResource(blueChannelResource);
                }
            }
        }
        return resourceList;
    }

    private void addResource(AbstractResourceData resourceData) {
        ResourcePair rp = new ResourcePair();
        rp.setResourceData(resourceData);
        rp.setLoadProperties(new LoadProperties());
        rp.setProperties(new ResourceProperties());
        resourceList.add(rp);
    }

    /**
     * Removes a resource from the resource data
     * 
     * @param rp
     */
    public void removeResource(ResourcePair rp) {
        resourceList.remove(rp);
        if (rp.getResourceData() == redChannelResource) {
            redChannelResource = null;
        }
        if (rp.getResourceData() == greenChannelResource) {
            greenChannelResource = null;
        }
        if (rp.getResourceData() == blueChannelResource) {
            blueChannelResource = null;
        }
    }

    /**
     * Get the composite name of the resource pair (Red, Red/Green, Blue, etc)
     * 
     * @param rp
     * @return
     */
    public String getCompositeName(ResourcePair rp) {
        String name = "";
        if (rp.getResourceData() == redChannelResource) {
            name += "Red";
        }
        if (rp.getResourceData() == greenChannelResource) {
            if (name.isEmpty() == false) {
                name += "/";
            }
            name += "Green";
        }
        if (rp.getResourceData() == blueChannelResource) {
            if (name.isEmpty() == false) {
                name += "/";
            }
            name += "Blue";
        }
        return name;
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
        return new TrueColorResourceGroup(this, loadProperties);
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
        // Nothing to update, updates will be handled by sub resources
    }

    /**
     * Get the resource pair associated with the {@link Channel}
     * 
     * @param channel
     * @return
     */
    public ResourcePair getResource(Channel channel) {
        AbstractResourceData toCheckFor = null;
        switch (channel) {
        case RED:
            toCheckFor = redChannelResource;
            break;
        case GREEN:
            toCheckFor = greenChannelResource;
            break;
        case BLUE:
            toCheckFor = blueChannelResource;
            break;
        }
        for (ResourcePair rp : getResourceList()) {
            if (rp.getResourceData() == toCheckFor) {
                return rp;
            }
        }
        return null;
    }

    /**
     * @return the groupName
     */
    public String getGroupName() {
        return groupName;
    }

    /**
     * @param groupName
     *            the groupName to set
     */
    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    /**
     * @return the redChannelResource
     */
    public AbstractResourceData getRedChannelResource() {
        return redChannelResource;
    }

    /**
     * @param redChannelResource
     *            the redChannelResource to set
     */
    public void setRedChannelResource(AbstractResourceData redChannelResource) {
        this.redChannelResource = redChannelResource;
    }

    /**
     * @return the greenChannelResource
     */
    public AbstractResourceData getGreenChannelResource() {
        return greenChannelResource;
    }

    /**
     * @param greenChannelResource
     *            the greenChannelResource to set
     */
    public void setGreenChannelResource(
            AbstractResourceData greenChannelResource) {
        this.greenChannelResource = greenChannelResource;
    }

    /**
     * @return the blueChannelResource
     */
    public AbstractResourceData getBlueChannelResource() {
        return blueChannelResource;
    }

    /**
     * @param blueChannelResource
     *            the blueChannelResource to set
     */
    public void setBlueChannelResource(AbstractResourceData blueChannelResource) {
        this.blueChannelResource = blueChannelResource;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        TrueColorResourceGroupData other = (TrueColorResourceGroupData) obj;
        if (blueChannelResource == null) {
            if (other.blueChannelResource != null)
                return false;
        } else if (!blueChannelResource.equals(other.blueChannelResource))
            return false;
        if (greenChannelResource == null) {
            if (other.greenChannelResource != null)
                return false;
        } else if (!greenChannelResource.equals(other.greenChannelResource))
            return false;
        if (groupName == null) {
            if (other.groupName != null)
                return false;
        } else if (!groupName.equals(other.groupName))
            return false;
        if (redChannelResource == null) {
            if (other.redChannelResource != null)
                return false;
        } else if (!redChannelResource.equals(other.redChannelResource))
            return false;
        return true;
    }

}
