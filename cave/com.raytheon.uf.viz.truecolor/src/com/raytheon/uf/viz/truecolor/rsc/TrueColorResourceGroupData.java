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

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.Channel;

/**
 * {@link TrueColorResourceGroup} resource data. Contains a red/blue/green
 * channel resource and a name.
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
@XmlAccessorType(XmlAccessType.NONE)
public class TrueColorResourceGroupData extends AbstractResourceData implements
        IResourceGroup {

    private ResourceList resourceList;

    @XmlElement
    private String groupName;

    @XmlElement(name = "channelResource")
    private List<ChannelResource> channelResources;

    private Map<Channel, ChannelInfo> channelInfo = new HashMap<Channel, ChannelInfo>();

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IResourceGroup#getResourceList()
     */
    @Override
    public ResourceList getResourceList() {
        if (resourceList == null) {
            resourceList = new ResourceList() {
                private static final long serialVersionUID = 1L;

                @Override
                protected boolean canAdd(ResourcePair e) {
                    // Don't allow a ResourcePair that == another in the list
                    Iterator<ResourcePair> iter = iterator();
                    while (iter.hasNext()) {
                        if (iter.next() == e) {
                            return false;
                        }
                    }
                    return true;
                }
            };
            for (ChannelResource resource : channelResources) {
                addResource(resource.getResourceData());
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
     * @return the channelResources
     */
    public List<ChannelResource> getChannelResources() {
        return channelResources;
    }

    /**
     * @param channelResources
     *            the channelResources to set
     */
    public void setChannelResources(List<ChannelResource> channelResources) {
        this.channelResources = channelResources;
    }

    /**
     * @return the channelInfo
     */
    public ChannelInfo getChannelInfo(Channel channel) {
        return channelInfo.get(channel);
    }

    /**
     * @param channelInfo
     *            the channelInfo to set
     */
    public void setChannelInfo(ChannelInfo channelInfo) {
        this.channelInfo.put(channelInfo.getChannel(), channelInfo);
    }

    public ChannelInfo[] getChannelInfoArray() {
        return this.channelInfo.values().toArray(
                new ChannelInfo[channelInfo.size()]);
    }

    @XmlElement(name = "channelInfo")
    public void setChannelInfoArray(ChannelInfo[] channelInfo) {
        if (channelInfo == null) {
            channelInfo = new ChannelInfo[0];
        }
        this.channelInfo.clear();
        for (ChannelInfo ci : channelInfo) {
            setChannelInfo(ci);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((channelInfo == null) ? 0 : channelInfo.hashCode());
        result = prime
                * result
                + ((channelResources == null) ? 0 : channelResources.hashCode());
        result = prime * result
                + ((groupName == null) ? 0 : groupName.hashCode());
        return result;
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
        if (channelInfo == null) {
            if (other.channelInfo != null)
                return false;
        } else if (!channelInfo.equals(other.channelInfo))
            return false;
        if (channelResources == null) {
            if (other.channelResources != null)
                return false;
        } else if (!channelResources.equals(other.channelResources))
            return false;
        if (groupName == null) {
            if (other.groupName != null)
                return false;
        } else if (!groupName.equals(other.groupName))
            return false;
        return true;
    }

}
