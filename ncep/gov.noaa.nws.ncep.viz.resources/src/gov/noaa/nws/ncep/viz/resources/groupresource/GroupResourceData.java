package gov.noaa.nws.ncep.viz.resources.groupresource;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "GroupResourceData")
public class GroupResourceData extends AbstractNatlCntrsResourceData implements
        IResourceGroup {
    /**
     * GroupResource - Display resources in group
     * 
     * This code has been developed by the SIB for use in the AWIPS2 system.
     * 
     * <pre>
     * SOFTWARE HISTORY
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * 05/14        ?           B. Yin     Initial creation.
     * 
     * </pre>
     * 
     * @author byin
     * @version 1.0
     */

    @XmlElement
    protected String groupName = "nc-group";

    @XmlElement
    private int funcKeyNum = 0;

    @XmlElement(name = "resource-in-group")
    private ResourceList resourceList;

    public GroupResourceData() {
        resourceList = new ResourceList();
    }

    public GroupResourceData(String name, int key, RGB color) {
        this.setGroupName(name);
        this.setFuncKeyNum(key);
        this.setLegendColor(color);
        resourceList = new ResourceList();
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new GroupResource(this, loadProperties);
    }

    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean equals(Object obj) {
        // TODO Auto-generated method stub
        return false;
    }

    public void addResource(ResourcePair rp) {
        resourceList.add(rp);
    }

    // @XmlElement(name = "resource-in-group")
    public String[] getSerializableResources() {
        if (resourceList != null) {
            List<String> rps = new ArrayList<String>(resourceList.size());
            for (ResourcePair rp : resourceList) {
                if (rp.getProperties().isSystemResource() == false) {
                    rps.add("test-yin");
                }
            }
            return rps.toArray(new String[rps.size()]);
        }
        return null;
    }

    public void setSerializableResources(String[] resources) {
    }

    public ResourceList getResourceList() {
        return resourceList;
    }

    public void setResourceList(ResourceList resourceList) {
        this.resourceList = resourceList;
    }

    public int getFuncKeyNum() {
        return funcKeyNum;
    }

    public void setFuncKeyNum(int funcHotKey) {
        this.funcKeyNum = funcHotKey;
    }

    @Override
    public boolean setRscAttrSet(ResourceAttrSet attrSet) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public ResourceAttrSet getRscAttrSet() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setResourceName(ResourceName rscName) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean getIsEdited() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void setIsEdited(boolean e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void setResourceVersion(String v) {
        // TODO Auto-generated method stub

    }

    @Override
    public String getResourceVersion() {
        // TODO Auto-generated method stub
        return null;
    }

    public String getGroupName() {
        return groupName;
    }

    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    @Override
    public AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        return construct(loadProperties, descriptor);
    }

    public void replaceResourcePair(ResourcePair oldPair, ResourcePair newPair) {
        getResourceList().remove(oldPair);
        getResourceList().add(newPair);
    }

}
