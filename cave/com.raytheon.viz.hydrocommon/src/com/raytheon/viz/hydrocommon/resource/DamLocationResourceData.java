package com.raytheon.viz.hydrocommon.resource;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.data.DamMaster;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2011            rgeorge     Initial creation
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "damLocationResourceData")
public class DamLocationResourceData extends AbstractResourceData {

    @XmlElement
    private List<DamMaster> damList;

    @XmlElement
    private String name;

    public DamLocationResourceData() {

    }

    public DamLocationResourceData(String name) {
        this.name = name;
    }

    /**
     * @return the damList
     */
    public List<DamMaster> getDamList() {
        return damList;
    }

    /**
     * @param damList
     *            the damList to set
     */
    public void setDamList(List<DamMaster> damList) {
        this.damList = damList;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public DamLocationResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new DamLocationResource(this, loadProperties);
    }

    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        DamLocationResourceData other = (DamLocationResourceData) obj;
        if (damList == null) {
            if (other.damList != null) {
                return false;
            }
        } else if (!damList.equals(other.damList)) {
            return false;
        }
        if (name == null) {
            if (other.name != null) {
                return false;
            }
        } else if (!name.equals(other.name)) {
            return false;
        }
        return true;
    }
}
