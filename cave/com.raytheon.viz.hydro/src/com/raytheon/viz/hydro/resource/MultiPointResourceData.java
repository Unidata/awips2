package com.raytheon.viz.hydro.resource;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.resource.AbstractMultiPointResourceData;

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
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "multiPointResourceData")
public class MultiPointResourceData extends AbstractMultiPointResourceData {

    public MultiPointResourceData() {

    }

    public MultiPointResourceData(String name) {
        this.name = name;
    }

    @Override
    public MultiPointResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new MultiPointResource(this, loadProperties);
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
        MultiPointResourceData other = (MultiPointResourceData) obj;
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
