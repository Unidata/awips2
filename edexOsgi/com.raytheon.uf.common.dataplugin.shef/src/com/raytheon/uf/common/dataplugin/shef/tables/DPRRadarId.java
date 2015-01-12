package com.raytheon.uf.common.dataplugin.shef.tables;

import java.io.Serializable;
import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAccessType;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/22/2014   Redmine #3454 (A2 14.3.1) new dualpol-related table ID
 * 
 * </pre>
 * 
 * @author OHD
 * @version 1.1
 */

@Embeddable
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DPRRadarId extends PersistableDataObject implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    @XmlElement
    @DynamicSerializeElement
    private String radid;

    @XmlElement
    @DynamicSerializeElement
    private Date obstime;

    public DPRRadarId() {
    }

    public DPRRadarId(String radid, Date obstime) {
        this.radid = radid;
        this.obstime = obstime;
    }

    @Column(name = "radid", nullable = false, length = 3)
    public String getRadid() {
        return this.radid;
    }

    public void setRadid(String radid) {
        this.radid = radid;
    }

    @Column(name = "obstime", nullable = false, length = 29)
    public Date getObstime() {
        return this.obstime;
    }

    public void setObstime(Date obstime) {
        this.obstime = obstime;
    }

    public boolean equals(Object other) {
        if ((this == other))
            return true;
        if ((other == null))
            return false;
        if (!(other instanceof DPRRadarId))
            return false;
        
        DPRRadarId castOther = (DPRRadarId) other;

        return ((this.getRadid() == castOther.getRadid()) || (this.getRadid() != null
                && castOther.getRadid() != null && this.getRadid().equals(
                castOther.getRadid())))
                && ((this.getObstime() == castOther.getObstime()) || (this
                        .getObstime() != null
                        && castOther.getObstime() != null && this.getObstime()
                        .equals(castOther.getObstime())));
    }

    public int hashCode() {
        int result = 17;

        result = 37 * result
                + (getRadid() == null ? 0 : this.getRadid().hashCode());
        result = 37 * result
                + (getObstime() == null ? 0 : this.getObstime().hashCode());
        return result;
    }

}
