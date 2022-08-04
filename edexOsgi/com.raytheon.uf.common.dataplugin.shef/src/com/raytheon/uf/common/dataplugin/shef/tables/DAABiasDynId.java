package com.raytheon.uf.common.dataplugin.shef.tables;

import java.io.Serializable;
import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Embeddable identifier for the {@link DAABiasDyn} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/22/2014   Redmine #3454 (A2 14.3.1) new dualpol-related table ID
 * 10/05/2016   5631       bkowal      Cleanup.
 * 
 * </pre>
 * 
 * @author OHD
 */

@Embeddable
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DAABiasDynId implements Serializable {

    private static final long serialVersionUID = 1L;

    @XmlElement
    @DynamicSerializeElement
    private String radid;

    @XmlElement
    @DynamicSerializeElement
    private String officeId;

    @XmlElement
    @DynamicSerializeElement
    private Date obstime;

    @XmlElement
    @DynamicSerializeElement
    private short memspanInd;

    public DAABiasDynId() {
    }

    public DAABiasDynId(String radid, String officeId, Date obstime,
            short memspanInd) {
        this.radid = radid;
        this.officeId = officeId;
        this.obstime = obstime;
        this.memspanInd = memspanInd;
    }

    @Column(name = "radid", nullable = false, length = 3)
    public String getRadid() {
        return this.radid;
    }

    public void setRadid(String radid) {
        this.radid = radid;
    }

    @Column(name = "office_id", nullable = false, length = 5)
    public String getOfficeId() {
        return this.officeId;
    }

    public void setOfficeId(String officeId) {
        this.officeId = officeId;
    }

    @Column(name = "obstime", nullable = false, length = 29)
    public Date getObstime() {
        return this.obstime;
    }

    public void setObstime(Date obstime) {
        this.obstime = obstime;
    }

    @Column(name = "memspan_ind", nullable = false)
    public short getMemspanInd() {
        return this.memspanInd;
    }

    public void setMemspanInd(short memspanInd) {
        this.memspanInd = memspanInd;
    }

    public boolean equals(Object other) {
        if ((this == other))
            return true;
        if ((other == null))
            return false;
        if (!(other instanceof DAABiasDynId))
            return false;
        RwbiasdynId castOther = (RwbiasdynId) other;

        return ((this.getRadid() == castOther.getRadid())
                || (this.getRadid() != null && castOther.getRadid() != null
                        && this.getRadid().equals(castOther.getRadid())))
                && ((this.getOfficeId() == castOther.getOfficeId())
                        || (this.getOfficeId() != null
                                && castOther.getOfficeId() != null
                                && this.getOfficeId()
                                        .equals(castOther.getOfficeId())))
                && ((this.getObstime() == castOther.getObstime())
                        || (this.getObstime() != null
                                && castOther.getObstime() != null
                                && this.getObstime()
                                        .equals(castOther.getObstime())))
                && (this.getMemspanInd() == castOther.getMemspanInd());
    }

    public int hashCode() {
        int result = 17;

        result = 37 * result
                + (getRadid() == null ? 0 : this.getRadid().hashCode());
        result = 37 * result
                + (getOfficeId() == null ? 0 : this.getOfficeId().hashCode());
        result = 37 * result
                + (getObstime() == null ? 0 : this.getObstime().hashCode());
        result = 37 * result + this.getMemspanInd();
        return result;
    }
}