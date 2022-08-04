package com.raytheon.uf.common.dataplugin.shef.tables;

import java.io.Serializable;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * IHFS database DAABiasDyn record representation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/22/2014   Redmine #3454 (A2 14.3.1) new dualpol-related table DAABiasDyn
 * 10/05/2016   #5631      bkowal      Removed ISerializableObject. Added named queries.
 * 
 * </pre>
 * 
 * @author OHD
 */

@Entity
@Table(name = "daabiasdyn")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@NamedQueries({
        @NamedQuery(name = DAABiasDyn.SELECT_FOR_RADID_AND_OFFICE_FOR_TIME, query = DAABiasDyn.SELECT_FOR_RADID_AND_OFFICE_FOR_TIME_HQL) })
public class DAABiasDyn extends PersistableDataObject<DAABiasDynId>
        implements Serializable {

    private static final long serialVersionUID = 1L;

    public static final String SELECT_FOR_RADID_AND_OFFICE_FOR_TIME = "selectDAABiasDynForRadIdAndOfficeForTime";

    /*
     * Note: not inclusive start to end.
     */
    protected static final String SELECT_FOR_RADID_AND_OFFICE_FOR_TIME_HQL = "FROM DAABiasDyn d WHERE d.id.radid = :radId AND d.id.obstime = :obsTime AND d.id.officeId = :officeId ORDER BY d.id.memspanInd";

    @XmlElement
    @DynamicSerializeElement
    private DAABiasDynId id;

    @XmlElement
    @DynamicSerializeElement
    private Double numpairs;

    @XmlElement
    @DynamicSerializeElement
    private Float sumgag;

    @XmlElement
    @DynamicSerializeElement
    private Float sumrad;

    @XmlElement
    @DynamicSerializeElement
    private Float bias;

    public DAABiasDyn() {
    }

    public DAABiasDyn(DAABiasDynId id) {
        this.id = id;
    }

    public DAABiasDyn(DAABiasDynId id, Double numpairs, Float sumgag,
            Float sumrad, Float bias) {
        this.id = id;
        this.numpairs = numpairs;
        this.sumgag = sumgag;
        this.sumrad = sumrad;
        this.bias = bias;
    }

    @EmbeddedId
    @AttributeOverrides({
            @AttributeOverride(name = "radid", column = @Column(name = "radid", nullable = false, length = 3) ),
            @AttributeOverride(name = "officeId", column = @Column(name = "office_id", nullable = false, length = 5) ),
            @AttributeOverride(name = "obstime", column = @Column(name = "obstime", nullable = false, length = 29) ),
            @AttributeOverride(name = "memspanInd", column = @Column(name = "memspan_ind", nullable = false) ) })
    public DAABiasDynId getId() {
        return this.id;
    }

    public void setId(DAABiasDynId id) {
        this.id = id;
    }

    @Column(name = "numpairs", precision = 17, scale = 17)
    public Double getNumpairs() {
        return this.numpairs;
    }

    public void setNumpairs(Double numpairs) {
        this.numpairs = numpairs;
    }

    @Column(name = "sumgag", precision = 8, scale = 8)
    public Float getSumgag() {
        return this.sumgag;
    }

    public void setSumgag(Float sumgag) {
        this.sumgag = sumgag;
    }

    @Column(name = "sumrad", precision = 8, scale = 8)
    public Float getSumrad() {
        return this.sumrad;
    }

    public void setSumrad(Float sumrad) {
        this.sumrad = sumrad;
    }

    @Column(name = "bias", precision = 8, scale = 8)
    public Float getBias() {
        return this.bias;
    }

    public void setBias(Float bias) {
        this.bias = bias;
    }

}
