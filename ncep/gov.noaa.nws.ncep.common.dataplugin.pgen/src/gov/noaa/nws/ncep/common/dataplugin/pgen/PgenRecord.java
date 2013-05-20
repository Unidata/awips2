package gov.noaa.nws.ncep.common.dataplugin.pgen;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Record implementation for the pgen data plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2013            sgilbert     Initial creation
 * 
 * </pre>
 * 
 * @author sgilbert
 * @version 1.0
 */
@Entity
@Table(name = "pgen", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PgenRecord extends PersistablePluginDataObject {

    private static final long serialVersionUID = 1L;

    public static final String DATAURI = "dataURI";

    public static final String ACTIVITY_TYPE = "activityType";

    public static final String ACTIVITY_LABEL = "activityLabel";

    public static final String ACTIVITY_NAME = "activityName";

    public static final String ACTIVITY_XML = "ActivityXML";

    public static final String REF_TIME = "dataTime.refTime";

    /**
     * 
     */
    @DataURI(position = 1)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String activityName;

    /**
     * 
     */
    @DataURI(position = 2)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String activityType;

    /**
     * 
     */
    @DataURI(position = 3)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String activitySubtype;

    /**
     * 
     */
    @DataURI(position = 4)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String activityLabel;

    /**
     * 
     */
    @DataURI(position = 5)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String site;

    /**
     * 
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String desk;

    /**
     * 
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String status;

    /**
     * 
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String operatingMode;

    /**
     * 
     */
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private String forecaster;

    /**
     * 
     @Column
     * @DynamicSerializeElement
     * @XmlAttribute private Geometry envelope;
     * @Column(name = "envelope", columnDefinition = "geometry")
     * @Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
     * @XmlJavaTypeAdapter(value = GeometryAdapter.class)
     * @DynamicSerializeElement private Geometry envelope;
     */

    @Transient
    private String activityXML;

    /**
     * No-arg Constructor
     */
    public PgenRecord() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A dataURI applicable to this class.
     */
    public PgenRecord(String uri) {
        super(uri);
    }

    public String getActivityName() {
        return activityName;
    }

    public void setActivityName(String activityName) {
        this.activityName = activityName;
    }

    public String getActivityType() {
        return activityType;
    }

    public void setActivityType(String activityType) {
        this.activityType = activityType;
    }

    public String getActivitySubtype() {
        return activitySubtype;
    }

    public void setActivitySubtype(String activitySubtype) {
        this.activitySubtype = activitySubtype;
    }

    public String getActivityLabel() {
        return activityLabel;
    }

    public void setActivityLabel(String activityLabel) {
        this.activityLabel = activityLabel;
    }

    public String getSite() {
        return site;
    }

    public void setSite(String site) {
        this.site = site;
    }

    public String getDesk() {
        return desk;
    }

    public void setDesk(String desk) {
        this.desk = desk;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getOperatingMode() {
        return operatingMode;
    }

    public void setOperatingMode(String operatingMode) {
        this.operatingMode = operatingMode;
    }

    public String getForecaster() {
        return forecaster;
    }

    public void setForecaster(String forecaster) {
        this.forecaster = forecaster;
    }

    /*
     * public Geometry getEnvelope() { return envelope; }
     * 
     * public void setEnvelope(Geometry envelope) { this.envelope = envelope; }
     */

    public static long getSerialversionuid() {
        return serialVersionUID;
    }

    public String getActivityXML() {
        return activityXML;
    }

    public void setActivityXML(String activityXML) {
        this.activityXML = activityXML;
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

}
