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
package com.raytheon.uf.common.dataplugin.pirep;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftCloudLayer;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftFlightCondition;

/**
 * This structure holds information about layer type data contained within a
 * pirep observation. For turbulence and icing data each layer must have a layer
 * type (i.e. icing) and at least one height to be considered valid. Cloud data
 * must have at least one cloud cover amount and a height to be considered
 * valid. These rules are enforced by using the getLayerData and
 * getCloudLayerData factory methods. The user may enforce their own rules by
 * setting the data directly.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080128            861 jkorman     Initial Coding.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@Table(name="pirep_anc_data")
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class PirepLayerData implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    public static final String LAYER_TYP_ICING = "ICING";

    public static final String LAYER_TYP_TURBC = "TURBC";

    public static final String LAYER_TYP_CLOUD = "CLOUD";

    public static final Integer INDETERMINATE = 99999;

    @Id
    @GeneratedValue
    private Integer recordId = null;

    @Column
    private Integer obsId;

    @ManyToOne
    @JoinColumn(name="parent", nullable=false)
    private PirepRecord parent = null;

    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String layerType;

    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String dataType;

    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String frequency;
    
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer baseLayerHeight;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer topLayerHeight;

    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String firstValue;

    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String secondValue;

    /**
     * Construct an empty base.
     */
    public PirepLayerData() {
    }

    /**
     * Constructor with known parent.
     * 
     * @param parent
     *            The parent of this class.
     */
    public PirepLayerData(PirepRecord parent) {
        this.parent = parent;
    }

    /**
     * Constructor with known parent and observation type.
     * 
     * @param parent
     *            The parent of this class.
     * @param type
     *            The observation type for this data.
     */
    public PirepLayerData(PirepRecord parent, String type) {
        this(parent);
        layerType = type;
    }

    /**
     * Get the record id.
     * 
     * @return The recordId. If not set returns null.
     */
    public Integer getRecordId() {
        return recordId;
    }

    /**
     * Set the record id.
     * 
     * @param recordId
     *            The recordId.
     */
    public void setRecordId(Integer recordId) {
        this.recordId = recordId;
    }

    /**
     * Get the observation id. This is the foreign key to the parent.
     * 
     * @return The observation id. If not set returns null.
     */
    public Integer getObsId() {
        return obsId;
    }

    /**
     * Set the observation id. This is the foreign key to the parent.
     * 
     * @param obsId
     *            The observation id.
     */
    public void setObsId(Integer obsId) {
        this.obsId = obsId;
    }

    /**
     * Get the parent for this class.
     * 
     * @return the parent
     */
    public PirepRecord getParent() {
        return parent;
    }

    /**
     * Set the parent for this class.
     * 
     * @param parent
     *            The parent to set.
     */
    public void setParent(PirepRecord parent) {
        this.parent = parent;
    }

    /**
     * @return the layerType
     */
    public String getLayerType() {
        return layerType;
    }

    /**
     * @param layerType
     *            the layerType to set
     */
    public void setLayerType(String layerType) {
        this.layerType = layerType;
    }

    /**
     * @return the dataType
     */
    public String getDataType() {
        return dataType;
    }

    /**
     * @param dataType
     *            the dataType to set
     */
    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    /**
     * @return the frequency
     */
    public String getFrequency() {
        return frequency;
    }

    /**
     * @param frequency the frequency to set
     */
    public void setFrequency(String frequency) {
        this.frequency = frequency;
    }

    /**
     * @return the baseLayerHeight
     */
    public Integer getBaseLayerHeight() {
        return baseLayerHeight;
    }

    /**
     * @param baseLayerHeight
     *            the baseLayerHeight to set
     */
    public void setBaseLayerHeight(Integer baseLayerHeight) {
        this.baseLayerHeight = baseLayerHeight;
    }

    /**
     * @return the topLayerHeight
     */
    public Integer getTopLayerHeight() {
        return topLayerHeight;
    }

    /**
     * @param topLayerHeight
     *            the topLayerHeight to set
     */
    public void setTopLayerHeight(Integer topLayerHeight) {
        this.topLayerHeight = topLayerHeight;
    }

    /**
     * @return the firstValue
     */
    public String getFirstValue() {
        return firstValue;
    }

    /**
     * @param firstValue
     *            the firstValue to set
     */
    public void setFirstValue(String firstValue) {
        this.firstValue = firstValue;
    }

    /**
     * @return the secondValue
     */
    public String getSecondValue() {
        return secondValue;
    }

    /**
     * @param secondValue
     *            the secondValue to set
     */
    public void setSecondValue(String secondValue) {
        this.secondValue = secondValue;
    }

    /**
     * Factored out code that creates icing or turbulence layer data.
     * 
     * @param layer
     *            Decoded flight conditions data (Turbulence or icing)
     * @return A populated entry.
     */
    public static PirepLayerData getLayerData(AircraftFlightCondition layer) {
        PirepLayerData dataLayer = null;

        boolean isValid = false;

        String intensity = layer.getIntensity1();
        if (intensity != null) {
            dataLayer = new PirepLayerData();
            dataLayer.setFirstValue(intensity);
            isValid = true;
        }
        intensity = layer.getIntensity2();
        if (intensity != null) {
            dataLayer = getLayer(dataLayer);
            dataLayer.setSecondValue(intensity);
            isValid = true;
        }
        String value = layer.getFrequency();
        if(value != null) {
            dataLayer = getLayer(dataLayer);
            dataLayer.frequency = value;
        }
        
        String type = layer.getType();
        if (type != null) {
            dataLayer = getLayer(dataLayer);
            dataLayer.setDataType(type);
            isValid = true;
        }
        // if we have at least one intensity and/or type get the height info.
        // Note, only reported if different from flight level
        if (isValid) {
            Integer hgt = layer.getBaseHeight();
            if (hgt != null) {
                dataLayer = getLayer(dataLayer);
                dataLayer.setBaseLayerHeight(hgt);
            }
            hgt = layer.getTopHeight();
            if (hgt != null) {
                dataLayer.setTopLayerHeight(hgt);
            }
        }
        if (!isValid) {
            dataLayer = null;
        }
        return dataLayer;
    }
    
    /**
     * Factored out code that creates icing or turbulence layer data.
     * 
     * @param layer
     *            Decoded flight conditions data (Turbulence or icing)
     * @return A populated entry.
     */
    public static PirepLayerData getCloudLayerData(AircraftCloudLayer layer) {
        PirepLayerData cloudLayer = null;

        boolean isValid = false;

        String intensity = layer.getCloudCover1();
        if (intensity != null) {
            cloudLayer = new PirepLayerData();
            cloudLayer.setFirstValue(intensity);
            isValid = true;
        }
        intensity = layer.getCloudCover2();
        if (intensity != null) {
            cloudLayer = getLayer(cloudLayer);
            cloudLayer.setSecondValue(intensity);
            isValid = true;
        }
        // if we have at least one cloud coverage
        if (isValid) {
            // reset so we can ensure at least one height.
            isValid = false;
            Integer hgt = layer.getCloudBaseHeight();
            if (hgt != null) {
                cloudLayer = getLayer(cloudLayer);
                cloudLayer.setBaseLayerHeight(hgt);
                isValid = true;
            }
            hgt = layer.getCloudTopHeight();
            if (hgt != null) {
                cloudLayer = getLayer(cloudLayer);
                cloudLayer.setTopLayerHeight(hgt);
                isValid = true;
            }
        }
        if (!isValid) {
            cloudLayer = null;
        }
        if (cloudLayer != null) {
            cloudLayer.setDataType(null);
            cloudLayer.setLayerType(PirepLayerData.LAYER_TYP_CLOUD);
        }
        return cloudLayer;
    }

    /**
     * 
     * @param layer
     * @return
     */
    private static PirepLayerData getLayer(PirepLayerData layer) {
        return (layer != null) ? layer : new PirepLayerData();
    }
    
}
