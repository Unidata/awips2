package gov.noaa.nws.ncep.common.dataplugin.pirep;

/**
 * This software was modified from Raytheon's pirep plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
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
 * 05/02/2011				F. J. Yen   Initial creation from Raytheon's pirep
 * 08/30/2011    286        qzhou       Added fields for TB, IC, SK. Remove general fileds.
 *                                     Append intensity2 to intensity1 for TB, IC, SK.
 *                                     Created getTurbLayerData and getIceLayerData method.
 * 08/31/2011    286        qzhou      Created project and moved this from ~edex.plugin.pirep
 * Sep 05, 2013  2316       bsteffen   Unify pirep and ncpirep.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class PirepLayerData implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    public static final String LAYER_TYP_ICING = "ICING";

    public static final String LAYER_TYP_TURBC = "TURBC";

    public static final String LAYER_TYP_CLOUD = "CLOUD";

    //public static final Integer INDETERMINATE = 99999; //use IDecoderConstantsN.INTEGER_MISSING

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
    private String hazardType;

//    @Column(length=8)
//    @XmlAttribute
//    @DynamicSerializeElement
//    private String dataType;
//
//    @Column(length=8)
//    @XmlAttribute
//    @DynamicSerializeElement
//    private String frequency;
//    
//    @Column
//    @XmlAttribute
//    @DynamicSerializeElement
//    private Integer baseLayerHeight;
//
//    @Column
//    @XmlAttribute
//    @DynamicSerializeElement
//    private Integer topLayerHeight;
//
//    @Column(length=8)
//    @XmlAttribute
//    @DynamicSerializeElement
//    private String firstValue;
//
//    @Column(length=8)
//    @XmlAttribute
//    @DynamicSerializeElement
//    private String secondValue;

    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String turbInten;
    
    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String turbFreq;
    
    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String turbType;
    
    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String iceInten;
    
    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String iceType;
    
    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String skyCover1;
    
    @Column(length=8)
    @XmlAttribute
    @DynamicSerializeElement
    private String skyCover2;
    
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer turbBaseHeight;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer turbTopHeight;
    
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer iceBaseHeight;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer iceTopHeight;
    
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer skyBaseHeight;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer skyTopHeight;
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
        hazardType = type;
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
        return hazardType;
    }

    /**
     * @param layerType
     *            the layerType to set
     */
    public void setLayerType(String hazardType) {
        this.hazardType = hazardType;
    }

    /**
     * @return the baseLayerHeight
     */
    public Integer getTurbBaseHeight() {
        return turbBaseHeight;
    }

    /**
     * @param baseLayerHeight
     *            the baseLayerHeight to set
     */
    public void setTurbBaseHeight(Integer turbBaseHeight) {
        this.turbBaseHeight = turbBaseHeight;
    }
    /**
     * @return the baseLayerHeight
     */
    public Integer getTurbTopHeight() {
        return turbTopHeight;
    }

    /**
     * @param baseLayerHeight
     *            the baseLayerHeight to set
     */
    public void setTurbTopHeight(Integer turbTopHeight) {
        this.turbTopHeight = turbTopHeight;
    }
    /**
     * @return the baseLayerHeight
     */
    public Integer getIceBaseHeight() {
        return iceBaseHeight;
    }

    /**
     * @param baseLayerHeight
     *            the baseLayerHeight to set
     */
    public void setIceBaseHeight(Integer iceBaseHeight) {
        this.iceBaseHeight = iceBaseHeight;
    }
    
    /**
     * @return the baseLayerHeight
     */
    public Integer getIceTopHeight() {
        return iceTopHeight;
    }

    /**
     * @param baseLayerHeight
     *            the baseLayerHeight to set
     */
    public void setIceTopHeight(Integer iceTopHeight) {
        this.iceTopHeight = iceTopHeight;
    }
    
    /**
     * @return the baseLayerHeight
     */
    public Integer getSkyBaseHeight() {
        return skyBaseHeight;
    }

    /**
     * @param baseLayerHeight
     *            the baseLayerHeight to set
     */
    public void setSkyBaseHeight(Integer skyBaseHeight) {
        this.skyBaseHeight = skyBaseHeight;
    }
    /**
     * @return the topLayerHeight
     */
    public Integer getSkyTopHeight() {
        return skyTopHeight;
    }

    /**
     * @param topLayerHeight
     *            the topLayerHeight to set
     */
    public void setSkyTopHeight(Integer skyTopHeight) {
        this.skyTopHeight = skyTopHeight;
    }

//    /**
//     * @return the firstValue
//     */
//    public String getFirstValue() {
//        return firstValue;
//    }
//
//    /**
//     * @param firstValue
//     */
//    public void setFirstValue(String firstValue) {
//        this.firstValue = firstValue;
//    }
//
//    /**
//     * @return the secondValue
//     */
//    public String getSecondValue() {
//        return secondValue;
//    }
//
//    /**
//     * @param secondValue
//     */
//    public void setSecondValue(String secondValue) {
//        this.secondValue = secondValue;
//    }

//  /**
//   * @return the dataType
//   */
//  public String getDataType() {
//      return dataType;
//  }
//
//  /**
//   * @param dataType
//   *            the dataType to set
//   */
//  public void setDataType(String dataType) {
//      this.dataType = dataType;
//  }
//
//  /**
//   * @return the frequency
//   */
//  public String getFrequency() {
//      return frequency;
//  }
//
//  /**
//   * @param frequency the frequency to set
//   */
//  public void setFrequency(String frequency) {
//      this.frequency = frequency;
//  }
//  
//  /**
//   * @return the baseLayerHeight
//   */
//  public Integer getBaseLayerHeight() {
//      return baseLayerHeight;
//  }
//
//  /**
//   * @param baseLayerHeight
//   *            the baseLayerHeight to set
//   */
//  public void setBaseLayerHeight(Integer baseLayerHeight) {
//      this.baseLayerHeight = baseLayerHeight;
//  }
//
//  /**
//   * @return the topLayerHeight
//   */
//  public Integer getTopLayerHeight() {
//      return topLayerHeight;
//  }
//
//  /**
//   * @param topLayerHeight
//   *            the topLayerHeight to set
//   */
//  public void setTopLayerHeight(Integer topLayerHeight) {
//      this.topLayerHeight = topLayerHeight;
//  }
    /**
     * @return the turbInten
     */
    public String getTurbInten() {
        return turbInten;
    }

    /**
     * @param turbInten
     */
    public void setTurbInten(String turbInten) {
        this.turbInten = turbInten;
    }
    
    /**
     * @return the turbFreq
     */
    public String getTurbFreq() {
        return turbFreq;
    }

    /**
     * @param turbFreq
     */
    public void setTurbFreq(String turbFreq) {
        this.turbFreq = turbFreq;
    }
    
    /**
     * @return the turbType
     */
    public String getTurbType() {
        return turbType;
    }

    /**
     * @param turbType
     */
    public void setTurbType(String turbType) {
        this.turbType = turbType;
    }
    
    /**
     * @return the skyInten
     */
    public String getIceInten() {
        return iceInten;
    }

    /**
     * @param skyInten
     */
    public void setIceInten(String iceInten) {
        this.iceInten = iceInten;
    }
    /**
     * @return the skyInten
     */
    public String getIceType() {
        return iceType;
    }

    /**
     * @param skyInten
     */
    public void setIceType(String iceType) {
        this.iceType = iceType;
    }
    
    /**
     * @return the skyInten
     */
    public String getSkyInten1() {
        return skyCover1;
    }

    /**
     * @param skyInten
     */
    public void setSkyInten1(String skyCover1) {
        this.skyCover1 = skyCover1;
    }
    
    /**
     * @return the skyInten
     */
    public String getSkyInten2() {
        return skyCover2;
    }

    /**
     * @param skyInten
     */
    public void setSkyInten2(String skyCover2) {
        this.skyCover2 = skyCover2;
    }
 
    /**
     * Factored out code that creates icing or turbulence layer data.
     * 
     * @param layer
     *            Decoded flight conditions data (Turbulence or icing)
     * @return A populated entry.
     */
    public static PirepLayerData getTurbLayerData(AircraftFlightCondition layer) {
        PirepLayerData dataLayer = new PirepLayerData(); //null;
        
        boolean isValid = false;

        String intensity = layer.getIntensity1();        
        if (intensity != null) {
            dataLayer.setTurbInten(intensity);
            isValid = true;
        }
        
        String intensity2 = layer.getIntensity2();
        if (intensity2 != null) {
            dataLayer.setTurbInten(intensity+intensity2);
            isValid = true;
        }
        
        String type = layer.getType();
        if (type != null) {
            dataLayer.setTurbType(type);
            isValid = true;
        }
        
        if (layer.getFrequency() != null) {
            dataLayer.setTurbFreq(layer.getFrequency());
            isValid = true;
        }
        
        // if we have at least one intensity and/or type get the height info.
        if (isValid) {
            // reset so we can ensure at least one height.
            //isValid = false;
            Integer hgt = layer.getBaseHeight();
            if (hgt != null) {
                dataLayer.setTurbBaseHeight(hgt);
                isValid = true;
            }
            hgt = layer.getTopHeight();
            if (hgt != null) {
                dataLayer.setTurbTopHeight(hgt);
                isValid = true;
            }
        }
        if (!isValid) {
            dataLayer = null;
        }
        if (dataLayer != null) {   
            dataLayer.setLayerType(PirepLayerData.LAYER_TYP_TURBC);
        }
        return dataLayer;
    }

    public static PirepLayerData getIceLayerData(AircraftFlightCondition layer) {
        PirepLayerData dataLayer = new PirepLayerData(); //null;
        boolean isValid = false;

        String intensity = layer.getIntensity1();
        if (intensity != null) {
            dataLayer.setIceInten(intensity);
            isValid = true;
            
        }
        String intensity2 = layer.getIntensity2();
        if (intensity2 != null) {
            dataLayer.setIceInten(intensity+intensity2);
            isValid = true;
        }
        String type = layer.getType();
        if (type != null) {
            dataLayer.setIceType(type);
            isValid = true;
        }
        
        // if we have at least one intensity and/or type get the height info.
        if (isValid) {
            // reset so we can ensure at least one height.
            //isValid = false;
            Integer hgt = layer.getBaseHeight();
            if (hgt != null) {
                dataLayer.setIceBaseHeight(hgt);
                isValid = true;
            }
            hgt = layer.getTopHeight();
            if (hgt != null) {
                dataLayer.setIceTopHeight(hgt);
                isValid = true;
            }
        }
        if (!isValid) {
            dataLayer = null;
        }
        if (dataLayer != null) {   
            dataLayer.setLayerType(PirepLayerData.LAYER_TYP_ICING);
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
        PirepLayerData cloudLayer = new PirepLayerData();

        boolean isValid = false;

        String intensity = layer.getCloudCover1();
        if (intensity != null) {
            cloudLayer.setSkyInten1(intensity);
            isValid = true;
        } 
        
        String intensity2 = layer.getCloudCover2();
        if (intensity2 != null) {
            cloudLayer.setSkyInten1(intensity +intensity2);
            cloudLayer.setSkyInten2(intensity2);
            isValid = true;
        }
        // if we have at least one cloud coverage
        if (isValid) {
            // reset so we can ensure at least one height.
            //isValid = false;
            Integer hgt = layer.getCloudBaseHeight();
            if (hgt != null) {
                cloudLayer.setSkyBaseHeight(hgt);
                isValid = true;
            }
            hgt = layer.getCloudTopHeight();
            if (hgt != null) {
                cloudLayer.setSkyTopHeight(hgt);
                isValid = true;
            }
        }
        if (!isValid) {
            cloudLayer = null;
        }
        if (cloudLayer != null) {
            //cloudLayer.setDataType(null);
            cloudLayer.setLayerType(PirepLayerData.LAYER_TYP_CLOUD);
        }
        
        return cloudLayer;
    }

}
