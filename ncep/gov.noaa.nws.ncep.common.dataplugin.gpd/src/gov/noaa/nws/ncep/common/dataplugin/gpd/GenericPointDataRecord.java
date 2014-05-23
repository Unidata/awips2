/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/30/2013				Chin J. Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin J. Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.common.dataplugin.gpd;

import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataProductInfo;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

@Entity
// @Table(name = "gpd", uniqueConstraints = { @UniqueConstraint(columnNames = {
// "dataURI" }) })
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "gpdseq")
@Table(name = "gpd")
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class GenericPointDataRecord extends PersistablePluginDataObject
        implements
        /* ISpatialEnabled, */IDecoderGettable, IPointData, IPersistable {

    private static final long serialVersionUID = 1L;

    @ManyToOne
    @PrimaryKeyJoinColumn
    // @DataURI(position = 1, embedded = true)
    @DynamicSerializeElement
    @XmlElement
    private GenericPointDataProductInfo productInfo;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @DynamicSerializeElement
    @XmlElement
    private ObStation location;

    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private float slat;

    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private float slon;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    /*
     * TBM - chin...delete these later..not used?
     * 
     * @Column
     * 
     * @DynamicSerializeElement
     * 
     * @XmlAttribute private int numLevel;
     */

    @Column
    @DynamicSerializeElement
    @XmlAttribute
    // @DataURI(position = 2)
    // product release version for product correction used only.
    private int productVersion = 0;

    // list of master level values
    @Transient
    @XmlElement
    private List<Float> levelValueLst = new ArrayList<Float>();

    // one level of parameters value map. Key (String) = parameter name, value
    // (double) = parameter value
    // @Transient
    // private HashMap<String, Double> Map_ParmToValue = new HashMap<String,
    // Double>();

    // Map of all levels of parameters value maps. Key (integer) = level value
    // in levelValueLst, value =
    // one level of parameter values = a Map_ParmToValue map).
    // @Transient
    // private HashMap<Float, HashMap<String, Double>> Map_LevelToParmValueMap =
    // new HashMap<Float, HashMap<String, Double> >();

    public GenericPointDataRecord() {
        super();
        // System.out.println("GenericPointDataRecord() entered");

    }

    public GenericPointDataRecord(String uri) {
        super(uri);
        // System.out.println("GenericPointDataRecord(String uri) entered");
    }

    public GenericPointDataRecord(String uri,
            GenericPointDataProductInfo productInfo, ObStation location,
            float slat, float slon, PointDataView pointDataView) {
        super(uri);
        this.productInfo = productInfo;
        this.location = location;
        this.slat = slat;
        this.slon = slon;
        this.pointDataView = pointDataView;
        // this.pluginName = "gpd";
        // System.out.println("GenericPointDataRecord(3) entered");
    }

    public GenericPointDataRecord(GenericPointDataProductInfo productInfo,
            ObStation location, float slat, float slon,
            PointDataView pointDataView, DataTime dataTime, int productVersion) {
        this.productInfo = productInfo;
        this.location = location;
        this.slat = slat;
        this.slon = slon;
        this.pointDataView = pointDataView;
        this.dataTime = dataTime;
        this.productVersion = productVersion;
        // this.pluginName = "gpd";
        // System.out.println("GenericPointDataRecord(4) entered");
    }

    /*
     * TBM - chin...delete it later..not used? public void
     * constructTransientListAndMap(GenericPointDataStationProduct stnPd){
     * setNumLevel(stnPd.getNumLevel()); for(int i=0; i<stnPd.getNumLevel(); i++
     * ){ GenericPointDataLevel gpdLevel=stnPd.getLevelLst().get(i);
     * getLevelValueLst().add(i, gpdLevel.getLevelValue());
     * for(GenericPointDataParameter gdpParm: gpdLevel.getGpdParameters()){
     * getMap_ParmToValue().put(gdpParm.getName(),(double) gdpParm.getValue());
     * } getMap_LevelToParmValueMap().put(gpdLevel.getLevelValue(),
     * getMap_ParmToValue()); } }
     */

    public GenericPointDataProductInfo getProductInfo() {
        return productInfo;
    }

    public void setProductInfo(GenericPointDataProductInfo productInfo) {
        this.productInfo = productInfo;
    }

    public float getSlat() {
        return slat;
    }

    public void setSlat(float slat) {
        this.slat = slat;
    }

    public float getSlon() {
        return slon;
    }

    public void setSlon(float slon) {
        this.slon = slon;
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public PointDataView getPointDataView() {
        return this.pointDataView;
    }

    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    @Override
    public Amount getValue(String paramName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Collection<Amount> getValues(String paramName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getString(String paramName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String[] getStrings(String paramName) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * TBM - chin...delete these later..not used?
     * 
     * @Override public void constructDataURI() throws PluginException { // TODO
     * Auto-generated method stub super.constructDataURI(); // ObStation class
     * does not define dataUri component. Therefore, // add
     * stationId/longitude/latitude here to end of dataUri
     * if(this.location!=null){ if(this.location.getStationId()!=null){
     * this.dataURI =
     * this.dataURI+DataURI.SEPARATOR+this.location.getStationId(); } else {
     * this.dataURI = this.dataURI+DataURI.SEPARATOR+"null"; }
     * if(this.location.getStationGeom()!=null){ this.dataURI =
     * this.dataURI+DataURI.SEPARATOR+ this.location.getStationGeom().getX()+
     * DataURI.SEPARATOR+ this.location.getStationGeom().getY(); } else {
     * this.dataURI =
     * this.dataURI+DataURI.SEPARATOR+"null"+DataURI.SEPARATOR+"null"; } } }
     */

    public ObStation getLocation() {
        return location;
    }

    public void setLocation(ObStation location) {
        this.location = location;
    }

    /*
     * TBM - chin...delete these later..not used? public int getNumLevel() {
     * return numLevel; }
     * 
     * public void setNumLevel(int numLevel) { this.numLevel = numLevel; }
     */

    public List<Float> getLevelValueLst() {
        return levelValueLst;
    }

    public void setLevelValueLst(List<Float> levelValueLst) {
        this.levelValueLst = levelValueLst;
    }

    /*
     * TBM - chin...delete these later..not used? public HashMap<String, Double>
     * getMap_ParmToValue() { return Map_ParmToValue; }
     * 
     * public void setMap_ParmToValue(HashMap<String, Double> map_ParmToValue) {
     * Map_ParmToValue = map_ParmToValue; }
     * 
     * public HashMap<Float, HashMap<String, Double>>
     * getMap_LevelToParmValueMap() { return Map_LevelToParmValueMap; }
     * 
     * public void setMap_LevelToParmValueMap( HashMap<Float, HashMap<String,
     * Double>> map_LevelToParmValueMap) { Map_LevelToParmValueMap =
     * map_LevelToParmValueMap; }
     */

    public int getProductVersion() {
        return productVersion;
    }

    public void setProductVersion(int productVersion) {
        this.productVersion = productVersion;
    }

    @Override
    public String getPluginName() {
        // TODO Auto-generated method stub
        return null;
    }

}
