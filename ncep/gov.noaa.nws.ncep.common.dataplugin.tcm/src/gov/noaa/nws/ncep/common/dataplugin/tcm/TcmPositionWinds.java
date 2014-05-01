/*
 * 
 * TcmPositionWinds
 * 
 * This java class defines the getters and setters for TCM storm position and 
 * winds data.
 *  
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 06/2009		128			T. Lee		Initial coding
 * 11/2009		128			T. Lee		Migrated to TO11D6
 * 09/2011      			Chin Chen   changed to improve purge performance and
 * 										removed xml serialization as well
 * </pre>
 * 
 * @author T.Lee
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.tcm;

import java.io.Serializable;
import java.util.Calendar;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

@Entity
@Table(name="tcm_position_winds")
@DynamicSerialize
public class TcmPositionWinds implements Serializable, ISerializableObject {
	private static final long serialVersionUID = 1L;
	
	@Id
    @GeneratedValue
    private Integer recordId = null;
	
	
	
	/** The forecast valid time **/
	@Column
	@DynamicSerializeElement
	private Calendar validTime;

	/** The forecast hour **/
	@Column
	@DynamicSerializeElement
	private String fcstHour;
	
	/** The latitude of the storm center **/
	@Column
	@DynamicSerializeElement  
	private Float clat;

	/** The longitude of the storm center **/
	@Column
	@DynamicSerializeElement  
	private Float clon;
	
	/** The maximum wind of the storm **/
	@Column
	@DynamicSerializeElement  
	private Integer windMax;

	/** The wind gust of the storm **/
	@Column
	@DynamicSerializeElement  
	private Integer gust;

	/** Storm moving direction in compass direction */
    @Column
    @DynamicSerializeElement
    private Integer stormDrct;
    
    /** Storm moving speed */
    @Column
    @DynamicSerializeElement
    private Integer stormSped;
    
	/** The extent of 34kts wind at NE quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String ne34k;
	
	/** The extent of 34kts wind at SE quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String se34k;
	
	/** The extent of 34kts wind at SW quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String sw34k;
	
	/** The extent of 34kts wind at NW quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String nw34k;
	
	/** The extent of 50kts wind at NE quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String ne50k;
	
	/** The extent of 50kts wind at SE quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String se50k;
	
	/** The extent of 50kts wind at SW quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String sw50k;
	
	/** The extent of 50kts wind at NW quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String nw50k;
	
	/** The extent of 64kts wind at NE quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String ne64k;
	
	/** The extent of 64kts wind at SE quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String se64k;
	
	/** The extent of 64kts wind at SW quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String sw64k;
	
	/** The extent of 64kts wind at NW quadrant of the storm **/
	@Column(length=8)
	@DynamicSerializeElement  
	private String nw64k;	

    /**
     * No-Arg Constructor
     */
	public TcmPositionWinds() {
		this.validTime = null;
		this.fcstHour = null;
		this.clat = IDecoderConstantsN.FLOAT_MISSING;
		this.clon = IDecoderConstantsN.FLOAT_MISSING;
		this.windMax = IDecoderConstantsN.INTEGER_MISSING;
		this.gust = IDecoderConstantsN.INTEGER_MISSING;
    	this.stormDrct = IDecoderConstantsN.INTEGER_MISSING;
    	this.stormSped = IDecoderConstantsN.INTEGER_MISSING;
		this.ne34k = this.se34k = this.sw34k = this.nw34k = null;
		this.ne50k = this.se50k = this.sw50k = this.nw50k = null;
		this.ne64k = this.se64k = this.sw64k = this.nw64k = null;
    }
	

	/**
	 * @return the serialVersionUID
	 */
	public static long getSerialVersionUID() {
		return serialVersionUID;
	}

	

	/**
	 * @return the recordId
	 */
	public Integer getRecordId() {
		return recordId;
	}

	/**
	 * @param recordId the recordId to set
	 */
	public void setRecordId(Integer recordId) {
		this.recordId = recordId;
	}

	/**
	 * @return the validTime
	 */
	public Calendar getValidTime() {
		return validTime;
	}

	/**
	 * @param validTime the validTime to set
	 */
	public void setValidTime(Calendar validTime) {
		this.validTime = validTime;
	}

	/**
	 * @return the fcstHour
	 */
	public String getFcstHour() {
		return fcstHour;
	}

	/**
	 * @param fcstHour the fcstHour to set
	 */
	public void setFcstHour(String fcstHour) {
		this.fcstHour = fcstHour;
	}

	/**
	 * @return the clat
	 */
	public Float getClat() {
		return clat;
	}

	/**
	 * @param clat the clat to set
	 */
	public void setClat(Float clat) {
		this.clat = clat;
	}

	/**
	 * @return the clon
	 */
	public Float getClon() {
		return clon;
	}

	/**
	 * @param clon the clon to set
	 */
	public void setClon(Float clon) {
		this.clon = clon;
	}

	/**
	 * @return the wmax
	 */
	public Integer getWindMax() {
		return windMax;
	}

	/**
	 * @param wmax the wmax to set
	 */
	public void setWindMax(Integer windMax) {
		this.windMax = windMax;
	}

	/**
	 * @return the gust
	 */
	public Integer getGust() {
		return gust;
	}

	/**
	 * @param gust the gust to set
	 */
	public void setGust(Integer gust) {
		this.gust = gust;
	}
	
	/**
	 * @return the storm moving stormDrct
	 */
	public Integer getStormDrct() {
		return stormDrct;
	}

	/**
	 * @return the storm moving stormSped
	 */
	public Integer getStormSped() {
		return stormSped;
	}

	/**
	 * @param drct the storm moving stormDrct to set
	 */
	public void setStormDrct(Integer stormDrct) {
		this.stormDrct = stormDrct;
	}

	/**
	 * @param sped the storm moving stormSped to set
	 */
	public void setStormSped(Integer stormSped) {
		this.stormSped = stormSped;
	}

	/**
	 * @return the ne34k
	 */
	public String getNe34k() {
		return ne34k;
	}

	/**
	 * @param ne34k the ne34k to set
	 */
	public void setNe34k(String ne34k) {
		this.ne34k = ne34k;
	}

	/**
	 * @return the se34k
	 */
	public String getSe34k() {
		return se34k;
	}

	/**
	 * @param se34k the se34k to set
	 */
	public void setSe34k(String se34k) {
		this.se34k = se34k;
	}

	/**
	 * @return the sw34k
	 */
	public String getSw34k() {
		return sw34k;
	}

	/**
	 * @param sw34k the sw34k to set
	 */
	public void setSw34k(String sw34k) {
		this.sw34k = sw34k;
	}

	/**
	 * @return the nw34k
	 */
	public String getNw34k() {
		return nw34k;
	}

	/**
	 * @param nw34k the nw34k to set
	 */
	public void setNw34k(String nw34k) {
		this.nw34k = nw34k;
	}

	/**
	 * @return the ne50k
	 */
	public String getNe50k() {
		return ne50k;
	}

	/**
	 * @param ne50k the ne50k to set
	 */
	public void setNe50k(String ne50k) {
		this.ne50k = ne50k;
	}

	/**
	 * @return the se50k
	 */
	public String getSe50k() {
		return se50k;
	}

	/**
	 * @param se50k the se50k to set
	 */
	public void setSe50k(String se50k) {
		this.se50k = se50k;
	}

	/**
	 * @return the sw50k
	 */
	public String getSw50k() {
		return sw50k;
	}

	/**
	 * @param sw50k the sw50k to set
	 */
	public void setSw50k(String sw50k) {
		this.sw50k = sw50k;
	}

	/**
	 * @return the nw50k
	 */
	public String getNw50k() {
		return nw50k;
	}

	/**
	 * @param nw50k the nw50k to set
	 */
	public void setNw50k(String nw50k) {
		this.nw50k = nw50k;
	}

	/**
	 * @return the ne64k
	 */
	public String getNe64k() {
		return ne64k;
	}

	/**
	 * @param ne64k the ne64k to set
	 */
	public void setNe64k(String ne64k) {
		this.ne64k = ne64k;
	}

	/**
	 * @return the se64k
	 */
	public String getSe64k() {
		return se64k;
	}

	/**
	 * @param se64k the se64k to set
	 */
	public void setSe64k(String se64k) {
		this.se64k = se64k;
	}

	/**
	 * @return the sw64k
	 */
	public String getSw64k() {
		return sw64k;
	}

	/**
	 * @param sw64k the sw64k to set
	 */
	public void setSw64k(String sw64k) {
		this.sw64k = sw64k;
	}

	/**
	 * @return the nw64k
	 */
	public String getNw64k() {
		return nw64k;
	}

	/**
	 * @param nw64k the nw64k to set
	 */
	public void setNw64k(String nw64k) {
		this.nw64k = nw64k;
	}
}
