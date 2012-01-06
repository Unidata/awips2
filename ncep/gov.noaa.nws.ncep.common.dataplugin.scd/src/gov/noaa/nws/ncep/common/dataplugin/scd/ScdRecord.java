/**
 * 
 * ScdRecord
 * 
 * This java class performs the mapping to the database tables for SCD.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		-------	 	--------	-----------
 * 12/2008		41			T. Lee		Created
 * 04/2009		41			T. Lee		Migrated to TO10
 * 07/2009		41			T. Lee		Migrated to TO11
 * 11/2009		41			T. Lee		Migrated to TO11D6
 * </pre>
 * 
 * @author T.Lee
 * @version 1.0
 * 
 */

package gov.noaa.nws.ncep.common.dataplugin.scd;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;  
import java.util.Calendar;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

@Entity
@Table(name = "scd", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class ScdRecord extends PluginDataObject{

	private static final long serialVersionUID = 1L;
	
	/** Report type */
	@Column(length=32)
	@XmlElement
	@DynamicSerializeElement
	private String reportType;

	/** Station ID */
	@Column(length=32)
	@DataURI(position=1)
	@XmlElement
	@DynamicSerializeElement
	private String stationID;

	/** Bulletin correction */
	@Column(length=8)
	@DataURI(position=2)
	@XmlElement
	@DynamicSerializeElement
	private String corr;

	/** Bulletin observation time */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private Calendar obsTime;

	/** Bulletin insurance time */
	@Column
	@DataURI(position=3)
	@XmlElement
	@DynamicSerializeElement
	private Calendar issueTime;

	/** Maximum 24h temperature in Celsius */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private float TDXC;

	/** Minimum 24h temperature in Celsius */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private float TDNC;

	/** Six hour accumulated precipitation in inches */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private float P06I;

	/** Twenty-four hour accumulated precipitation in inches */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private float P24I;

	/** Character weather phenomenon */
	@Column(length=16)
	@XmlElement
	@DynamicSerializeElement
	private String WTHR;

	/** Snow depth */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private float SNOW;

	/** New snow depth on the ground */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private float SNEW;

	/** Total snow depth in a Calendar day */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private float S24I;

	/** Water equivalent of snow */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private float WEQS;

	/** Duration of sunshine */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private int MSUN;

	/** Low-level cloud genera from WMO Code 0513 */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private int CTYL;

	/** Mid-level cloud genera from WMO Code 0513 */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private int CTYM;

	/** High-level cloud genera from WMO Code 0513 */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private int CTYH;

	/** Fraction of celestial dome covered by cloud from WMO Code 2700 */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private int CFRT;

	/** Fraction of celestial dome covered by low or mid cloud from WMO Code 2700 */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private int CFRL;

	/** Cloud base height from WMO Code 1600 */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private int CBAS;

	/** Suspect time flag */
	@Column
	@XmlElement
	@DynamicSerializeElement
	private Boolean suspectTimeFlag;

	/** Raw report */
	@Column(length=128)
	@XmlElement
	@DynamicSerializeElement
	private String report;

	/**
	 * Default constructor
	 * 
	 */
	public ScdRecord(){
		this.stationID = null;
		this.issueTime = null;
		this.corr = "REG";
		this.obsTime = null;
		this.TDXC = IDecoderConstantsN.FLOAT_MISSING;
		this.TDNC = IDecoderConstantsN.FLOAT_MISSING;
		this.P06I = IDecoderConstantsN.FLOAT_MISSING;
		this.P24I = IDecoderConstantsN.FLOAT_MISSING;
		this.WTHR = "";
		this.SNOW = IDecoderConstantsN.FLOAT_MISSING;
		this.SNEW = IDecoderConstantsN.FLOAT_MISSING;
		this.S24I = IDecoderConstantsN.FLOAT_MISSING;
		this.WEQS = IDecoderConstantsN.FLOAT_MISSING;
		this.MSUN = IDecoderConstantsN.INTEGER_MISSING;
		this.CTYL = IDecoderConstantsN.INTEGER_MISSING;
		this.CTYM = IDecoderConstantsN.INTEGER_MISSING;
		this.CTYH = IDecoderConstantsN.INTEGER_MISSING;
		this.CFRT = IDecoderConstantsN.INTEGER_MISSING;
		this.CFRL = IDecoderConstantsN.INTEGER_MISSING;
		this.CBAS = IDecoderConstantsN.INTEGER_MISSING;
		this.suspectTimeFlag = false;
		this.report = "";
	}
	
	/**
	 * Constructs a SCD record from a dataURI
	 * 
	 * @param uri	The dataURI
	 */
	public ScdRecord(String uri) {
		super(uri);
	}

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }
    
    public String getReportType() {
        return reportType;
    }

    public void setReportType(String reportType) {
        this.reportType = reportType;
    }
    
	public String getStationID(){
		return stationID;
	}

	public void setStationID(String stationID){
		this.stationID=stationID;
	}
	public Calendar getIssueTime(){
		return issueTime;
	}

	public void setIssueTime(Calendar issueTime){
		this.issueTime=issueTime;
	}
	public String getCorr(){
		return corr;
	}

	public void setCorr(String corr){
		this.corr=corr;
	}
	public Calendar getObsTime(){
		return obsTime;
	}

	public void setObsTime(Calendar obsTime){
		this.obsTime=obsTime;
	}
	public float getTDXC(){
		return TDXC;
	}

	public void setTDXC(float TDXC){
		this.TDXC=TDXC;
	}
	public float getTDNC(){
		return TDNC;
	}

	public void setTDNC(float TDNC){
		this.TDNC=TDNC;
	}
	public float getP06I(){
		return P06I;
	}

	public void setP06I(float P06I){
		this.P06I=P06I;
	}
	public float getP24I(){
		return P24I;
	}

	public void setP24I(float P24I){
		this.P24I=P24I;
	}
	public String getWTHR(){
		return WTHR;
	}

	public void setWTHR(String WTHR){
		this.WTHR=WTHR;
	}
	public float getSNOW(){
		return SNOW;
	}

	public void setSNOW(float SNOW){
		this.SNOW=SNOW;
	}
	public float getSNEW(){
		return SNEW;
	}

	public void setSNEW(float SNEW){
		this.SNEW=SNEW;
	}
	public float getS24I(){
		return S24I;
	}

	public void setS24I(float S24I){
		this.S24I=S24I;
	}
	public float getWEQS(){
		return WEQS;
	}

	public void setWEQS(float WEQS){
		this.WEQS=WEQS;
	}
	public int getMSUN(){
		return MSUN;
	}

	public void setMSUN(int MSUN){
		this.MSUN=MSUN;
	}
	public int getCTYL(){
		return CTYL;
	}

	public void setCTYL(int CTYL){
		this.CTYL=CTYL;
	}
	public int getCTYM(){
		return CTYM;
	}

	public void setCTYM(int CTYM){
		this.CTYM=CTYM;
	}
	public int getCTYH(){
		return CTYH;
	}

	public void setCTYH(int CTYH){
		this.CTYH=CTYH;
	}
	public int getCFRT(){
		return CFRT;
	}

	public void setCFRT(int CFRT){
		this.CFRT=CFRT;
	}
	public int getCFRL(){
		return CFRL;
	}

	public void setCFRL(int CFRL){
		this.CFRL=CFRL;
	}
	public int getCBAS(){
		return CBAS;
	}

	public void setCBAS(int CBAS){
		this.CBAS=CBAS;
	}

	public String getReport() {
		return report;
	}

	public void setReport(String report) {
		this.report = report;
	}
	
	public Boolean getSuspectTimeFlag() {
		return suspectTimeFlag;
	}

	public void setSuspectTimeFlag(Boolean suspectTimeFlag) {
		this.suspectTimeFlag = suspectTimeFlag;
	}
}
