/**
 * AtcfTrack
 * 
 * Date created 07 Sep 2010
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.edex.uengine.tasks.atcf;

import java.util.Calendar;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.common.dataplugin.atcf.AtcfRecord;

/**
 * Atcf Track
 * 
 * <pre>
 * SOFTWARE HISTORY
 *    Date                  Ticket#     Engineer                  Description
 * ------------------      ----------   ----------------------- --------------------------
 * 07- Sep-2010         284      mgamazaychikov     Initial creation.
 * @author mgamazaychikov
 *</pre>
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AtcfTrack {
	@XmlElement
	@DynamicSerializeElement
	private String technique;
	@XmlElement
	@DynamicSerializeElement
	private float clat[];
	@XmlElement
	@DynamicSerializeElement
	private float clon[];
	@XmlElement
	@DynamicSerializeElement
	private int forecastHour[];
	@XmlElement
	@DynamicSerializeElement
	private Calendar warningTime;
	
	@XmlElement
	@DynamicSerializeElement
	private float windMax[];

	@XmlElement
	@DynamicSerializeElement	
    private String stormName;  
	
	public AtcfTrack() {
	}
	
	public AtcfTrack(String technique, List<PluginDataObject> dataRecords) {
		this.technique = technique;
		int recordSize = dataRecords.size();
		clat = new float[recordSize];
		clon = new float[recordSize];
		windMax = new float[recordSize];
		forecastHour = new int[recordSize];
		stormName = new String();
		for ( int ii=0; ii< recordSize; ii++ ) {
			AtcfRecord atcf = (AtcfRecord)dataRecords.get(ii);
			clat[ii] = atcf.getClat();
			clon[ii] = atcf.getClon();
			forecastHour[ii] = atcf.getFcstHour();
			windMax[ii]            = atcf.getWindMax(); 
			if(ii==0){
				warningTime = atcf.getWarnTime();
			}
			
			if( this.stormName.isEmpty() && this.technique.contains("CARQ") && this.forecastHour[ii] == 0){
				this.stormName = new String(atcf.getStormName());
			}
			
			
		}
	}

	public String getTechnique() {
		return technique;
	}

	public void setTechnique(String technique) {
		this.technique = technique;
	}

	public float[] getClat() {
		return clat;
	}

	public void setClat(float[] clat) {
		this.clat = clat;
	}

	public float[] getClon() {
		return clon;
	}

	public void setClon(float[] clon) {
		this.clon = clon;
	}

	public int[] getForecastHour() {
		return forecastHour;
	}

	public void setForecastHour(int[] forecastHour) {
		this.forecastHour = forecastHour;
	}

	public void setWarningTime(Calendar warntime) {
		this.warningTime = warntime;
	}

	public Calendar getWarningTime() {
		return this.warningTime;
	}

	public void setWindMax(float windMax[]) {
		this.windMax = windMax;
	}

	public float[] getWindMax() {
		return windMax;
	}

	/**
	 * @return the stormName
	 */
	public String getStormName() {
		return stormName;
	}

	/**
	 * @param stormName the stormName to set
	 */
	public void setStormName(String stormName) {
		this.stormName = stormName;
	}

}
