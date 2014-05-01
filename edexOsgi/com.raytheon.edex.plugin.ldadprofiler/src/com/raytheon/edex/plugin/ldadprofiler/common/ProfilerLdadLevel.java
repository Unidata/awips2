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
package com.raytheon.edex.plugin.ldadprofiler.common;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * ProfilerLdadLevel contains the data for a single vertical level observation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                     
 * ate          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 10/14/09                 vkorolev    Initial creation
 * </pre>
 * 
 * @author vkorolev
 * @version 1
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class ProfilerLdadLevel implements Serializable, ISerializableObject{
	
    private static final long serialVersionUID = 1L;
	
    /** The primary key for the database table * */
    @Id
    @GeneratedValue
    private Integer key;
    
	// Vertical Gate Height meters
	@Column
	@DynamicSerializeElement
	@XmlElement
	Integer levelHeight;
	

	// U (toward east) component of wind "m/s"
	@Column
	@DynamicSerializeElement
	@XmlElement
	Double ucWind;

	// U confidence 0.0-1.0
	@Column
	@DynamicSerializeElement
	@XmlElement
	Double uconf;

	// V (toward north) component of wind "m/s"
	@Column
	@DynamicSerializeElement
	@XmlElement
	Double vcWind;

	// V confidence 0.0-1.0
	@Column
	@DynamicSerializeElement
	@XmlElement
	Double vconf;

	// W (upward) component of wind "m/s"
	@Column
	@DynamicSerializeElement
	@XmlElement
	Double wcWind;

	// W confidence 0.0-1.0
	@Column
	@DynamicSerializeElement
	@XmlElement
	Double wconf;

	// wind speed "knots"
	@Column
	@DynamicSerializeElement
	@XmlElement
	Double windSpeed;

	// wind direction (true) "degrees"
	@Column
	@DynamicSerializeElement
	@XmlElement
	Double windDir;
	
    @ManyToOne
    @JoinColumn(name = "parentLdadprofiler", nullable = false)
    private ProfilerLdadObs parentLdadprofiler;

	/**
	 * @return the ucWind
	 */
	public Double getUcWind() {
		return ucWind;
	}

	/**
	 * @param ucWind the ucWind to set
	 */
	public void setUcWind(Double ucWind) {
		this.ucWind = ucWind;
	}

	/**
	 * @return the uconf
	 */
	public Double getUconf() {
		return uconf;
	}

	/**
	 * @param uconf the uconf to set
	 */
	public void setUconf(Double uconf) {
		this.uconf = uconf;
	}

	/**
	 * @return the vcWind
	 */
	public Double getVcWind() {
		return vcWind;
	}

	/**
	 * @param vcWind the vcWind to set
	 */
	public void setVcWind(Double vcWind) {
		this.vcWind = vcWind;
	}

	/**
	 * @return the vconf
	 */
	public Double getVconf() {
		return vconf;
	}

	/**
	 * @param vconf the vconf to set
	 */
	public void setVconf(Double vconf) {
		this.vconf = vconf;
	}

	/**
	 * @return the wcWind
	 */
	public Double getWcWind() {
		return wcWind;
	}

	/**
	 * @param wcWind the wcWind to set
	 */
	public void setWcWind(Double wcWind) {
		this.wcWind = wcWind;
	}

	/**
	 * @return the wconf
	 */
	public Double getWconf() {
		return wconf;
	}

	/**
	 * @param wconf the wconf to set
	 */
	public void setWconf(Double wconf) {
		this.wconf = wconf;
	}

	/**
	 * @return the windSpeed
	 */
	public Double getWindSpeed() {
		return windSpeed;
	}

	/**
	 * @param windSpeed the windSpeed to set
	 */
	public void setWindSpeed(Double windSpeed) {
		this.windSpeed = windSpeed;
	}

	/**
	 * @return the windDir
	 */
	public Double getWindDir() {
		return windDir;
	}

	/**
	 * @param windDir the windDir to set
	 */
	public void setWindDir(Double windDir) {
		this.windDir = windDir;
	}

	/**
	 * @return the levelHeight
	 */
	public Integer getLevelHeight() {
		return levelHeight;
	}

	/**
	 * @param levelHeight the levelHeight to set
	 */
	public void setLevelHeight(Integer levelHeight) {
		this.levelHeight = levelHeight;
	}
	
	/**
	 * @param key the key to set
	 */
	public void setKey(Integer key) {
		this.key = key;
	}

	/**
	 * @return the key
	 */
	public Integer getKey() {
		return key;
	}
	
	/**
	 * @param parentLdadprofiler the parentLdadprofiler to set
	 */
	public void setParentLdadprofiler(ProfilerLdadObs parentLdadprofiler) {
		this.parentLdadprofiler = parentLdadprofiler;
	}

	/**
	 * @return the parentLdadprofiler
	 */
	public ProfilerLdadObs getParentLdadprofiler() {
		return parentLdadprofiler;
	}
	
	//---------------------------------------------------------
	/**
	 * Construct an empty instance.
	 */
	public ProfilerLdadLevel() {
	}

}
