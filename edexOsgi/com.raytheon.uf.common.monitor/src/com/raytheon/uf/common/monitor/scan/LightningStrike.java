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
package com.raytheon.uf.common.monitor.scan;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * LightningStrike, Used to hold data derived from BinLightning
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/12/2009   2307      dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class LightningStrike implements ISerializableObject {
	
    @XmlElement
    @DynamicSerializeElement
	public Double lat = 0.0;
    @XmlElement
    @DynamicSerializeElement
	public Double lon = 0.0;
    @XmlElement
    @DynamicSerializeElement
	public int intensity = 0;
    @XmlElement
    @DynamicSerializeElement
	public int strikeType = 0;
    @XmlElement
    @DynamicSerializeElement
	public int msgType = 0;
    @XmlElement
    @DynamicSerializeElement
	public int strikeCount = 0;
	
	/**
	 * Public lightning strike 
	 * @param lat
	 * @param lon
	 * @param intensity
	 * @param strikeType
	 * @param msgType
	 * @param strikeCount
	 */
	public LightningStrike(Double lat, Double lon, int intensity, int strikeType, int msgType, int strikeCount) {
		this.lat = lat;
		this.lon = lon;
		this.intensity = intensity;
		this.strikeType = strikeType;
		this.msgType = msgType;
		this.strikeCount = strikeCount;
	}
	
	/**
	 * Serializable 
	 */
	public LightningStrike() {
   
    }
	
	/**
	 * Gets the lat
	 * @return
	 */
	public Double getLat() {
		return lat;
	}
	/**
	 * sets the lat
	 * @param lat
	 */
	public void setLat(Double lat) {
		this.lat = lat;
	}
	/**
	 * Gets the lon
	 * @return
	 */
	public Double getLon() {
		return lon;
	}
	/**
	 * set the lon
	 * @param lon
	 */
	public void setLon(Double lon) {
		this.lon = lon;
	}
	/**
	 * gets the intensity (polarity) of the strike
	 * @return
	 */
	public int getIntensity() {
		return intensity;
	}
	/**
	 * Set the intensity (polarity) of strike
	 * @param intensity
	 */
	public void setIntensity(int intensity) {
		this.intensity = intensity;
	}
	/**
	 * Get the type of strike
	 * @return
	 */
	public int getStrikeType() {
		return strikeType;
	}
	/**
	 * Set the type of the strike
	 * @param strikeType
	 */
	public void setStrikeType(int strikeType) {
		this.strikeType = strikeType;
	}
	/**
	 * get the type of the message
	 * @return
	 */
	public int getMsgType() {
		return msgType;
	}
	/**
	 * Set the type of the message
	 * @param msgType
	 */
	public void setMsgType(int msgType) {
		this.msgType = msgType;
	}
	/**
	 * Get the total strike count
	 * @return
	 */
	public int getStrikeCount() {
		return strikeCount;
	}
	/**
	 * Set the total strike count
	 * @param strikeCount
	 */
	public void setStrikeCount(int strikeCount) {
		this.strikeCount = strikeCount;
	}
	
}
