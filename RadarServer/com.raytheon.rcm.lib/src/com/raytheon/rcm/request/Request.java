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
package com.raytheon.rcm.request;

import java.util.Arrays;
import java.util.Calendar;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.rcm.message.Message;


@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(propOrder={})
/**
 * Encapsulates a Nexrad product request.
 */
public class Request implements Cloneable {
	public static final int SELECT_SPECIFIC = 0;
	public static final int SELECT_CURRENT = -1;
	public static final int SELECT_LATEST = -2; // latest available
	
	public static final int SPECIFIC_ELEVATION = 0;
	public static final int LOWER_ELEVATIONS = 1 << 13;
	public static final int ALL_ELEVATIONS = 1 << 14;
	public static final int N_ELEVATIONS = (1 << 14) | (1 << 13);
	
	/** Repeat count value that indicates continuous transmission of a 
	 * product */
	public static final int CONTINUOUS = -1; 
	
	protected static final int ELEVATION_TYPE_MASK = (1 << 14) | (1 << 13);
	protected static final int ELEVATION_ANGLE_MASK = (1 << 13) - 1;

	@XmlElement(required=false)	public String comment; // Not used for sending requests
	
	@XmlElement public short productCode;
	@XmlElement(required=false) public boolean highPriority;
	@XmlElement(required=false) public boolean mapRequested;
	@XmlElement(required=false) public short sequence;
	@XmlElement(required=false) public short count = 1; // Default to 1 because 0 is always invalid
	@XmlElement(required=false) public short interval = 1; // Default to 1 because 0 is always invalid
	
	@XmlElement protected int volumeScanSelection = SELECT_CURRENT;
	@XmlElement(required=false) protected Calendar volumeScanTime;

	public int getVolumeScanSelection()  { return volumeScanSelection; }
	public Calendar getVolumeScanTime() { return volumeScanTime; }
	public void selectCurrent() { volumeScanSelection = SELECT_CURRENT; }
	public void selectLatest() { volumeScanSelection = SELECT_LATEST; }
	public void selectTime(Calendar time) {
		if (time == null)
			throw new IllegalArgumentException("time argument must not be null");
		volumeScanSelection = SELECT_SPECIFIC;
		volumeScanTime = time;
	}	
	
	@XmlElement(required=false,defaultValue="0") 
	public int pdw20, pdw21, pdw22, pdw23, pdw24, pdw25;
	
	// maybe setRawElevationCode(), setElevation(mode, tenthsOfDegrees)
	public int getElevationSelection() { return pdw22 & ELEVATION_TYPE_MASK; } 
	public int getElevationAngle() {
		int elev = pdw22 & ELEVATION_ANGLE_MASK;
		if ((pdw22 & ELEVATION_TYPE_MASK) == N_ELEVATIONS)
			return elev;
		else
			return Message.decodeElevation(elev);
	}
	public void setElevationAngle(int angle) { pdw22 = Message.encodeElevation(angle); }
	public void selectLowerCuts(int highestCut) {	pdw22 = N_ELEVATIONS | highestCut; } 
	public void selectLowerEelevations(int highestElevation) { 
		pdw22 = LOWER_ELEVATIONS | Message.encodeElevation(highestElevation);
	}
	public void selectAllElevations() { pdw22 = ALL_ELEVATIONS; }
	public void selectAllElevations(int angle) { pdw22 = ALL_ELEVATIONS | Message.encodeElevation(angle); }
	
	public int getAzimuth() { return pdw20; }
	public void setAzimuth(int azimuth) { pdw20 = azimuth; }
	
	public int getRange() { return pdw21; }
	public void setRange(int range) { pdw21 = range; }
	
	public int getAzimuth2() { return pdw22; }
	public void setAzimuth2(int azimuth) { pdw22 = azimuth; }
	
	public int getRange2() { return pdw23; }
	public void setRange2(int range) { pdw23 = range; }
	
	public int getStormSpeed() { return pdw23; }
	public void setStormSpeed(int speed) { pdw23 = speed; }
	
	public int getStormDirection() { return pdw24; }
	public void setStormDirection(int direction) { pdw24 = direction; }

	// for VAD
	public int getAltitude() { return pdw22; }
	public void setAltitude(int altitude) { pdw22 = altitude; }
	
	public int getEndHour() { return pdw20; }
	public void setEndHour(int endHour) { pdw20 = endHour; }
	
	public int getTimeSpan() { return pdw21; }
	public void setTimeSpan(int timeSpan) { pdw21 = timeSpan; }
	
	public int getBottomAltitude() { return pdw20; }
	public void setBottomAltitude(int altitude) { pdw20 = altitude; }
	
	public int getTopAltitude() { return pdw21; }
	public void setTopAltitude(int altitude) { pdw21 = altitude; }

	public int getCfcWord() { return pdw20; }
	public void setCfcWord(int cfcWord) { pdw20 = cfcWord; }
	
	public int getContourInterval() { return pdw25; }
	public void setContourInterval(int contourInterval) { pdw25 = contourInterval; }
	
	public int getMiniVolume() { return pdw20; }
	public void setMiniVolume(int miniVolume) { pdw20 = miniVolume; }

	@Override
	public String toString() {
		int sel = getVolumeScanSelection();
		return String.format("{code=%d %s%scount=%d interval=%d pdw=%s sel=%s seq=%s}", 
				productCode,
				highPriority ? "high-priority " : "",
				mapRequested ? "map-requested " : "",
				count, interval, 
				Arrays.asList(pdw20,pdw21,pdw22,pdw23,pdw24,pdw25),
				(sel==SELECT_CURRENT?"current":
					(sel==SELECT_LATEST?"latest":
						String.format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS",
								getVolumeScanTime()))),
				sequence != 0 ? Short.toString(sequence) : ""
				);
	}
	public Request clone() {
		Request r;
		try {
			r = (Request) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new RuntimeException(e);
		}
		if (volumeScanTime != null)
			r.volumeScanTime = (Calendar) volumeScanTime.clone();
		return r;
	}
}
