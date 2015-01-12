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
package com.raytheon.uf.common.dataplugin.binlightning.impl;

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Record implementation for the Binary Lightning data decoder.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * 20130227        DCS 152 jgerth      Support for WWLLN and multiple sources
 * Jun 3, 2014  3226      bclement     refactor to support pulse data
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class LightningStrikePoint extends BaseLightningPoint
{
    
    private LtgStrikeType type;
    
    private LtgMsgType msgType;

    // Number of pulses for this record.
    private int pulseCount;

    // JJG - Lightning data source
	private String lightSource;

    private List<LightningPulsePoint> pulses;
    
    /**
     * Construct a LightningStrikePoint using given data.
     * 
     * @param latitude
     *            The latitude of the strike.
     * @param longitude
     *            The longitude of the strike.
     * @param time
     * @param type
     *            The strike message type.
     */
    public LightningStrikePoint(double latitude, double longitude,
            Calendar time, LtgMsgType type)
    {
        super(latitude, longitude, time);
        setElevation(0);
        this.msgType = type;
    }
    
    /**
     * Construct a LightningStrikePoint using given data.
     * @param latitude The latitude of the strike.
     * @param longitude The longitude of the strike.
     */
    public LightningStrikePoint(double latitude, double longitude)
    {
        super(latitude, longitude, TimeUtil.newGmtCalendar());
        setElevation(0);
    }

    /**
     * @param time
     * @param type
     */
    public LightningStrikePoint(Calendar time, LtgMsgType type) {
        super(time);
        this.msgType = type;
    }

    /**
     * Get the pulse count.
     * 
     * @return The pulse count.
     */
    public int getPulseCount()
    {
        return pulseCount;
    }

    /**
     * Set the pulse count.
     * 
     * @return The pulse count.
     */
    public void setPulseCount(int pulseCount)
    {
        this.pulseCount = pulseCount;
    }

    /**
     * Get the strike type.
     * @return The strike message type.
     */
   
    public LtgStrikeType getType()
    {
        return type;
    }

    /**
     * Set the strike type.
     * @return The strike type.
     */
    public void setType(LtgStrikeType type)
    {
        this.type = type;
    }

    /**
     * Get the strike message type.
     * @return The strike message type.
     */
    public LtgMsgType getMsgType()
    {
        return msgType;
    }

    /**
     * Set the strike message type.
     * @return The strike message type.
     */
    public void setMsgType(LtgMsgType msgType)
    {
        this.msgType = msgType;
    }

    /**
     * JJG - Get the lightning source
     * @return
     */
    public String getLightSource()
    {
    	return lightSource;
    }
    
    /**
     * JJG - Set the lightning source
     * @param lightSource
     */
    public void setLightSource(String lightSource)
    {
    	this.lightSource = lightSource;
    }
    
    /**
     * Format this lightning strike report using the NCDC strike observation format.
     * 
     * See Document
     *    DATA SET 9603 (DSI-9603)
     *    National Lightning Detection Network (NLDN)
     *    January 31, 2007
     *
     * @param buffer Buffer to receive the formatted lightning information.
     * @return The formatted buffer.
     */
    public StringBuilder toString(StringBuilder buffer)
    {
    	if(buffer == null)
    	{
    		buffer = new StringBuilder();
    	}
        Calendar obsTime = getTime();
        int year = obsTime.get(Calendar.YEAR);
        int month = obsTime.get(Calendar.MONTH) + 1;
        int day = obsTime.get(Calendar.DAY_OF_MONTH);
        int hour = obsTime.get(Calendar.HOUR);
        int minute = obsTime.get(Calendar.MINUTE);
        int second = obsTime.get(Calendar.SECOND);
        int millis = obsTime.get(Calendar.MILLISECOND);
        buffer.append(String.format("%4d%02d%02d", year, month, day));
        buffer.append(String.format("%02d%02d%02d", hour, minute, second));
        
    	buffer.append(String.format(" %9.5f %10.5f ",getLatitude(), getLongitude() ));
    	buffer.append(String.format("%2s %2s ",msgType.getType(),type.getType()));

        buffer.append(String.format("%4.0f %02d %02d", getStrikeStrength(),
                (millis / 100), pulseCount));
    	
    	return buffer;
    }

    /**
     * Format this lightning strike report using the NCDC strike observation format.
     * 
     * See Document
     *    DATA SET 9603 (DSI-9603)
     *    National Lightning Detection Network (NLDN)
     *    January 31, 2007
     *    
     */
    public String toString()
    {
        return toString(null).toString();
    }

    /**
     * @return the pulses
     */
    public List<LightningPulsePoint> getPulses() {
        return pulses;
    }

    /**
     * @param pulses
     *            the pulses to set
     */
    public void setPulses(List<LightningPulsePoint> pulses) {
        this.pulses = pulses;
    }
    
}
