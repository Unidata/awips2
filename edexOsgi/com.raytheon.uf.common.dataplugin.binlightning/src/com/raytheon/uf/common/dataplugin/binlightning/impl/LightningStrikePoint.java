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

import com.raytheon.uf.edex.decodertools.core.BasePoint;

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
 * 
 * </pre>
 *
 * @author jkorman
 * @version 1.0
 */
public class LightningStrikePoint extends BasePoint
{
    
    private LtgStrikeType type;
    
    private LtgMsgType msgType;

    // Lightning strike strength and polarity
    private double strikeStrength;

    // Number of strikes for this record.
    private int strikeCount;

    // JJG - Lightning data source
	private String lightSource;
    
    /**
     * Construct a LightningStrikePoint using given data.
     * @param base The base point which should contain a valid time.
     * @param latitude The latitude of the strike.
     * @param longitude The longitude of the strike.
     * @param type The strike message type.
     */
    public LightningStrikePoint(BasePoint base, double latitude, double longitude, LtgMsgType type)
    {
        super(base);
        setLatitude(latitude);
        setLongitude(longitude);
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
        super(latitude,longitude);
        setElevation(0);
    }

    /**
     * Get the strike count.
     * @return The strike count.
     */
    public int getStrikeCount()
    {
        return strikeCount;
    }

    /**
     * Set the strike count.
     * @return The strike count.
     */
    public void setStrikeCount(int strikeCount)
    {
        this.strikeCount = strikeCount;
    }

    /**
     * Get the strike strength. 
     * @return The strike strength.
     */
    public double getStrikeStrength()
    {
        return strikeStrength;
    }

    /**
     * Set the strike strength. 
     * @return The strike strength.
     */
    public void setStrikeStrength(double strikeStrength)
    {
        this.strikeStrength = strikeStrength;
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
    	buffer.append(String.format("%4d%02d%02d",getYear(),getMonth(),getDay()));
    	buffer.append(String.format("%02d%02d%02d",getHour(),getMinute(),getSecond()));
        
    	buffer.append(String.format(" %9.5f %10.5f ",getLatitude(), getLongitude() ));
    	buffer.append(String.format("%2s %2s ",msgType.getType(),type.getType()));

    	buffer.append(String.format("%4.0f %02d %02d", strikeStrength,(getMillis() / 100),strikeCount));
    	
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
    
}
