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
package com.raytheon.edex.plugin.binlightning.impl;

import java.util.Calendar;

import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgMsgType;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Decode one or more Flash lightning observations. Decode algorithm is taken
 * from the NWS D2D binary lightning decoder.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * Jun 05, 2014 3226       bclement    LightningStikePoint refactor
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class FlashLightningDecoder extends BaseLightningDecoder
{
    /**
     * Construct and decode a portion of a lightning observation.
     * @param msgData Message data.
     * @param count Number of flash reports contained in the message part.
     */
    public FlashLightningDecoder(IBinDataSource msgData, int count)
    {
        super();
        doDecode(msgData,count);
    }

    /**
     * Perform the message decode.
     * @param msgData Message data.
     * @param count Number of flash reports contained in the message part.
     */
    private void doDecode(IBinDataSource msgData, int count)
    {
        if(msgData.available(TIME_SIZE))
        {
            Calendar baseTime = parseDate(msgData);

            if(msgData.available(FLASH_MSG_SIZE * count))
            {
                for(int i = 0;i < count;i++)
                {
                    double lon = getFlashLon(msgData);
                    double lat = getFlashLat(msgData);

                    double strikeStrength = msgData.getS8() * 2.0;
                    
                    // strike count and 1/10s seconds
                    int u8 = msgData.getU8();
                    int flashCount = u8 & 0x0F;

                    Calendar obsTime = TimeUtil.newCalendar(baseTime);
                    obsTime.set(Calendar.MILLISECOND, ((u8 & 0xF0) >> 4) * 100);
        
                    // Create the strike record from the report info and base
                    // time information.
                    LightningStrikePoint strikeData = new LightningStrikePoint(
                            lat, lon, baseTime, LtgMsgType.STRIKE_MSG_FL);
                    strikeData.setType(DEFAULT_FLASH_TYPE);
                    strikeData.setStrikeStrength(strikeStrength);
                    strikeData.setPulseCount(flashCount);
                    addStrike(strikeData);
                }
            }
            else
            {
                setError(IBinLightningDecoder.NOT_ENOUGH_DATA);
            }
        }
        else
        {
            setError(IBinLightningDecoder.NO_TIME_INFO);
        }
    }
    
    /** 
     * Calculate the lightning strike longitude. From D2D lightning decoder.
     * @param msgData Message data source.
     * @return The lightning longitude.
     */
    private double getFlashLon(IBinDataSource msgData)
    {
        int value = msgData.getU16();
        double lon = (value * 0.001068115234) - 130.0;
        
        return lon;
    }

    /** 
     * Calculate the lightning strike latitude. From D2D lightning decoder.
     * @param msgData Message data source.
     * @return The lightning latitude.
     */
    private double getFlashLat(IBinDataSource msgData)
    {
        int value = msgData.getU16() & 0x7FFF;
        double lat = (value * 0.001281738) + 18.0;
        
        return lat;
    }

}
