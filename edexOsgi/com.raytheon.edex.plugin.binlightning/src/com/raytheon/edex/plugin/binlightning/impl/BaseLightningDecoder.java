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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.decodertools.core.BasePoint;
import com.raytheon.uf.edex.decodertools.core.IBinDataSource;

/**
 * Provide the base class for the binary lightning decoders. This class
 * abstracts data and methods common to the current lightning decoder types.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * 20070912            379 jkorman     Code review cleanup.
 * May 14, 2014 2536       bclement    removed TimeTools
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
abstract class BaseLightningDecoder implements IBinLightningDecoder
{
    private final Calendar BASE_TIME = TimeUtil.newGmtCalendar(1980, 2, 29);
    
    private static final int DAYS_MASK = 0xFFFE;
    
    private static final int DAYS_SHFT = 1;
    
    private static final int HOURS_HI_BIT_MASK = 0x0001;
    
    private static final int HOURS_HI_BIT_SHFT = 0x0004;
    
    private static final int HOURS_LO_NYB_MASK = 0x00F0;
    
    private static final int HOURS_LO_NYB_SHFT = 0x0004;
    
    private static final int MIN_P1_MASK = 0x000F;
    
    private static final int MIN_P1_SHFT = 2;

    private static final int MIN_P2_MASK = 0x00C0;
    
    private static final int MIN_P2_SHFT = 6;

    private static final int SECONDS_MASK = 0x003F;

    // package private visibility - only sub-classes need to see these.
    static final int FLASH_MSG_SIZE = 6;

    static final int RT_MSG_SIZE = 8;

    static final int TIME_SIZE = 4;
    
    private int lastError = NO_ERROR;

    private List<LightningStrikePoint> strikes = new ArrayList<LightningStrikePoint>();

    /**
     * Parse the date field from a given data source. It is assumed that the
     * data source is pointing to the current date/time data.
     * @return A BasePoint object with the time fields set to the observation
     * time.
     */
    BasePoint parseDate(IBinDataSource msgData)
    {
        BasePoint point = new BasePoint();
        
        //********* Don't reorder these reads!!!
        int b1 = msgData.getU8();
        int b2 = msgData.getU8();
        int word1 = msgData.getU16();
        //********* Don't reorder these reads!!!
        Calendar obsTime = (Calendar) BASE_TIME.clone();
        // number of days since BASE_TIME
        int days = ((word1 & DAYS_MASK) >> DAYS_SHFT);
        obsTime.add(Calendar.DAY_OF_MONTH, days);
        
        point.setYear(obsTime.get(Calendar.YEAR));
        //Increment month, Calendar returns 0..11
        point.setMonth(obsTime.get(Calendar.MONTH) + 1);
        point.setDay(obsTime.get(Calendar.DAY_OF_MONTH));

        int hours = (word1 & HOURS_HI_BIT_MASK) << HOURS_HI_BIT_SHFT;
        hours += (b2 & HOURS_LO_NYB_MASK) >>> HOURS_LO_NYB_SHFT;
        point.setHour(hours);

        int minutes = (b2 & MIN_P1_MASK) << MIN_P1_SHFT;
        minutes += (b1 & MIN_P2_MASK) >>> MIN_P2_SHFT;
        point.setMinute(minutes);
        
        point.setSecond((b1 & SECONDS_MASK));
        return point;
    }
    
    /**
     * Add a strike report the strikes collection.
     * @param strike A strike report.
     */
    void addStrike(LightningStrikePoint strike)
    {
        strikes.add(strike);
    }
    
    /**
     * Set the current error code for this decoder.
     * @param errorCode The error code.
     */
    void setError(int errorCode)
    {
        lastError = errorCode;
    }
    
    /**
     * Get the last error code set for this decoder.
     * @return The last error code.
     */
    public int getError()
    {
        return lastError;
    }

    /**
     * Get an iterator to the decoded lightning strikes.
     * @return The lightning strike iterator.
     */
    @Override
    public Iterator<LightningStrikePoint> iterator()
    {
        return strikes.iterator();
    }

}
