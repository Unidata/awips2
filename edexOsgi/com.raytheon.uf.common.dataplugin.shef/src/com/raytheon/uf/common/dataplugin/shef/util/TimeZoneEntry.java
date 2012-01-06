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
package com.raytheon.uf.common.dataplugin.shef.util;

import java.util.TimeZone;

public class TimeZoneEntry
{

    private String key;
    
    private int offsetMinutes;
    
    private TimeZone daylightSaving;
    
    private String description;
    
    /**
     * 
     * @param key
     * @param offsetMinutes
     * @param daylightSavingFlag
     * @param description
     */
    public TimeZoneEntry(String key, int offsetMinutes, TimeZone tz, String description)
    {
        this.key = key;
        
        this.offsetMinutes = offsetMinutes;
        
        this.daylightSaving = tz;
        
        this.description = description;
    }
    
    /**
     * 
     * @return
     */
    public TimeZone getDaylightSaving()
    {
        return daylightSaving;
    }

    /**
     * 
     * @param daylightSavingFlag
     */
    public void setDaylightSavingFlag(TimeZone daylightSaving)
    {
        this.daylightSaving = daylightSaving;
    }

    /**
     * 
     * @return
     */
    public String getDescription()
    {
        return description;
    }

    /**
     * 
     * @param description
     */
    public void setDescription(String description)
    {
        this.description = description;
    }

    /**
     * 
     * @return
     */
    public String getKey()
    {
        return key;
    }

    /**
     * 
     * @param key
     */
    public void setKey(String key)
    {
        this.key = key;
    }

    /**
     * 
     * @return
     */
    public int getOffsetMinutes()
    {
        return offsetMinutes;
    }

    /**
     * 
     * @param offsetMinutes
     */
    public void setOffsetMinutes(int offsetMinutes)
    {
        this.offsetMinutes = offsetMinutes;
    }
}
