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
package com.raytheon.edex.plugin.sfcobs.decoder;

import static org.junit.Assert.*;
import org.junit.Test;

import java.util.Calendar;

import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.AbstractSynopticDecoder;
import com.raytheon.edex.tools.time.TimeTools;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 20071010            391  jkorman     Initial coding.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public class TestSynopticObsTime {


    /**
     * Set an obstime within the same day. 
     */
    @Test
    public void sameDayObs() {
        
        int year = 2007;
        int month = 10;
        int day = 15;
        
        int obsDay = 15;
        int obsHour = 12;
        
        Calendar sysTime = TimeTools.getBaseCalendar(year,month,day);
        assertNotNull(sysTime);

        Calendar obsTime =
            AbstractSynopticDecoder.calculateObsDateTime(sysTime,obsDay,obsHour);
        assertNotNull(obsTime);
        
        assertEquals(sysTime.get(Calendar.YEAR),obsTime.get(Calendar.YEAR));
        assertEquals(sysTime.get(Calendar.MONTH),obsTime.get(Calendar.MONTH));
        assertEquals(sysTime.get(Calendar.DAY_OF_MONTH),obsTime.get(Calendar.DAY_OF_MONTH));
        assertEquals(obsHour,obsTime.get(Calendar.HOUR_OF_DAY));
    }

    /**
     * Set an obstime in the next day! This test crosses day, month, and year
     * boundaries. 
     */
    @Test
    public void nextDayObs() {
        
        int year = 2007;
        int month = 12;
        int day = 31;
        
        int obsDay = 1;
        int obsHour = 0;
        
        Calendar sysTime = TimeTools.getBaseCalendar(year,month,day);
        assertNotNull(sysTime);

        Calendar obsTime =
            AbstractSynopticDecoder.calculateObsDateTime(sysTime,obsDay,obsHour);
        assertNotNull(obsTime);
        
        assertEquals(2008,obsTime.get(Calendar.YEAR));
        assertEquals(0,obsTime.get(Calendar.MONTH)); // (month = 0..11)
        assertEquals(1,obsTime.get(Calendar.DAY_OF_MONTH));
        assertEquals(0,obsTime.get(Calendar.HOUR_OF_DAY));
    }
    
    /**
     * Set an obstime in a previous day! This test crosses day, month, and year
     * boundaries. 
     */
    @Test
    public void nextPreviousObs() {
        
        int year = 2008;
        int month = 1;
        int day = 6;
        
        int obsDay = 17;
        int obsHour = 15;
        
        Calendar sysTime = TimeTools.getBaseCalendar(year,month,day);
        assertNotNull(sysTime);

        Calendar obsTime =
            AbstractSynopticDecoder.calculateObsDateTime(sysTime,obsDay,obsHour);
        assertNotNull(obsTime);
        
        assertEquals(2007,obsTime.get(Calendar.YEAR));
        assertEquals(11,obsTime.get(Calendar.MONTH)); // (month = 0..11)
        assertEquals(17,obsTime.get(Calendar.DAY_OF_MONTH));
        assertEquals(15,obsTime.get(Calendar.HOUR_OF_DAY));
    }
    
    /**
     * Set an obstime in a previous day that crosses from March to Feb. during
     * a leap year.
     */
    @Test
    public void leapYearObs() {
        int year = 2008;
        int month = 3;
        int day = 6;
        
        int obsDay = 29;
        int obsHour = 15;
        
        Calendar sysTime = TimeTools.getBaseCalendar(year,month,day);
        assertNotNull(sysTime);

        Calendar obsTime =
            AbstractSynopticDecoder.calculateObsDateTime(sysTime,obsDay,obsHour);
        assertNotNull(obsTime);
        
        assertEquals(2008,obsTime.get(Calendar.YEAR));
        assertEquals(1,obsTime.get(Calendar.MONTH)); // (month = 0..11)
        assertEquals(29,obsTime.get(Calendar.DAY_OF_MONTH));
        assertEquals(15,obsTime.get(Calendar.HOUR_OF_DAY));
    }

}


