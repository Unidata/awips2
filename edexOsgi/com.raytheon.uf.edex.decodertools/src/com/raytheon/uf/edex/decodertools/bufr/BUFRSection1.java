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
package com.raytheon.uf.edex.decodertools.bufr;

import java.nio.ByteBuffer;
import java.util.Calendar;

import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080214            862 jkorman     Factored datetime info from subclasses.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public abstract class BUFRSection1 extends BUFRSection {

    boolean section2Present = false;

    int masterTable = -1;

    int subcenter = -1;

    int center = -1;

    int dataCatagory = -1;

    int masterTableVersion = -1;

    int localTableVersion = -1;

    int year;

    int month;

    int day;

    int hour;

    int minute;

    BUFRSection1(ByteBuffer dataBuffer) {
        super(dataBuffer);
        getSectionData();
        setValidity(true);
    }

    public BUFRSection2 getSection2(ByteBuffer dataBuffer) {
        BUFRSection2 sec2 = null;
        if (section2Present) {
            sec2 = new BUFRSection2(dataBuffer);
        }

        return sec2;
    }

    abstract void getSectionData();

    public StringBuilder getStringData(StringBuilder buffer) {
        buffer = super.getStringData(buffer);
        buffer.append("Section 1:");

        return buffer;
    }

    /**
     * @return the year
     */
    public int getYear() {
        return year;
    }

    /**
     * @return the month
     */
    public int getMonth() {
        return month;
    }

    /**
     * @return the day
     */
    public int getDay() {
        return day;
    }

    /**
     * @return the hour
     */
    public int getHour() {
        return hour;
    }

    /**
     * @return the minute
     */
    public int getMinute() {
        return minute;
    }

    /**
     * 
     * @return
     */
    public Calendar getSectionDate() {
        Calendar cal = TimeTools.getBaseCalendar(year, month, day);
        cal.set(Calendar.HOUR_OF_DAY, hour);
        cal.set(Calendar.MINUTE, minute);

        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        return cal;
    }
}
