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
package com.raytheon.uf.viz.xy.graph.labeling;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.time.DataTime;

/**
 * Graph label type for a DataTime object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DataTimeLabel extends AbstractGraphLabel<DataTime> {

    /** The underlying dataTime */
    private DataTime dataTime;

    /** The label, delimited by newlines */
    private String label;

    private static final SimpleDateFormat f1 = new SimpleDateFormat("dd.HH");

    private static final SimpleDateFormat f2 = new SimpleDateFormat("HH'Z' EEE");

    public DataTimeLabel(DataTime time) {
        this.dataTime = time;
        // Make sure label is in GMT time
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Date date = time.getRefTime();
        cal.setTime(date);
        String a = "^";
        f1.setCalendar(cal);
        String b = f1.format(cal.getTime());
        int fcstInHrs = time.getFcstTime() / 3600;
        String c = String.format("%dHR", fcstInHrs);
        cal.setTimeInMillis((long) getDiscreteValue());
        f2.setCalendar(cal);
        String d = f2.format(cal.getTime());
        label = a + '\n' + b + '\n' + c + '\n' + d;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.ui.xy.graph.plots.IGraphLabel#getDiscreteValue()
     */
    @Override
    public double getDiscreteValue() {
        return dataTime.getValidTime().getTimeInMillis();
    }

    @Override
    public String toLabelString() {
        return label;
    }

    @Override
    public DataTime getUnderlyingObject() {
        return dataTime;
    }

}
