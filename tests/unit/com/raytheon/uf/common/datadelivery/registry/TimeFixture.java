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
package com.raytheon.uf.common.datadelivery.registry;

import java.text.ParseException;
import java.util.Arrays;
import java.util.Calendar;

import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.AbstractFixture;

/**
 * Fixture for {@link Time} objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2012 1187       djohnson     Initial creation
 * Oct 16, 2012 0726       djohnson     Use {@link TimeUtil}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class TimeFixture extends AbstractFixture<Time> {

    public static final TimeFixture INSTANCE = new TimeFixture();

    /**
     * Disabled.
     */
    private TimeFixture() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Time get(long seedValue) {
        Time time = new Time();
        time.setFormat("HHddMMMyyyy");
        time.setCycleTimes(Arrays.<Integer> asList(getCycleForSeed(seedValue)));
        time.setStep((double) (seedValue + 1));
        time.setStepUnit("hour");
        time.setSelectedTimeIndices(time.getCycleTimes());
        try {
            time.setStartDate(new ImmutableDate(TimeUtil.currentTimeMillis()));
            time.setEndDate(new ImmutableDate(TimeUtil.currentTimeMillis()
                    + seedValue));
        } catch (ParseException e) {
            throw new RuntimeException(e);
        }

        return time;
    }

    public static int getCycleForSeed(long seedValue) {
        Calendar cal = TimeUtil.newCalendar();
        cal.setTimeInMillis(TimeUtil.currentTimeMillis());
        return cal.get(Calendar.HOUR_OF_DAY);
    }
}
