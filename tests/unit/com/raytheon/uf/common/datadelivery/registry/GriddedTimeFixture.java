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

import java.util.Arrays;
import java.util.Calendar;
import java.util.Random;

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
 * Feb 07, 2013 1543       djohnson     Set request start/end dates.
 * Sept 30, 2013 1797      dhladky      Generics
 * Oct 10, 2013 1797       bgonzale     Refactored registry Time objects.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class GriddedTimeFixture extends AbstractFixture<GriddedTime> {

    public static final GriddedTimeFixture INSTANCE = new GriddedTimeFixture();

    /**
     * Disabled.
     */
    private GriddedTimeFixture() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GriddedTime getInstance(long seedValue, Random random) {
        GriddedTime time = new GriddedTime();
        time.setFormat("HHddMMMyyyy");
        time.setCycleTimes(Arrays.<Integer> asList(getCycleForSeed(seedValue)));
        time.setStep((double) (seedValue + 1));
        time.setStepUnit("hour");
        time.setSelectedTimeIndices(time.getCycleTimes());
        time.setStart(new ImmutableDate(TimeUtil.currentTimeMillis()));
        time.setEnd(new ImmutableDate(TimeUtil.currentTimeMillis() + seedValue));
        time.setRequestStart(time.getStart());
        time.setRequestEnd(time.getEnd());

        return time;
    }

    public static int getCycleForSeed(long seedValue) {
        Calendar cal = TimeUtil.newCalendar();
        cal.setTimeInMillis(TimeUtil.currentTimeMillis());
        return cal.get(Calendar.HOUR_OF_DAY);
    }
}
