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
package com.raytheon.edex.plugin.gfe.db.objects;

import java.util.Calendar;
import java.util.Date;

import junit.framework.Assert;

import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Implements unit tests for TimeConstraints
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/19/2008              chammack    Initial creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class TestTimeConstraints {

    private static final int ONEHR = 60 * 60;

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints#constraintTime(java.util.Date)}.
     */
    @Test
    public void testConstraintTime() {
        TimeConstraints tc = new TimeConstraints(ONEHR, ONEHR, 0);
        Date d = new Date();

        TimeRange tr = tc.constraintTime(d);

        // Calculate the answer
        Calendar c = Calendar.getInstance();
        c.setTime(d);
        c.set(Calendar.MILLISECOND, 0);
        c.set(Calendar.SECOND, 0);
        c.set(Calendar.MINUTE, 0);
        Date d1 = c.getTime();
        c.add(Calendar.HOUR, 1);
        Date d2 = c.getTime();
        TimeRange answer = new TimeRange(d1, d2);

        Assert.assertEquals(answer, tr);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints#anyConstraints()}.
     */
    @Test
    public void testAnyConstraints() {
        TimeConstraints tc = new TimeConstraints();
        Assert.assertFalse(tc.anyConstraints());

        tc = new TimeConstraints(ONEHR, ONEHR, 0);
        Assert.assertTrue(tc.anyConstraints());

        tc = new TimeConstraints(ONEHR, ONEHR * 2, 0);
        Assert.assertTrue(tc.anyConstraints());

        tc = new TimeConstraints(ONEHR, ONEHR * 2, ONEHR);
        Assert.assertTrue(tc.anyConstraints());

    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints#equals(java.lang.Object)}.
     */
    @Test
    public void testEqualsObject() {
        TimeConstraints tc1 = new TimeConstraints();
        TimeConstraints tc2 = new TimeConstraints();
        Assert.assertTrue(tc1.equals(tc2));

        tc1 = new TimeConstraints(ONEHR, ONEHR * 2, 0);
        tc2 = new TimeConstraints(ONEHR, ONEHR * 2, 0);
        Assert.assertTrue(tc1.equals(tc2));

        tc2 = new TimeConstraints(ONEHR, ONEHR * 3, 0);
        Assert.assertFalse(tc1.equals(tc2));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints#validTR(com.raytheon.uf.common.time.TimeRange)}.
     */
    @Test
    public void testValidTR() {
        Calendar c = Calendar.getInstance();
        c.set(2008, 01, 15, 00, 00, 00);
        c.set(Calendar.MILLISECOND, 0);
        Date d1 = c.getTime();
        c.add(Calendar.HOUR, 1);
        Date d2 = c.getTime();

        TimeRange tr = new TimeRange(d1, d2);
        TimeConstraints tc1 = new TimeConstraints(ONEHR, ONEHR * 2, 0);
        Assert.assertTrue(tc1.validTR(tr));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints#constraintTimes(com.raytheon.uf.common.time.TimeRange)}.
     */
    @Test
    public void testConstraintTimes() {
        Calendar c = Calendar.getInstance();
        c.set(2008, 01, 15, 00, 00, 00);
        c.set(Calendar.MILLISECOND, 0);
        Date d1 = c.getTime();
        c.add(Calendar.HOUR, 1);
        Date d2 = c.getTime();

        TimeRange tr = new TimeRange(d1, d2);

        TimeConstraints tc1 = new TimeConstraints(ONEHR, ONEHR * 2, 0);
        TimeRange[] result = tc1.constraintTimes(tr);
        Assert.assertEquals(result.length, 1);
        Assert.assertEquals(result[0], tr);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints#toString()}.
     */
    @Test
    public void testToString() {
        TimeConstraints tc1 = new TimeConstraints(ONEHR, ONEHR, 0);
        Assert.assertEquals(tc1.toString(), "[s=0h,i=1h,d=1h]");

    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints#expandTRToQuantum(com.raytheon.uf.common.time.TimeRange)}.
     */
    @Test
    public void testExpandTRToQuantum() {

        Calendar c = Calendar.getInstance();
        c.set(2008, 01, 15, 00, 00, 00);
        c.set(Calendar.MILLISECOND, 0);
        Date d1 = c.getTime();
        c.add(Calendar.MINUTE, 30);
        Date d2 = c.getTime();

        TimeRange tr1 = new TimeRange(d1, d2);

        TimeConstraints tc1 = new TimeConstraints(ONEHR, ONEHR, 0);
        TimeRange tr2 = tc1.expandTRToQuantum(tr1);
        c.add(Calendar.MINUTE, 30);
        d2 = c.getTime();
        TimeRange trExpected = new TimeRange(d1, d2);

        Assert.assertEquals(trExpected, tr2);

    }

}
