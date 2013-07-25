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
package com.raytheon.uf.common.time;

import java.util.Date;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.junit.After;
import org.junit.Before;

/**
 * Test case for TimeRange class based on AWIPS Common test case
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2007            chammack    Initial Creation.
 * Jul 25, 2013 2208       njensen     Moved to tests project
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class TestTimeRange extends TestCase {
    private final TimeRange n1 = new TimeRange();

    private final TimeRange n2 = new TimeRange();

    private final TimeRange tr = new TimeRange(100000, 200); // good time

    // range
    public static void main(String[] args) {

    }

    /**
     * @throws java.lang.Exception
     */
    @Override
    @Before
    public void setUp() throws Exception {
    }

    /**
     * @throws java.lang.Exception
     */
    @Override
    @After
    public void tearDown() throws Exception {
    }

    public void testValid() {
        Assert.assertFalse(n1.isValid());
    }

    public void testEquals() {
        Assert.assertTrue(n1.equals(n2));
        Assert.assertFalse(n1.equals(tr));
        Assert.assertFalse(tr.equals(n1));
    }

    public void testJoin() {
        TimeRange x = n1.join(n2);
        TimeRange y = n1.join(tr);
        TimeRange z = tr.join(n1);

        Assert.assertFalse(x.isValid());
        Assert.assertFalse(y.isValid());
        Assert.assertFalse(z.isValid());
    }

    public void testIntersection() {
        TimeRange x = n1.intersection(n2);
        TimeRange y = n1.intersection(tr);
        TimeRange z = tr.intersection(n1);

        Assert.assertFalse(x.isValid());
        Assert.assertFalse(y.isValid());
        Assert.assertFalse(z.isValid());
    }

    public void testGap() {
        TimeRange x = n1.gap(n2);
        TimeRange y = n1.gap(tr);
        TimeRange z = tr.gap(n1);

        Assert.assertFalse(x.isValid());
        Assert.assertFalse(y.isValid());
        Assert.assertFalse(z.isValid());
    }

    public void testSpan() {
        TimeRange x = n1.span(n2);
        TimeRange y = n1.span(tr);
        TimeRange z = tr.span(n1);

        Assert.assertFalse(x.isValid());
        Assert.assertFalse(y.isValid());
        Assert.assertFalse(z.isValid());
    }

    public void testCombineWith() {
        TimeRange x = n1.combineWith(n2);
        TimeRange y = n1.combineWith(tr);
        TimeRange z = tr.combineWith(n1);

        Assert.assertFalse(x.isValid());
        Assert.assertTrue(y.isValid());
        Assert.assertTrue(z.isValid());

    }

    public void testConstructor() {
        Date t1 = new Date();
        Date t2 = new Date(t1.getTime() + 1000);
        TimeRange tr10 = new TimeRange(t1, t2);
        TimeRange tr11 = new TimeRange(t2, t1);
        TimeRange tr12 = new TimeRange(t1, 1000);
        TimeRange tr13 = new TimeRange(t2, -1000);

        Assert.assertEquals(tr10.getDuration(), 1000);
        Assert.assertEquals(tr11.getDuration(), 1000);
        Assert.assertEquals(tr12.getDuration(), 1000);
        Assert.assertEquals(tr13.getDuration(), 1000);

        Assert.assertEquals(tr10.getStart(), t1);
        Assert.assertEquals(tr11.getStart(), t1);
        Assert.assertEquals(tr12.getStart(), t1);

        Assert.assertEquals(tr10.getEnd(), t2);
        Assert.assertEquals(tr11.getEnd(), t2);
        Assert.assertEquals(tr12.getEnd(), t2);

        Assert.assertEquals(tr10, tr11);
        Assert.assertEquals(tr10, tr12);
        Assert.assertEquals(tr10, tr13);
    }

    public void testComparison() {
        Date[] t = new Date[8];
        TimeRange[] trtest = new TimeRange[29];
        for (int i = 0; i < t.length; i++) {
            t[i] = new Date(i * 100);
        }

        for (int i = 0, k = 0; i < 7; i++) {
            for (int j = i + 1; j < 8; j++) {
                trtest[k++] = new TimeRange(t[i], t[j]);
            }
        }

        TimeRange trbase = new TimeRange(t[2], t[5]);

        boolean[] answers = { false, false, true, true, true, true, true,
                false, true, true, true, true, true, true, true, true, true,
                true, true, true, true, true, true, true, true, false, false,
                false };

        for (int k = 0; k < 28; k++) {
            Assert.assertEquals(trbase.overlaps(trtest[k]), answers[k]);
        }

        answers = new boolean[] { false, true, false, false, false, false,
                false, true, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false,
                false, true, true, false };

        for (int k = 0; k < 28; k++) {
            Assert.assertEquals(trbase.isAdjacentTo(trtest[k]), answers[k]);
        }

        answers = new boolean[] { false, false, true, true, true, false, false,
                false };

        for (int k = 0; k < 8; k++) {
            Assert.assertEquals(trbase.contains(t[k]), answers[k]);
        }

        int[] answersInt = { 28, 4, 4, 4, 4, 5, 6, 10, 10, 10, 10, 11, 12, 15,
                15, 15, 16, 17, 15, 15, 16, 17, 15, 16, 17, 16, 17, 28 };

        for (int k = 0; k < 28; k++) {
            TimeRange trJoin = trbase.join(trtest[k]);
            if (answersInt[k] == 28) {
                Assert.assertFalse(trJoin.isValid());
            } else if (answersInt[k] != 28) {
                Assert.assertTrue(trJoin.isValid());
            } else if (!trJoin.isValid()) {
                continue;
            } else {
                Assert.assertEquals(trJoin, trtest[answersInt[k]]);
            }

        }

        answersInt = new int[] { 28, 28, 13, 14, 15, 15, 15, 28, 13, 14, 15,
                15, 15, 13, 14, 15, 15, 15, 18, 19, 19, 19, 22, 22, 22, 28, 28,
                28 };

        for (int k = 0; k < 28; k++) {
            TimeRange trInt = trbase.intersection(trtest[k]);
            if (answersInt[k] == 28) {
                Assert.assertFalse(trInt.isValid());
            } else if (answersInt[k] != 28) {
                Assert.assertTrue(trInt.isValid());
            } else if (!trInt.isValid()) {
                continue;
            } else {
                Assert.assertEquals(trInt, trtest[answersInt[k]]);
            }

        }

        answersInt = new int[] { 7, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
                28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 25 };

        for (int k = 0; k < 28; k++) {
            TimeRange trgap = trbase.gap(trtest[k]);
            if (answersInt[k] == 28) {
                Assert.assertFalse(trgap.isValid());
            } else if (answersInt[k] != 28) {
                Assert.assertTrue(trgap.isValid());
            } else if (!trgap.isValid()) {
                continue;
            } else {
                Assert.assertEquals(trgap, trtest[answersInt[k]]);
            }

        }

        answersInt = new int[] { 4, 4, 4, 4, 4, 5, 6, 10, 10, 10, 10, 11, 12,
                15, 15, 15, 16, 17, 15, 15, 16, 17, 15, 16, 17, 16, 17, 17 };

        for (int k = 0; k < 28; k++) {
            TimeRange trcombine = trbase.combineWith(trtest[k]);
            if (answersInt[k] == 28) {
                Assert.assertFalse(trcombine.isValid());
            } else if (answersInt[k] != 28) {
                Assert.assertTrue(trcombine.isValid());
            } else if (!trcombine.isValid()) {
                continue;
            } else {
                Assert.assertEquals(trcombine, trtest[answersInt[k]]);
            }

        }

        answersInt = new int[] { 4, 4, 4, 4, 4, 5, 6, 10, 10, 10, 10, 11, 12,
                15, 15, 15, 16, 17, 15, 15, 16, 17, 15, 16, 17, 16, 17, 17 };

        for (int k = 0; k < 28; k++) {
            TimeRange trspan = trbase.span(trtest[k]);
            if (answersInt[k] == 28) {
                Assert.assertFalse(trspan.isValid());
            } else if (answersInt[k] != 28) {
                Assert.assertTrue(trspan.isValid());
            } else if (!trspan.isValid()) {
                continue;
            } else {
                Assert.assertEquals(trspan, trtest[answersInt[k]]);
            }

        }
    }

    public void testContains() {
        TimeRange con1 = new TimeRange(1000, 2000);
        TimeRange con2 = new TimeRange(50, 2500);
        TimeRange con3 = new TimeRange(1000, 2500);
        TimeRange con4 = new TimeRange(20, 25);
        TimeRange con5 = new TimeRange(3000, 3500);
        TimeRange con6 = new TimeRange(200, 1500);
        TimeRange con7 = new TimeRange(1900, 2100);
        TimeRange con8 = new TimeRange(1100, 1800);
        TimeRange con9 = new TimeRange(1000, 1400);
        TimeRange con10 = new TimeRange(1100, 2000);

        Assert.assertFalse(con1.contains(con2));

        Assert.assertFalse(con1.contains(con3));

        Assert.assertFalse(con1.contains(con4));

        Assert.assertFalse(con1.contains(con5));

        Assert.assertFalse(con1.contains(con6));

        Assert.assertFalse(con1.contains(con7));

        Assert.assertTrue(con1.contains(con8));

        Assert.assertTrue(con1.contains(con9));

        Assert.assertTrue(con1.contains(con10));

        Assert.assertTrue(con1.contains(con1));

    }

    public void testContainsSpecial() {
        TimeRange zero = new TimeRange(1000, 1000);
        TimeRange nonZeroBefore = new TimeRange(900, 1000);
        TimeRange nonZeroAfter = new TimeRange(1001, 1100);
        TimeRange nonZeroIncAfter = new TimeRange(1000, 1100);
        TimeRange bigSpan = new TimeRange(900, 1100);
        Date beforeBaseTime = new Date(999);
        Date baseTime = new Date(1000);
        Date afterBaseTime = new Date(1001);

        Assert.assertTrue(zero.contains(baseTime));
        Assert.assertFalse(zero.contains(beforeBaseTime));
        Assert.assertFalse(zero.contains(afterBaseTime));

        Assert.assertFalse(nonZeroBefore.contains(zero));
        Assert.assertFalse(nonZeroAfter.contains(zero));
        Assert.assertTrue(nonZeroIncAfter.contains(zero));

        Assert.assertTrue(zero.contains(zero));

        Assert.assertFalse(zero.contains(nonZeroBefore));
        Assert.assertFalse(zero.contains(nonZeroAfter));

        Assert.assertFalse(zero.contains(bigSpan));
        Assert.assertFalse(zero.contains(nonZeroIncAfter));

        Assert.assertFalse(nonZeroBefore.overlaps(zero));
        Assert.assertFalse(nonZeroAfter.overlaps(zero));
        Assert.assertTrue(nonZeroIncAfter.overlaps(zero));
        Assert.assertTrue(bigSpan.overlaps(zero));
        Assert.assertTrue(zero.overlaps(zero));
        Assert.assertFalse(zero.overlaps(nonZeroBefore));
        Assert.assertFalse(zero.overlaps(nonZeroAfter));
        Assert.assertTrue(zero.overlaps(nonZeroIncAfter));
    }

    public void testIntersectionZero() {
        // intersection with 0 duration test
        TimeRange zero = new TimeRange(1000, 1000);
        TimeRange interLess = new TimeRange(900, 1000);
        TimeRange interAfter = new TimeRange(1002, 2000);
        TimeRange interAfterInc = new TimeRange(1000, 2000);
        TimeRange interSpan = new TimeRange(500, 2000);

        TimeRange answer = interLess.intersection(zero);
        Assert.assertFalse(answer.isValid());

        answer = interAfter.intersection(zero);
        Assert.assertFalse(answer.isValid());

        answer = interAfterInc.intersection(zero);
        Assert.assertFalse(answer.isValid());

        answer = interSpan.intersection(zero);
        Assert.assertFalse(answer.isValid());

        answer = zero.intersection(interLess);
        Assert.assertFalse(answer.isValid());

        answer = zero.intersection(interAfter);
        Assert.assertFalse(answer.isValid());

        answer = zero.intersection(interAfterInc);
        Assert.assertFalse(answer.isValid());

        answer = zero.intersection(interSpan);
        Assert.assertFalse(answer.isValid());

    }

    public void testCenterTime() {
        Date start = new Date();
        TimeRange trvalid = new TimeRange(start, 3600);
        Date trcenter = new Date(start.getTime() + 1800);

        Assert.assertEquals(trvalid.getCenterTime(), trcenter);

    }

    public void testDurationAsPrettyString() {
        Date start = new Date();
        TimeRange tr = new TimeRange(start, 3600 * 24);
        Assert.assertEquals("1d", tr.durationAsPrettyString());

        tr = new TimeRange(start, 3600);
        Assert.assertEquals("1h", tr.durationAsPrettyString());

        tr = new TimeRange(start, 60);
        Assert.assertEquals("0h 1m", tr.durationAsPrettyString());

        tr = new TimeRange(start, 1);
        Assert.assertEquals("0h 0m 1s", tr.durationAsPrettyString());

        tr = new TimeRange(start, 3600 * 24 + 1);
        Assert.assertEquals("1d 0h 0m 1s", tr.durationAsPrettyString());

    }

}