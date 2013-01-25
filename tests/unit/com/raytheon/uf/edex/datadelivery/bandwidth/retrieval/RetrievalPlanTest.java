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
package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.Calendar;

import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * Utilities and tests for {@link RetrievalPlan}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2012 0726       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class RetrievalPlanTest {

    /**
     * Resize a {@link RetrievalPlan} to start at the specified milliseconds,
     * and end at the specified milliseconds.
     * 
     * @param plan
     *            the plan to resize
     * @param start
     *            the start
     * @param end
     *            the end
     */
    public static void resizePlan(RetrievalPlan plan, long start, long end) {
        Calendar startCal = BandwidthUtil.now();
        startCal.setTimeInMillis(start);

        Calendar endCal = BandwidthUtil.now();
        endCal.setTimeInMillis(end);

        plan.resize(startCal, endCal);
    }

    @Test
    public void testResizeOfPlanCreatesCorrectBuckets() {
        // Placeholder test method that should be added later
    }
}
