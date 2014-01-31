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
package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.PointDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.registry.RegistryException;

/**
 * Test dataset offset average calculations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2013   2636     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class AveragingAvailabilityCalculatorTest {

    private static final String NAME = "DataSetName";

    private static final String NOMADS = "NOMADS";

    private static final String MADIS = "MADIS";

    private static final int[] offsets = new int[] { 5, 4, 6, 7, 9, 5, 4, 5, 6,
            4, 3, 6, 2 };

    private final IDataSetMetaDataHandler handler = mock(IDataSetMetaDataHandler.class);

    private final AveragingAvailablityCalculator availabilityCalculator = new AveragingAvailablityCalculator(
            handler);

    @Before
    public void setUp() throws RegistryException, RegistryHandlerException {
        List<DataSetMetaData> gmdList = new ArrayList<DataSetMetaData>();
        List<DataSetMetaData> pointList = new ArrayList<DataSetMetaData>();

        Calendar cal = TimeUtil.newGmtCalendar();
        cal.set(Calendar.MONTH, 2);
        cal.set(Calendar.DATE, 20);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        // Create 20 DataSetMetaData objects, 10 each for grid and point
        for (int i = 0; i < offsets.length; i++) {
            cal.add(Calendar.DATE, -1);
            DataSetMetaData gmd = new OpenDapGriddedDataSetMetaData();
            gmd.setAvailabilityOffset(offsets[i]);
            gmd.setDate(new ImmutableDate(cal.getTime()));
            ((GriddedDataSetMetaData) gmd).setCycle(cal
                    .get(Calendar.HOUR_OF_DAY));
            gmdList.add(gmd);

            DataSetMetaData pmd = new PointDataSetMetaData();
            pmd.setAvailabilityOffset(offsets[i]);
            pmd.setDate(new ImmutableDate(cal.getTime()));
            pointList.add(pmd);
        }

        when(handler.getByDataSet(NAME, NOMADS)).thenReturn(gmdList);
        when(handler.getByDataSet(NAME, MADIS)).thenReturn(pointList);
    }

    @Test
    public void testGridCalculation() throws RegistryHandlerException {
        Calendar refTime = TimeUtil.newGmtCalendar();
        refTime.set(Calendar.MONTH, 2);
        refTime.set(Calendar.DATE, 21);

        Subscription subscription = new SiteSubscription();
        subscription.setProvider(NOMADS);
        subscription.setDataSetName(NAME);
        int offset = availabilityCalculator.getDataSetAvailablityOffset(
                subscription, refTime);

        assertTrue("Average not correct.", offset == getAvg());
    }

    /**
     * Get the average. The average calculator uses the last 10 values in the
     * avg calculation.
     * 
     * @return the average
     */
    private int getAvg() {
        int total = 0;
        for (int i = 0; i < 10; i++) {
            total += offsets[i];
        }
        return total / 10;
    }
}
