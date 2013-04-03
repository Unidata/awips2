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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.junit.Test;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Test {@link OpenDapGriddedDataSet}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2012 1022       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class OpenDapGriddedDataSetTest {

    @Test
    public void testCombineWillOverwriteCycleInformation() {
        OpenDapGriddedDataSet oldOne = new OpenDapGriddedDataSet();
        oldOne.cycleUpdated(0);
        oldOne.cycleUpdated(1);
        oldOne.getCyclesToUrls().put(0, "url0");
        oldOne.getCyclesToUrls().put(1, "url1");

        OpenDapGriddedDataSet newOne = new OpenDapGriddedDataSet();
        newOne.cycleUpdated(0);
        newOne.getCyclesToUrls().put(0, "newurl0");

        newOne.combine(oldOne);

        Iterator<Integer> iter = newOne.newestToOldestIterator();
        assertEquals(0, iter.next().intValue());
        assertEquals(1, iter.next().intValue());

        assertEquals("newurl0", newOne.getCyclesToUrls().get(0));
        assertEquals("url1", newOne.getCyclesToUrls().get(1));
    }

    @Test
    public void testRemovesAllOldVersionsOfElement() {
        OpenDapGriddedDataSet dataSet = new OpenDapGriddedDataSet();
        dataSet.cycleUpdated(1);
        dataSet.cycleUpdated(1);
        dataSet.cycleUpdated(1);

        // Should still just have one occurrence
        Iterator<Integer> cycleIter = dataSet.newestToOldestIterator();
        assertTrue(cycleIter.hasNext());
        cycleIter.next();
        assertFalse(cycleIter.hasNext());
    }

    @Test
    public void testIteratesFromNewestToOldest() {
        OpenDapGriddedDataSet dataSet = new OpenDapGriddedDataSet();
        dataSet.cycleUpdated(1);
        dataSet.cycleUpdated(2);

        Iterator<Integer> cycleIter = dataSet.newestToOldestIterator();
        assertEquals(2, cycleIter.next().intValue());
        assertEquals(1, cycleIter.next().intValue());
    }

    @Test
    public void testIteratesFromOldestToNewest() {
        OpenDapGriddedDataSet dataSet = new OpenDapGriddedDataSet();
        dataSet.cycleUpdated(1);
        dataSet.cycleUpdated(2);

        Iterator<Integer> cycleIter = dataSet.oldestToNewestIterator();
        assertEquals(1, cycleIter.next().intValue());
        assertEquals(2, cycleIter.next().intValue());
    }

    @Test
    public void testDynamicSerializeAndUnserializeRestoresProperCycleUpdates()
            throws SerializationException {
        OpenDapGriddedDataSet dataSet = new OpenDapGriddedDataSet();
        dataSet.cycleUpdated(1);
        dataSet.cycleUpdated(2);

        byte[] serialized = SerializationUtil.transformToThrift(dataSet);
        OpenDapGriddedDataSet unserialized = (OpenDapGriddedDataSet) SerializationUtil
                .transformFromThrift(serialized);

        Iterator<Integer> iter = unserialized.newestToOldestIterator();
        assertEquals(2, iter.next().intValue());
        assertEquals(1, iter.next().intValue());
    }
}
