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
package com.raytheon.viz.hydrocommon.datamanager;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import com.raytheon.viz.hydrocommon.data.DataIngestFilterData;
import com.raytheon.viz.hydrocommon.datamanager.DataIngestFilterDataManager.LocationPredicate;
import com.raytheon.viz.hydrocommon.datamanager.DataIngestFilterDataManager.PhysicalElementPredicate;
import com.raytheon.viz.hydrocommon.datamanager.DataIngestFilterDataManager.SwitchPredicate;
import com.raytheon.viz.hydrocommon.datamanager.DataIngestFilterDataManager.TypeSourcePredicate;
import com.raytheon.viz.hydrocommon.datamanager.DataIngestFilterDataManager.WfoPredicate;

/**
 * Class to test the filtering {@Predicate} classes of the Ingest Filter Dialog
 * of Hydrobase.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 03, 2018  6806      mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class TestDataIngestFilterDataManagerPredicates {

    @Test
    public void testLocationPredicate() {
        DataIngestFilterData data = new DataIngestFilterData();
        data.setLid("Test1");

        LocationPredicate pred = new LocationPredicate("Test1");
        assertTrue(pred.evaluate(data));

        pred = new LocationPredicate("Test2");
        assertFalse(pred.evaluate(data));
    }

    @Test
    public void testWfoPredicate() {
        DataIngestFilterData data = new DataIngestFilterData();
        data.setWfo("OAX");

        List<String> wfos = new ArrayList<>(3);
        wfos.add("OAX");
        wfos.add("DMX");
        wfos.add("GID");

        WfoPredicate pred = new WfoPredicate(wfos);
        assertTrue(pred.evaluate(data));

        data = new DataIngestFilterData();
        data.setWfo("BOX");
        assertFalse(pred.evaluate(data));
    }

    @Test
    public void testTypeSourcePredicate() {
        DataIngestFilterData data = new DataIngestFilterData();
        data.setTypeSource("RG");

        TypeSourcePredicate pred = new TypeSourcePredicate("RG");
        assertTrue(pred.evaluate(data));

        pred = new TypeSourcePredicate("FF");
        assertFalse(pred.evaluate(data));
    }

    @Test
    public void testPhysicalElementPredicate() {
        DataIngestFilterData data = new DataIngestFilterData();
        data.setPe("HG");

        List<String> peList = new ArrayList<>(3);
        peList.add("AD");
        peList.add("HG");
        peList.add("PP");

        PhysicalElementPredicate pred = new PhysicalElementPredicate(peList);
        assertTrue(pred.evaluate(data));

        data = new DataIngestFilterData();
        data.setPe("AW");
        pred = new PhysicalElementPredicate(peList);
        assertFalse(pred.evaluate(data));
    }

    @Test
    public void testSwitchPredicate() {
        DataIngestFilterData data = new DataIngestFilterData();
        data.setIngest("T");
        data.setOfsInput("T");
        data.setStg2Input("F");

        SwitchPredicate pred = new SwitchPredicate("T T F");
        assertTrue(pred.evaluate(data));

        pred = new SwitchPredicate("T F T");
        assertFalse(pred.evaluate(data));
    }
}
