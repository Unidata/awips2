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
package com.raytheon.viz.gfe.core.internal;

import static org.junit.Assert.fail;

import java.awt.Point;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager.RefSetMode;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetIDChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetInvChangedListener;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * ReferenceSetManager Unit Test
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 8, 2008				randerso	Initial creation
 * Aug 14, 2013       1571  randerso    Changed to use ProjectionType enum
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

// TODO fix?
@Ignore
public class ReferenceSetManagerTest {
    private static class RefChangedListener implements
            IReferenceSetChangedListener {
        public ReferenceData refData = null;

        @Override
        public void referenceSetChanged(ReferenceData refSet,
                ArrayList<Envelope> domains) {
            System.out.println("NOTIFICATION: ReferenceSetChanged");
            refData = refSet;
        }

    }

    private static class RefInvChangedListener implements
            IReferenceSetInvChangedListener {
        public ReferenceData refData = null;

        /*
         * (non-Javadoc)
         * 
         * @seecom.raytheon.viz.gfe.core.msgs.IReferenceSetInvChangedListener#
         * referenceSetInvChanged(java.util.List, java.util.List,
         * java.util.List, java.util.List)
         */
        @Override
        public void referenceSetInvChanged(List<ReferenceID> refIDs,
                List<ReferenceID> additions, List<ReferenceID> deletions,
                List<ReferenceID> changes) {
            // TODO Auto-generated method stub
            System.out.println("NOTIFICATION: ReferenceSetInvChanged");

        }

    }

    private static class RefIDChangedListener implements
            IReferenceSetIDChangedListener {
        public ReferenceData refData = null;

        /*
         * (non-Javadoc)
         * 
         * @seecom.raytheon.viz.gfe.core.msgs.IReferenceSetIDChangedListener#
         * referenceSetIDChanged
         * (com.raytheon.edex.plugin.gfe.reference.ReferenceID)
         */
        @Override
        public void referenceSetIDChanged(ReferenceID refID) {
            // TODO Auto-generated method stub
            System.out.println("NOTIFICATION: ReferenceSetIDChanged");

        }

    }

    private static final RefChangedListener refChangedListener = new RefChangedListener();

    private static final RefInvChangedListener refInvChangedListener = new RefInvChangedListener();

    private static final RefIDChangedListener refIDChangedListener = new RefIDChangedListener();

    private static final ProjectionData grid211 = new ProjectionData("Grid211",
            ProjectionType.LAMBERT_CONFORMAL, new Coordinate(-133.459, 12.190),
            new Coordinate(-49.385, 57.290), new Coordinate(-95.0, 25.0),
            25.0f, 25.0f, new Point(1, 1), new Point(93, 65), 0.0f, 0.0f, 0.0f);

    private static final GridLocation gloc = new GridLocation("OAX", grid211,
            new Point(145, 145), new Coordinate(45, 30), new Coordinate(9, 9),
            "CST6CDT");

    private static ReferenceData ref1;
    static {
        String path = "../AWIPSEdex/opt/utility/common_static/site/OAX/editAreas/OAX.xml";
        File file = new File(path);
        System.out.println(file.getAbsolutePath());

        try {
            ref1 = (ReferenceData) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(path);
            ref1.setId(new ReferenceID("OAX"));
            ref1.setGloc(gloc);
            ref1.getGrid();
        } catch (SerializationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private static ReferenceData ref2;
    static {
        String path = "../AWIPSEdex/opt/utility/common_static/site/OAX/editAreas/DMX.xml";

        try {
            ref2 = (ReferenceData) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(path);
            ref2.setId(new ReferenceID("DMX"));
            ref2.setGloc(gloc);
            ref2.getGrid();
        } catch (SerializationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private static final GridLocation gloc1 = new GridLocation("XXX", grid211,
            new Point(11, 11), new Coordinate(45, 30), new Coordinate(1, 1),
            "CST6CDT");

    private static final ReferenceData ref3;
    static {
        Grid2DBit grid = new Grid2DBit(gloc1.getNx(), gloc1.getNy(), true);
        for (int i = 0; i < grid.getXdim(); i++) {
            if ((i == 0) || (i == (grid.getXdim() - 1))) {
                for (int j = 0; j < grid.getYdim(); j++) {
                    grid.clear(i, j);
                }
            } else {
                grid.clear(i, 0);
                grid.clear(i, grid.getYdim() - 1);
            }
        }
        ref3 = new ReferenceData(gloc1, new ReferenceID("XXX"), grid);
    }

    private static final ReferenceData ref4;
    static {
        Grid2DBit grid = new Grid2DBit(gloc.getNx(), gloc.getNy(), false);
        int x0 = (int) (Math.random() * gloc.getNx());
        int x1 = (int) (Math.random() * gloc.getNx());
        int y0 = (int) (Math.random() * gloc.getNy());
        int y1 = (int) (Math.random() * gloc.getNy());

        if (x0 > x1) {
            int tmp = x0;
            x0 = x1;
            x1 = tmp;
        }
        if (y0 > y1) {
            int tmp = y0;
            y0 = y1;
            y1 = tmp;
        }

        for (int i = x0; i < x1; i++) {
            for (int j = y0; j < y1; j++) {
                grid.set(i, j);
            }
        }
        ref4 = new ReferenceData(gloc, new ReferenceID("Ref4"), grid);
    }

    private IReferenceSetManager refMgr = DataManager.getCurrentInstance()
            .getRefManager();

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        System.out.println("SETUP");

        refMgr.addReferenceSetChangedListener(refChangedListener);
        refMgr.addReferenceSetInvChangedListener(refInvChangedListener);
        refMgr.addReferenceSetIDChangedListener(refIDChangedListener);
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        System.out.println("TEARDOWN");
        refMgr.removeReferenceSetChangedListener(refChangedListener);
        refMgr.removeReferenceSetInvChangedListener(refInvChangedListener);
        refMgr.removeReferenceSetIDChangedListener(refIDChangedListener);

        // refMgr = null;
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#getGroupInventory()}
     * .
     */
    @Test
    public void testGetGroupInventory() {
        List<String> groupInv = refMgr.getGroupInventory();
        Assert.assertTrue(groupInv.contains("Counties"));
        Assert.assertTrue(groupInv.contains("FIPS"));
        Assert.assertTrue(groupInv.contains("FireWxAOR"));
        Assert.assertTrue(groupInv.contains("FireWxZones"));
        Assert.assertTrue(groupInv.contains("ISC"));
        Assert.assertTrue(groupInv.contains("States"));
        Assert.assertTrue(groupInv.contains("WFOs"));
        Assert.assertTrue(groupInv.contains("Zones"));
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#getGroupData(java.lang.String)}
     * .
     */
    @Test
    public void testGetGroupData() {
        List<String> results = refMgr.getGroupData("States");
        Assert.assertTrue(results.contains("Colorado"));
        Assert.assertTrue(results.contains("Iowa"));
        Assert.assertTrue(results.contains("Kansas"));
        Assert.assertTrue(results.contains("Minnesota"));
        Assert.assertTrue(results.contains("Missouri"));
        Assert.assertTrue(results.contains("Nebraska"));
        Assert.assertTrue(results.contains("SouthDakota"));
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#setActiveRefSet(com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData)}
     * .
     */
    @Test
    public void testSetGetActiveRefSet() {
        Assert.assertFalse(refMgr.getActiveRefSet().getGrid().isAnyBitsSet());
        refMgr.setActiveRefSet(ref1);
        ReferenceData refData = refMgr.getActiveRefSet();
        Assert.assertEquals(ref1, refData);
        Assert.assertEquals(ref1, refChangedListener.refData);
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#getAvailableSets()}
     * .
     */
    @Test
    public void testGetAvailableSets() {
        Assert.assertTrue(refMgr.getAvailableSets().contains(
                new ReferenceID("Nebraska", false, LocalizationLevel.SITE)));
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#emptyRefSet()}
     * .
     */
    @Test
    public void testEmptyRefSet() {
        Assert.assertFalse(refMgr.emptyRefSet().getGrid().isAnyBitsSet());
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#fullRefSet()}
     * .
     */
    @Test
    public void testFullRefSet() {
        ReferenceData refData = refMgr.fullRefSet();
        Assert.assertEquals(new Grid2DBit(refData.getGloc().getNx(), refData
                .getGloc().getNy(), true), refData.getGrid());
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#loadRefSet(com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID)}
     * .
     */
    @Test
    public void testLoadRefSet() {
        ReferenceData refData = refMgr.loadRefSet(new ReferenceID("OAX", false,
                LocalizationLevel.SITE));
        Assert.assertEquals(ref1.getGrid(), refData.getGrid());
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#saveActiveRefSet(com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID)}
     * .
     */
    @Test
    public void testSaveActiveRefSet() {
        fail("Not yet implemented");
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#saveRefSet(com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData)}
     * .
     */
    @Test
    public void testSaveDeleteRefSetGroup() {
        // save a new ref set
        refMgr.saveRefSet(ref4);
        ReferenceData ref = refMgr.loadRefSet(ref4.getId());
        Assert.assertEquals(ref4.getGrid(), ref.getGrid());

        // wait for the notification from the server to be received
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            // do nothing
        }

        Assert.assertTrue(refMgr.getGroupData("Misc").contains(
                ref4.getId().getName()));

        // add the ref set to a new group
        String group = "TestGroup";
        List<String> list = new ArrayList<String>();
        list.add(ref4.getId().getName());
        refMgr.saveGroup(group, list);

        // wait for the notification from the server to be received
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            // do nothing
        }

        Assert.assertFalse(refMgr.getGroupData("Misc").contains(
                ref4.getId().getName()));

        Assert.assertTrue(refMgr.getGroupInventory().contains(group));

        Assert.assertTrue(refMgr.getGroupData(group).contains(
                ref4.getId().getName()));

        // delete the ref set
        refMgr.deleteRefSet(ref4.getId(), true);

        // wait for the notification from the server to be received
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            // do nothing
        }

        Assert.assertFalse(refMgr.getGroupData(group).contains(
                ref4.getId().getName()));

        // delete the group
        refMgr.deleteGroup(group);

        // wait for the notification from the server to be received
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            // do nothing
        }

        Assert.assertFalse(refMgr.getGroupInventory().contains(group));
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#undoRefSet()}
     * .
     */
    @Test
    public void testUndoRefSet() {
        refMgr.setActiveRefSet(ref1);
        refMgr.setActiveRefSet(ref2);
        refMgr.undoRefSet();
        Assert.assertEquals(ref1, refMgr.getActiveRefSet());
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#taperGrid(com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData, int)}
     * .
     */
    @Test
    public void testTaperGrid() {
        Grid2DFloat taper = refMgr.taperGrid(ref3, 4);
        System.out.println(taper);
        fail("Don't know what the right answer is");
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#directionTaperGrid(com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData, java.lang.String)}
     * .
     */
    @Test
    public void testDirectionTaperGrid() {
        try {
            refMgr.directionTaperGrid(ref3, "");
            fail("IllegalArgumentException not thrown for empty string");
        } catch (IllegalArgumentException e) {
            System.out.println(e);
        }

        try {
            refMgr.directionTaperGrid(ref3, "NNNN");
            fail("IllegalArgumentException not thrown for string too long");
        } catch (IllegalArgumentException e) {
            System.out.println(e);
        }

        try {
            refMgr.directionTaperGrid(ref3, "X");
            fail("IllegalArgumentException not thrown for invalid character");
        } catch (IllegalArgumentException e) {
            System.out.println(e);
        }
        Grid2DFloat taper = refMgr.directionTaperGrid(ref3, "NE");
        System.out.println(taper);
        fail("Don't know what the right answer is");
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#siteGridpoints(java.lang.String[], boolean)}
     * .
     */
    @Test
    public void testSiteGridpoints() {
        Assert.assertEquals(ref1.getGrid().or(ref2.getGrid()),
                refMgr.siteGridpoints(Arrays.asList("OAX", "DMX"), true));
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#mySiteGridpoints()}
     * .
     */
    @Test
    public void testMySiteGridpoints() {
        Assert.assertEquals(ref1.getGrid(), refMgr.mySiteGridpoints());
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#getMode()}.
     */
    @Test
    public void testSetGetMode() {
        Assert.assertEquals(RefSetMode.REPLACE, refMgr.getMode());
        refMgr.setMode(RefSetMode.UNION);
        Assert.assertEquals(RefSetMode.UNION, refMgr.getMode());
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#incomingRefSet(com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData, com.raytheon.viz.gfe.core.IReferenceSetManager.RefSetMode)}
     * .
     */
    @Test
    public void testIncomingRefSet() {
        refMgr.setActiveRefSet(ref1);
        refMgr.setMode(RefSetMode.UNION);
        refMgr.incomingRefSet(ref2, RefSetMode.REPLACE);
        Assert.assertEquals(ref2.getGrid(), refMgr.getActiveRefSet().getGrid());
        Assert.assertEquals(ref2.getGrid(),
                refChangedListener.refData.getGrid());

        refMgr.incomingRefSet(ref1, RefSetMode.USE_CURRENT);

        Assert.assertEquals(ref1.getGrid().or(ref2.getGrid()), refMgr
                .getActiveRefSet().getGrid());
        Assert.assertEquals(ref1.getGrid().or(ref2.getGrid()),
                refChangedListener.refData.getGrid());
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#clearRefSet()}
     * .
     */
    @Test
    public void testClearRefSet() {
        refMgr.setActiveRefSet(ref1);
        refMgr.clearRefSet();
        Assert.assertFalse(refMgr.getActiveRefSet().getGrid().isAnyBitsSet());
        Assert.assertFalse(refChangedListener.refData.getGrid().isAnyBitsSet());
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.internal.ReferenceSetManager#toggleRefSet()}
     * .
     */
    @Test
    public void testToggleRefSet() {
        refMgr.setActiveRefSet(ref1);
        refMgr.toggleRefSet();
        Grid2DBit grid = new Grid2DBit(ref1.getGrid());
        grid.negate();
        Assert.assertEquals(grid, refMgr.getActiveRefSet().getGrid());
        Assert.assertEquals(grid, refChangedListener.refData.getGrid());
    }

}
