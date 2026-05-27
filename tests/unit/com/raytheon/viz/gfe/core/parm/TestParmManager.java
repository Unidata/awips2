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
package com.raytheon.viz.gfe.core.parm;

import java.util.List;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class TestParmManager {

    private IParmManager parmMgr;

    @Before
    public void setUp() throws Exception {
        parmMgr = DataManager.getInstance(null).getParmManager();
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void testGetAvailable() {
        List<DatabaseID> dbs = parmMgr.getAvailableDbs();
        Assert.assertNotNull(dbs);
        Assert.assertTrue(dbs.size() > 0);

        for (DatabaseID db : dbs) {
            System.out.println("Checking db: " + db);
            ParmID[] parmids = parmMgr.getAvailableParms(db);
            Assert.assertNotNull(parmids);
            Assert.assertTrue(parmids.length > 0);

        }
    }

    @Test
    public void testGetParmInExpr() {
        Parm p = parmMgr.getParmInExpr("T", false, null);
        Assert.assertNotNull(p);
    }

}
