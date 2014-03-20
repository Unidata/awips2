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
package com.raytheon.edex.plugin.redbook.ingest;

import java.io.File;

import com.raytheon.edex.plugin.redbook.menu.RedbookHazardsMenuUtil;
import com.raytheon.edex.plugin.redbook.menu.RedbookHpcMenuUtil;
import com.raytheon.edex.plugin.redbook.menu.RedbookMpcMenuUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * Redbook menu subscriber. Takes redbook menu files and passes them to the
 * correct menu generators.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2014    2858    mpduff      Initial creation.
 * Mar 17, 2014    2855    mpduff      Implement HPC.
 * Mar 19, 2014    2859    mpduff      Implement MPC.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RedbookMenuSubscriber implements INationalDatasetSubscriber {
    /** Status handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookMenuSubscriber.class);

    /** Hazard menu file */
    private static final String HAZARD_MENU_FILE = "RedbookHazardMenus.xml";

    /** HPC menu file */
    private static final String HPC_MENU_FILE = "RedbookHPCMenus.xml";

    /** MPC menu file */
    private static final String MPC_MENU_FILE = "RedbookMPCMenus.xml";

    /**
     * {@inheritDoc}
     */
    @Override
    public void notify(String fileName, File file) {
        statusHandler.info("Processing " + fileName);
        if (HAZARD_MENU_FILE.equals(fileName)) {
            // Convert input file to output menu format
            RedbookHazardsMenuUtil menuUtil = new RedbookHazardsMenuUtil();
            menuUtil.createMenusFromFile(file.getAbsolutePath());
        } else if (HPC_MENU_FILE.equals(fileName)) {
            RedbookHpcMenuUtil menuUtil = new RedbookHpcMenuUtil();
            menuUtil.createMenusFromFile(file.getAbsolutePath());
        } else if (MPC_MENU_FILE.equals(fileName)) {
            RedbookMpcMenuUtil menuUtil = new RedbookMpcMenuUtil();
            menuUtil.createMenusFromFile(file.getAbsolutePath());
        }
    }
}
