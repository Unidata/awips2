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
package com.raytheon.edex.plugin.redbook.menu;

import java.io.File;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Builds the NCEP/Hydro SPC menu contributions for NDM (Redbook hazards).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 07, 2014    2858    mpduff      Initial creation
 * Mar 14, 2014    2855    mpduff      Renamed and refactored common code.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RedbookHazardsMenuUtil extends RedbookMenuUtil {
    /** Status handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookHpcMenuUtil.class);

    /** Menu type constant */
    private static final String MENU_TYPE = "ncepHydro" + File.separator
            + "spc";

    /** SPC hazard menu file */
    private static final String HAZARD_MENU = "hazardMenus.xml";

    /** SPC hazard menu file including full path */
    private static final String MENU_FILE = "menus" + File.separator
            + MENU_TYPE + File.separator + HAZARD_MENU;

    /**
     * Constructor.
     */
    public RedbookHazardsMenuUtil() {
        super();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createMenusFromFile(String filename) {
        xml = read(filename);

        createMenus();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createMenus() {
        statusHandler.info("Creating menus for " + MENU_FILE);
        createMenusForFile(MENU_FILE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean checkCreated() {
        return super.checkCreated(HAZARD_MENU, MENU_TYPE);
    }
}
