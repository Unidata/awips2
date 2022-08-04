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
package com.raytheon.uf.edex.menus.dataplugins.redbook;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Builds the NCEP/Hydro Upper Air menu contributions for NDM (Redbook UA).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2014    2860    mpduff      Initial creation
 * Jun 26, 2015    4512    mapeters    Abstract out createMenusFromFile()
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RedbookUaMenuUtil extends RedbookMenuUtil {
    /** Status handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookUaMenuUtil.class);

    /** Menu type constant */
    private static final String MENU_TYPE = "upperair";

    /** Upper Air menu file */
    private static final String UA_MENU = "uaMenus.xml";

    /** Upper Air menu file full path */
    private static final String MENU_FILE = MENUS + IPathManager.SEPARATOR
            + MENU_TYPE + IPathManager.SEPARATOR + UA_MENU;

    /**
     * Constructor.
     */
    public RedbookUaMenuUtil() {
        super();
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
        return super.checkCreated(UA_MENU, MENU_TYPE);
    }
}
