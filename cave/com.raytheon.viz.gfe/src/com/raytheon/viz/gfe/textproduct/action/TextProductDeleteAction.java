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
package com.raytheon.viz.gfe.textproduct.action;

import org.eclipse.jface.action.Action;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.script.IScriptUtil;
import com.raytheon.viz.gfe.ui.AccessMgr;

/**
 * Action to delete a text product.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2010            dgilling     Initial creation
 * Mar 07, 2013  15717     jzeng        Change CAVE_STATIC to COMMON_STATIC
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class TextProductDeleteAction extends Action {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(TextProductDeleteAction.class);
    protected String scriptName;

    protected IScriptUtil util;

    /**
     * Constructor
     * 
     * @param scriptName
     *            The simple name of the script to be deleted.
     * @param util
     *            The utility object that will actually delete the script
     */
    public TextProductDeleteAction(String scriptName, IScriptUtil util) {
        super("Delete");
        this.scriptName = scriptName;
        this.util = util;
    }

    /**
     * Delete the procedure this action is tied to.
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        if (AccessMgr.verifyDelete(util.scripted(scriptName),
                LocalizationType.COMMON_STATIC, false)) {
            try {
                // Delete the script
                util.delete(scriptName, LocalizationLevel.USER);
                statusHandler.handle(Priority.VERBOSE,
                        "USER " + util.getScriptType() + " " + scriptName
                                + " deleted.");
            } catch (GFEException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error deleting USER " + util.getScriptType() + " "
                                + scriptName, e);
            }
        }
    }
}
