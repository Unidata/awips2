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
package com.raytheon.viz.gfe.localization.actions;

import org.eclipse.jface.action.Action;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.localization.perspective.service.LocalizationPerspectiveUtils;
import com.raytheon.viz.gfe.localization.util.AbstractScriptUtil.Overwrite;
import com.raytheon.viz.gfe.localization.util.ServerConfigUtil;

/**
 * Action to override GFE server configuration files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 10, 2016  5816     randerso  Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 */
public class OverrideAction extends Action {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(OverrideAction.class);

    private String path;

    private ServerConfigUtil util;

    /**
     * Constructor
     * 
     * @param path
     */
    public OverrideAction(String path) {
        super("Override...");
        this.path = path;
        this.util = new ServerConfigUtil(path);
    }

    @Override
    public void run() {
        String script = util.getOverrideFilename();
        try {
            LocalizationFile fileToEdit = util.createNew(script,
                    Overwrite.DISALLOW);
            statusHandler.handle(Priority.VERBOSE, script + " created.");
            LocalizationPerspectiveUtils.openLocalizationFile(fileToEdit);
        } catch (Exception e) {
            String message = String.format("Error creating '%s': %s", script,
                    e.getLocalizedMessage());
            statusHandler.error(message, e);
        }
    }
}
