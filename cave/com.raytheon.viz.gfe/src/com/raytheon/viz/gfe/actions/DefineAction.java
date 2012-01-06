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
package com.raytheon.viz.gfe.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.localization.LocalizationPerspectiveUtils;
import com.raytheon.uf.viz.localization.service.ILocalizationService;
import com.raytheon.viz.gfe.LocalizationConstants;

/**
 * Open the Localization Perspective with the appropriate set of files selected.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DefineAction extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        String parameter = event.getParameter("DefineParameter");
        ILocalizationService service = LocalizationPerspectiveUtils
                .changeToLocalizationPerspective();
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationFile file = null;
        if (parameter.equalsIgnoreCase(LocalizationConstants.SMART_TOOLS)) {
            file = pathManager
                    .getStaticLocalizationFile("gfe/userPython/smartTools");
        } else if (parameter.equalsIgnoreCase(LocalizationConstants.CONFIG)) {
            file = pathManager
                    .getStaticLocalizationFile("gfe/userPython/gfeConfig");
        } else if (parameter.equalsIgnoreCase(LocalizationConstants.PROCEDURES)) {
            file = pathManager
                    .getStaticLocalizationFile("gfe/userPython/procedures");
        } else if (parameter
                .equalsIgnoreCase(LocalizationConstants.TEXT_PRODUCTS)) {
            file = pathManager
                    .getStaticLocalizationFile("gfe/userPython/textProducts");
        }

        service.refresh(file);
        service.selectFile(file);

        return null;
    }
}
