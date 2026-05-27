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
package com.raytheon.viz.hydro.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.localization.perspective.service.ILocalizationService;
import com.raytheon.uf.viz.localization.perspective.service.LocalizationPerspectiveUtils;
import com.raytheon.uf.viz.pdc.engine.PointControlLocationShift;

/**
 * Edit Shift Location configuration menu action.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2010 2635       mpduff      Initial creation
 * Jul 16, 2013 2088       rferrel     Changes for non-blocking TextEditorDlg.
 * Aug 07, 2018 7400       dgilling    Edit using Localization Perspective.
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 *
 * </pre>
 *
 * @author mpduff
 */

public class EditLocationShiftAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile file = pm.getStaticLocalizationFile(
                PointControlLocationShift.LOCATION_SHIFT_CONFIG);

        if (file != null) {
            ILocalizationService service = LocalizationPerspectiveUtils
                    .changeToLocalizationPerspective();
            service.selectFile(file);
            service.openFile(file);
        } else {
            Shell shell = HandlerUtil.getActiveShellChecked(event);
            MessageDialog.openError(shell, "File Error",
                    "The following file does not exist:\n"
                            + PointControlLocationShift.LOCATION_SHIFT_CONFIG);
        }

        return null;
    }
}
