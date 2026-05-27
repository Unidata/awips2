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
package com.raytheon.uf.viz.grib;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.grib.wizard.GribWizard;
import com.raytheon.uf.viz.localization.perspective.adapter.LocalizationPerspectiveAdapter;
import com.raytheon.uf.viz.localization.perspective.view.FileTreeEntryData;

/**
 * 
 * Adapter for providing a menu item for running the {@link GribWizard}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 19, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GribLocalizationPerspectiveAdapter extends
        LocalizationPerspectiveAdapter {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribLocalizationPerspectiveAdapter.class);

    @Override
    public boolean addContextMenuItems(IMenuManager menuMgr,
            FileTreeEntryData[] selectedData) {
        if (selectedData.length == 1
                && selectedData[0].getClass() == FileTreeEntryData.class) {
            IMenuManager newMenu = menuMgr.findMenuUsingPath(NEW_ID);
            newMenu.add(new Action("Grib Model") {

                @Override
                public void run() {
                    runWizard();
                }

            });
        }
        return false;
    }

    private void runWizard() {
        Shell shell = Display.getCurrent().getActiveShell();
        try {
            WizardDialog dialog = new WizardDialog(shell, new GribWizard());
            dialog.open();
        } catch (Exception e) {
            statusHandler.error("Error opening Grib Wizard", e);
        }
    }

}
