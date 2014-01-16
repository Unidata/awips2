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
package com.raytheon.viz.mpe.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.hydrocommon.colorscalemgr.ColorScaleMgrDlg;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.colors.MPEColorManager;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResource;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResourceData;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 5, 2008             mschenke    Initial creation
 * Apr 18, 2013 #1920      mpduff      Set new ColorMap.
 * Jul 02, 2013 #2088      rferrel     Changes for non-blocking ColorScaleMgrDlg.
 * Jan 16, 2014 #2691      lvenable    Fixed null pointer exception that occurs when closing
 *                                     the MPE perspective while the ColorScaleMgrDlg is
 *                                     visible.
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MPEColorScaleMgrAction extends AbstractHandler {

    private ColorScaleMgrDlg colorScaleDlg = null;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        if (colorScaleDlg == null || colorScaleDlg.isDisposed()) {
            String username = LocalizationManager.getInstance()
                    .getCurrentUser();

            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            colorScaleDlg = new ColorScaleMgrDlg(shell, username);
            colorScaleDlg.setTitle("MPE Color Scale Manager - User: "
                    + username);
            colorScaleDlg.setColorManager(new MPEColorManager());
            colorScaleDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    colorScaleDlg = null;

                    MPEDisplayManager mdm = MPEDisplayManager.getCurrent();

                    // If the MPE Display Manager is null then return as no
                    // action is needed.
                    if (mdm == null) {
                        return;
                    }

                    MPEFieldResource displayedFieldResource = mdm
                            .getDisplayedFieldResource();

                    if (displayedFieldResource != null) {
                        MPEFieldResourceData resourceData = displayedFieldResource
                                .getResourceData();
                        displayedFieldResource
                                .getCapability(ColorMapCapability.class)
                                .setColorMapParameters(
                                        MPEDisplayManager.createColorMap(
                                                resourceData.getCvUseString(),
                                                resourceData
                                                        .getDurationInHours(),
                                                resourceData.getDataUnits(),
                                                resourceData.getDisplayUnits()));
                        DisplayFieldData dt = MPEDisplayManager.getCurrent()
                                .getDisplayFieldType();

                        MPEDisplayManager.getCurrent().displayFieldData(dt);
                    }
                }
            });
            colorScaleDlg.open();
        } else {
            colorScaleDlg.bringToTop();
        }

        return null;
    }
}