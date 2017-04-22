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
package com.raytheon.uf.viz.damagepath;

import java.util.Map;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * {@code Action} class to launch the {@code EditGeoJsonPropertiesDlg}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2015  #4354     dgilling     Initial creation
 * Jun 09, 2015  #4355     dgilling     Rename action for UI.
 * Jun 18, 2015  #4354     dgilling     Allow individual properties object for
 *                                      each polygon.
 * Jun 30, 2015  #4354     dgilling     Fix NullPointerException.
 * Jan 27, 2016  #5287     dgilling     Support updated 
 *                                      EditGeoJsonPropertiesDlg.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class OpenGeoJsonPropertiesDlgAction extends AbstractRightClickAction {

    private final DamagePathPolygon damagePath;

    public OpenGeoJsonPropertiesDlgAction(final DamagePathPolygon damagePath) {
        super("Set Properties...");
        this.damagePath = damagePath;
    }

    @Override
    public void run() {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                Shell shell = VizWorkbenchManager.getInstance()
                        .getCurrentWindow().getShell();

                final Map<String, String> geoJsonProps = damagePath
                        .getProperties();
                final EditGeoJsonPropertiesDlg dlg = new EditGeoJsonPropertiesDlg(
                        shell, geoJsonProps);
                dlg.addCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        int returnCode = ((Number) returnValue).intValue();
                        Map<String, String> updatedProperties = dlg
                                .getProperties();
                        if ((Window.OK == returnCode)
                                && (!geoJsonProps.equals(updatedProperties))) {
                            damagePath.setProperties(updatedProperties);
                        }
                    }
                });
                dlg.open();
            }
        });
    }
}
