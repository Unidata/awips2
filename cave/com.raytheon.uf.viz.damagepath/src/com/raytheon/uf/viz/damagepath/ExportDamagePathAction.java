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

import java.io.FileOutputStream;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.geotools.data.simple.SimpleFeatureCollection;

import com.raytheon.uf.common.json.geo.IGeoJsonService;
import com.raytheon.uf.common.json.geo.SimpleGeoJsonService;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Action to export a damage path as GeoJSON to a file specified by the user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 09, 2015  3975      njensen     Initial creation
 * Apr 23, 2015  4354      dgilling    Export as GeoJSON Feature object.
 * Jun 05, 2015  4375      dgilling    Prompt user before exporting feature 
 *                                     with no polygons.
 * Jun 09, 2015  4355      dgilling    Rename action for UI.
 * Jun 18, 2015  #4354     dgilling    Support FeatureCollections so each 
 *                                     polygon can have its own properties.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ExportDamagePathAction extends AbstractRightClickAction {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExportDamagePathAction.class);

    private static final String CONFIRM_DLG_MSG = "Damage Path has no polygons. Continue with exporting file?";

    private static final String CONFIRM_DLG_TITLE = "Damage Path has no polygons!";

    protected static final String[] EXTENSIONS = new String[] { "*.json" };

    public ExportDamagePathAction() {
        super("Export to File");
    }

    @Override
    public void run() {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                Shell shell = VizWorkbenchManager.getInstance()
                        .getCurrentWindow().getShell();
                FileDialog fd = new FileDialog(shell, SWT.SAVE);
                fd.setFilterExtensions(EXTENSIONS);
                String filename = fd.open();

                if (filename != null) {
                    DamagePathLayer<?> layer = (DamagePathLayer<?>) getSelectedRsc();
                    SimpleFeatureCollection featureCollection = layer
                            .buildFeatureCollection();

                    if (featureCollection.size() < 1) {
                        boolean export = MessageDialog.openConfirm(shell,
                                CONFIRM_DLG_TITLE, CONFIRM_DLG_MSG);
                        if (!export) {
                            statusHandler
                                    .info("User chose to cancel exporting damage path file because it had no polygons.");
                            return;
                        }
                    }

                    try (FileOutputStream fos = new FileOutputStream(filename)) {
                        IGeoJsonService json = new SimpleGeoJsonService();
                        json.serialize(featureCollection, fos);
                    } catch (Exception e) {
                        statusHandler.error(
                                "Error exporting damage path file to "
                                        + filename, e);
                    }
                }
            }
        });
    }

}
