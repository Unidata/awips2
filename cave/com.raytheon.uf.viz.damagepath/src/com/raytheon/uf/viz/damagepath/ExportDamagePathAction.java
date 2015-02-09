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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.json.geo.GeoJsonUtil;
import com.raytheon.uf.common.json.geo.GeoJsonUtilSimpleImpl;
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
 * Feb 9, 2015  3975       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ExportDamagePathAction extends AbstractRightClickAction {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExportDamagePathAction.class);

    protected static final String[] EXTENSIONS = new String[] { "*.json" };

    public ExportDamagePathAction() {
        super("Export GeoJSON");
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
                    try (FileOutputStream fos = new FileOutputStream(filename)) {
                        GeoJsonUtil json = new GeoJsonUtilSimpleImpl();
                        json.serialize(layer.getPolygon(), fos);
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
