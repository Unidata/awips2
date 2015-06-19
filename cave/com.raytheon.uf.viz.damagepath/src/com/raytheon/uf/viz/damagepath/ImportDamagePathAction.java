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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.json.JsonException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.drawing.polygon.DrawablePolygon;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Action to import a damage path from a GeoJSON file specified by the user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2015  3975      njensen     Initial creation
 * Mar 31, 2015  3977      nabowle     Make sure the polygon is not empty.
 * Apr 23, 2015  4354      dgilling    Support GeoJSON Feature objects.
 * Jun 03, 2015  4375      dgilling    Support multiple polygons.
 * Jun 09, 2015  4355      dgilling    Rename action for UI.
 * Jun 18, 2015  4354      dgilling    Support modifications to loader so each 
 *                                     polygon can have its own properties.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ImportDamagePathAction extends AbstractRightClickAction {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ImportDamagePathAction.class);

    public ImportDamagePathAction() {
        super("Import from File");
    }

    @Override
    public void run() {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                Shell shell = VizWorkbenchManager.getInstance()
                        .getCurrentWindow().getShell();
                FileDialog fd = new FileDialog(shell, SWT.OPEN);
                fd.setFilterExtensions(ExportDamagePathAction.EXTENSIONS);
                String filename = fd.open();

                if (filename != null) {
                    DamagePathLayer<?> layer = (DamagePathLayer<?>) getSelectedRsc();

                    try {
                        DamagePathLoader loader = new DamagePathLoader(filename);

                        Collection<Pair<Polygon, Map<String, String>>> newData = loader
                                .getDamagePathData();
                        if (!newData.isEmpty()) {
                            Collection<DrawablePolygon> newDamagePaths = new ArrayList<>(
                                    newData.size());
                            for (Pair<Polygon, Map<String, String>> data : newData) {
                                newDamagePaths.add(new DamagePathPolygon(data
                                        .getFirst(), data.getSecond(), layer));
                            }
                            layer.resetPolygons(newDamagePaths);
                        } else {
                            throw new JsonException(
                                    "Damage path file contains no polygons.");
                        }
                    } catch (Exception e) {
                        statusHandler.error("Error importing damage path from "
                                + filename, e);
                    }
                }
            }
        });
    }

    @Override
    public boolean isEnabled() {
        AbstractVizResource<?, ?> layer = getSelectedRsc();
        return layer.getCapability(EditableCapability.class).isEditable();
    }
}
