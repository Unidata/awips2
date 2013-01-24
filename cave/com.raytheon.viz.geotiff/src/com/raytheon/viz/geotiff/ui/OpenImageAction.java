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

package com.raytheon.viz.geotiff.ui;

import java.io.File;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.maps.actions.AbstractMapHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ProgressiveDisclosureProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.geotiff.Activator;
import com.raytheon.viz.geotiff.rsc.GeoTiffResource;
import com.raytheon.viz.geotiff.rsc.GeoTiffResourceData;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Open an GeoTIFF image
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class OpenImageAction extends AbstractMapHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        final IDisplayPaneContainer container = EditorUtil
                .getActiveVizContainer();
        if (container == null) {
            return null;
        }
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        FileDialog fd = new FileDialog(shell, SWT.OPEN);
        fd.setFilterExtensions(new String[] { "*.tif;*.tiff" });
        fd.open();

        final String fileName = fd.getFilterPath() + File.separator
                + fd.getFileName();

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                try {
                    IDescriptor mapDesc = container.getActiveDisplayPane()
                            .getRenderableDisplay().getDescriptor();

                    if (mapDesc == null) {
                        Activator
                                .getDefault()
                                .getLog()
                                .log(new Status(Status.ERROR,
                                        Activator.PLUGIN_ID,
                                        "Map does not support GeoTIFFs", null));
                    }
                    GeoTiffResourceData data = new GeoTiffResourceData(fileName);
                    LoadProperties lProps = new LoadProperties();
                    GeoTiffResource gtiff = data.construct(lProps, mapDesc);

                    ResourceProperties rProps = new ResourceProperties();

                    rProps.setMapLayer(true);
                    rProps.setVisible(true);
                    rProps.setPdProps(new ProgressiveDisclosureProperties());

                    mapDesc.getResourceList().add(gtiff);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });

        return null;
    }
}
