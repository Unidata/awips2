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

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.actions.AbstractMapHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ProgressiveDisclosureProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.geotiff.rsc.GeoTiffResource;
import com.raytheon.viz.geotiff.rsc.GeoTiffResourceData;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Open an GeoTIFF image
 *
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------------------------
 * Jul 01, 2006           chammack  Initial Creation.
 * Dec 04, 2014  3848     nabowle   Fix file dialog being cancelled.
 * Oct 25, 2017  5773     bsteffen  Log exceptions with statusHandler.
 * 
 * </pre>
 *
 * @author chammack
 */
public class OpenImageAction extends AbstractMapHandler {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenImageAction.class);

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

        final String fileName = fd.open();

        if (fileName != null) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    try {
                        IDescriptor mapDesc = container.getActiveDisplayPane()
                                .getRenderableDisplay().getDescriptor();

                        if (mapDesc == null) {
                            statusHandler
                                    .error("Map does not support GeoTIFFs");
                        }
                        GeoTiffResourceData data = new GeoTiffResourceData(
                                fileName);
                        LoadProperties lProps = new LoadProperties();
                        GeoTiffResource gtiff = data.construct(lProps, mapDesc);

                        ResourceProperties rProps = new ResourceProperties();

                        rProps.setMapLayer(true);
                        rProps.setVisible(true);
                        rProps.setPdProps(
                                new ProgressiveDisclosureProperties());

                        mapDesc.getResourceList().add(gtiff);
                    } catch (VizException e) {
                        statusHandler.error("Error loading GeoTIFF", e);
                    }
                }
            });
        }
        return null;
    }
}
