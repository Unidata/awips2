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

package com.raytheon.viz.spi;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.maps.actions.AbstractMapHandler;
import com.raytheon.uf.viz.core.maps.rsc.MapResourceGroup;
import com.raytheon.uf.viz.core.maps.rsc.MapResourceGroupData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ProgressiveDisclosureProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.ui.EditorUtil;

/**
 * 
 * Eclipse handler for opening SPI resources
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jan 10, 2008 562         bphillip    Initial Creation.
 * Nov 13, 2012      #1326  randerso    Changed to extend AbstractMapHandler
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 * 
 */
public class OpenSPIHandler extends AbstractMapHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenSPIHandler.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container == null) {
            return null;
        }
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        FileDialog fd = new FileDialog(shell, SWT.OPEN | SWT.MULTI);
        fd.setFilterExtensions(new String[] { "*.spi" });
        fd.setFilterPath(PathManagerFactory.getPathManager()
                .getStaticFile(VizApp.getMapsDir()).getAbsolutePath());
        if (fd.open() != null) {

            String path = fd.getFilterPath();
            String fileNames[] = fd.getFileNames();

            try {
                IDescriptor mapDesc = container.getActiveDisplayPane()
                        .getRenderableDisplay().getDescriptor();
                MapResourceGroupData mrgd = new MapResourceGroupData();
                mrgd.setMapName(fileNames[0]);
                MapResourceGroup group = mrgd.construct(new LoadProperties(),
                        mapDesc);
                ResourceProperties props = new ResourceProperties();
                props.setMapLayer(true);
                props.setVisible(true);

                if (mapDesc == null) {
                    Activator
                            .getDefault()
                            .getLog()
                            .log(new Status(Status.ERROR, Activator.PLUGIN_ID,
                                    "Map does not support SPI Files", null));
                    return null;
                }

                mapDesc.getResourceList().add(group, props);

                for (String fileName : fileNames) {
                    SPIResourceData rd = new SPIResourceData();
                    rd.setFilename(FileUtil.join(path, fileName));
                    SPIResource spi = rd.construct(new LoadProperties(),
                            mapDesc);

                    props = new ResourceProperties();
                    props.setMapLayer(true);
                    props.setVisible(true);
                    props.setPdProps(new ProgressiveDisclosureProperties());
                    mrgd.getResourceList().add(spi, props);
                }

            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading SPI File", e);
            }
        }
        return null;
    }

}
