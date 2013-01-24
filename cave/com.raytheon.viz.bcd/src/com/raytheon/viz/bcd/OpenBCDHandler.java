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

package com.raytheon.viz.bcd;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
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
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.actions.AbstractMapHandler;
import com.raytheon.uf.viz.core.maps.rsc.MapResourceGroup;
import com.raytheon.uf.viz.core.maps.rsc.MapResourceGroupData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ProgressiveDisclosureProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.ui.EditorUtil;

/**
 * 
 * Eclipse handler for opening BCD resources
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Sep 24, 2007 377         randerso    Initial Creation.
 * Nov 13, 2012      #1326  randerso    Changed to extend AbstractMapHandler
 * </pre>
 * 
 * @author randerso
 * @version 1
 * 
 */
public class OpenBCDHandler extends AbstractMapHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenBCDHandler.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        FileDialog fd = new FileDialog(shell, SWT.OPEN | SWT.MULTI);
        fd.setFilterExtensions(new String[] { "*.bcd", "*.bcx" });
        fd.setFilterPath(PathManagerFactory.getPathManager()
                .getStaticFile(VizApp.getMapsDir()).getAbsolutePath());
        if (fd.open() != null) {

            String path = fd.getFilterPath();
            String fileNames[] = fd.getFileNames();

            try {
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    IDescriptor desc = container.getActiveDisplayPane()
                            .getDescriptor();
                    if (desc instanceof IMapDescriptor) {
                        IMapDescriptor mapDesc = (IMapDescriptor) desc;

                        ResourceList list;
                        if (fileNames.length > 1) {
                            MapResourceGroupData mrgd = new MapResourceGroupData();
                            mrgd.setMapName(fileNames[0]);
                            MapResourceGroup group = mrgd.construct(
                                    new LoadProperties(), mapDesc);
                            ResourceProperties props = new ResourceProperties();
                            props.setMapLayer(true);
                            props.setVisible(true);

                            mapDesc.getResourceList().add(group, props);
                            list = group.getResourceList();
                        } else {
                            list = mapDesc.getResourceList();
                        }
                        for (String fileName : fileNames) {
                            BCDResourceData rd = new BCDResourceData();
                            rd.setFilename(FileUtil.join(path, fileName));
                            BCDResource bcd = rd.construct(
                                    new LoadProperties(), mapDesc);
                            rd.setMapName(fileName);

                            ResourceProperties props = new ResourceProperties();
                            props.setMapLayer(true);
                            props.setVisible(true);
                            props.setPdProps(new ProgressiveDisclosureProperties());
                            list.add(bcd, props);
                        }
                    }
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading BCD File", e);
            }
        }
        return null;
    }
}
