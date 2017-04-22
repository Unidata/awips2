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
package com.raytheon.uf.viz.radarapps.fsi;

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.ui.EditorUtil;

public class FSILauncherCommandHandler extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(FSILauncherCommandHandler.class);

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            FSILauncherLayer layer = findLayer(container);
            if (layer == null) {
                IDescriptor desc = container.getActiveDisplayPane()
                        .getDescriptor();
                FSILauncherResourceData rd = new FSILauncherResourceData();
                try {
                    LoadProperties loadProps = new LoadProperties();
                    layer = rd.construct(loadProps, desc);
                    desc.getResourceList().add(layer);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getMessage(), e);
                    return null;
                }
            }
        }
        return null;
    }

    private FSILauncherLayer findLayer(IDisplayPaneContainer container) {
        List<FSILauncherLayer> layers = container.getActiveDisplayPane()
                .getDescriptor().getResourceList()
                .getResourcesByTypeAsType(FSILauncherLayer.class);
        for (FSILauncherLayer layer : layers) {
            return layer;
        }
        return null;
    }

}
