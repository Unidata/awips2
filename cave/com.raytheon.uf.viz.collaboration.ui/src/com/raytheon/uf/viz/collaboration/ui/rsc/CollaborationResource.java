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
package com.raytheon.uf.viz.collaboration.ui.rsc;

import org.eclipse.swt.graphics.RGB;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRenderable;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * A resource for displaying rendered data that is received from the Data
 * Provider.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CollaborationResource extends
        AbstractVizResource<CollaborationResourceData, IDescriptor> {

    protected CollaborationResource(CollaborationResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        // TODO Auto-generated constructor stub
    }

    @Override
    protected void disposeInternal() {
        // TODO Auto-generated method stub

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        // TODO Auto-generated method stub
        DrawableString string = new DrawableString(
                "You can't see it but there is a collaboration resource on this editor",
                new RGB(255, 0, 255));
        string.setCoordinates(800, 3000);
        target.drawStrings(string);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // TODO Auto-generated method stub

    }

    @Subscribe
    public void renderableArrived(IRenderable render) {
        // TODO add it to a list or something
        System.out.println("Received renderable " + render);
    }

}
