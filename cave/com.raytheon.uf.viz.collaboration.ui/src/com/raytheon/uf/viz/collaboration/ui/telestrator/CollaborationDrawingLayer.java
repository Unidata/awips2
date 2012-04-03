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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import java.util.Arrays;
import java.util.Map;

import org.eclipse.swt.graphics.Color;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.drawing.DrawingLayer;
import com.raytheon.uf.viz.drawing.PathDrawingResourceData;
import com.vividsolutions.jts.geom.LineString;

/**
 * A layer that extends Drawing Layer but allows other users to have things
 * drawn as well
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationDrawingLayer extends DrawingLayer {

    private Multimap<Color, IWireframeShape> collaboratorShapes;

    /**
     * @param data
     * @param props
     */
    public CollaborationDrawingLayer(PathDrawingResourceData data,
            LoadProperties props) {
        super(data, props);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingLayer#initInternal(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        this.collaboratorShapes = HashMultimap.create();
        getCapabilities().removeCapability(ColorableCapability.class);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingLayer#paintInternal(com.raytheon.uf
     * .viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);

        OutlineCapability outline = getCapability(OutlineCapability.class);
        // paint the shapes that come over from others
        for (Color color : collaboratorShapes.keySet()) {
            for (IWireframeShape sh : collaboratorShapes.get(color)) {
                target.drawWireframeShape(sh, color.getRGB(),
                        outline.getOutlineWidth(), outline.getLineStyle());
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingLayer#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        for (IWireframeShape shape : collaboratorShapes.values()) {
            shape.dispose();
        }
        collaboratorShapes.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingLayer#finalizeLine(com.vividsolutions
     * .jts.geom.LineString, java.lang.String)
     */
    @Override
    public void finalizeLine(LineString line, String uuid) {
        super.finalizeLine(line, uuid);

        Map<String, IVenueSession> sessions = CollaborationDataManager
                .getInstance().getSessions();

        TelestratorLine tObject = new TelestratorLine();
        // set the coordinates of the TransferLine
        tObject.setCoordinates(Arrays.asList(line.getCoordinates()));
        // get the color of the user here, before sending it off
        tObject.setColor(null);
        // for (String str : sessions.keySet()) {
        // try {
        // ((ISharedDisplaySession) sessions.get(str)).sendEvent(tObject);
        // } catch (CollaborationException e) {
        // e.printStackTrace();
        // }
        // }
    }
}
