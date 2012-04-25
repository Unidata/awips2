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
package com.raytheon.uf.viz.collaboration.ui.editor;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationWrapperResource;
import com.raytheon.uf.viz.collaboration.ui.rsc.DataProviderRsc;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.display.PlainMapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Utilities for setting up collaboration editors.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class EditorSetup {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditorSetup.class);

    /**
     * Extracts a SharedEditorData object from the editor passed in.
     * 
     * @param editor
     *            the editor to extract a shared editor for.
     * @return
     */
    public static SharedEditorData extractSharedEditorData(AbstractEditor editor) {
        SharedEditorData se = new SharedEditorData();

        // extract grid geometry
        IDescriptor desc = editor.getActiveDisplayPane().getDescriptor();
        se.setGeometry(desc.getGridGeometry());

        // extract extent to get the proper zoom/pan
        IExtent extent = editor.getActiveDisplayPane().getRenderableDisplay()
                .getExtent();
        se.setEnvelope(new Envelope(extent.getMinX(), extent.getMaxX(), extent
                .getMinY(), extent.getMaxY()));

        List<ResourcePair> rscList = new ArrayList<ResourcePair>();

        // extract map resources
        Iterator<ResourcePair> itr = desc.getResourceList().iterator();
        while (itr.hasNext()) {
            ResourcePair rp = itr.next();
            if (rp.getResource() instanceof CollaborationWrapperResource) {
                ResourcePair copy = new ResourcePair();
                copy.setLoadProperties(rp.getLoadProperties());
                copy.setProperties(rp.getProperties());
                copy.setResourceData(rp.getResource().getResourceData());
                rscList.add(copy);
            }
        }
        se.setLocalResources(rscList);

        return se;
    }

    /**
     * Creates and opens a CollaborationEditor based on the SharedEditorData.
     * 
     * @param sharedEditor
     *            the data necessary to create the editor
     * @return
     */
    public static CollaborationEditor createEditor(SharedEditorData sharedEditor) {
        CollaborationEditor editor = null;
        AbstractRenderableDisplay[] displays = new AbstractRenderableDisplay[1];
        try {
            // TODO make it work with any IDescriptor and
            // AbstractRenderableDisplay
            MapDescriptor descriptor = new MapDescriptor(
                    sharedEditor.getGeometry());

            // setup renderable display
            PlainMapRenderableDisplay disp = new PlainMapRenderableDisplay(
                    descriptor);
            PixelExtent extent = new PixelExtent(sharedEditor.getEnvelope()
                    .getMinX(), sharedEditor.getEnvelope().getMaxX(),
                    sharedEditor.getEnvelope().getMinY(), sharedEditor
                            .getEnvelope().getMaxY());
            disp.setExtent(extent);

            displays[0] = disp;
            editor = (CollaborationEditor) UiUtil.createEditor(
                    CollaborationEditor.EDITOR_ID, displays);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return editor;
    }

    /**
     * Initializes the resources necessary to share the editor with others
     * connected to the collaboration session.
     * 
     * @param sessionId
     *            the id of the session to share the editor with
     * @param editor
     *            the editor to share
     * @throws CollaborationException
     */
    public static void shareEditor(String sessionId, AbstractEditor editor)
            throws CollaborationException {
        ISharedDisplaySession session = (ISharedDisplaySession) CollaborationDataManager
                .getInstance().getSession(sessionId);
        if (!session.getUserID().equals(session.getCurrentDataProvider())) {
            throw new CollaborationException(
                    "Incorrect role to share an editor");
        } else if (editor instanceof CollaborationEditor) {
            throw new CollaborationException(
                    "Cannot share a CollaborationEditor");
        }

        addIndicator(editor, session);

        // TODO this method should be called by the ShareEditorAction

        // TODO should max's target injection be over here too?

    }

    private static void addIndicator(AbstractEditor editor,
            ISharedDisplaySession session) {
        IDescriptor desc = editor.getActiveDisplayPane().getRenderableDisplay()
                .getDescriptor();
        GenericResourceData grd = new GenericResourceData(
                DataProviderRsc.class);
        ResourcePair rp = new ResourcePair();
        rp.setResourceData(grd);
        desc.getResourceList().add(rp);
        desc.getResourceList().instantiateResources(desc, true);
        DataProviderRsc rsc = (DataProviderRsc) rp
                .getResource();
        rsc.setRoomName(((IVenueSession) session).getVenue().getInfo()
                .getVenueDescription());
        rsc.setSubject(((IVenueSession) session).getVenue().getInfo()
                .getVenueSubject());
        rsc.setSession(session);
    }

}
