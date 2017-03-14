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
package com.raytheon.viz.gfe.edittool.sample;

import java.awt.Point;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.edittool.AbstractGFEEditTool;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Implements the GFE Sample Tool
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/05/2008              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class SampleTool extends AbstractGFEEditTool {

    /** The distance in pixels that a user can click to delete a sample */
    private static final int PIXEL_THRESHOLD = 10;

    protected Coordinate roamingSampleCoord;

    protected TransitorySampleRenderable transSample;

    // protected SampleRenderable sample;

    public SampleTool() {
        super();
        this.transSample = new TransitorySampleRenderable();
        this.renderables.add(this.transSample);
        // this.sample = new SampleRenderable();
        // this.persistentRenderables.add(this.sample);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.edittool.AbstractGFEEditTool#handleEvent(int,
     * com.raytheon.viz.gfe.edittool.AbstractGFEEditTool.EventType,
     * java.awt.Point, com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected void handleEvent(int button, EventType type, Point point2D,
            Coordinate coordinate) {
        if (type == EventType.MOUSE_CLICK) {
            if (button == 1) {

                this.dataManager.getSampleSetManager().addAnchoredSample(
                        coordinate);

                refresh();

            } else if (button == 2) {
                // Calculate the fuzz
                IRenderableDisplay display = this.editor.getActiveDisplayPane()
                        .getRenderableDisplay();
                MapDescriptor descriptor = (MapDescriptor) display
                        .getDescriptor();
                IExtent extent = display.getView().getExtent();
                Envelope e = descriptor.pixelToWorld(extent,
                        descriptor.getCRS());
                float fuzz = (float) (((e.getWidth() + e.getHeight()) / 2) * 0.02);
                this.dataManager.getSampleSetManager().removeAnchoredSample(
                        coordinate, fuzz);

                refresh();
            }

        } else if (type == EventType.IN_DRAG) {
            this.transSample.setCoordinate(coordinate);
            refresh();
        } else if (type == EventType.END_DRAG) {
            this.transSample.setCoordinate(null);
            refresh();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.edittool.AbstractGFEEditTool#getToolType()
     */
    @Override
    protected ToolType getToolType() {
        return ToolType.PARM_BASED;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IRightClickCapableResource#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        boolean showMarkers = this.dataManager.getSpatialDisplayManager()
                .isShowISCMarkers();

        // only add isc items when markers are enabled and
        // a visible parm is one from the ISC database.
        if (showMarkers) {
            Parm[] parms = this.dataManager.getSpatialDisplayManager()
                    .getCurrentlyEnabledParms();
            for (Parm parm : parms) {
                if (parm.getParmID().getDbId().getModelName().equals("ISC")) {
                    GridID gid = new GridID(parm, dataManager
                            .getSpatialDisplayManager().getSpatialEditorTime());
                    Coordinate location = this.editor.translateClick(x, y);
                    menuManager.add(new AddISCMarkerAction(location, gid));

                    // are we close enough to remove an ISC Marker?
                    IRenderableDisplay display = this.editor
                            .getActiveDisplayPane().getRenderableDisplay();
                    MapDescriptor descriptor = (MapDescriptor) display
                            .getDescriptor();
                    IExtent extent = display.getView().getExtent();
                    Envelope e = descriptor.pixelToWorld(extent,
                            descriptor.getCRS());
                    float fuzz = (float) (((e.getWidth() + e.getHeight()) / 2) * 0.03);
                    if (dataManager.getSampleSetManager()
                            .anchoredMarkerAtLocation(location, gid, fuzz)) {
                        menuManager.add(new RemoveISCMarkerAction(location,
                                gid, fuzz));
                    }

                    menuManager.add(new Separator());
                    break;
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#dispose()
     */
    @Override
    public void dispose() {
        transSample.dispose();
        super.dispose();
    }

    private class AddISCMarkerAction extends AbstractRightClickAction {

        private Coordinate location;

        private GridID gid;

        private AddISCMarkerAction(Coordinate location, GridID gid) {
            super("Add ISC Marker");
            this.location = location;
            this.gid = gid;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            dataManager.getSampleSetManager().addAnchoredMarker(location, gid);
        }
    }

    private class RemoveISCMarkerAction extends AbstractRightClickAction {

        private Coordinate location;

        private GridID gid;

        private Float fuzz;

        private RemoveISCMarkerAction(Coordinate location, GridID gid,
                Float fuzz) {
            super("Remove ISC Marker");
            this.location = location;
            this.gid = gid;
            this.fuzz = fuzz;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            dataManager.getSampleSetManager().removeAnchoredMarker(location,
                    gid, fuzz);
        }
    }
}
