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
package com.raytheon.viz.gfe.rsc;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.edittool.AbstractGFEEditTool;
import com.raytheon.viz.gfe.edittool.EditToolPaintProperties;
import com.raytheon.viz.gfe.edittool.sample.SampleRenderable;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;

/**
 * Resource that provides a variety of services (time matching support) as well
 * as a location to plug in tool specific renderables.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/19/2008              chammack    Added sample set to persistent renderable
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class GFESystemResource extends
        AbstractVizResource<GFEResourceData, MapDescriptor> implements
        IContextMenuContributor {

    private class UndoGridEditAction extends AbstractRightClickAction {

        @Override
        public String getText() {
            return "Undo Grid Edit";
        }

        @Override
        public void run() {
            DataManager.getCurrentInstance().getParmOp().undo();
        }

    }

    private class UndoEditAreaAction extends AbstractRightClickAction {

        @Override
        public String getText() {
            return "Undo Edit Area";
        }

        @Override
        public void run() {
            super.run();
            DataManager.getCurrentInstance().getRefManager().undoRefSet();
        }

    }

    private final DataManager dataManager;

    private DataTime dataTime;

    private final Set<AbstractGFEEditTool> editTools;

    private final Set<IRenderable> persistentRenderables;

    /**
     * Construct the system resource with the datamanager
     * 
     * @param dataManager
     */
    public GFESystemResource(DataManager dataManager) {
        super(new GFEResourceData(), new LoadProperties());
        this.dataManager = dataManager;
        this.editTools = new HashSet<AbstractGFEEditTool>();
        this.persistentRenderables = new HashSet<IRenderable>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        for (AbstractGFEEditTool tool : this.editTools) {
            tool.deactivateTool();
        }

        for (IRenderable r : this.persistentRenderables) {
            if (r instanceof SampleRenderable) {
                ((SampleRenderable) r).dispose();
            }
        }
        this.persistentRenderables.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        return "Internal GFE Resource: " + this.dataTime;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // The following renderables are always ready
        this.persistentRenderables.add(new SampleRenderable());
    }

    /**
     * Register an edit tool with the system resource
     * 
     * This allows it to be discovered for visualizations
     * 
     * @param tool
     */
    public void addEditTool(AbstractGFEEditTool tool) {
        this.editTools.add(tool);
    }

    /**
     * Remove an edit tool from the system resource
     * 
     * @param tool
     */
    public void removeEditTool(AbstractGFEEditTool tool) {
        this.editTools.remove(tool);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        // Create an edit tool paint properties
        EditToolPaintProperties editToolPaintProperties = new EditToolPaintProperties(
                paintProps);
        editToolPaintProperties.setDataManager(this.dataManager);
        editToolPaintProperties.setDescriptor(this.descriptor);

        for (AbstractGFEEditTool tool : this.editTools) {
            IRenderable[] renderables = tool.getRenderables();
            for (IRenderable renderable : renderables) {
                renderable.paint(target, editToolPaintProperties);
            }
        }

        for (AbstractGFEEditTool tool : this.editTools) {
            IRenderable[] renderables = tool.getPersistentRenderables();
            for (IRenderable renderable : renderables) {
                this.persistentRenderables.add(renderable);
            }
        }

        for (IRenderable persistentRenderable : this.persistentRenderables) {
            persistentRenderable.paint(target, editToolPaintProperties);
        }

    }

    // /*
    // * (non-Javadoc)
    // *
    // * @see
    // * com.raytheon.viz.core.rsc.capabilities.ITimeSeqResource#getDataTimes()
    // */
    // @Override
    // public DataTime[] getDataTimes() {
    // IParmManager parmManager = this.dataManager.getParmManager();
    // List<DataTime> dataTimes = new ArrayList<DataTime>();
    //
    // for (Parm p : parmManager.getDisplayedParms()) {
    // for (IGridData gd : p.getGridInventory()) {
    // TimeRange tr = gd.getGridTime();
    // DataTime dt = new DataTime(tr.getStart());
    // dt.setValidPeriod(tr);
    // if (!dataTimes.contains(dt)) {
    // dataTimes.add(dt);
    // }
    // }
    // }
    //
    // return dataTimes.toArray(new DataTime[dataTimes.size()]);
    // }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IRightClickCapableResource#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        menuManager.add(new UndoGridEditAction());
        menuManager.add(new UndoEditAreaAction());
        menuManager.add(new Separator());
        menuManager.add(new GFELegendPopup());

        if (dataManager == null) {
            return;
        }

        menuManager.add(new Separator());
        for (AbstractGFEEditTool tool : this.editTools) {

            if (tool instanceof IContextMenuContributor) {
                ((IContextMenuContributor) tool).addContextMenuItems(
                        menuManager, x, y);
            }
        }
    }

    /**
     * Return a list of the renderable objects
     * 
     * @return
     */
    public IRenderable[] getRenderables() {
        return this.persistentRenderables
                .toArray(new IRenderable[this.persistentRenderables.size()]);
    }
}
