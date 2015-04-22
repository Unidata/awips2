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
package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.display.IPowerLegend;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.EditableManager;

/**
 * This handler is responsible for picking up mouse clicks and key press events
 * on resources in the legend
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/03/2012              S. Gurung     Initial creation
 * 06/25/2012    827       Archana       Updated handleKeyUp() to
 *                                       toggle the display of the
 *                                       resources based on the
 *                                       UP/DOWN arrow key pressed.
 * 07/27/2012	695			B. Yin		 Added middle mouse click to toggle editable resource.
 * 08/09/2012   839       Archana        Updated to toggle the colorbar when 
 *                                       its corresponding resource is toggled on/off.
 * 10/19/2012   897         S. Gurung    Updated handleKeyUp() to not toggle PgenResource and added code to 
 * 										 refresh the editor after handling events.
 * 12/19/2012   960       G. Hull        use propertiesChanged() to toggle colorBar resources
 * 08/18/2014   ?         B. Yin         Handle GroupResource.
 * 
 * 
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class NCLegendHandler extends AbstractNCLegendInputHandler {

    /**
     * @param resource
     */
    protected NCLegendHandler(NCLegendResource resource) {
        super(resource);
    }

    private ResourcePair mouseDownRsc = null;

    private static int currentRscIndex = 0;

    private boolean isShiftDown = false;

    private static boolean isFirstTime = true;

    private boolean doubleClick = false;

    private int doubleClickInterval = 300;

    // (Integer)
    // Toolkit.getDefaultToolkit().getDesktopProperty("awt.multiClickInterval");

    private Job singleClickJob;

    protected final Object singleClickJobLock = new Object();

    private boolean isCtrlDown = false;
    private boolean ctrlDown = false;

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {

        if (mouseButton == 1 || mouseButton == 2) {
            // IDisplayPaneContainer editor = getResourceContainer();
            // if (prefManager.handleClick(HIDE_RESOURCE_PREF, mouseButton)) {
            AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
            if (editor != null && editor instanceof AbstractNcEditor) {
                IDisplayPane activePane = editor.getActiveDisplayPane();
                IRenderableDisplay display = editor.getActiveDisplayPane()
                        .getRenderableDisplay();
                List<ResourcePair> resourcesClicked = resource
                        .getResourceClicked(display.getDescriptor(),
                                activePane.getTarget(), x, y);
                if (resourcesClicked != null && !resourcesClicked.isEmpty()) {
                    mouseDownRsc = resourcesClicked
                            .get(resourcesClicked.size() - 1);
                } else {
                    mouseDownRsc = null;
                }
            }
        }
        return false;
    }

    @Override
    public boolean handleMouseUp(final int x, final int y, final int mouseButton) {

        //Because we wait certain milliseconds for double click,
        //we have to save the status of the ctrl key.
        if ( this.isCtrlDown ){
            ctrlDown = true;
        }
        else {
            ctrlDown = false;
        }
        
        
        if (mouseDownRsc != null) {

            if (doubleClick) {
                synchronized (singleClickJobLock) {
                    if (singleClickJob != null) {
                        singleClickJob.cancel();
                    }
                }
                doubleClick = false;
                return doubleClickMouseUp(x, y, mouseButton);

            }

            singleClickJob = new Job("SingleClickMouseUp") {
                @Override
                protected IStatus run(IProgressMonitor monitor) {
                    VizApp.runSync(new Runnable() {
                        @Override
                        public void run() {
                            singleClickMouseUp(x, y, mouseButton);
                        }
                    });
                    synchronized (singleClickJobLock) {
                        singleClickJob = null;
                    }
                    return Status.OK_STATUS;
                }
            };
            singleClickJob.schedule(doubleClickInterval);

            return true;
        } else {
            return false;
        }

    }

    @Override
    public boolean handleDoubleClick(int x, int y, int mouseButton) {
        if (mouseDownRsc != null && mouseButton == 1) {
            doubleClick = true;
        }
        return true;
    }

    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        return (mouseDownRsc != null);
    }

    public boolean handleMouseHover(int x, int y) {
        return false;
    }

    public boolean handleMouseMove(int x, int y) {
        return false;
    }

    public boolean handleMouseWheel(Event event, int x, int y) {
        return false;
    }

    public boolean handleMouseExit(Event event) {
        return false;
    }

    public boolean handleMouseEnter(Event event) {
        return false;
    }

    @Override
    public boolean handleKeyUp(int keyCode) {

        if (keyCode == SWT.CONTROL) {
            isCtrlDown = false;
            return true;
        }

        if (keyCode != SWT.SHIFT && keyCode != SWT.ARROW_UP
                && keyCode != SWT.ARROW_DOWN) {
            return false;
        }

        if (keyCode == SWT.SHIFT) {
            isShiftDown = true;
        }

        AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
        ResourceList theMainList = editor.getActiveDisplayPane()
                .getDescriptor().getResourceList();

        List<ResourcePair> subListOfResourcesToToggle = new ArrayList<ResourcePair>(
                0);

        if (isShiftDown) {
            /*
             * Pressing the Shift key with either the up or the down arrow key
             * makes all the non-system/non map layer resources visible.
             */
            if ((keyCode == SWT.ARROW_UP || keyCode == SWT.ARROW_DOWN)) {
                for (ResourcePair resPair : theMainList) {
                    resPair.getProperties().setVisible(true);
                }
            }

        } else {

            /*
             * Create 2 sublists.One for the colorbar resources and one for the
             * requestable resources (non-system and non-map layer resources)
             * Set the visibility for all the resources in both lists to false.
             */
            boolean allVisible = true;

            for (ResourcePair resPair : theMainList) {

                if (!resPair.getProperties().isSystemResource()
                        && !resPair.getProperties().isMapLayer()
                        && resPair.getResource().getClass().getSimpleName()
                                .compareTo("PgenResource") != 0) {
                    subListOfResourcesToToggle.add(resPair);
                    allVisible = allVisible
                            && resPair.getProperties().isVisible();
                    resPair.getProperties().setVisible(false);
                }
            }

            if (subListOfResourcesToToggle.isEmpty())
                return false;

            if (allVisible)
                isFirstTime = true;

            int listSize = subListOfResourcesToToggle.size();

            if (keyCode == SWT.ARROW_UP) {
                /*
                 * The navigation seems counter-intuitive. Yet this works since
                 * the elements displayed in the legend resource are listed from
                 * bottom-up
                 * 
                 * The very first time either the up arrow is pressedthe
                 * currentRscIndex gets initialized to the first element in the
                 * listSubsequently, if the up arrow is pressed, the index is
                 * incremented.If it points beyond the index of the last
                 * resource,then it gets reset to the index of the first
                 * resource
                 */
                if (isFirstTime || isShiftDown)
                    currentRscIndex = 0;
                else {
                    currentRscIndex++;
                    if (currentRscIndex > (listSize - 1))
                        currentRscIndex = 0;
                }

            } else if (keyCode == SWT.ARROW_DOWN) {
                /*
                 * The very first time either the down arrow is pressedthe
                 * currentRscIndex gets initialized to the index of the last
                 * resource in the listSubsequently, if the down arrow is
                 * pressed, the index is decremented.If it points beyond the
                 * index of the first resource,then it gets set to the index of
                 * the last resource
                 */

                if (isFirstTime || isShiftDown)
                    currentRscIndex = listSize - 1;
                else {
                    currentRscIndex--;
                    if (currentRscIndex < 0)
                        currentRscIndex = listSize - 1;
                }

            }

            /* Make the resource visible */
            ResourcePair rscToSetVisible = subListOfResourcesToToggle
                    .get(currentRscIndex);
            rscToSetVisible.getProperties().setVisible(true);

            // some resources may have an associated colorBar resource. This
            // will
            // be toggled when the resource's propertiesChanged() method is
            // called.
            // This is triggered by setVisible();

            if (isFirstTime
                    && ((keyCode == SWT.ARROW_DOWN) || (keyCode == SWT.ARROW_UP)))
                isFirstTime = false;

        }

        editor.refresh();

        if (isShiftDown) {
            /*
             * If the shift key was used to make all the resourcesvisible again,
             * the isFirstTime boolean is set to trueSo in effect the
             * currentRscIndex is reset to either the first or the lastnon
             * system/non map layer resource depending on which arrow key is
             * subsequently pressed.
             */
            isShiftDown = false;
            isFirstTime = true;
        }
        return false;
    }

    @Override
    public boolean handleKeyDown(int keyCode) {

        if (keyCode == SWT.SHIFT) {
            isShiftDown = true;
            return false;
        } else if (keyCode == SWT.CONTROL) {
            isCtrlDown = true;
        }
        return false;
    }

    private void toggleVisibility(ResourcePair rp, ResourcePair grp) {
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (rsc != null) {
            if (rsc.hasCapability(BlendedCapability.class)) {
                ResourcePair parentRsc = rsc.getCapability(
                        BlendedCapability.class).getBlendableResource();
                ResourceList children = parentRsc.getResource()
                        .getCapability(BlendableCapability.class)
                        .getResourceList();
                if (parentRsc.getProperties().isVisible() == false) {
                    parentRsc.getProperties().setVisible(true);
                    for (ResourcePair child : children) {
                        child.getProperties().setVisible(true);
                    }
                } else {
                    // topmost resource is visible, toggle us and other rsc
                    if (rp.getProperties().isVisible() == false) {
                        rp.getProperties().setVisible(true);
                        parentRsc
                                .getResource()
                                .getCapability(BlendableCapability.class)
                                .setAlphaStep(BlendableCapability.BLEND_MAX / 2);
                    } else {
                        parentRsc.getResource()
                                .getCapability(BlendableCapability.class)
                                .toggle(rp);
                    }
                }

                return;
            }
        }

        // If the resource is turned on, turn on the resource group.
        // Otherwise the resource is not visible.
        if (grp != null && !rp.getProperties().isVisible()) {
            grp.getProperties().setVisible(!rp.getProperties().isVisible());
        }

        rp.getProperties().setVisible(!rp.getProperties().isVisible());

        if (rp.getResource() instanceof IPowerLegend) {
            IPowerLegend gr = (IPowerLegend) rp.getResource();
            // gr.setVisibleForAllResources(rp.getProperties().isVisible());

            // if the group is turned
            // if (!rp.getProperties().isVisible()) {
            // gr.setNameExpanded(false);
            // }

            // if CTRL is not down, disable all other groups
            if (!ctrlDown) {
                AbstractEditor editor = NcDisplayMngr
                        .getActiveNatlCntrsEditor();
                if (editor != null && editor instanceof AbstractNcEditor) {
                    IRenderableDisplay display = editor.getActiveDisplayPane()
                            .getRenderableDisplay();
                    for (ResourcePair pair : display.getDescriptor()
                            .getResourceList()) {
                        if (pair.getResource() instanceof IPowerLegend
                                && pair != rp && rp.getProperties().isVisible()) {
                            pair.getProperties().setVisible(false);
                            // ((IPowerLegend) pair.getResource())
                            // .setVisibleForAllResources(false);
                        }
                    }
                }
            }
        }
    }

    private boolean singleClickMouseUp(int x, int y, int mouseButton) {

        AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
        if (mouseButton == 1) {
            if (editor != null && editor instanceof AbstractNcEditor) {
                IDisplayPane activePane = editor.getActiveDisplayPane();
                IRenderableDisplay display = editor.getActiveDisplayPane()
                        .getRenderableDisplay();

                ResourcePair rsc;
                ResourcePair grp = null;
                List<ResourcePair> resourcesClicked = resource
                        .getResourceClicked(display.getDescriptor(),
                                activePane.getTarget(), x, y);
                if (resourcesClicked != null && !resourcesClicked.isEmpty()) {
                    rsc = resourcesClicked.get(resourcesClicked.size() - 1);
                    // get the group that rsc belongs to.
                    if (resourcesClicked.size() > 1) {
                        grp = resourcesClicked.get(0);
                    }
                } else {
                    rsc = null;
                }

                if (rsc != null && rsc == mouseDownRsc) {

                    mouseDownRsc = null;
                    toggleVisibility(rsc, grp);
                    editor.refresh();

                    return true;
                }
            }
        } else if (mouseButton == 2) {

            if (mouseDownRsc != null
                    && mouseDownRsc.getResource().hasCapability(
                            EditableCapability.class)) {
                // check / make editable
                EditableManager.makeEditable(
                        mouseDownRsc.getResource(),
                        !mouseDownRsc.getResource()
                                .getCapability(EditableCapability.class)
                                .isEditable());
                mouseDownRsc = null;

                editor.refresh();
                return true;
            }
        }
        return false;
    }

    private boolean doubleClickMouseUp(int x, int y, int mouseButton) {

        if (mouseDownRsc.getResource() instanceof IPowerLegend) {
            IPowerLegend gr = (IPowerLegend) mouseDownRsc.getResource();
            if (mouseDownRsc.getResource().getProperties().isVisible()) {
                if (gr.isNameExpanded()) {
                    gr.setNameExpanded(false);
                } else {
                    gr.setNameExpanded(true);
                }
            }

            resource.issueRefresh();
            return true;
        } else {
            return false;
        }
    }

}
