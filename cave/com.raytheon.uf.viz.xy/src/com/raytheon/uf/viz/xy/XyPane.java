/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.xy;

import java.util.List;
import java.util.Map.Entry;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IPane;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.InputManager;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.IGlobalChangedListener;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.xy.map.IInsetMapContainer;
import com.raytheon.uf.viz.xy.map.IInsetMapContainer.InsetMapUtil;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.panes.AbstractComboPane;
import com.raytheon.viz.ui.panes.VizDisplayPane;

/**
 * Represents a single X/Y graph pane, such as a cross section or time series
 * pane. X/Y panes typically have the main graph canvas, along with an inset map
 * canvas.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2022 8791       mapeters    Initial creation
 * Sep 12, 2022 8792       mapeters    Updated registerHandlers() signature,
 *                                     extend new class AbstractComboPane, move
 *                                     active canvas tracking to superclass
 * Nov 17, 2022 8978       mapeters    Remove unnecessary inset display sharing
 *
 * </pre>
 *
 * @author mapeters
 */
public class XyPane extends AbstractComboPane
        implements IPropertyChangeListener, IGlobalChangedListener,
        IRenderableDisplayChangedListener {

    protected final IDisplayPaneContainer paneContainer;

    /**
     * Pixel width to hide inset map
     */
    private int displayInsetWidth;

    /**
     * Density used to hide inset map
     */
    protected double displayInsetDensity;

    /**
     * Current density
     */
    protected double currentDensity;

    /**
     * Constructor.
     *
     * @param paneContainer
     *            the editor/container that this pane is going in
     * @param composite
     *            SWT composite for this pane's area
     * @param renderableDisplay
     *            main display to initially render on this pane
     * @param panes
     *            the other panes that are already in the pane container
     * @throws VizException
     */
    public XyPane(IDisplayPaneContainer paneContainer, Composite composite,
            IRenderableDisplay renderableDisplay, List<IPane> panes)
            throws VizException {
        super(composite);
        this.paneContainer = paneContainer;
        this.paneContainer.addRenderableDisplayChangedListener(this);
        IPersistentPreferenceStore store = Activator.getDefault()
                .getPreferenceStore();
        displayInsetWidth = store.getInt(Activator.MAP_DISPLAY_WIDTH);
        displayInsetDensity = store.getDouble(Activator.MAP_DISPLAY_DENSITY);
        store.addPropertyChangeListener(this);
        VizGlobalsManager.addListener(VizConstants.DENSITY_ID, this);
        currentDensity = (Double) VizGlobalsManager.getCurrentInstance()
                .getProperty(VizConstants.DENSITY_ID);

        composite.setLayout(new FormLayout());
        initInsetCanvases(paneContainer, composite, renderableDisplay, panes);

        IDisplayPane mainCanvas = new VizDisplayPane(paneContainer, composite,
                CanvasType.MAIN, renderableDisplay);
        ((VizDisplayPane) mainCanvas).getCanvas()
                .setLayoutData(getFullFormData());
        addCanvas(mainCanvas);
    }

    /**
     * Initialize any inset canvases that should be in this pane. This should
     * only be called once during construction, and is only split out from the
     * constructor to allow subclasses to initialize extra inset canvases before
     * the main canvas is created, which is necessary for them to display
     * correctly.
     *
     * @param paneContainer
     *            the editor/container that this pane is going in
     * @param composite
     *            SWT composite for this pane's area
     * @param renderableDisplay
     *            main display to initially render on this pane
     * @param panes
     *            the other panes that are already in the pane container
     * @throws VizException
     */
    protected void initInsetCanvases(IDisplayPaneContainer paneContainer,
            Composite composite, IRenderableDisplay renderableDisplay,
            List<IPane> panes) throws VizException {
        if (InsetMapUtil.isInsetMapDisplay(renderableDisplay)) {
            final Composite insetComp = new Composite(composite, SWT.NONE);
            insetComp.setLayoutData(((IInsetMapContainer) renderableDisplay)
                    .getInsetMapLocation());
            insetComp.setLayout(new FormLayout());

            IRenderableDisplay insetDisplay = InsetMapUtil
                    .loadInsetMap(renderableDisplay);
            IDisplayPane insetCanvas = new VizDisplayPane(paneContainer,
                    insetComp, CanvasType.INSET, insetDisplay);
            ((VizDisplayPane) insetCanvas).getCanvas()
                    .setLayoutData(getFullFormData());
            setInsetVisibility(insetCanvas);
            addCanvas(insetCanvas);
        }
    }

    @Override
    public void registerHandlers(InputManager inputManager) {
        for (Entry<CanvasType, IDisplayPane> canvasEntry : getCanvasMap()
                .entrySet()) {
            CanvasType type = canvasEntry.getKey();
            IDisplayPane canvas = canvasEntry.getValue();
            canvas.addListener(inputManager);
            if (type != CanvasType.MAIN) {
                canvas.addListener(SWT.Resize, event -> {
                    setInsetVisibility(canvas);
                    composite.layout();
                });

            }
        }
    }

    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (Activator.MAP_DISPLAY_DENSITY.equals(event.getProperty())) {
            displayInsetDensity = Double
                    .parseDouble(String.valueOf(event.getNewValue()));
        } else if (Activator.MAP_DISPLAY_WIDTH.equals(event.getProperty())) {
            displayInsetWidth = Integer
                    .parseInt(String.valueOf(event.getNewValue()));
        }
        VizApp.runAsync(() -> {
            for (Entry<CanvasType, IDisplayPane> canvasEntry : getCanvasMap()
                    .entrySet()) {
                if (canvasEntry.getKey() != CanvasType.MAIN) {
                    setInsetVisibility(canvasEntry.getValue());
                }
            }
            composite.layout();
        });
    }

    @Override
    public void updateValue(IWorkbenchWindow changedWindow, Object value) {
        if (paneContainer == EditorUtil.getActiveVizContainer(changedWindow)) {
            // Applies to us, set new density
            currentDensity = (Double) value;
            for (Entry<CanvasType, IDisplayPane> canvasEntry : getCanvasMap()
                    .entrySet()) {
                if (canvasEntry.getKey() != CanvasType.MAIN) {
                    setInsetVisibility(canvasEntry.getValue());
                }
            }
            composite.layout();
        }
    }

    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
        if (pane == getMainCanvas() && type == DisplayChangeType.ADD) {
            /*
             * This is needed for inset maps to display correctly when loading
             * Procedures.
             */
            IDisplayPane insetCanvas = getCanvas(CanvasType.INSET);
            if (insetCanvas != null
                    && InsetMapUtil.isInsetMapDisplay(newRenderableDisplay)) {
                IRenderableDisplay insetDisplay = InsetMapUtil
                        .loadInsetMap(newRenderableDisplay);
                insetCanvas.setRenderableDisplay(insetDisplay);
                insetDisplay.refresh();
            }
        }
    }

    @Override
    protected void onCompositeDispose() {
        Activator.getDefault().getPreferenceStore()
                .removePropertyChangeListener(this);
        VizGlobalsManager.removeListener(VizConstants.DENSITY_ID, this);
        super.onCompositeDispose();
    }

    /**
     * Update the visibility of the given inset canvas based on the current
     * density setting and inset width.
     *
     * @param insetCanvas
     *            the inset canvas to update the visibility of
     */
    protected void setInsetVisibility(IDisplayPane insetCanvas) {
        Composite insetComp = ((VizDisplayPane) insetCanvas).getComposite();
        boolean shouldBeVisByWidth = insetComp
                .getBounds().width >= displayInsetWidth;
        boolean shouldBeVisByDensity = currentDensity >= displayInsetDensity;
        boolean visible = shouldBeVisByDensity && shouldBeVisByWidth;
        insetComp.setVisible(visible);
    }

    protected static FormData getFullFormData() {
        FormData fd = new FormData();
        fd.left = new FormAttachment(0, 0);
        fd.right = new FormAttachment(100, 0);
        fd.top = new FormAttachment(0, 0);
        fd.bottom = new FormAttachment(100, 0);
        return fd;
    }
}
