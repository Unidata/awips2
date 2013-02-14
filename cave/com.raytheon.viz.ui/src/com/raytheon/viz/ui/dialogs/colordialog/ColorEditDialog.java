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

package com.raytheon.viz.ui.dialogs.colordialog;

import java.io.File;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IVizEditorChangedListener;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;

/**
 * This is the main dialog for the Color Edit Dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         lvenable    Initial Creation.
 * Jul 24, 2007            njensen     Hooked into backend.
 * Oct 17, 2012 1229       rferrel     Changes for non-blocking SaveColorMapDialog.
 * Jan 10, 2013 15648      ryu         Editing GFE discrete colormap: a check button
 *                                     is added and duplicate entries in the colormap
 *                                     are removed when it is selected.
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class ColorEditDialog extends CaveSWTDialog implements
        IVizEditorChangedListener, IRenderableDisplayChangedListener,
        RemoveListener, AddListener, IResourceDataChanged,
        ISelectedPanesChangedListener, IColorEditCompCallback {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ColorEditDialog.class);

    private final String NO_COLOR_TABLE = "No color table is being edited";

    private String currentColormapName;

    private ColorMapCapability cap;

    private ColorEditComposite colorEditComp;

    private boolean rightImages;

    private IDisplayPaneContainer container;

    /**
     * Interpolate button.
     */
    private Button interpolateBtn;

    /**
     * Undo button.
     */
    private Button undoBtn;

    /**
     * Redo button.
     */
    private Button redoBtn;

    /**
     * Revert button.
     */
    private Button revertBtn;

    /**
     * Save button.
     */
    private Button saveBtn;

    /**
     * Save As button.
     */
    private Button saveAsBtn;

    /**
     * Office save as button.
     */
    private Button officeSaveAsBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    private AbstractVizResource<?, ?> singleResourceToEdit;

    private static ColorEditDialog instance = null;

    private SaveColorMapDialog officeSaveAsDialog;

    private SaveColorMapDialog saveAsDialog;

    public static void openDialog(Shell parent,
            IDisplayPaneContainer container,
            AbstractVizResource<?, ?> singleRscToEdit, boolean rightImages,
            boolean block) {
        if (instance == null || !instance.isOpen()) {
            instance = new ColorEditDialog(parent, container, singleRscToEdit,
                    rightImages);
            instance.open();
        } else {
            instance.update(container, singleRscToEdit, rightImages);
            if (block) {
                while (instance.getShell().isDisposed() == false) {
                    if (!instance.getDisplay().readAndDispatch()) {
                        instance.getDisplay().sleep();
                    }
                }
            }
        }
    }

    /**
     * Constructor.
     *
     * @param parent
     *            Parent shell.
     */
    private ColorEditDialog(Shell parent, IDisplayPaneContainer container,
            AbstractVizResource<?, ?> singleRscToEdit, boolean rightImages) {
        super(parent, SWT.DIALOG_TRIM | SWT.MIN);
        this.rightImages = rightImages;
        if (container == null && singleRscToEdit == null) {
            container = EditorUtil.getActiveVizContainer();
        }
        this.container = container;
        this.singleResourceToEdit = singleRscToEdit;
        cap = getCapabilityToEdit();
        currentColormapName = getCurrentColormapName(cap);

        if (container != null) {
            // Editor switching (who is the active editor!?)
            VizWorkbenchManager.getInstance().addListener(this);
            addListeners(container);
        }
    }

    private String getCurrentColormapName(ColorMapCapability cap) {
        return cap != null ? cap.getColorMapParameters().getColorMapName()
                : null;
    }

    private ColorMapCapability getCapabilityToEdit() {
        if (singleResourceToEdit != null
                && singleResourceToEdit.hasCapability(ColorMapCapability.class)) {
            return singleResourceToEdit.getCapability(ColorMapCapability.class);
        } else {
            IDisplayPane imagePane = null;
            ColorMapCapability cap = null;
            if (container instanceof IMultiPaneEditor) {
                imagePane = ((IMultiPaneEditor) container)
                        .getSelectedPane(IMultiPaneEditor.VISIBLE_PANE);
                if (imagePane == null) {
                    imagePane = ((IMultiPaneEditor) container)
                            .getSelectedPane(IMultiPaneEditor.IMAGE_ACTION);
                }
            }

            IDisplayPane[] panes = null;

            if (imagePane == null) {
                panes = container.getDisplayPanes();
            } else {
                panes = new IDisplayPane[] { imagePane };
            }

            boolean hasBlended = false;

            for (IDisplayPane pane : panes) {
                if (pane.isVisible()) {
                    for (ResourcePair rp : pane.getDescriptor()
                            .getResourceList()) {
                        if (rp.getResource() != null
                                && rp.getResource().hasCapability(
                                        BlendableCapability.class)) {
                            hasBlended = true;
                            break;
                        }
                    }
                }
            }

            // find first visible pane with image
            for (IDisplayPane pane : panes) {
                if (pane.isVisible()) {
                    for (ResourcePair rp : pane.getDescriptor()
                            .getResourceList()) {
                        if (rp.getResource() != null) {
                            AbstractVizResource<?, ?> rsc = rp.getResource();
                            if (!hasBlended
                                    && rsc.hasCapability(ColorMapCapability.class)) {
                                cap = rsc
                                        .getCapability(ColorMapCapability.class);
                                break;
                            } else if (rsc
                                    .hasCapability(BlendableCapability.class)) {
                                ResourceList blendedList = rsc.getCapability(
                                        BlendableCapability.class)
                                        .getResourceList();
                                if (!rightImages && blendedList.size() > 0) {
                                    ResourcePair pair = blendedList.get(0);
                                    if (pair.getResource() != null
                                            && pair.getResource()
                                                    .hasCapability(
                                                            ColorMapCapability.class)) {
                                        cap = pair.getResource().getCapability(
                                                ColorMapCapability.class);
                                        break;
                                    }
                                } else if (rightImages
                                        && blendedList.size() > 1) {
                                    ResourcePair pair = blendedList.get(1);
                                    if (pair.getResource() != null
                                            && pair.getResource()
                                                    .hasCapability(
                                                            ColorMapCapability.class)) {
                                        cap = pair.getResource().getCapability(
                                                ColorMapCapability.class);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if (cap == null && hasBlended) {
                // none of the blended resources were usable!
                for (IDisplayPane pane : panes) {
                    if (pane.isVisible()) {
                        for (ResourcePair rp : pane.getDescriptor()
                                .getResourceList()) {
                            if (rp.getResource() != null
                                    && rp.getResource().hasCapability(
                                            ColorMapCapability.class)) {
                                cap = rp.getResource().getCapability(
                                        ColorMapCapability.class);
                                break;
                            }
                        }
                    }
                }
            }

            return cap;
        }
    }

    public void update(IDisplayPaneContainer container,
            AbstractVizResource<?, ?> singleRscToEdit, boolean rightImages) {
        // Don't update qwhen disposed.
        if (this.isDisposed()) {
            return;
        }
        this.container = container;
        this.singleResourceToEdit = singleRscToEdit;
        this.rightImages = rightImages;
        ColorMapCapability cap = getCapabilityToEdit();
        if (cap == null) {
            currentColormapName = null;
        } else if (cap != this.cap) {
            currentColormapName = getCurrentColormapName(cap);
        }

        this.cap = cap;

        boolean enabled = true;
        if (cap != null) {
            this.container = container;
            this.rightImages = rightImages;

            if (cap != null && cap.getColorMapParameters() != null
                    && cap.getColorMapParameters().getColorMap() != null) {
                colorEditComp.getColorBar().updateColorMap(
                        cap.getColorMapParameters());
            }
            setText(currentColormapName != null ? currentColormapName
                    : "Untitled Colormap");
        } else {
            // disable everything
            enabled = false;

            setText(NO_COLOR_TABLE);
        }

        colorEditComp.getUpperColorWheel().setEnabled(enabled);
        colorEditComp.getLowerColorWheel().setEnabled(enabled);
        colorEditComp.getColorBar().setEnabled(enabled);
        colorEditComp.getRgbRdo().setEnabled(enabled);
        colorEditComp.getHsbRdo().setEnabled(enabled);
        colorEditComp.getColorBar().setEnabled(enabled);
        interpolateBtn.setEnabled(enabled);
        undoBtn.setEnabled(enabled);
        redoBtn.setEnabled(enabled);
        revertBtn.setEnabled(enabled);
        saveBtn.setEnabled(enabled);
        saveAsBtn.setEnabled(enabled);
        officeSaveAsBtn.setEnabled(enabled);
        deleteBtn.setEnabled(enabled);

        if (enabled) {
            undoBtn.setEnabled(colorEditComp.getColorBar().canUndo());
            redoBtn.setEnabled(colorEditComp.getColorBar().canRedo());
            saveBtn.setEnabled(currentColormapName != null);
        }
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setText(cap != null ? (currentColormapName != null ? currentColormapName
                : "Untitled Colormap")
                : NO_COLOR_TABLE);
        colorEditComp = new ColorEditComposite(shell, SWT.FILL, this);
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        colorEditComp.setLayout(mainLayout);
        colorEditComp
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        createBottomButtons(shell);
    }

    /**
     * Create the bottom control buttons.
     */
    private void createBottomButtons(Composite parent) {
        // Create a composite that will contain the control buttons.
        Composite bottonBtnComposite = new Composite(parent, SWT.NONE);
        GridLayout gl = new GridLayout(4, true);
        gl.horizontalSpacing = 10;
        bottonBtnComposite.setLayout(gl);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        bottonBtnComposite.setLayoutData(gd);

        // Create the Interpolate button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        interpolateBtn = new Button(bottonBtnComposite, SWT.PUSH);
        interpolateBtn.setText("Interpolate");
        interpolateBtn.setLayoutData(gd);
        interpolateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ColorData upperColorData = colorEditComp.getUpperColorWheel()
                        .getColorData();
                ColorData lowerColorData = colorEditComp.getLowerColorWheel()
                        .getColorData();

                colorEditComp.getColorBar().interpolate(upperColorData,
                        lowerColorData,
                        colorEditComp.getRgbRdo().getSelection());
                undoBtn.setEnabled(true);
                colorEditComp.updateColorMap();
            }
        });

        // Create the Undo button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        undoBtn = new Button(bottonBtnComposite, SWT.PUSH);
        undoBtn.setText("Undo");
        undoBtn.setEnabled(false);
        undoBtn.setLayoutData(gd);
        undoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                undoBtn.setEnabled(colorEditComp.getColorBar().undoColorBar());
                colorEditComp.updateColorMap();
                redoBtn.setEnabled(true);
            }
        });

        // Create the Redo button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        redoBtn = new Button(bottonBtnComposite, SWT.PUSH);
        redoBtn.setText("Redo");
        redoBtn.setEnabled(false);
        redoBtn.setLayoutData(gd);
        redoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                redoBtn.setEnabled(colorEditComp.getColorBar().redoColorBar());
                colorEditComp.updateColorMap();
                undoBtn.setEnabled(true);
            }
        });

        // Create the Revert button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        revertBtn = new Button(bottonBtnComposite, SWT.PUSH);
        revertBtn.setText("Revert");
        revertBtn.setLayoutData(gd);
        revertBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                revert();
            }
        });

        // Create the Save button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        saveBtn = new Button(bottonBtnComposite, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);

        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                save();
            }

        });

        // Create the Save As button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        saveAsBtn = new Button(bottonBtnComposite, SWT.PUSH);
        saveAsBtn.setText("Save As...");
        saveAsBtn.setLayoutData(gd);
        saveAsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveAs();
            }
        });

        // Create the Office Save As button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        officeSaveAsBtn = new Button(bottonBtnComposite, SWT.PUSH);
        officeSaveAsBtn.setText("Office Save As...");
        officeSaveAsBtn.setLayoutData(gd);
        officeSaveAsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                officeSaveAs();
            }
        });

        // Create the Delete button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        deleteBtn = new Button(bottonBtnComposite, SWT.PUSH);
        deleteBtn.setText("Delete...");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteSelected();
            }
        });

        if (currentColormapName == null) {
            saveBtn.setEnabled(false);
        }
    }

    private void applyToAll(ColorMapCapability applyFrom) {
        if (singleResourceToEdit == null) {
            IDisplayPane[] panes = container.getDisplayPanes();
            if (container instanceof IMultiPaneEditor) {
                IMultiPaneEditor mpe = (IMultiPaneEditor) container;
                if (mpe.getSelectedPane(IMultiPaneEditor.VISIBLE_PANE) != null) {
                    panes = new IDisplayPane[] { mpe
                            .getSelectedPane(IMultiPaneEditor.VISIBLE_PANE) };
                } else if (mpe.getSelectedPane(IMultiPaneEditor.IMAGE_ACTION) != null) {
                    panes = new IDisplayPane[] { mpe
                            .getSelectedPane(IMultiPaneEditor.IMAGE_ACTION) };
                }
            }

            for (IDisplayPane pane : panes) {
                for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                    if (rp.getResource() != null) {
                        AbstractVizResource<?, ?> rsc = rp.getResource();
                        if (rsc.hasCapability(BlendableCapability.class)) {
                            ResourceList blendedList = rsc.getCapability(
                                    BlendableCapability.class)
                                    .getResourceList();
                            if (rightImages && blendedList.size() > 1) {
                                AbstractVizResource<?, ?> rightRsc = blendedList
                                        .get(1).getResource();
                                if (rightRsc != null
                                        && rightRsc
                                                .hasCapability(ColorMapCapability.class)) {
                                    applyToCapability(
                                            rightRsc.getCapability(
                                                    ColorMapCapability.class)
                                                    .getColorMapParameters(),
                                            applyFrom.getColorMapParameters());
                                }
                            } else if (!rightImages && blendedList.size() > 0) {
                                AbstractVizResource<?, ?> leftRsc = blendedList
                                        .get(0).getResource();
                                if (leftRsc != null
                                        && leftRsc
                                                .hasCapability(ColorMapCapability.class)) {
                                    applyToCapability(
                                            leftRsc.getCapability(
                                                    ColorMapCapability.class)
                                                    .getColorMapParameters(),
                                            applyFrom.getColorMapParameters());
                                }
                            }
                        } else if (!rightImages
                                && rsc.hasCapability(ColorMapCapability.class)) {
                            applyToCapability(
                                    rsc.getCapability(ColorMapCapability.class)
                                            .getColorMapParameters(),
                                    applyFrom.getColorMapParameters());
                        }
                    }
                }
            }
            container.refresh();
        } else {
            applyToCapability(cap.getColorMapParameters(),
                    applyFrom.getColorMapParameters());
            singleResourceToEdit.issueRefresh();
        }
    }

    private void applyToCapability(ColorMapParameters applyTo,
            ColorMapParameters applyFrom) {
        applyTo.setColorMap(applyFrom.getColorMap().clone());
        applyTo.setDirty(true);
    }

    @Override
    protected void disposed() {
        VizWorkbenchManager.getInstance().removeListener(this);
        if (container != null) {
            removeListeners(container);
        }
    }

    private void updateOnUIThread() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                update(container, singleResourceToEdit, rightImages);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.viz.core.IVizEditorChangedListener#editorChanged(com.
     * raytheon.uf.viz.core.IDisplayPaneContainer)
     */
    @Override
    public void editorChanged(IDisplayPaneContainer container) {
        if (this.container != container) {
            removeListeners(this.container);
            this.container = container;
            if (this.container != null) {
                addListeners(this.container);
                this.singleResourceToEdit = null;
                updateOnUIThread();
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @seecom.raytheon.uf.viz.core.IRenderableDisplayChangedListener#
     * renderableDisplayChanged(com.raytheon.uf.viz.core.IDisplayPane,
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
     * com.raytheon.uf.viz
     * .core.IRenderableDisplayChangedListener.DisplayChangeType)
     */
    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
        this.singleResourceToEdit = null;
        if (DisplayChangeType.ADD == type) {
            addListeners(newRenderableDisplay);
        } else if (DisplayChangeType.REMOVE == type) {
            removeListeners(newRenderableDisplay);
        }
        updateOnUIThread();
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener#notifyRemove
     * (com.raytheon.uf.viz.core.drawables.ResourcePair)
     */
    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        if (rp.getResource() != null
                && rp.getResource().getResourceData() != null) {
            removeListeners(rp.getResource());
        }

        if (rp.getResource() == singleResourceToEdit) {
            singleResourceToEdit = null;
        }
        updateOnUIThread();
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.viz.core.rsc.ResourceList.AddListener#notifyAdd(com.raytheon
     * .uf.viz.core.drawables.ResourcePair)
     */
    @Override
    public void notifyAdd(ResourcePair rp) throws VizException {
        if (rp.getResource() != null
                && rp.getResource().getResourceData() != null) {
            addListeners(rp.getResource());
        }

        updateOnUIThread();
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (ChangeType.CAPABILITY == type
                && object instanceof ColorMapCapability) {
            updateOnUIThread();
        }
    }

    /**
     * @param container
     */
    private void removeListeners(IDisplayPaneContainer container) {
        container.removeRenderableDisplayChangedListener(this);
        if (container instanceof IMultiPaneEditor) {
            ((IMultiPaneEditor) container)
                    .removeSelectedPaneChangedListener(this);
        }
        for (IDisplayPane pane : container.getDisplayPanes()) {
            removeListeners(pane.getRenderableDisplay());
        }
    }

    /**
     *
     * @param display
     */
    private void removeListeners(IRenderableDisplay display) {
        display.getDescriptor().getResourceList().removePreRemoveListener(this);
        display.getDescriptor().getResourceList().removePostAddListener(this);
        removeListeners(display.getDescriptor().getResourceList());
    }

    /**
     *
     * @param rl
     */
    private void removeListeners(ResourceList rl) {
        for (ResourcePair rp : rl) {
            if (rp.getResource() != null
                    && rp.getResource().getResourceData() != null) {
                removeListeners(rp.getResource());
            }
        }
    }

    /**
     * @param resource
     */
    private void removeListeners(AbstractVizResource<?, ?> resource) {
        resource.getResourceData().removeChangeListener(this);
        if (resource instanceof IResourceGroup) {
            removeListeners(((IResourceGroup) resource).getResourceList());
        }
    }

    /**
     * @param container
     */
    private void addListeners(IDisplayPaneContainer container) {
        container.addRenderableDisplayChangedListener(this);
        if (container instanceof IMultiPaneEditor) {
            ((IMultiPaneEditor) container).addSelectedPaneChangedListener(this);
        }
        for (IDisplayPane pane : container.getDisplayPanes()) {
            addListeners(pane.getRenderableDisplay());
        }
    }

    /**
     *
     * @param display
     */
    private void addListeners(IRenderableDisplay display) {
        display.getDescriptor().getResourceList().addPreRemoveListener(this);
        display.getDescriptor().getResourceList().addPostAddListener(this);
        addListeners(display.getDescriptor().getResourceList());
    }

    /**
     *
     * @param rl
     */
    private void addListeners(ResourceList rl) {
        for (ResourcePair rp : rl) {
            if (rp.getResource() != null
                    && rp.getResource().getResourceData() != null) {
                addListeners(rp.getResource());
            }
        }
    }

    /**
     * @param resource
     */
    private void addListeners(AbstractVizResource<?, ?> resource) {
        resource.getResourceData().addChangeListener(this);
        if (resource instanceof IResourceGroup) {
            addListeners(((IResourceGroup) resource).getResourceList());
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.ui.editor.ISelectedPaneChangedListener#selectedPaneChanged
     * (java.lang.String, com.raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public void selectedPanesChanged(String id, IDisplayPane[] pane) {
        update(container, singleResourceToEdit, rightImages);
    }

    /**
     * Resets the colorbar to an initial state with the newly saved data
     */
    private void completeSave() {
        colorEditComp.getColorBar().updateRevertToCurrent();
        colorEditComp.getColorBar().revertColorBar();
        undoBtn.setEnabled(false);
        redoBtn.setEnabled(false);
        cap.getColorMapParameters().setColorMapName(currentColormapName);
        saveBtn.setEnabled(true);
        cap.getColorMapParameters().setDirty(false);

        applyToAll(cap);
    }

    private void deleteSelected() {
        int index = currentColormapName.lastIndexOf(File.separator);
        String shortName = currentColormapName.substring(index + 1,
                currentColormapName.length());
        String message = "Delete color table: " + shortName + ". Are you sure?";
        boolean okToDelete = MessageDialog.openConfirm(shell,
                "Confirm Delete Color Table", message);
        if (okToDelete) {
            try {
                ColorUtil.deleteColorMap(shortName);
            } catch (LocalizationOpFailedException e) {
                String err = "Error performing delete of colormap";
                statusHandler.handle(Priority.PROBLEM, err, e);
            }

        }
    }

    private ColorMap getColorMap() {
        ColorMap cm = (ColorMap) cap.getColorMapParameters().getColorMap();
        if (colorEditComp.isGFEDiscrete()) {
            cm.removeDuplicates();
        }
        return cm;
    }

    private void officeSaveAs() {
        if (mustCreate(officeSaveAsDialog)) {
            officeSaveAsDialog = new SaveColorMapDialog(shell,
                    getColorMap(), true,
                    currentColormapName);
            officeSaveAsDialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        currentColormapName = (String) returnValue;
                        completeSave();
                    }
                }
            });
            officeSaveAsDialog.open();
        } else {
            officeSaveAsDialog.bringToTop();
        }
    }

    private void saveAs() {
        if (mustCreate(saveAsDialog)) {
            saveAsDialog = new SaveColorMapDialog(shell,
                    getColorMap(), false,
                    currentColormapName);
            saveAsDialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        currentColormapName = (String) returnValue;
                        completeSave();
                    }
                }
            });
            saveAsDialog.open();
        } else {
            saveAsDialog.bringToTop();
        }
    }

    private void save() {
        ColorMap cm = getColorMap();
        try {
            ColorUtil.saveColorMapLocal(cm, currentColormapName, false);
        } catch (VizException e1) {
            String err = "Error saving locally";
            Status status = new Status(Status.ERROR, UiPlugin.PLUGIN_ID, 0,
                    err, e1);
            ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                    "ERROR", err, status);
        }

        // now save it to EDEX
        try {
            ColorUtil.saveColorMapServer(cm, currentColormapName, false);
        } catch (LocalizationException e1) {
            String err = "Error saving to server";
            Status status = new Status(Status.ERROR, UiPlugin.PLUGIN_ID, 0,
                    err, e1);
            ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                    "ERROR", err, status);
        }
        completeSave();
    }

    private void revert() {
        colorEditComp.getColorBar().revertColorBar();
        colorEditComp.updateColorMap();
        undoBtn.setEnabled(false);
        redoBtn.setEnabled(false);
        cap.getColorMapParameters().setDirty(false);
        applyToAll(cap);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.ui.dialogs.colordialog.IColorBarAction#updateColor(com
     * .raytheon.viz.ui.dialogs.colordialog.ColorData, boolean)
     */
    @Override
    public void updateColor(ColorData colorData, boolean upperFlag) {

    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.ui.dialogs.colordialog.IColorWheelAction#setColor(com
     * .raytheon.viz.ui.dialogs.colordialog.ColorData, java.lang.String)
     */
    @Override
    public void setColor(ColorData colorData, String colorWheelTitle) {
        undoBtn.setEnabled(true);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.ui.dialogs.colordialog.IColorWheelAction#fillColor(com
     * .raytheon.viz.ui.dialogs.colordialog.ColorData)
     */
    @Override
    public void fillColor(ColorData colorData) {
        undoBtn.setEnabled(true);
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.viz.ui.dialogs.colordialog.IColorEditCompCallback#
     * colorMapUpdated(com.raytheon.uf.common.colormap.ColorMap)
     */
    @Override
    public void updateColorMap(ColorMap newColorMap) {
        // this is only called when the color map is edited, add asterisk to
        // current color map name to indicate that it wasn't saved
        // -------------------------
        // This can break caching of colormaps in GlTarget
        // to check load visible satellite, edit colormap, close dialog without
        // saving
        // if edited colormap is still used ( it is at the time of writing )
        // save the editor display
        // restart cave, load visible sat ( cmap is the correct default ),
        // clear, load editor display with edited colormap saved in it, notice
        // edited colormap is used, clear and reload vis sat from menu, edited
        // colormap is used because the name matches the default and it is
        // chached in GLTarget
        // -------------------------
        // newColorMap
        // .setName(cap.getColorMapParameters().getColorMap().getName());

        // set colormap to new colormap passed in
        cap.getColorMapParameters().setColorMap(newColorMap);
        // set dirty even though we now have a name ( we haven't saved yet )
        cap.getColorMapParameters().setDirty(true);
        applyToAll(cap);

        // refresh container if it is not null
        if (container != null) {
            container.refresh();
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.viz.ui.dialogs.colordialog.IColorEditCompCallback#
     * getColorMapParameters()
     */
    @Override
    public ColorMapParameters getColorMapParameters() {
        return cap.getColorMapParameters();
    }
}
