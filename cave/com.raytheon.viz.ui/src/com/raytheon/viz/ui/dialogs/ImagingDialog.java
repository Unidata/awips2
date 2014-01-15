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

package com.raytheon.viz.ui.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IVizEditorChangedListener;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.core.imagery.ImageCombiner;
import com.raytheon.viz.core.imagery.ImageCombiner.IImageCombinerListener;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.dialogs.ColormapComp.IColormapCompChangeListener;
import com.raytheon.viz.ui.dialogs.colordialog.ColorEditDialog;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Imaging Dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Nov 26, 2006           chammack    Initial Creation.
 * Jul 10, 2008  980      lvenable    Changed to a SWT dialog to correct a
 *                                    problem caused by a JFace/shell issue.  I
 *                                    also rearranged the controls a bit, used
 *                                    labels instead of text controls since the
 *                                    text is not being edited, changed sliders
 *                                    to scales, added tooltiptext to buttons to
 *                                    show full colormap names, and added
 *                                    comments &amp; Javadoc.
 * Aug 20, 2008	          dglazesk    Updated for the new ColorMap interface
 * Feb 10, 2011  7842     bkowal      set caveStyle for dialog to INDEPENDENT
 *                                    SHELL
 * Oct 17, 2012  1229     rferrel     Make dialog non-blocking.
 * Jan 15, 2015  2313     bsteffen    Disable color map selection when
 *                                    ColorMapCapability is not present.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ImagingDialog extends CaveSWTDialog implements
        IVizEditorChangedListener, IRenderableDisplayChangedListener,
        AddListener, RemoveListener, IResourceDataChanged {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ImagingDialog.class);

    private static enum Type {
        TOP, BOTTOM, ALL;
    }

    /**
     * Brightness value text
     */
    private Label brightnessText;

    /**
     * Contrast value text
     */
    private Label contrastText;

    /**
     * Blend Alpha scale control.
     */
    private Scale blendAlphaScale;

    /**
     * Brightness scale control.
     */
    private Scale brightnessScale;

    /**
     * Contrast scale control.
     */
    private Scale contrastScale;

    /**
     * Alpha scale control.
     */
    private Scale alphaScale;

    /**
     * Alpha label text
     */
    private Label alphaLabel;

    /**
     * Alpha value text
     */
    private Label alphaText;

    /**
     * Interpolation check box control.
     */
    private Button interpolationChk;

    /**
     * Interpolation check box control.
     */
    private Button combineNextImage;

    private final String UNSAVED_CMAP_DISPLAY_NAME = "Untitled Colormap";

    private boolean fromControl = false;

    private IImageCombinerListener combineNextImageListener = new IImageCombinerListener() {
        @Override
        public void preferenceChanged(boolean newPref) {
            combineNextImage.setSelection(newPref);
        }
    };

    /**
     * Top color map button.
     */
    private Button topColorMapButton;

    /**
     * Bottom color map button.
     */
    private Button bottomColorMapButton;

    /**
     * Blended flag.
     */
    private boolean blended = false;

    private ColormapComp topColormapComp;

    private ColormapComp bottomColormapComp;

    private IDisplayPaneContainer currentEditor = null;

    private AbstractVizResource<?, ?> rscToEdit = null;

    private AbstractVizResource<?, ?> firstResource = null;

    private AbstractVizResource<?, ?> topResource = null;

    private AbstractVizResource<?, ?> bottomResource = null;

    /**
     * Constructor
     * 
     * @param parentShell
     * @param dialogTitle
     */
    public ImagingDialog(Shell parentShell, IDisplayPaneContainer initialEditor) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN, CAVE.INDEPENDENT_SHELL
                | CAVE.DO_NOT_BLOCK);
        setText("Imaging...");
        this.currentEditor = initialEditor;

        // Setup listeners, this is dynamic mode
        setupListeners(null, currentEditor);
        // Editor switching (who is the active editor!?)
        VizWorkbenchManager.getInstance().addListener(this);
    }

    public ImagingDialog(Shell parentShell, AbstractVizResource<?, ?> rscToEdit) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN, CAVE.INDEPENDENT_SHELL
                | CAVE.DO_NOT_BLOCK);
        setText("Imaging...");
        this.rscToEdit = rscToEdit;
    }

    @Override
    protected void disposed() {
        ImageCombiner.removeListener(combineNextImageListener);
        setupListeners(currentEditor, null);
        VizWorkbenchManager.getInstance().removeListener(this);
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(1, false));
        mainComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        initializeTopControls(mainComp);
        initializeBlendedAlphaScale(mainComp);
        initializeBottomControls(mainComp);
        initializeABCControls(mainComp);

        refreshComponents();
    }

    private void initializeTopControls(Composite mainComp) {
        GridData buttonGD = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonGD.widthHint = 250;
        topColorMapButton = new Button(mainComp, SWT.PUSH);
        topColorMapButton.setAlignment(SWT.CENTER);
        topColorMapButton.setLayoutData(buttonGD);

        topColorMapButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (topResource != null) {
                    ColorEditDialog.openDialog(new Shell(getDisplay()),
                            currentEditor, rscToEdit, false, true);
                    // refresh to update color map name ( in case it was edited
                    // but not saved )
                    refreshComponents();
                }
            }
        });

        topColormapComp = new ColormapComp(mainComp, null, null);
        topColormapComp.addChangeListener(new IColormapCompChangeListener() {

            @Override
            public void colormapChanged(String colorMap) {
                try {
                    IColorMap cxml = ColorMapLoader.loadColorMap(colorMap);
                    ColorMap glColorMap = new ColorMap(colorMap,
                            (ColorMap) cxml);

                    for (AbstractVizResource<?, ?> rsc : getResourcesToEdit()) {
                        if (rsc.hasCapability(BlendableCapability.class)) {
                            ResourceList subList = rsc.getCapability(
                                    BlendableCapability.class)
                                    .getResourceList();
                            if (subList.size() > 0) {
                                rsc = subList.get(0).getResource();
                            } else {
                                rsc = null;
                            }
                        }
                        if (rsc != null) {
                            rsc.getCapability(ColorMapCapability.class)
                                    .getColorMapParameters()
                                    .setColorMap(glColorMap);
                        }
                    }
                    refreshEditor();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error changing colormap", e);
                }
            }
        });
    }

    private void initializeBlendedAlphaScale(Composite parent) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 200;
        blendAlphaScale = new Scale(parent, SWT.NONE);
        blendAlphaScale.setLayoutData(gd);

        blendAlphaScale.setMinimum(0);
        blendAlphaScale.setMaximum(BlendableCapability.BLEND_MAX);
        blendAlphaScale.setIncrement(1);
        blendAlphaScale.setPageIncrement(1);

        blendAlphaScale.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org
             * .eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                for (AbstractVizResource<?, ?> rsc : getResourcesToEdit()) {
                    if (rsc.hasCapability(BlendableCapability.class)) {
                        fromControl = true;
                        rsc.getCapability(BlendableCapability.class)
                                .setAlphaStep(blendAlphaScale.getSelection());
                        fromControl = false;
                    }
                }
                refreshEditor();
            }

        });
    }

    /**
     * @param mainComp
     */
    private void initializeABCControls(Composite mainComp) {
        Composite body = new Composite(mainComp, SWT.NONE);
        body.setLayout(new GridLayout(3, false));
        Label label = new Label(body, SWT.BOLD);
        label.setText("Brightness: ");

        brightnessScale = new Scale(body, SWT.NONE);
        brightnessScale.setLayoutData(new GridData(200, SWT.DEFAULT));

        brightnessText = new Label(body, SWT.NONE);
        brightnessText.setLayoutData(new GridData(50, SWT.DEFAULT));

        brightnessScale.setMinimum(0);
        brightnessScale.setMaximum(100);
        brightnessScale.setIncrement(1);
        brightnessScale.setPageIncrement(5);

        Label label2 = new Label(body, SWT.BOLD);
        label2.setText("Contrast: ");

        contrastScale = new Scale(body, SWT.NONE);
        contrastScale.setLayoutData(new GridData(200, SWT.DEFAULT));

        contrastText = new Label(body, SWT.NONE);
        contrastText.setLayoutData(new GridData(50, SWT.DEFAULT));

        contrastScale.setMinimum(0);
        contrastScale.setMaximum(100);
        contrastScale.setIncrement(1);
        contrastScale.setPageIncrement(5);

        initializeAlphaScale(body);

        Composite checkBoxComp = new Composite(body, SWT.NONE);
        checkBoxComp.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, true,
                true, 3, 1));
        checkBoxComp.setLayout(new GridLayout(2, false));

        interpolationChk = new Button(checkBoxComp, SWT.CHECK);
        interpolationChk.setText("Interpolate");
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        interpolationChk.setLayoutData(gd);

        combineNextImage = new Button(checkBoxComp, SWT.CHECK);
        combineNextImage.setText("Combine Next Image Load");
        combineNextImage.setLayoutData(gd);
        combineNextImage.setSelection(ImageCombiner.isCombineImages());
        combineNextImage.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                // Lets call our command
                IHandlerService handlerService = (IHandlerService) PlatformUI
                        .getWorkbench().getActiveWorkbenchWindow()
                        .getService(IHandlerService.class);
                try {
                    handlerService
                            .executeCommand(
                                    "com.raytheon.uf.viz.d2d.ui.actions.imageCombination",
                                    null);
                } catch (Exception ex) {
                    // Eat exception
                }
                combineNextImage.setSelection(ImageCombiner.isCombineImages());
                refreshEditor();
            }
        });

        ImageCombiner.addListener(combineNextImageListener);

        brightnessScale.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org
             * .eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                for (AbstractVizResource<?, ?> rsc : getResourcesToEdit()) {
                    ImagingCapability imgCap = rsc
                            .getCapability(ImagingCapability.class);
                    fromControl = true;
                    imgCap.setBrightness(brightnessScale.getSelection() / 100.0f);
                    fromControl = false;
                }
                brightnessText.setText(brightnessScale.getSelection() + "%");
                refreshEditor();
            }

        });

        contrastScale.addSelectionListener(new SelectionAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org
             * .eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                for (AbstractVizResource<?, ?> rsc : getResourcesToEdit()) {
                    ImagingCapability imgCap = rsc
                            .getCapability(ImagingCapability.class);
                    fromControl = true;
                    imgCap.setContrast(contrastScale.getSelection() / 100.0f);
                    fromControl = false;
                }
                contrastText.setText(contrastScale.getSelection() + "%");
                refreshEditor();
            }

        });

        interpolationChk.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (AbstractVizResource<?, ?> rsc : getResourcesToEdit()) {
                    ImagingCapability imgCap = rsc
                            .getCapability(ImagingCapability.class);
                    imgCap.setInterpolationState(interpolationChk
                            .getSelection());
                }
                refreshEditor();
            }

        });
    }

    private void initializeBottomControls(Composite parent) {
        bottomColormapComp = new ColormapComp(parent, null, null);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 250;
        bottomColorMapButton = new Button(parent, SWT.PUSH | SWT.CENTER);
        bottomColorMapButton.setLayoutData(gd);

        bottomColorMapButton.setAlignment(SWT.CENTER);

        bottomColormapComp.addChangeListener(new IColormapCompChangeListener() {

            @Override
            public void colormapChanged(String colorMap) {
                try {
                    IColorMap cxml = ColorMapLoader.loadColorMap(colorMap);
                    ColorMap glColorMap = new ColorMap(colorMap,
                            (ColorMap) cxml);

                    for (AbstractVizResource<?, ?> rsc : getResourcesToEdit(Type.BOTTOM)) {
                        if (rsc.hasCapability(BlendableCapability.class)) {
                            ResourceList subList = rsc.getCapability(
                                    BlendableCapability.class)
                                    .getResourceList();
                            if (subList.size() > 1) {
                                rsc = subList.get(1).getResource();
                            } else {
                                rsc = null;
                            }
                        }
                        if (rsc != null) {
                            rsc.getCapability(ColorMapCapability.class)
                                    .getColorMapParameters()
                                    .setColorMap(glColorMap);
                        }
                    }
                    refreshEditor();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error applying colormap", e);
                }
            }

        });

        bottomColorMapButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                ColorEditDialog.openDialog(new Shell(getDisplay()),
                        currentEditor, null, true, true);
            }

        });
    }

    private void initializeAlphaScale(Composite parent) {
        alphaLabel = new Label(parent, SWT.BOLD);
        alphaLabel.setText("Alpha: ");
        alphaLabel.setLayoutData(new GridData());

        alphaScale = new Scale(parent, SWT.NONE);
        alphaScale.setLayoutData(new GridData(200, SWT.DEFAULT));

        alphaText = new Label(parent, SWT.NONE);
        alphaText.setLayoutData(new GridData(50, SWT.DEFAULT));

        alphaScale.setMinimum(0);
        alphaScale.setMaximum(100);
        alphaScale.setIncrement(1);
        alphaScale.setPageIncrement(5);

        alphaScale.addSelectionListener(new SelectionAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org
             * .eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                for (AbstractVizResource<?, ?> rsc : getResourcesToEdit()) {
                    ImagingCapability imgCap = rsc
                            .getCapability(ImagingCapability.class);
                    fromControl = true;
                    imgCap.setAlpha(alphaScale.getSelection() / 100.0f);
                    fromControl = false;
                }
                alphaText.setText(alphaScale.getSelection() + "%");
                refreshEditor();
            }
        });
    }

    /**
     * Get the top most resource to use as the basis for the dialog
     * 
     * @return
     */
    private AbstractVizResource<?, ?> getTopResource() {
        AbstractVizResource<?, ?> rsc = null;
        List<AbstractVizResource<?, ?>> editables = getResourcesToEdit();

        // Put precedence on blended resources
        for (AbstractVizResource<?, ?> rsc2 : editables) {
            if (rsc2.hasCapability(BlendableCapability.class)) {
                if (rsc2.getCapability(BlendableCapability.class)
                        .getResourceList().size() > 1) {
                    rsc = rsc2;
                }
            }
        }
        if (rsc == null && editables.size() > 0) {
            rsc = editables.get(0);
        }
        return rsc;
    }

    private List<AbstractVizResource<?, ?>> getResourcesToEdit() {
        return getResourcesToEdit(Type.ALL);
    }

    /**
     * Get a list of the resources to update when changes are made
     * 
     * @return
     */
    private List<AbstractVizResource<?, ?>> getResourcesToEdit(Type type) {
        List<AbstractVizResource<?, ?>> rscsToEdit = new ArrayList<AbstractVizResource<?, ?>>();

        if (currentEditor != null) {
            for (IDisplayPane pane : getDisplayPanesToEdit()) {
                for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                    AbstractVizResource<?, ?> rsc = rp.getResource();
                    if (rsc != null
                            && rsc.hasCapability(ImagingCapability.class)
                            && (rsc.hasCapability(ColorMapCapability.class) || rsc
                                    .hasCapability(BlendableCapability.class))) {
                        switch (type) {
                        case TOP:
                        case ALL: {
                            rscsToEdit.add(rsc);
                            break;
                        }
                        case BOTTOM: {
                            if (rsc.hasCapability(BlendableCapability.class)
                                    && rsc.getCapability(
                                            BlendableCapability.class)
                                            .getResourceList().size() > 1) {
                                rscsToEdit.add(rsc);
                            }
                            break;
                        }
                        }

                    }
                }
            }
        } else if (rscToEdit != null) {
            rscsToEdit.add(rscToEdit);
        }
        return rscsToEdit;
    }

    /**
     * Get the visible display panes to edit
     * 
     * @return
     */
    private IDisplayPane[] getDisplayPanesToEdit() {
        List<IDisplayPane> panesToEdit = new ArrayList<IDisplayPane>();
        if (currentEditor != null) {
            IDisplayPane[] panes = currentEditor.getDisplayPanes();
            if (currentEditor instanceof IMultiPaneEditor) {
                IMultiPaneEditor multiEditor = (IMultiPaneEditor) currentEditor;
                IDisplayPane pane = multiEditor
                        .getSelectedPane(IMultiPaneEditor.IMAGE_ACTION);
                if (pane != null) {
                    panes = new IDisplayPane[] { pane };
                }
            }

            for (IDisplayPane pane : panes) {
                if (pane.isVisible()) {
                    panesToEdit.add(pane);
                }
            }
        }
        return panesToEdit.toArray(new IDisplayPane[panesToEdit.size()]);
    }

    /**
     * Force a repaint on the current editor
     */
    private void refreshEditor() {
        if (currentEditor != null) {
            currentEditor.refresh();
        } else if (rscToEdit != null) {
            rscToEdit.issueRefresh();
        }
    }

    /**
     * Refresh the components using the latest data
     */
    public void refreshComponents() {
        firstResource = null;
        topResource = null;
        bottomResource = null;

        blended = false;

        // setup the resources
        List<AbstractVizResource<?, ?>> editables = getResourcesToEdit();
        firstResource = getTopResource();
        if (firstResource != null) {
            if (firstResource.hasCapability(BlendableCapability.class)) {
                blended = true;
                ResourceList list = firstResource.getCapability(
                        BlendableCapability.class).getResourceList();
                if (list.size() > 0) {
                    topResource = list.get(0).getResource();
                }
                if (list.size() > 1) {
                    bottomResource = list.get(1).getResource();
                }
                if (bottomResource == null) {
                    blended = false;
                }
            } else {
                topResource = firstResource;
            }
        }

        // Setup top controls
        if (topResource != null) {
            topColorMapButton.setEnabled(true);
            topColormapComp.getCMapButton().setEnabled(true);
            brightnessScale.setEnabled(true);
            contrastScale.setEnabled(true);
            interpolationChk.setEnabled(true);

            String topResourceName = topResource.getName();
            if (editables.size() > 1) {
                if (blended) {
                    topResourceName = "all left images...";
                } else {
                    topResourceName = "all images...";
                }
            }
            // Truncate the name for the button if it is too long.
            if (topResourceName.length() > 25) {
                topColorMapButton.setToolTipText("Edit " + topResourceName);
                topResourceName = topResourceName.substring(0, 24) + "...";
            } else {
                topColorMapButton.setToolTipText(null);
            }
            if (topResource.hasCapability(ColorMapCapability.class)) {
                final ColorMapCapability cmcap = topResource
                        .getCapability(ColorMapCapability.class);
                String currentCMap = "Not Selected";
                if (cmcap.getColorMapParameters() != null
                        && cmcap.getColorMapParameters().getColorMap() != null) {
                    currentCMap = cmcap.getColorMapParameters().getColorMap()
                            .getName();
                    if (currentCMap == null) {
                        currentCMap = "";
                    }
                }
                topColormapComp.setCap(cmcap);
                topColormapComp.setParams(cmcap.getColorMapParameters());
                if (currentCMap.isEmpty()) {
                    currentCMap = UNSAVED_CMAP_DISPLAY_NAME;
                }
                topColormapComp.getCMapButton().setText(currentCMap);

                topColorMapButton.setText("Edit " + topResourceName);
            } else {
                topColorMapButton.setText(topResourceName
                        + " is not color mapped.");
                topColormapComp.getCMapButton().setText("Not Selected");
                topColorMapButton.setEnabled(false);
                topColormapComp.getCMapButton().setEnabled(false);
                interpolationChk.setEnabled(false);
                interpolationChk.setSelection(false);
            }
            ImagingCapability imgCap = topResource
                    .getCapability(ImagingCapability.class);
            brightnessScale
                    .setSelection((int) (imgCap.getBrightness() * 100.0f));

            brightnessText.setText(brightnessScale.getSelection() + "%");

            contrastScale.setSelection((int) (imgCap.getContrast() * 100.0f));

            contrastText.setText(contrastScale.getSelection() + "%");

            interpolationChk.setSelection(imgCap.isInterpolationState());
        } else {
            topColorMapButton.setText("Top image is not displayed.");
            topColormapComp.getCMapButton().setText("Not Selected");
            topColorMapButton.setEnabled(false);
            topColormapComp.getCMapButton().setEnabled(false);
            brightnessScale.setEnabled(false);
            contrastScale.setEnabled(false);
            interpolationChk.setEnabled(false);
            interpolationChk.setSelection(false);
        }

        // Setup blended alpha
        if (blended) {
            blendAlphaScale.setEnabled(true);
            blendAlphaScale.setVisible(true);
            ((GridData) blendAlphaScale.getLayoutData()).exclude = false;
            blendAlphaScale.setSelection(firstResource.getCapability(
                    BlendableCapability.class).getAlphaStep());
        } else {
            blendAlphaScale.setEnabled(false);
            blendAlphaScale.setVisible(false);
            ((GridData) blendAlphaScale.getLayoutData()).exclude = true;
        }

        // Setup bottom controls
        if (blended) {
            ((GridData) bottomColormapComp.getCMapButton().getLayoutData()).exclude = false;
            ((GridData) bottomColorMapButton.getLayoutData()).exclude = false;
            bottomColorMapButton.setVisible(true);
            bottomColormapComp.getCMapButton().setVisible(true);
            bottomColorMapButton.setEnabled(true);
            bottomColormapComp.getCMapButton().setEnabled(true);

            ColorMapCapability bottomCap = bottomResource
                    .getCapability(ColorMapCapability.class);

            String currentCMap = "Not Selected";
            if (bottomCap.getColorMapParameters() != null
                    && bottomCap.getColorMapParameters().getColorMap() != null) {
                currentCMap = bottomCap.getColorMapParameters().getColorMap()
                        .getName();
                if (currentCMap == null) {
                    currentCMap = "";
                }
            }

            bottomColormapComp.setCap(bottomCap);
            bottomColormapComp.setParams(bottomCap.getColorMapParameters());
            if (currentCMap.isEmpty()) {
                currentCMap = UNSAVED_CMAP_DISPLAY_NAME;
            }
            bottomColormapComp.getCMapButton().setText(currentCMap);
            String bottomResourceName = bottomResource.getName();

            if (editables.size() > 1) {
                bottomResourceName = "all right images...";
            }

            // Truncate the name for the button if it is too long.
            if (bottomResourceName.length() > 25) {
                bottomColorMapButton.setToolTipText("Edit "
                        + bottomResourceName);
                bottomResourceName = bottomResourceName.substring(0, 24)
                        + "...";
            } else {
                bottomColorMapButton.setToolTipText(null);
            }

            bottomColorMapButton.setText("Edit " + bottomResourceName);
        } else {
            // exclude bottom components
            bottomColorMapButton.setText("Bottom image is not displayed.");
            bottomColorMapButton.setEnabled(false);
            bottomColormapComp.getCMapButton().setEnabled(false);
            bottomColorMapButton.setVisible(false);
            bottomColormapComp.getCMapButton().setVisible(false);

            ((GridData) bottomColorMapButton.getLayoutData()).exclude = true;
            ((GridData) bottomColormapComp.getCMapButton().getLayoutData()).exclude = true;
        }

        // setup ABC components
        if (topResource != null) {
            brightnessText.setVisible(true);
            contrastText.setVisible(true);

            ImagingCapability imgCap = topResource
                    .getCapability(ImagingCapability.class);
            brightnessScale
                    .setSelection((int) (imgCap.getBrightness() * 100.0f));

            brightnessText.setText(brightnessScale.getSelection() + "%");

            contrastScale.setSelection((int) (imgCap.getContrast() * 100.0f));
            contrastText.setText(contrastScale.getSelection() + "%");

            if (!blended) {
                alphaScale.setEnabled(true);
                alphaText.setEnabled(true);
                alphaLabel.setEnabled(true);
                alphaScale.setVisible(true);
                alphaText.setVisible(true);
                alphaLabel.setVisible(true);
                ((GridData) alphaScale.getLayoutData()).exclude = false;
                ((GridData) alphaText.getLayoutData()).exclude = false;
                ((GridData) alphaLabel.getLayoutData()).exclude = false;

                alphaScale.setSelection((int) (imgCap.getAlpha() * 100.0f));
                alphaText.setText(alphaScale.getSelection() + "%");
            } else {
                alphaScale.setEnabled(false);
                alphaText.setEnabled(false);
                alphaLabel.setEnabled(false);
                alphaScale.setVisible(false);
                alphaText.setVisible(false);
                alphaLabel.setVisible(false);
                ((GridData) alphaScale.getLayoutData()).exclude = true;
                ((GridData) alphaText.getLayoutData()).exclude = true;
                ((GridData) alphaLabel.getLayoutData()).exclude = true;
            }

            interpolationChk.setSelection(imgCap.isInterpolationState());
        } else {
            brightnessText.setVisible(false);
            contrastText.setVisible(false);
            brightnessScale.setSelection(0);
            contrastScale.setSelection(0);
            brightnessScale.setEnabled(false);
            contrastScale.setEnabled(false);
            if (!blended) {
                alphaText.setVisible(false);
                alphaScale.setVisible(true);
                alphaLabel.setVisible(true);
                alphaScale.setSelection(0);
                alphaScale.setEnabled(false);

                ((GridData) alphaScale.getLayoutData()).exclude = false;
                ((GridData) alphaText.getLayoutData()).exclude = false;
                ((GridData) alphaLabel.getLayoutData()).exclude = false;
            }
        }
        getShell().layout();
        getShell().pack();
    }

    /**
     * Call refresh components on the UI Thread
     */
    private void refreshComponentsUpdate() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                refreshComponents();
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
        IDisplayPaneContainer oldEditor = currentEditor;
        currentEditor = container;
        refreshComponents();
        setupListeners(oldEditor, currentEditor);
    }

    private void setupListeners(IDisplayPaneContainer oldEditor,
            IDisplayPaneContainer currentEditor) {
        // remove old listeners
        if (oldEditor != null) {
            // for each pane, remove as add/remove listener
            for (IDisplayPane pane : oldEditor.getDisplayPanes()) {
                removeListeners(pane.getRenderableDisplay().getDescriptor()
                        .getResourceList());
            }

            // Start with Renderable Display listeners...
            oldEditor.removeRenderableDisplayChangedListener(this);
        }

        // add new listeners
        if (currentEditor != null) {
            // for each pane, register as resource add/remove listener
            for (IDisplayPane pane : currentEditor.getDisplayPanes()) {
                addListeners(pane.getRenderableDisplay().getDescriptor()
                        .getResourceList());
            }

            // Start with Renderable Display listeners...
            currentEditor.addRenderableDisplayChangedListener(this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.IRenderableDisplayChangedListener#
     * renderableDisplayChanged(com.raytheon.uf.viz.core.IDisplayPane,
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay)
     */
    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay display, DisplayChangeType type) {
        if (type == DisplayChangeType.ADD) {
            addListeners(display.getDescriptor().getResourceList());
        } else if (type == DisplayChangeType.REMOVE) {
            removeListeners(display.getDescriptor().getResourceList());
        }
        refreshComponentsUpdate();
    }

    private void addListeners(ResourceList list) {
        for (ResourcePair rp : list) {
            addListeners(rp.getResource());
        }

        list.addPostAddListener(this);
        list.addPreRemoveListener(this);
    }

    private void removeListeners(ResourceList list) {
        for (ResourcePair rp : list) {
            removeListeners(rp.getResource());
        }

        list.removePostAddListener(this);
        list.removePreRemoveListener(this);
    }

    private void addListeners(AbstractVizResource<?, ?> rsc) {
        if (rsc != null && rsc.getResourceData() != null) {
            rsc.getResourceData().addChangeListener(this);

            if (rsc.hasCapability(BlendableCapability.class)) {
                addListeners(rsc.getCapability(BlendableCapability.class)
                        .getResourceList());
            }
        }
    }

    private void removeListeners(AbstractVizResource<?, ?> rsc) {
        if (rsc != null && rsc.getResourceData() != null) {
            rsc.getResourceData().removeChangeListener(this);

            if (rsc.hasCapability(BlendableCapability.class)) {
                removeListeners(rsc.getCapability(BlendableCapability.class)
                        .getResourceList());
            }
        }
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
        addListeners(rp.getResource());
        refreshComponentsUpdate();
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
        removeListeners(rp.getResource());
        refreshComponentsUpdate();
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
    public synchronized void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY
                && (object instanceof ColorMapCapability || object instanceof ImagingCapability)) {
            if (!fromControl) {
                refreshComponentsUpdate();
            }
        }
    }
}
