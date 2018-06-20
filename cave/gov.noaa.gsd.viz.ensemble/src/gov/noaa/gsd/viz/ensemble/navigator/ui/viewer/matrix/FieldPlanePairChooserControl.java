package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import gov.noaa.gsd.viz.ensemble.control.EnsembleResourceManager;
import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.control.IResourceRegisteredListener;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.ContextMenuManager;
import gov.noaa.gsd.viz.ensemble.util.EnsembleToolImageStore;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.RequestableResourceMetadata;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;

/**
 * This chooser control is a Composite and allows the user to choose from those
 * field and plane pairs (elements) that were loaded from a model family.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2015  12302      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */

public class FieldPlanePairChooserControl extends Composite implements
        IFieldPlanePairVisibilityChangedListener {

    protected static int resourceCount = 0;

    private final String spacerPadding = "  ";

    private ScrolledComposite rootScrolledComposite = null;

    private Composite innerComposite = null;

    private IFieldPlanePairVisibilityChangedListener visibilityListener = null;

    private IModelSourceSelectionProvider sourceSelectionProvider = null;

    private IMatrixEditorFocusProvider focusProvider = null;

    /**
     * Create the composite.
     * 
     * @param parent
     *            the composite into which this control sits.
     * @param style
     *            the SWT style.
     * @param etv
     *            the ensemble tool viewer (which is the ensemble tool's top
     *            level GUI component.
     * @param evcl
     *            the field/plane pair visiblity change listener to be notified
     *            when an element's visibility is changed by the user.
     * @param mssp
     *            the provider which notifies this class when a model source is
     *            selected by the user.
     */
    public FieldPlanePairChooserControl(Composite parent, int style,
            IFieldPlanePairVisibilityChangedListener evcl,
            IModelSourceSelectionProvider mssp, IMatrixEditorFocusProvider mefp) {
        super(parent, style);
        visibilityListener = evcl;
        sourceSelectionProvider = mssp;
        focusProvider = mefp;
        createContents();

    }

    @Override
    public void dispose() {
        clearFieldPlanePairControls();
        super.dispose();
    }

    /**
     * Create the contents of the entire control's composite.
     */
    private void createContents() {

        configureRootArea();

        createControlArea();

    }

    /**
     * Configure the root area layout and layout data.
     */
    private void configureRootArea() {
        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
        setLayout(new GridLayout(1, false));
    }

    /**
     * Create the contents of the control area where all field/plane pair
     * widgets are displayed.
     */
    private void createControlArea() {

        rootScrolledComposite = new ScrolledComposite(this, SWT.H_SCROLL
                | SWT.V_SCROLL);
        GridData elementSetComposite_gd = new GridData(SWT.FILL, SWT.FILL,
                true, true, 1, 1);
        rootScrolledComposite.setLayoutData(elementSetComposite_gd);
        GridLayout rootScrolledComposite_gl = new GridLayout(1, true);
        rootScrolledComposite_gl.marginHeight = 0;
        rootScrolledComposite_gl.marginWidth = 0;
        rootScrolledComposite_gl.verticalSpacing = 1;
        rootScrolledComposite.setLayout(rootScrolledComposite_gl);
        rootScrolledComposite.setExpandHorizontal(true);
        rootScrolledComposite.setExpandVertical(true);

        innerComposite = new Composite(rootScrolledComposite, SWT.NONE);
        GridLayout innerComposite_gl = new GridLayout(2, true);
        innerComposite_gl.marginHeight = 0;
        innerComposite_gl.marginWidth = 0;
        innerComposite.setLayout(innerComposite_gl);
        rootScrolledComposite.setContent(innerComposite);

        Composite choosersComposite = new Composite(this, SWT.NONE);
        choosersComposite.setLayout(new GridLayout(1, false));
        GridData gd_choosersComposite = new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1);
        choosersComposite.setLayoutData(gd_choosersComposite);

    }

    /**
     * Clear all field/plane pairs displayed in this control.
     */
    public void clearFieldPlanePairControls() {

        if (innerComposite != null && !innerComposite.isDisposed()) {
            Control[] children = innerComposite.getChildren();
            for (Control c : children) {
                c.dispose();
            }
        }

    }

    /**
     * This method extract the field/plane pairs from the formal argument
     * modelFamily and for every field/plane pair, creates an
     * <code>FieldPlanePair</code> widget.
     * 
     * @param modelFamily
     *            the model family object to be added.
     */
    public void addModelFamily(ModelFamily modelFamily) {

        Control[] children = innerComposite.getChildren();
        for (Control c : children) {
            c.dispose();
        }
        FieldPlanePairSet set = modelFamily.getFieldPlanePairs();
        List<FieldPlanePair> nodes = set.getNodes();
        for (FieldPlanePair e : nodes) {
            new FieldPlanePairControl(innerComposite, e);
        }
        innerComposite.layout();
        rootScrolledComposite.layout();
        pack();
    }

    /**
     * This convenience method returns a list of all elements whose respective
     * resources are currently visible in the active map editor.
     * 
     * @return the list of elements having visibile resources.
     */
    public List<FieldPlanePair> getVisibleFieldPlanePairs() {
        List<FieldPlanePair> elementSet = new ArrayList<FieldPlanePair>();
        if (innerComposite != null) {
            Control[] controls = innerComposite.getChildren();
            for (Control c : controls) {
                if (c instanceof FieldPlanePairControl) {
                    FieldPlanePairControl ecc = (FieldPlanePairControl) c;
                    if (ecc.fieldPlanePair.isVisible()) {

                        elementSet.add(ecc.fieldPlanePair);
                    }
                }
            }
        }
        return elementSet;
    }

    /**
     * Return the controls which match a certain field and plane.
     */
    public List<FieldPlanePairControl> getFieldPlanePairControls(
            String fieldAbbrev, String plane) {

        List<FieldPlanePairControl> controls = new ArrayList<>();
        FieldPlanePairControl fppc = null;
        Control[] children = innerComposite.getChildren();
        for (Control c : children) {
            if (c instanceof FieldPlanePairControl) {
                fppc = (FieldPlanePairControl) c;
                if (fppc.fieldPlanePair.getFieldAbbrev().equals(fieldAbbrev)
                        && fppc.fieldPlanePair.getPlane().equals(plane)) {
                    controls.add(fppc);
                }
            }
        }
        return controls;
    }

    /**
     * Return the all the field/plane pair controls.
     */
    public List<FieldPlanePairControl> getFieldPlanePairControls() {
        List<FieldPlanePairControl> controls = new ArrayList<>();
        FieldPlanePairControl fppc = null;
        Control[] children = innerComposite.getChildren();
        for (Control c : children) {
            if (c instanceof FieldPlanePairControl) {
                fppc = (FieldPlanePairControl) c;
                controls.add(fppc);
            }
        }
        return controls;
    }

    /**
     * 
     */
    public void disableEmptyFieldPlanePairControls() {

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                List<FieldPlanePairControl> fieldPlanePairs = new ArrayList<>();
                Control[] controls = innerComposite.getChildren();
                for (Control c : controls) {
                    if (c instanceof FieldPlanePairControl) {
                        FieldPlanePairControl fppc = (FieldPlanePairControl) c;
                        if (fppc.getAssociatedRscList().isEmpty()) {
                            fieldPlanePairs.add(fppc);
                        }
                    }
                }
                for (FieldPlanePairControl fppc : fieldPlanePairs) {
                    fppc.setEmpty();
                    fppc.setViewEditable(false);
                }
            }
        });
    }

    /**
     * This convenience method returns a list of all elements of those resources
     * currently loaded in the active map editor.
     * 
     * @return the list of elements associated with all loaded resources.
     */
    public List<FieldPlanePair> getFieldPlanePairs() {
        List<FieldPlanePair> fieldPlanePairs = new ArrayList<FieldPlanePair>();
        if (innerComposite != null) {
            Control[] controls = innerComposite.getChildren();
            for (Control c : controls) {
                if (c instanceof FieldPlanePairControl) {
                    FieldPlanePairControl fppc = (FieldPlanePairControl) c;
                    fieldPlanePairs.add(fppc.fieldPlanePair);
                }
            }
        }
        return fieldPlanePairs;
    }

    /**
     * Notify the listener that the visibility on the formal argument
     * field/plane pair has changed.
     */
    @Override
    public void visibilityChanged(FieldPlanePair e) {
        visibilityListener.visibilityChanged(e);
    }

    /**
     * Enable or disable this control (and all of its child controls) based on
     * the formal argument <code>enabled</code>.
     * 
     * @param enabled
     *            a boolean flag defining whether the entire control should be
     *            enabled or disabled.
     */
    public void setViewEditable(final boolean enabled) {

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                if (innerComposite != null) {
                    Control[] controls = innerComposite.getChildren();
                    for (Control c : controls) {
                        if (c instanceof FieldPlanePairControl) {
                            FieldPlanePairControl fppc = (FieldPlanePairControl) c;
                            fppc.setViewEditable(enabled);
                        }
                    }
                }
            }
        });
    }

    public int getNumberOfFieldPlanePairs() {
        int count = 0;
        if (innerComposite != null) {
            Control[] controls = innerComposite.getChildren();
            for (Control c : controls) {
                if (c instanceof FieldPlanePairControl) {
                    count++;
                }
            }
        }
        return count;

    }

    /**
     * This method is called with the field/plane pair for an image resource
     * that was just made visible. Walk all field/plane pairs to find the other
     * image-based field/plane pairs and set their resources to to be hidden.
     */
    protected void toggleOtherImagesOff(FieldPlanePair fpp) {
        if (innerComposite != null) {
            Control[] controls = innerComposite.getChildren();
            for (Control c : controls) {
                if (c instanceof FieldPlanePairControl) {
                    FieldPlanePairControl fppc = (FieldPlanePairControl) c;
                    if (fppc.isEnabled()
                            && fppc.getFieldPlanePair() != fpp
                            && fppc.getFieldPlanePair().getDisplayType() == DisplayType.IMAGE) {
                        fppc.setVisible(false);
                        visibilityChanged(fppc.getFieldPlanePair());
                    }
                }
            }
        }
    }

    /**
     * 
     * This class represents and individual field/plane pair widget control.
     * 
     * The control is composed of a label/image and a button.
     * 
     * The label/image defines what type of graphic display the field/plane pair
     * is (e.g. Wind Barb, Image, Contour, Icon, etc.) Possible display types
     * are defined in the enumeration <code>DisplayType</code>.
     * 
     * The button is a checkbox button for controlling associated resource
     * visibility. When the button is selected, the background of the Button is
     * highlighted in a more vibrant color.
     * 
     */
    public class FieldPlanePairControl extends Composite implements
            IResourceDataChanged, IResourceRegisteredListener {

        private FieldPlanePair fieldPlanePair = null;

        private List<AbstractVizResource<?, ?>> associatedRscList = null;

        private Button fieldPlanePairNameBtn = null;

        private Label displayTypeLbl = null;

        private Color unselectedBkgdColor = null;

        private Color selectedBkgdColor = null;

        private boolean hasNoResources = false;

        public FieldPlanePairControl(Composite parent, FieldPlanePair e) {
            super(parent, SWT.BORDER);
            associatedRscList = new CopyOnWriteArrayList<>();
            fieldPlanePair = e;
            unselectedBkgdColor = GlobalColor.get(GlobalColor.LIGHTISH_GRAY);
            selectedBkgdColor = GlobalColor
                    .get(GlobalColor.LIGHT_CARIBBEAN_GREEN);
            createContents();
            EnsembleResourceManager.getResourceProvider()
                    .addResourceRegisteredListener(this);
        }

        @Override
        public void dispose() {
            fieldPlanePairNameBtn.dispose();
            displayTypeLbl.dispose();
            associatedRscList.clear();

            fieldPlanePairNameBtn = null;
            displayTypeLbl = null;
            associatedRscList = null;

            super.dispose();
        }

        /**
         * NOTE: This does not show/hide *this composite.
         * 
         * If visible then highlight the field/plane pair button. Otherwise
         * unhighlight. Also, set the state on the field/plane pair instance.
         */
        public void setVisible(boolean isRscVisible) {
            fieldPlanePairNameBtn.setSelection(isRscVisible);
            fieldPlanePair.setVisible(isRscVisible);
            if (isRscVisible) {
                fieldPlanePairNameBtn.setBackground(selectedBkgdColor);
            } else {
                fieldPlanePairNameBtn.setBackground(unselectedBkgdColor);
            }
        }

        public void setEmpty() {
            hasNoResources = true;
        }

        public void setViewEditable(boolean enabled) {
            if (hasNoResources) {
                enabled = false;
            }
            setEnabled(enabled);
            fieldPlanePairNameBtn.setEnabled(enabled);
            displayTypeLbl.setEnabled(enabled);
            if (enabled) {
                if (fieldPlanePair.isVisible()) {
                    setVisible(true);
                }
            } else {
                setVisible(false);
            }
        }

        private void createContents() {

            setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true, 1, 1));

            GridLayout gridLayout = new GridLayout(5, false);
            gridLayout.marginBottom = 2;
            gridLayout.marginTop = 2;
            gridLayout.marginWidth = 2;
            gridLayout.marginHeight = 0;
            gridLayout.verticalSpacing = 1;
            gridLayout.horizontalSpacing = 1;
            setLayout(gridLayout);

            displayTypeLbl = new Label(this, SWT.BORDER);
            GridData displayTypeLbl_gd = new GridData(SWT.FILL, SWT.CENTER,
                    false, false, 1, 1);
            displayTypeLbl.setLayoutData(displayTypeLbl_gd);
            if (fieldPlanePair.getDisplayType() == DisplayType.CONTOUR) {
                displayTypeLbl.setToolTipText("Contour");
                displayTypeLbl
                        .setImage(EnsembleToolImageStore.ELEMENT_CONTOUR_IMG);
            } else if (fieldPlanePair.getDisplayType() == DisplayType.IMAGE) {
                displayTypeLbl.setToolTipText("Image");
                displayTypeLbl
                        .setImage(EnsembleToolImageStore.ELEMENT_IMAGE_IMG);
            } else if (fieldPlanePair.getDisplayType() == DisplayType.ARROW) {
                displayTypeLbl.setToolTipText("Arrow");
                displayTypeLbl
                        .setImage(EnsembleToolImageStore.ELEMENT_ARROW_IMG);
            } else if (fieldPlanePair.getDisplayType() == DisplayType.ICON) {
                displayTypeLbl.setToolTipText("Icon");
                displayTypeLbl
                        .setImage(EnsembleToolImageStore.ELEMENT_ICON_IMG);
            } else if (fieldPlanePair.getDisplayType() == DisplayType.BARB) {
                displayTypeLbl.setToolTipText("Wind Barb");
                displayTypeLbl
                        .setImage(EnsembleToolImageStore.ELEMENT_WINDBARB_IMG);
            } else if (fieldPlanePair.getDisplayType() == DisplayType.STREAMLINE) {
                displayTypeLbl.setToolTipText("Streamline");
                displayTypeLbl
                        .setImage(EnsembleToolImageStore.ELEMENT_STREAMLINE_IMG);
            }
            /* spacer */
            Label spacer = new Label(this, SWT.None);
            GridData spacer_gd = new GridData(SWT.LEFT, SWT.CENTER, false,
                    false, 1, 1);
            spacer_gd.widthHint = 3;
            spacer.setLayoutData(spacer_gd);

            fieldPlanePairNameBtn = new Button(this, SWT.BORDER);
            // fieldPlanePairNameBtn = new Button(this, SWT.BORDER | SWT.CHECK);
            GridData gd_elementNameBtn = new GridData(SWT.FILL, SWT.CENTER,
                    true, false, 3, 1);
            fieldPlanePairNameBtn.setLayoutData(gd_elementNameBtn);
            fieldPlanePairNameBtn.setFont(SWTResourceManager.getFont("Sans", 9,
                    SWT.NORMAL));
            fieldPlanePairNameBtn.setText(spacerPadding
                    + fieldPlanePair.getShortName());
            fieldPlanePairNameBtn.setToolTipText(fieldPlanePair.getLongName());

            setVisible(fieldPlanePair.isVisible());

            fieldPlanePairNameBtn.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {

                    toggleVisibility();
                    /**
                     * If this resource (that was just made visible) is an image
                     * resource then toggle all other images off.
                     */
                    if (fieldPlanePair.isVisible()
                            && fieldPlanePair.getDisplayType() == DisplayType.IMAGE) {

                        toggleOtherImagesOff(fieldPlanePair);
                    }
                    focusProvider.giveEditorFocus();
                }

            });

            final FieldPlanePairControl thisCtrl = this;
            fieldPlanePairNameBtn.addMouseListener(new MouseListener() {

                @Override
                public void mouseDoubleClick(MouseEvent e) {
                    // TODO Save for future use.
                }

                @Override
                public void mouseDown(MouseEvent e) {

                    /*
                     * is this a mouse-button-3 (e.g. typical RIGHT-CLICK) over
                     * a tree item?
                     */
                    if (e.button == 3) {
                        final FieldPlanePair clickedFieldPlanePair = fieldPlanePair;
                        MenuManager menuMgr = new MenuManager("#PopupMenu");
                        menuMgr.setRemoveAllWhenShown(true);

                        /* The popup menu is generated by the ContextMenuManager */
                        menuMgr.addMenuListener(new IMenuListener() {
                            public void menuAboutToShow(IMenuManager manager) {
                                ResourcePair rp = EnsembleTool.getInstance()
                                        .getResourcePair(
                                                clickedFieldPlanePair,
                                                sourceSelectionProvider
                                                        .getSelected());
                                if (rp != null) {
                                    ContextMenuManager.fillContextMenu(manager,
                                            rp, rp.getResource()
                                                    .getResourceContainer());
                                }
                            }
                        });

                        final Menu legendMenu = menuMgr
                                .createContextMenu(thisCtrl);
                        legendMenu.setVisible(true);
                        focusProvider.giveEditorFocus();
                    }

                }

                @Override
                public void mouseUp(MouseEvent e) {
                }
            });

            setToolTipText(fieldPlanePair.getLongName());
            layout();

        }

        public List<AbstractVizResource<?, ?>> getAssociatedRscList() {
            return associatedRscList;
        }

        protected void toggleVisibility() {

            fieldPlanePair.setVisible(!fieldPlanePair.isVisible());
            visibilityChanged(fieldPlanePair);

            if (fieldPlanePair.isVisible()) {
                fieldPlanePairNameBtn.setSelection(true);
                fieldPlanePairNameBtn.setBackground(selectedBkgdColor);

            } else {
                fieldPlanePairNameBtn.setSelection(false);
                fieldPlanePairNameBtn.setBackground(unselectedBkgdColor);
            }
        }

        /**
         * Whenever any capability on any registered resource
         */
        @Override
        public void resourceChanged(final ChangeType type, final Object object) {

            final FieldPlanePairControl finalThis = this;
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (type == ChangeType.CAPABILITY) {

                        if (object instanceof ColorableCapability) {
                            ColorableCapability cc = (ColorableCapability) object;
                            RGB rgb = cc.getColor();
                            for (AbstractVizResource<?, ?> r : associatedRscList) {
                                if (r.hasCapability(ColorableCapability.class)) {
                                    if (r.getCapability(
                                            ColorableCapability.class)
                                            .getColor() != rgb) {

                                        RGB rgb1 = r.getCapability(
                                                ColorableCapability.class)
                                                .getColor();

                                        r.getResourceData()
                                                .removeChangeListener(finalThis);
                                        r.getCapability(
                                                ColorableCapability.class)
                                                .setColor(rgb);
                                        r.getResourceData().addChangeListener(
                                                finalThis);
                                        displayTypeLbl
                                                .setBackground(SWTResourceManager
                                                        .getColor(rgb));
                                    }
                                }
                            }
                        }
                        if (object instanceof DensityCapability) {
                            DensityCapability dc = (DensityCapability) object;
                            Double density = dc.getDensity();
                            for (AbstractVizResource<?, ?> r : associatedRscList) {
                                if (r.hasCapability(DensityCapability.class)) {
                                    if (r.getCapability(DensityCapability.class)
                                            .getDensity() != density) {
                                        r.getResourceData()
                                                .removeChangeListener(finalThis);
                                        r.getCapability(DensityCapability.class)
                                                .setDensity(density);
                                        r.getResourceData().addChangeListener(
                                                finalThis);
                                    }
                                }
                            }
                        }

                        if (object instanceof OutlineCapability) {
                            OutlineCapability oc = (OutlineCapability) object;
                            LineStyle style = oc.getLineStyle();
                            int width = oc.getOutlineWidth();
                            for (AbstractVizResource<?, ?> r : associatedRscList) {
                                if (r.hasCapability(OutlineCapability.class)) {
                                    if (!(r.getCapability(
                                            OutlineCapability.class)
                                            .getLineStyle().equals(style))) {
                                        r.getResourceData()
                                                .removeChangeListener(finalThis);
                                        r.getCapability(OutlineCapability.class)
                                                .setLineStyle(style);
                                        r.getResourceData().addChangeListener(
                                                finalThis);
                                    }
                                    if (!(r.getCapability(
                                            OutlineCapability.class)
                                            .getOutlineWidth() != width)) {
                                        r.getResourceData()
                                                .removeChangeListener(finalThis);
                                        r.getCapability(OutlineCapability.class)
                                                .setOutlineWidth(width);
                                        r.getResourceData().addChangeListener(
                                                finalThis);
                                    }
                                }
                            }
                        }

                        if (object instanceof MagnificationCapability) {
                            MagnificationCapability oc = (MagnificationCapability) object;
                            Double mag = oc.getMagnification();
                            for (AbstractVizResource<?, ?> r : associatedRscList) {
                                if (r.hasCapability(MagnificationCapability.class)) {
                                    if (!(r.getCapability(
                                            MagnificationCapability.class)
                                            .getMagnification() == mag)) {
                                        r.getResourceData()
                                                .removeChangeListener(finalThis);
                                        r.getCapability(
                                                MagnificationCapability.class)
                                                .setMagnification(mag);
                                        r.getResourceData().addChangeListener(
                                                finalThis);
                                    }
                                }
                            }
                        }

                    }
                }
            });
        }

        public FieldPlanePair getFieldPlanePair() {
            return fieldPlanePair;
        }

        /**
         * TODO: This is being called from the Ensemble Resource Manager when it
         * is registering a new resource. This is probably not how we want to do
         * this in the long term as the call is being made from the resource
         * manager to a user interface component.
         */
        @Override
        synchronized public void resourceRegistered(
                final AbstractVizResource<?, ?> rsc) {

            if (rsc != null) {
                final FieldPlanePairControl finalThis = this;
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        String matchToRscField = null;
                        String matchToRscPlane = null;
                        RequestableResourceMetadata matchToRscMetaData = null;

                        AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) rsc
                                .getResourceData();
                        matchToRscMetaData = new RequestableResourceMetadata(
                                arrd);

                        matchToRscField = matchToRscMetaData.getFieldAbbrev();
                        matchToRscPlane = matchToRscMetaData.getPlane();

                        if (matchToRscField == null
                                || matchToRscField.length() == 0
                                || matchToRscPlane == null
                                || matchToRscField.length() == 0) {
                            return;
                        }

                        if (matchToRscField.equals(fieldPlanePair
                                .getFieldAbbrev())
                                && matchToRscPlane.equals(fieldPlanePair
                                        .getPlane())) {
                            associatedRscList.add(rsc);
                            rsc.getResourceData().addChangeListener(
                                    (IResourceDataChanged) finalThis);
                            if (rsc.getName().startsWith(
                                    MatrixNavigatorComposite
                                            .getPrimaryTimeBasisModelSource()
                                            .getModelName())) {
                                RGB rgb = null;
                                if (rsc.hasCapability(ColorableCapability.class)) {
                                    if (rsc.getCapability(
                                            ColorableCapability.class)
                                            .getColor() != rgb) {
                                        if (!rsc.hasCapability(ImagingCapability.class)) {
                                            rgb = rsc.getCapability(
                                                    ColorableCapability.class)
                                                    .getColor();
                                            displayTypeLbl
                                                    .setBackground(SWTResourceManager
                                                            .getColor(rgb));
                                        }
                                    }
                                }
                            }
                            FieldPlanePairChooserControl.resourceCount++;

                        }

                    }

                });
            }
        }
    }

}
