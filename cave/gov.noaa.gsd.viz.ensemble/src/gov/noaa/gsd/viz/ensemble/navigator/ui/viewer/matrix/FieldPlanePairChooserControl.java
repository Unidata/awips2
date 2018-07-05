package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.d2d.core.map.D2DColorBarResource;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.control.IResourceRegisteredListener;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common.ContextMenuManager;
import gov.noaa.gsd.viz.ensemble.util.EnsembleToolImageStore;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.RequestableResourceMetadata;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

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
 * Nov 19, 2016  19443      polster     Refactoring for 17.1.1 release
 * Dec 19, 2016  19443      polster     added isWidgetReady methods
 * Mar 01, 2017  19443      polster     no longer remove viz resources associated with widget
 * Dec 01, 2017  20328      polster     Method registerResource now handles correct resource
 * Dec 01, 2017  20328      polster     Cosemtics of FPP control enhanced
 * Jan 10, 2018  20525      polster     FPP now has remove 'x' button
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */

public class FieldPlanePairChooserControl extends Composite
        implements IFieldPlanePairVisibilityChangedListener {

    static public GlobalColor activeBackgroundDefaultColor = null;

    static public GlobalColor inactiveBackgroundDefaultColor = null;

    protected static int resourceCount = 0;

    private final String spacerPadding = "  ";

    private ScrolledComposite activeFieldPlanePairScrolledComposite = null;

    private Composite activeFieldPlanePairComposite = null;

    private ScrolledComposite notActiveFieldPlanePairScrolledComposite = null;

    private Composite notActiveFieldPlanePairComposite = null;

    private IFieldPlanePairVisibilityChangedListener visibilityListener = null;

    private IModelSourceSelectionProvider sourceSelectionProvider = null;

    private IMatrixResourceLoadProvider resourceLoader = null;

    private IMatrixResourceMatcher resourceMatcher = null;

    private IModelFamilyProvider modelFamilyProvider = null;

    private IMatrixEditorFocusProvider focusProvider = null;

    private EnsembleToolLayer toolLayer = null;

    private FieldPlanePairControllerMouseListener transferFocusOnMouseUp = null;

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
     *            the field/plane pair visibility change listener to be notified
     *            when an element's visibility is changed by the user.
     * @param mssp
     *            the provider which notifies this class when a model source is
     *            selected by the user.
     */
    public FieldPlanePairChooserControl(Composite parent, int style,
            EnsembleToolLayer layer,
            IFieldPlanePairVisibilityChangedListener evcl,
            IModelSourceSelectionProvider mssp, IMatrixEditorFocusProvider mefp,
            IMatrixResourceLoadProvider mrl, IMatrixResourceMatcher m,
            IModelFamilyProvider mfp, IMatrixEditorFocusProvider fp) {
        super(parent, style);
        toolLayer = layer;
        modelFamilyProvider = mfp;
        visibilityListener = evcl;
        sourceSelectionProvider = mssp;
        focusProvider = mefp;
        resourceLoader = mrl;
        resourceMatcher = m;
        focusProvider = fp;
        createContents();
    }

    /**
     * Create the contents of the entire control's composite.
     */
    private void createContents() {

        transferFocusOnMouseUp = new FieldPlanePairControllerMouseListener();

        activeBackgroundDefaultColor = GlobalColor.DARKER_GRAY;
        inactiveBackgroundDefaultColor = GlobalColor.DARKISH_GRAY;

        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
        setLayout(new GridLayout(1, true));

        createActiveFPPcontrolArea();
        createNotActiveFPPcontrolArea();
        pack();
    }

    /**
     * Create the contents of the control area where the loaded field/plane pair
     * widgets are displayed.
     */
    private void createActiveFPPcontrolArea() {

        activeFieldPlanePairScrolledComposite = new ScrolledComposite(this,
                SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);

        GridData elementSetComposite_gd = new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1);
        activeFieldPlanePairScrolledComposite
                .setLayoutData(elementSetComposite_gd);
        GridLayout fieldPlanePairScrolledComposite_gl = new GridLayout(1, true);
        fieldPlanePairScrolledComposite_gl.horizontalSpacing = 0;
        fieldPlanePairScrolledComposite_gl.marginHeight = 0;
        fieldPlanePairScrolledComposite_gl.marginWidth = 0;
        fieldPlanePairScrolledComposite_gl.verticalSpacing = 0;
        activeFieldPlanePairScrolledComposite
                .setLayout(fieldPlanePairScrolledComposite_gl);
        activeFieldPlanePairScrolledComposite.setExpandHorizontal(true);
        activeFieldPlanePairScrolledComposite.setExpandVertical(true);

        activeFieldPlanePairComposite = new Composite(
                activeFieldPlanePairScrolledComposite, SWT.NONE);
        activeFieldPlanePairComposite
                .setBackground(GlobalColor.get(activeBackgroundDefaultColor));
        GridData fieldPlanePairComposite_gd = new GridData(SWT.FILL, SWT.FILL,
                true, true, 1, 1);
        activeFieldPlanePairComposite.setLayoutData(fieldPlanePairComposite_gd);
        GridLayout fieldPlanePairComposite_gl = new GridLayout(2, true);
        fieldPlanePairComposite_gl.marginTop = 3;
        fieldPlanePairComposite_gl.marginWidth = 3;
        activeFieldPlanePairComposite.setLayout(fieldPlanePairComposite_gl);
        activeFieldPlanePairScrolledComposite
                .setContent(activeFieldPlanePairComposite);

        activeFieldPlanePairScrolledComposite
                .addMouseListener(transferFocusOnMouseUp);

    }

    /**
     * Create the contents of the control area where the not loaded but
     * available field/plane pair widgets are displayed.
     */
    private void createNotActiveFPPcontrolArea() {

        notActiveFieldPlanePairScrolledComposite = new ScrolledComposite(this,
                SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);

        GridData elementSetComposite_gd = new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1);
        notActiveFieldPlanePairScrolledComposite
                .setLayoutData(elementSetComposite_gd);
        GridLayout fieldPlanePairScrolledComposite_gl = new GridLayout(1, true);
        fieldPlanePairScrolledComposite_gl.horizontalSpacing = 0;
        fieldPlanePairScrolledComposite_gl.marginHeight = 0;
        fieldPlanePairScrolledComposite_gl.marginWidth = 0;
        fieldPlanePairScrolledComposite_gl.verticalSpacing = 1;
        notActiveFieldPlanePairScrolledComposite
                .setLayout(fieldPlanePairScrolledComposite_gl);
        notActiveFieldPlanePairScrolledComposite.setExpandHorizontal(true);
        notActiveFieldPlanePairScrolledComposite.setExpandVertical(true);

        notActiveFieldPlanePairComposite = new Composite(
                notActiveFieldPlanePairScrolledComposite, SWT.NONE);
        notActiveFieldPlanePairComposite
                .setBackground(GlobalColor.get(inactiveBackgroundDefaultColor));
        GridData fieldPlanePairComposite_gd = new GridData(SWT.FILL, SWT.FILL,
                true, true, 1, 1);
        notActiveFieldPlanePairComposite
                .setLayoutData(fieldPlanePairComposite_gd);
        GridLayout fieldPlanePairComposite_gl = new GridLayout(2, true);
        fieldPlanePairComposite_gl.marginTop = 3;
        fieldPlanePairComposite_gl.marginWidth = 3;
        notActiveFieldPlanePairComposite.setLayout(fieldPlanePairComposite_gl);
        notActiveFieldPlanePairScrolledComposite
                .setContent(notActiveFieldPlanePairComposite);

        activeFieldPlanePairScrolledComposite
                .addMouseListener(transferFocusOnMouseUp);

    }

    @Override
    public void dispose() {
        clearFieldPlanePairControls();

        activeFieldPlanePairComposite.dispose();
        activeFieldPlanePairScrolledComposite.dispose();

        notActiveFieldPlanePairComposite.dispose();
        notActiveFieldPlanePairScrolledComposite.dispose();
    }

    /**
     * Clear all field/plane pairs displayed in this control. Also, from the
     * active component remove all associated resources.
     */
    public void clearFieldPlanePairControls() {

        if (isWidgetReady()) {
            Control[] children = activeFieldPlanePairComposite.getChildren();
            for (Control c : children) {
                c.dispose();
                activeFieldPlanePairComposite.update();
            }
            children = notActiveFieldPlanePairComposite.getChildren();
            for (Control c : children) {
                c.dispose();
                notActiveFieldPlanePairComposite.update();
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
    public void addModelFamily(ResolvedModelFamily modelFamily) {

        if (isWidgetReady()) {
            clearFieldPlanePairControls();
            FieldPlanePairSet set = modelFamily.getFieldPlanePairs();
            List<FieldPlanePair> nodes = set.getNodes();

            for (FieldPlanePair e : nodes) {
                new FieldPlanePairControl(notActiveFieldPlanePairComposite,
                        this, e, modelFamilyProvider, resourceMatcher,
                        focusProvider);
            }
            notActiveFieldPlanePairComposite.layout();

        }
    }

    /**
     * Return the all the active (already loaded) field/plane pair controls.
     */
    public List<FieldPlanePairControl> getActiveFieldPlanePairControls() {
        List<FieldPlanePairControl> controls = new ArrayList<>();

        FieldPlanePairControl fppc = null;
        if (isWidgetReady()) {
            Control[] children = activeFieldPlanePairComposite.getChildren();
            for (Control c : children) {
                if (c instanceof FieldPlanePairControl) {
                    fppc = (FieldPlanePairControl) c;
                    controls.add(fppc);
                }
            }
        }
        return controls;
    }

    /**
     * Disable all field/plane pair controls that have no actively loaded
     * resources with them.
     */
    public void disableEmptyFieldPlanePairControls() {

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                disableEmptyFieldPlanePairControls(
                        activeFieldPlanePairComposite);
            }
        });
    }

    private void disableEmptyFieldPlanePairControls(Composite fppComposite) {

        if (isWidgetReady()) {
            Control[] controls = fppComposite.getChildren();
            for (Control c : controls) {
                if (c instanceof FieldPlanePairControl) {
                    FieldPlanePairControl fppc = (FieldPlanePairControl) c;
                    if (modelFamilyProvider.getActiveModelFamily()
                            .getAssociatedRscSet(fppc.fieldPlanePair)
                            .isEmpty()) {
                        fppc.setHasNoResource(fppc.fieldPlanePair);
                        fppc.setToolTipText("No resource available: "
                                + fppc.getFieldPlanePair().getShortName());
                    }
                }
            }
        }
    }

    /**
     * This convenience method returns a list of all active field/plane pairs;
     * those resources currently loaded in the active map editor.
     * 
     * @return the list of elements associated with all loaded resources.
     */
    public List<FieldPlanePair> getActiveFieldPlanePairs() {

        List<FieldPlanePair> fieldPlanePairs = new ArrayList<>();
        List<FieldPlanePairControl> fppControls = getActiveFieldPlanePairControls();

        for (FieldPlanePairControl c : fppControls) {
            fieldPlanePairs.add(c.fieldPlanePair);
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
    public void setEditable(final boolean enabled) {

        if (isWidgetReady()) {
            setEditable(activeFieldPlanePairComposite, enabled,
                    activeBackgroundDefaultColor);
            setEditable(notActiveFieldPlanePairComposite, enabled,
                    inactiveBackgroundDefaultColor);
        }
    }

    private void setEditable(final Composite comp, final boolean enabled,
            GlobalColor background) {

        Control[] controls = comp.getChildren();
        for (Control c : controls) {
            if (c instanceof FieldPlanePairControl) {
                FieldPlanePairControl fppc = (FieldPlanePairControl) c;
                fppc.setEditable(enabled);
            }
        }
        if (enabled) {
            comp.setBackground(GlobalColor.get(background));
        } else {
            comp.setBackground(GlobalColor.get(inactiveBackgroundDefaultColor));
        }
    }

    /**
     * This method is called with the field/plane pair for an image resource
     * that was just made visible. Walk all field/plane pairs to find the other
     * image-based field/plane pairs and set their resources to to be hidden.
     */
    protected void toggleOtherImagesOff(FieldPlanePair fpp) {
        if (isWidgetReady()) {
            Control[] controls = activeFieldPlanePairComposite.getChildren();
            for (Control c : controls) {
                if (c instanceof FieldPlanePairControl) {
                    FieldPlanePairControl fppc = (FieldPlanePairControl) c;
                    if (fppc.isEnabled() && fppc.getFieldPlanePair() != fpp
                            && fppc.getFieldPlanePair()
                                    .getDisplayType() == DisplayType.IMAGE) {
                        fppc.setResourcesVisible(false);
                        visibilityChanged(fppc.getFieldPlanePair());
                    }
                }
            }
        }
    }

    public int getActiveFieldPlanePairCount() {
        int count = 0;
        if (isWidgetReady()) {
            count = activeFieldPlanePairComposite.getChildren().length;
        }
        return count;
    }

    protected void activate(FieldPlanePairControl fppc) {
        if (isWidgetReady()) {
            fppc.setParent(activeFieldPlanePairComposite);
            fppc.setEmpty(true);
            activeFieldPlanePairComposite.pack();
            notActiveFieldPlanePairComposite.pack();
            resourceLoader.loadResources(fppc.fieldPlanePair);
            layout();
        }
    }

    protected void deactivate(FieldPlanePairControl fppc) {
        if (isWidgetReady()) {
            fppc.setParent(notActiveFieldPlanePairComposite);
            activeFieldPlanePairComposite.pack();
            notActiveFieldPlanePairComposite.pack();
            resourceLoader.unloadResources(fppc.fieldPlanePair);
            layout();
        }
    }

    private boolean isWidgetReady() {
        boolean isReady = false;
        if (!isDisposed() && activeFieldPlanePairComposite != null
                && !activeFieldPlanePairComposite.isDisposed()
                && notActiveFieldPlanePairComposite != null
                && !notActiveFieldPlanePairComposite.isDisposed()
                && activeFieldPlanePairScrolledComposite != null
                && !activeFieldPlanePairScrolledComposite.isDisposed()
                && notActiveFieldPlanePairScrolledComposite != null
                && !notActiveFieldPlanePairScrolledComposite.isDisposed()) {
            isReady = true;
        }
        return isReady;
    }

    public void addResourceRegisteredListener(
            IResourceRegisteredListener listener) {

        toolLayer.addResourceRegisteredListener(listener);

    }

    public void removeResourceRegisteredListener(
            IResourceRegisteredListener listener) {
        toolLayer.removeResourceRegisteredListener(listener);

    }

    public void updateControls(List<AbstractResourceHolder> rscHolderList) {

        for (AbstractResourceHolder arh : rscHolderList) {
            for (FieldPlanePairControl fppc : getActiveFieldPlanePairControls()) {
                Set<AbstractVizResource<?, ?>> assocRscs = modelFamilyProvider
                        .getActiveModelFamily()
                        .getAssociatedRscSet(fppc.getFieldPlanePair());
                for (AbstractVizResource<?, ?> rsc : assocRscs) {
                    if (rsc.equals(arh.getRsc())
                            && rsc.getProperties().isVisible()) {
                        fppc.setResourcesVisible(true);
                    }
                }
            }
        }
    }

    protected void remove(FieldPlanePairControl fppc) {
        fppc.dispose();
        activeFieldPlanePairComposite.layout();
        notActiveFieldPlanePairComposite.layout();
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
     * The button has two actions: the first action only works when the control
     * is in the lower "not-yet-active" and acts by moving the control from the
     * lower "not-yet-active" composite to the upper "active" composite; once
     * moved the first action also automatically starts loading the field plane
     * pair across all model sources in the model family.
     * 
     * The second action only works when the control is in the upper "active"
     * composite and solely controls the respective and currently displayed
     * resource's visibility.
     * 
     */
    public class FieldPlanePairControl extends Composite
            implements IResourceDataChanged, IResourceRegisteredListener {

        private FieldPlanePairChooserControl fppController = null;

        private IMatrixResourceMatcher resourceMatcher = null;

        private FieldPlanePair fieldPlanePair = null;

        private Composite fieldPlanePairActionBtnContainer = null;

        private Button fieldPlanePairActionBtn = null;

        private Composite displayTypeRoot = null;

        private Button displayTypeBtn = null;

        private Button removeFPPControlBtn = null;

        private Color defaultBkgdColor = null;

        private Color rscVisibleBorderBkgdColor = null;

        private Color activeBkgdColor = null;

        private Color inactiveBkgdColor = null;

        private Color notLoadedBkgdColor = null;

        private Color notVisibleBkgdColor = null;

        private Color visibleBkgdColor = null;

        private Color noResourceForSourceBkgdColor = null;

        private boolean hasNoResources = false;

        private Font fppActiveFont = null;

        private Font fppNotActiveFont = null;

        /*
         * The user chooses which field/plane pair (fpp) is active for the
         * currently loaded model family. When users click on a
         * FieldPlanePairControl when it is in the not-yet-loaded (lower)
         * composite, it goes from being not-active to active. Active fpps are
         * moved to the upper "loaded" composite and have their associated
         * resources loaded to the active matrix editor with visibility of the
         * resource asserted.
         */
        private boolean isActive = false;

        private boolean isVisible = false;

        private IMatrixEditorFocusProvider focusProvider = null;

        private IModelFamilyProvider modelFamilyProvider = null;

        public FieldPlanePairControl(Composite parent,
                FieldPlanePairChooserControl fppcc, FieldPlanePair e,
                IModelFamilyProvider mfp, IMatrixResourceMatcher rm,
                IMatrixEditorFocusProvider fp) {

            super(parent, SWT.BORDER);

            modelFamilyProvider = mfp;
            fppController = fppcc;
            fieldPlanePair = e;
            resourceMatcher = rm;
            focusProvider = fp;

            createColorsAndFonts();
            createContents();

            /*
             * All controls are associated with resources that are not loaded by
             * default and so the control (this->) is therefore not active.
             */
            isActive = false;

            /*
             * All controls will have their visibility by default negated as
             * they are by default not active. The action of the user loading
             * the resource will automatically turn visibility on.
             */
            isVisible = false;

            fppController.addResourceRegisteredListener(this);

        }

        private void createColorsAndFonts() {

            noResourceForSourceBkgdColor = GlobalColor
                    .get(GlobalColor.MEDIUM_LIGHT_GRAY);
            activeBkgdColor = SWTResourceManager
                    .getSystemColor(SWT.COLOR_LIST_BACKGROUND);

            inactiveBkgdColor = GlobalColor.get(GlobalColor.LIGHT_GRAY);
            visibleBkgdColor = GlobalColor.get(GlobalColor.BLACK);
            notVisibleBkgdColor = GlobalColor.get(GlobalColor.DARKISH_GRAY);
            defaultBkgdColor = this.getBackground();
            rscVisibleBorderBkgdColor = GlobalColor
                    .get(GlobalColor.LIGHTER_YELLOW);

            notLoadedBkgdColor = GlobalColor.get(GlobalColor.LIGHT_GRAY);
            fppActiveFont = SWTResourceManager.getFont("Sans", 9, SWT.NORMAL);
            fppNotActiveFont = SWTResourceManager.getFont("Sans", 8,
                    SWT.NORMAL);
        }

        private void createContents() {

            GridData gridData = new GridData(SWT.FILL, SWT.TOP, true, false, 1,
                    1);
            setLayoutData(gridData);

            GridLayout gridLayout = new GridLayout(11, false);
            gridLayout.marginWidth = 2;
            gridLayout.marginHeight = 3;
            gridLayout.verticalSpacing = 1;
            gridLayout.horizontalSpacing = 1;
            setLayout(gridLayout);

            displayTypeRoot = new Composite(this, SWT.BORDER);

            displayTypeRoot.addMouseListener(transferFocusOnMouseUp);
            GridData displayTypeRoot_gd = new GridData(SWT.FILL, SWT.FILL,
                    false, false, 3, 1);
            displayTypeRoot.setLayoutData(displayTypeRoot_gd);
            GridLayout displayTypeRoot_gl = new GridLayout(3, false);
            displayTypeRoot.setLayout(displayTypeRoot_gl);

            Label tl0 = new Label(displayTypeRoot, SWT.NONE);
            GridData tl0_gd = new GridData(SWT.LEFT, SWT.CENTER, false, false,
                    1, 1);
            tl0.setLayoutData(tl0_gd);

            displayTypeBtn = new Button(displayTypeRoot, SWT.BORDER | SWT.PUSH);
            GridData displayTypeBtn_gd = new GridData(SWT.CENTER, SWT.CENTER,
                    true, false, 1, 1);
            displayTypeBtn.setLayoutData(displayTypeBtn_gd);

            Label tl1 = new Label(displayTypeRoot, SWT.NONE);
            GridData tl1_gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false,
                    1, 1);
            tl1.setLayoutData(tl1_gd);

            if (fieldPlanePair.getDisplayType() == DisplayType.CONTOUR) {
                displayTypeBtn.setToolTipText("Contour");
                displayTypeBtn
                        .setImage(EnsembleToolImageStore.ELEMENT_CONTOUR_IMG);
            } else if (fieldPlanePair.getDisplayType() == DisplayType.IMAGE) {
                displayTypeBtn.setToolTipText("Image");
                displayTypeBtn
                        .setImage(EnsembleToolImageStore.ELEMENT_IMAGE_IMG);
            } else if (fieldPlanePair.getDisplayType() == DisplayType.ARROW) {
                displayTypeBtn.setToolTipText("Arrow");
                displayTypeBtn
                        .setImage(EnsembleToolImageStore.ELEMENT_ARROW_IMG);
            } else if (fieldPlanePair.getDisplayType() == DisplayType.ICON) {
                displayTypeBtn.setToolTipText("Icon");
                displayTypeBtn
                        .setImage(EnsembleToolImageStore.ELEMENT_ICON_IMG);
            } else if (fieldPlanePair.getDisplayType() == DisplayType.BARB) {
                displayTypeBtn.setToolTipText("Wind Barb");
                displayTypeBtn
                        .setImage(EnsembleToolImageStore.ELEMENT_WINDBARB_IMG);
            } else if (fieldPlanePair
                    .getDisplayType() == DisplayType.STREAMLINE) {
                displayTypeBtn.setToolTipText("Streamline");
                displayTypeBtn.setImage(
                        EnsembleToolImageStore.ELEMENT_STREAMLINE_IMG);
            }

            displayTypeBtn.addMouseListener(transferFocusOnMouseUp);

            fieldPlanePairActionBtnContainer = new Composite(this, SWT.NONE);

            GridData fieldPlanePairActionBtnContainer_gd = new GridData(
                    SWT.FILL, SWT.FILL, true, true, 8, 1);
            fieldPlanePairActionBtnContainer
                    .setLayoutData(fieldPlanePairActionBtnContainer_gd);
            GridLayout fieldPlanePairActionBtnContainer_gl = new GridLayout(4,
                    true);
            fieldPlanePairActionBtnContainer_gl.horizontalSpacing = 0;
            fieldPlanePairActionBtnContainer_gl.marginHeight = 6;
            fieldPlanePairActionBtnContainer_gl.marginWidth = 9;
            fieldPlanePairActionBtnContainer_gl.verticalSpacing = 0;
            fieldPlanePairActionBtnContainer
                    .setLayout(fieldPlanePairActionBtnContainer_gl);
            fieldPlanePairActionBtnContainer
                    .addMouseListener(new MouseAdapter() {

                        /*
                         * the call to toggleActive will transfer input focus
                         * back to the viz matrix editor with approriate key
                         * bindings.
                         */
                        @Override
                        public void mouseUp(MouseEvent e) {
                            /*
                             * Only act on mouse-up event for primary button
                             * (usually left click)
                             */
                            if (e.button == 1) {
                                toggleActive();
                            }
                        }
                    });

            fieldPlanePairActionBtn = new Button(
                    fieldPlanePairActionBtnContainer, SWT.NONE);
            GridData gd_fieldPlanePairActionBtn = new GridData(SWT.FILL,
                    SWT.FILL, true, true, 3, 1);
            fieldPlanePairActionBtn.setLayoutData(gd_fieldPlanePairActionBtn);
            fieldPlanePairActionBtn.setFont(fppNotActiveFont);
            /*
             * Happens that the inactiveBkgdColor is best for default of action
             * button
             */
            fieldPlanePairActionBtn.setBackground(activeBkgdColor);
            fieldPlanePairActionBtn.setText(spacerPadding
                    + fieldPlanePair.getShortName() + spacerPadding);
            fieldPlanePairActionBtn
                    .setToolTipText(fieldPlanePair.getLongName());
            fieldPlanePairActionBtnContainer.setBackground(inactiveBkgdColor);

            final int closeBtnSize = 33;
            removeFPPControlBtn = new Button(fieldPlanePairActionBtnContainer,
                    SWT.BOLD);
            GridData gd_removeFPPControlBtn = new GridData(SWT.RIGHT,
                    SWT.CENTER, true, true, 1, 1);
            gd_removeFPPControlBtn.heightHint = closeBtnSize;
            gd_removeFPPControlBtn.widthHint = closeBtnSize;
            removeFPPControlBtn.setLayoutData(gd_removeFPPControlBtn);
            removeFPPControlBtn
                    .setImage(EnsembleToolImageStore.CLOSE_LIGHTENED_IMG);

            final FieldPlanePairControl finalThis = this;

            removeFPPControlBtn.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (isActive) {
                        deactivate();
                    } else {
                        boolean shouldClose = MessageDialog.openConfirm(
                                Display.getCurrent().getActiveShell(),
                                "Confirm Field/Plane Pair Removal",
                                "Remove the field/plane pair from the currently opened model family?");
                        if (shouldClose) {
                            fppController.remove(finalThis);
                        }
                    }
                }

            });

            fieldPlanePairActionBtn
                    .addSelectionListener(new SelectionAdapter() {

                        @Override
                        public void widgetSelected(SelectionEvent e) {
                            toggleActive();
                        }

                    });

            final FieldPlanePairControl thisCtrl = this;
            fieldPlanePairActionBtn.addMouseListener(new MouseAdapter() {

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

                        /*
                         * The pop-up menu is generated by the
                         * ContextMenuManager
                         */
                        menuMgr.addMenuListener(new IMenuListener() {
                            public void menuAboutToShow(IMenuManager manager) {
                                ResourcePair rp = EnsembleTool.getInstance()
                                        .getResourcePair(clickedFieldPlanePair,
                                                sourceSelectionProvider
                                                        .getSelected());
                                if (rp != null) {
                                    MatrixNavigatorContextMenuManager
                                            .fillContextMenu(manager, rp, rp
                                                    .getResource()
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

            });

            deactivate();

            layout();

        }

        @Override
        public void dispose() {
            fppController.removeResourceRegisteredListener(this);

            if (fieldPlanePairActionBtnContainer != null) {
                fieldPlanePairActionBtnContainer.dispose();
            }

            if (fieldPlanePairActionBtn != null) {
                fieldPlanePairActionBtn.dispose();
            }

            if (displayTypeBtn != null) {
                displayTypeBtn.dispose();
            }

            fieldPlanePairActionBtnContainer = null;
            fieldPlanePairActionBtn = null;
            displayTypeBtn = null;

            super.dispose();
        }

        /**
         * If visible then highlight the field/plane pair button. Otherwise do
         * not highlight. Also, set the state on the field/plane pair instance.
         */
        public void setResourcesVisible(boolean isVisible) {
            if (isActive) {
                fieldPlanePair.setResourceVisible(isVisible);
                if (isVisible) {
                    fieldPlanePairActionBtnContainer
                            .setBackground(visibleBkgdColor);
                    setBackground(rscVisibleBorderBkgdColor);
                } else {
                    fieldPlanePairActionBtnContainer
                            .setBackground(notVisibleBkgdColor);
                    setBackground(defaultBkgdColor);
                }
            } else {
                fieldPlanePairActionBtnContainer
                        .setBackground(inactiveBkgdColor);
                setBackground(defaultBkgdColor);
            }
        }

        public boolean isVisible() {
            return isVisible;
        }

        public void setEmpty(boolean isEmpty) {
            hasNoResources = isEmpty;
        }

        public boolean isEmpty() {
            return hasNoResources;
        }

        /*
         * This method turns on/off user interactivity with this FPP instance.
         */
        public void setEditable(boolean enabled) {
            if (isWidgetReady()) {
                fieldPlanePairActionBtn.setEnabled(enabled);
                displayTypeBtn.setEnabled(enabled);
                if (enabled) {
                    if (fieldPlanePair.isResourceVisible()) {
                        setResourcesVisible(true);
                    }
                } else {
                    setResourcesVisible(false);
                }
            }
        }

        public void setHasNoResource(FieldPlanePair fpp) {

            if (isWidgetReady()) {
                fieldPlanePairActionBtnContainer
                        .setBackground(noResourceForSourceBkgdColor);
                fieldPlanePairActionBtn
                        .setBackground(noResourceForSourceBkgdColor);
            }
        }

        private boolean isWidgetReady() {
            boolean isReady = false;
            if (!isDisposed() //
                    && fieldPlanePairActionBtnContainer != null
                    && !fieldPlanePairActionBtnContainer.isDisposed()
                    && fieldPlanePairActionBtn != null //
                    && !fieldPlanePairActionBtn.isDisposed()
                    && displayTypeBtn != null //
                    && !displayTypeBtn.isDisposed()) {
                isReady = true;
            }
            return isReady;
        }

        private void activate() {

            isActive = true;

            fppController.activate(this);
            setToolTipText(fieldPlanePair.getLongName());
            fieldPlanePairActionBtn.setFont(fppActiveFont);
            fppActiveFont = SWTResourceManager.getFont("Sans", 9, SWT.NORMAL);
            fppNotActiveFont = SWTResourceManager.getFont("Sans", 8,
                    SWT.NORMAL);
        }

        private void deactivate() {

            isActive = false;

            displayTypeRoot.setBackground(inactiveBkgdColor);
            fieldPlanePairActionBtnContainer.setBackground(inactiveBkgdColor);
            fieldPlanePairActionBtn.setFont(fppNotActiveFont);
            fppController.deactivate(this);

            setToolTipText(
                    fieldPlanePair.getLongName() + " : <click to activate>");

            fppActiveFont = SWTResourceManager.getFont("Sans", 8, SWT.NORMAL);
            fppNotActiveFont = SWTResourceManager.getFont("Sans", 7,
                    SWT.NORMAL);
        }

        protected void toggleActive() {

            if (isActive) {
                toggleVisibility();
                /**
                 * If this resource (that was just made visible) is an image
                 * resource then toggle all other images off.
                 */
                if (fieldPlanePair.isResourceVisible() && fieldPlanePair
                        .getDisplayType() == DisplayType.IMAGE) {

                    toggleOtherImagesOff(fieldPlanePair);
                }
            } else {
                activate();
            }

            focusProvider.giveEditorFocus();

        }

        protected void toggleVisibility() {

            if (!isWidgetReady()) {
                return;
            }

            boolean isVisible = !fieldPlanePair.isResourceVisible();
            if (isActive) {
                fieldPlanePair.setResourceVisible(isVisible);
                if (isVisible) {
                    fieldPlanePairActionBtnContainer
                            .setBackground(visibleBkgdColor);
                } else {
                    fieldPlanePairActionBtnContainer
                            .setBackground(notVisibleBkgdColor);
                }
                visibilityChanged(fieldPlanePair);
            } else {
                fieldPlanePairActionBtnContainer
                        .setBackground(inactiveBkgdColor);

                rscVisibleBorderBkgdColor = GlobalColor.get(GlobalColor.WHITE);
            }
        }

        /**
         * Whenever any capability on any registered resource has changed then
         * make the change across all other associated resources (i.e. resources
         * having the same field/plane pairs).
         */
        @Override
        public void resourceChanged(final ChangeType type,
                final Object object) {

            final FieldPlanePairControl finalThis = this;

            resourceMatcher.matchProductCapabilityByFieldPlanePair(finalThis,
                    object);

            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (type == ChangeType.CAPABILITY) {

                        if (object instanceof ColorableCapability) {
                            RGB rgb = modelFamilyProvider.getActiveModelFamily()
                                    .getResourceColor(finalThis.fieldPlanePair);
                            if (isWidgetReady() && rgb != null) {
                                if (finalThis.getFieldPlanePair()
                                        .getDisplayType() != DisplayType.IMAGE) {
                                    displayTypeRoot.setBackground(
                                            SWTResourceManager.getColor(rgb));
                                } else {
                                    displayTypeRoot.setBackground(GlobalColor
                                            .get(GlobalColor.DARKISH_GRAY));

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
         * Called from the Ensemble Resource Manager when it registers a new
         * resource.
         * 
         * If the new resource that was just received by the client has the same
         * field/plane pair as this FieldPlanePairControl instance, then store
         * the resource in the associated resource list and register with the
         * new resource for when its state (color, density, etc) is changed.
         */
        @Override
        public void resourceRegistered(final AbstractVizResource<?, ?> rsc) {

            if (rsc != null) {

                /*
                 * Do we need to keep this check for a rsc that has no name
                 * defined (i.e. other than the default defined by the
                 * GridNameGenerator class)?
                 */
                if (rsc.getName().equals("Grid")) {
                    return;
                }
                VizApp.runSync(new Runnable() {

                    @Override
                    public void run() {

                        /*
                         * Only handle if the rsc argument contains the same
                         * field/plane pair as FieldPlanePairControl.this.
                         */
                        if (getFieldPlanePair().isCompatible(rsc)) {

                            rsc.getProperties().setVisible(false);
                            /*
                             * Is the incoming resource an image? If so, make
                             * sure the colorbar is visible.
                             */
                            if (rsc.hasCapability(ImagingCapability.class)) {
                                List<D2DColorBarResource> colorBarList = rsc
                                        .getDescriptor().getResourceList()
                                        .getResourcesByTypeAsType(
                                                D2DColorBarResource.class);
                                /* only one color bar in any given editor. */
                                if (colorBarList != null
                                        && !colorBarList.isEmpty()) {
                                    D2DColorBarResource cbr = colorBarList
                                            .get(0);
                                    if (!cbr.getProperties().isVisible()) {
                                        cbr.getProperties().setVisible(true);
                                    }
                                }
                            }
                            String incomingRscField = null;
                            String incomingRscPlane = null;
                            RequestableResourceMetadata incomingRscMetaData = null;

                            AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) rsc
                                    .getResourceData();
                            incomingRscMetaData = new RequestableResourceMetadata(
                                    arrd);

                            incomingRscField = incomingRscMetaData
                                    .getFieldAbbrev();
                            incomingRscPlane = incomingRscMetaData.getPlane();

                            if (incomingRscField == null
                                    || incomingRscField.length() == 0
                                    || incomingRscPlane == null
                                    || incomingRscField.length() == 0) {
                                return;
                            }

                            if (incomingRscField
                                    .equals(fieldPlanePair.getFieldAbbrev())
                                    && incomingRscPlane.equals(
                                            fieldPlanePair.getPlane())) {

                                if (modelFamilyProvider
                                        .isResourceInVisibleSource(rsc)) {
                                    /*
                                     * only set the resources that are
                                     * associated with the currently selected
                                     * model source.
                                     */
                                    setResourcesVisible(true);
                                    rsc.getProperties().setVisible(true);

                                    /*
                                     * Only one image to be visible at a time.
                                     */
                                    if (getFieldPlanePair()
                                            .getDisplayType() == DisplayType.IMAGE) {
                                        toggleOtherImagesOff(
                                                getFieldPlanePair());
                                    }

                                }

                                /*
                                 * the incoming resource is kindred so bring it
                                 * into the fold.
                                 */
                                modelFamilyProvider.getActiveModelFamily()
                                        .getAssociatedRscSet(
                                                FieldPlanePairControl.this.fieldPlanePair)
                                        .add(rsc);

                                resourceMatcher.matchDefaultProductCapability(
                                        FieldPlanePairControl.this);

                                /*
                                 * listen for any resource state changes on our
                                 * new resource.
                                 */
                                rsc.getResourceData().addChangeListener(
                                        (IResourceDataChanged) FieldPlanePairControl.this);

                                setEmpty(false);

                                FieldPlanePairChooserControl.resourceCount++;
                            }
                        }
                    }
                });
            }
        }

    }

    class FieldPlanePairControllerMouseListener extends MouseAdapter {

        @Override
        public void mouseUp(MouseEvent e) {
            if (focusProvider != null) {
                focusProvider.giveEditorFocus();
            }
        }

    }

}
