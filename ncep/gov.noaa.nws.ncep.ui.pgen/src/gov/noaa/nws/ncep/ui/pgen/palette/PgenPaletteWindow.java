/*
 * gov.noaa.nws.ncep.ui.pgen.palette.PgenPaletteWindow
 * 
 * 25 November 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.palette;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil.PgenMode;
import gov.noaa.nws.ncep.ui.pgen.controls.CommandStackListener;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.filter.CategoryFilter;
import gov.noaa.nws.ncep.ui.pgen.gfa.PreloadGfaDataThread;
import gov.noaa.nws.ncep.ui.pgen.productmanage.ProductDialogStarter;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenSelectingTool;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.contexts.IContextActivation;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.raytheon.viz.ui.tools.AbstractModalTool;

/**
 * The PGEN View is used for all interaction with the objects in the PGEN
 * Resource, and it represents a current interactive "session". It is
 * responsible for managing many listeners as well as loading/unloading the
 * appropriate modal tools required to create and modify PGEN drawable objects
 * in the resource.
 * 
 * The Display of the View consists of many buttons representing a drawing
 * Palette. They allow users to pick specific objects, modify their attributes,
 * and create various products based on the geographic objects created.
 * 
 * PGEN can be run in one of two modes. SINGLE mode mimics the behavior in
 * legacy NAWIPS application NMAP, where a single PGEN ResourceData is displayed
 * on every editor. Any change made to the data objects in any one map editor
 * are reflected in all the others as well. Optionally, PGEN can be run in
 * MULTIPLE mode which allows any map editor to contain its own unique instance
 * of a PGEN resource.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		?		    S. Gilbert  Initial Creation.
 * 08/13		TTR696/774	J. Wu		Reset title/Close product manage dialog.
 * 11/13		#1081		B. Yin		Get selected DE to change front/line type.
 * 
 * </pre>
 * 
 * @author sgilbert
 * 
 */
public class PgenPaletteWindow extends ViewPart implements SelectionListener,
        DisposeListener, CommandStackListener, IPartListener2,
        ISelectedPanesChangedListener {

    /*
     * 1. Constants should be put in one place, probably in Utils.java. 2. The
     * number of column is a configurable constant. User should be able to
     * change it. We need to find a way to deal with these constants.
     */

    // private final int bgcolor = (178 * 65536 ) + ( 34 * 256 ) + 34;
    // private final int fgcolor = (255 * 65536 ) + ( 215 * 256 ) + 0;
    private final int bgcolor = (0 * 65536) + (0 * 256) + 255; // blue

    private final int fgcolor = (255 * 65536) + (255 * 256) + 255; // white

    private final String EXTENSION_POINT = "gov.noaa.nws.ncep.ui.pgen.palette";

    private final String CONTROL_SECTION = "control";

    private final String ACTION_SECTION = "action";

    private final String CLASS_SECTION = "class";

    private final String OBJECT_SECTION = "object";

    private final String CONTROL_LABEL = "Controls:";

    private final String ACTION_LABEL = "Actions:";

    private final String CLASS_LABEL = "Classes:";

    private final String OBJECT_LABEL = "Objects:";

    private IWorkbenchPage page;

    private Composite mainComp;

    private Composite paletteComp;

    private ScrolledComposite scroll;

    // Registered items with pgen.palette extension point
    private static IConfigurationElement[] paletteElements = null;

    // Map of all registered items with their name as the key
    private static HashMap<String, IConfigurationElement> itemMap = null;

    // List of items registered with the Control Section
    private ArrayList<String> controlNames = null;

    // List of items registered with the Action Section
    private ArrayList<String> actionNames = null;

    // List of items registered with the Class Section
    private ArrayList<String> classNames = null;

    // List of items registered with the Object Section
    private ArrayList<String> objectNames = null;

    private static Group objectBox;

    private Button undoButton = null;

    private Button redoButton = null;

    private String currentCategory = null;

    private String currentObject = null;

    private String currentAction = "";

    private HashMap<String, Button> buttonMap = null; // map of buttons
                                                      // currently displayed on
                                                      // palette

    private HashMap<String, Image> iconMap = null; // map of available icons

    private HashMap<String, Image> activeIconMap = null; // map of available
                                                         // "active" icons

    private List<String> buttonList = null; // Names of items that should appear
                                            // on the palette

    private IContextActivation pgenContextActivation;

    private AbstractEditor currentIsMultiPane = null;

    /**
     * Constructor
     * 
     */
    public PgenPaletteWindow() {

        super();

    }

    /**
     * Invoked by the workbench to initialize this View.
     */
    public void init(IViewSite site) {

        try {

            super.init(site);

        } catch (PartInitException pie) {

            pie.printStackTrace();

        }

        page = site.getPage();
        page.addPartListener(this);

        /*
         * Get a list from registry of all elements that registered with the
         * gov.noaa.nws.ncep.ui.pgen.palette extension point
         */
        if (paletteElements == null) {

            IExtensionRegistry registry = Platform.getExtensionRegistry();
            IExtensionPoint epoint = registry
                    .getExtensionPoint(EXTENSION_POINT);
            paletteElements = epoint.getConfigurationElements();

        }

        /*
         * create a hash map of the items registered with the
         * gov.noaa.nws.ncep.ui.pgen.palette extension point, using the item's
         * name attribute as the key
         */
        // itemMap = new HashMap<String, IConfigurationElement>(
        // paletteElements.length );
        itemMap = new LinkedHashMap<String, IConfigurationElement>(
                paletteElements.length);
        controlNames = new ArrayList<String>();
        actionNames = new ArrayList<String>();
        classNames = new ArrayList<String>();
        objectNames = new ArrayList<String>();

        for (int i = 0; i < paletteElements.length; i++) {

            // Add item to hash map
            String itemName = paletteElements[i].getAttribute("name");
            itemMap.put(itemName, paletteElements[i]);

            /*
             * create a list of item names that have been registered with each
             * section of the palette
             */
            String type = paletteElements[i].getName();
            if (type.equals(CONTROL_SECTION))
                controlNames.add(itemName);
            else if (type.equals(ACTION_SECTION))
                actionNames.add(itemName);
            else if (type.equals(CLASS_SECTION))
                classNames.add(itemName);
            else if (type.equals(OBJECT_SECTION))
                objectNames.add(itemName);

        }

        /*
         * create hashmaps that will keep track of the buttons that appear on
         * the palette along with their images.
         */
        buttonMap = new HashMap<String, Button>();
        iconMap = new HashMap<String, Image>();
        activeIconMap = new HashMap<String, Image>();

        /*
         * change the title to show cycle day and cycle hour
         */
        PgenCycleTool.updateTitle();

    }

    /**
     * Disposes resource. invoked by the workbench
     */
    public void dispose() {

        // System.out.println("Palette is being Disposed!!");
        /* TODO: save on exit? dialog */
        super.dispose();

        /*
         * remove product manage/layer dialog
         */
        PgenResource pgen = PgenSession.getInstance().getPgenResource();
        if (pgen != null) {
            pgen.closeDialogs();
        }

        /*
         * remove this palette from Pgen Session
         */
        PgenSession.getInstance().removePalette();

        /*
         * remove the workbench part listener
         */
        page.removePartListener(this);

        /*
         * clear map of SWT buttons on the palette
         */
        buttonMap.clear();

        /*
         * dispose of icons
         */
        // System.out.println("DISPOSING ICONS NOW ");
        for (Image icon : iconMap.values()) {
            icon.dispose();
        }
        for (Image icon : activeIconMap.values()) {
            icon.dispose();
        }

        // clear icon maps
        iconMap.clear();
        activeIconMap.clear();

        /*
         * change the title back to "CAVE"
         */
        // NmapUiUtils.resetCaveTitle();
        PgenUtil.resetCaveTitle();
    }

    /**
     * Invoked by the workbench, this method sets up the SWT controls for the
     * PGEN palette
     */
    @Override
    public void createPartControl(Composite comp) {

        mainComp = comp;
        scroll = new ScrolledComposite(comp, SWT.V_SCROLL | SWT.BORDER);
        paletteComp = new Composite(scroll, SWT.NONE);
        scroll.setContent(paletteComp);

        /*
         * Single column, no equal width.
         */
        paletteComp.setLayout(new GridLayout(1, false));

        /*
         * Add listener to scrolled composite to change palette size when scroll
         * size changes
         */
        scroll.addControlListener(new ControlAdapter() {
            public void controlResized(ControlEvent e) {
                Rectangle r = scroll.getClientArea();
                paletteComp.setSize(paletteComp.computeSize(r.width,
                        SWT.DEFAULT));
                paletteComp.layout();
            }
        });

        /*
         * create each section of the palette
         */
        resetPalette(null);

        /*
         * Set this palette with the Pgen Session
         */
        PgenSession.getInstance().setPalette(this);

        /*
         * Check current editor for a PgenResource. If found, register it with
         * the PgenSession
         */
        PgenResource current = PgenUtil.findPgenResource(null);
        if (current != null)
            PgenSession.getInstance().setResource(current);

    }

    /**
     * create section of Palette where Objects will be displayed later
     * 
     * @param parent
     *            parent widget
     */
    private void createObjectSection(Composite parent) {

        Label control = new Label(parent, SWT.NONE);
        control.setText(OBJECT_LABEL);

        objectBox = new Group(parent, SWT.SHADOW_IN);
        // objectBox.setLayout( new GridLayout( NCOLUMN, true ) );
        objectBox.setLayout(new RowLayout(SWT.HORIZONTAL));
        objectBox.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

    }

    /**
     * Creates a section of the palette window and adds buttons for each item
     * that was registered with the specified section
     * 
     * @param parent
     *            - parent Widget
     * @param section
     *            - string indicating which section of the palette is being
     *            built
     */
    private void createPaletteSection(Composite parent, String section) {

        /*
         * Create label for the section
         */
        Label control = new Label(parent, SWT.NONE);
        control.setText(section);

        /*
         * create new composite widget for buttons and set a GridLayout
         */
        Group box = new Group(parent, SWT.SHADOW_IN);

        // box.setLayout( new GridLayout( NCOLUMN, true ) );
        box.setLayout(new RowLayout(SWT.HORIZONTAL));
        box.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        /*
         * Get a list of buttons that are registered for this section of the
         * palette
         */
        List<String> buttons = null;
        if (section.equals(CONTROL_LABEL))
            buttons = getControlNames();
        else if (section.equals(ACTION_LABEL))
            buttons = getActionNames();
        else if (section.equals(CLASS_LABEL))
            buttons = getClassNames();

        /*
         * Loop through each item registered with this section, and add the item
         * to the palette if it is also in the buttonList. If the buttonList is
         * null, add all items.
         */
        for (String bname : buttons) {

            IConfigurationElement element = itemMap.get(bname);

            /*
             * determine if button should be added to palette
             */
            String always = element.getAttribute("alwaysVisible");
            if ((always == null) || always.equalsIgnoreCase("false")) {
                if (buttonList != null) {
                    if (!buttonList.contains(bname))
                        continue;
                }
            }

            Button item = new Button(box, SWT.PUSH);

            /*
             * set label of button
             */
            if (element.getAttribute("label") != null)
                item.setToolTipText(element.getAttribute("label"));

            /*
             * create an icon image for the button, if an icon was specified in
             * the registered item.
             */
            if (element.getAttribute("icon") != null) {

                Image icon = getIcon(element.getAttribute("icon"));

                if (icon != null) {

                    item.setImage(icon);
                    item.addDisposeListener(this);

                } else {

                    // No icon available. Set text to display on button
                    item.setText(element.getAttribute("name"));

                }
            } else {

                // No icon available. Set text to display on button
                item.setText(element.getAttribute("name"));

            }

            /*
             * set the ConfigurationElement name in the button, so that all the
             * endpoint info can be accessed by the widgetSelected listener
             */
            item.setData(element.getAttribute("name"));
            item.addSelectionListener(this);

            // Add button name to map of all buttons currently displayed in the
            // palette
            buttonMap.put(element.getAttribute("name"), item);

            /*
             * Save references to Undo and Redo buttons for future use
             */
            if (item.getData().equals("Undo")) {
                undoButton = item;
            }
            if (item.getData().equals("Redo")) {
                redoButton = item;
            }

        }
        box.pack();
        box.redraw();

    }

    /**
     * Resets the Pgen Palette to display only the buttons specified in the
     * buttonNames list
     * 
     * @param buttonNames
     *            list of item Names that should be displayed on the palette. If
     *            null, display all possible buttons.
     */
    public void resetPalette(List<String> buttonNames) {

        // save for later use
        buttonList = buttonNames;

        /*
         * Dispose of all widgets currently in the palette
         */
        Control[] kids = paletteComp.getChildren();
        for (int j = 0; j < kids.length; j++) {
            kids[j].dispose();
        }

        /*
         * create each section of the palette
         */
        createPaletteSection(paletteComp, CONTROL_LABEL);
        createPaletteSection(paletteComp, ACTION_LABEL);
        createPaletteSection(paletteComp, CLASS_LABEL);
        createObjectSection(paletteComp);

        /*
         * Force a resize
         */
        Rectangle r = scroll.getClientArea();
        paletteComp.setSize(paletteComp.computeSize(r.width, SWT.DEFAULT));
        paletteComp.layout();

        // wait for buttons to be created
        disableUndoRedo();
    }

    /**
     * Disable the Undo and Redo buttons on the palette
     */
    public void disableUndoRedo() {
        undoButton.setEnabled(false);
        redoButton.setEnabled(false);
    }

    /**
     * Invoked by the workbench when needed
     */
    @Override
    public void setFocus() {

        mainComp.setFocus();

    }

    /**
     * Invoked when SWT item is selected
     */
    public void widgetSelected(SelectionEvent se) {

        IEditorPart editor = VizWorkbenchManager.getInstance()
                .getActiveEditor();
        if (editor instanceof AbstractEditor) {// && ((NCMapEditor)
                                               // editor).getApplicationName().equals("NA")
                                               // ) {

            /*
             * get the endpoint information associated with this button.
             */
            Button btn = (Button) se.getSource();
            IConfigurationElement elem = itemMap.get(btn.getData());

            /*
             * get the section of the palette that this item was registered with
             */
            String point = elem.getName();

            /*
             * If the button selected is in the "control", "action", or "object"
             * section of the palette, then execute the command that is
             * registered with the commandId set for this button.
             */
            if (point.equals(CONTROL_SECTION) || point.equals(ACTION_SECTION)
                    || point.equals(OBJECT_SECTION)) {

                if (point.equals(OBJECT_SECTION)
                        && currentAction.equalsIgnoreCase("MultiSelect")) {
                    if (currentCategory != null
                            && currentCategory.equalsIgnoreCase("MET")) {
                        if (currentObject != null) {
                            resetIcon(currentObject);
                        }

                        currentObject = elem.getAttribute("name");
                        setActiveIcon(currentObject);

                    }
                    elem = itemMap.get("MultiSelect");
                } else if (currentObject != null) {
                    resetIcon(currentObject);
                }

                // change front/line type
                PgenSelectingTool selTool = null;
                if (point.equals(OBJECT_SECTION)
                        && currentAction.equalsIgnoreCase("Select")
                        && (currentCategory.equalsIgnoreCase("Front") || currentCategory
                                .equalsIgnoreCase("Lines"))) {
                    AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                            .getCurrentPerspectiveManager();
                    for (AbstractModalTool tool : mgr.getToolManager()
                            .getSelectedModalTools()) {
                        // check selecting tool and change front/line type
                        if (tool instanceof PgenSelectingTool) {
                            DrawableElement currentDe = ((PgenSelectingTool) tool)
                                    .getSelectedDE();
                            if (currentDe != null
                                    && (currentDe.getPgenCategory()
                                            .equalsIgnoreCase("Lines") || currentDe
                                            .getPgenCategory()
                                            .equalsIgnoreCase("Front"))) {
                                selTool = (PgenSelectingTool) tool;
                            }
                            break;
                        }
                    }
                }

                if (selTool != null) {
                    selTool.changeSelectedLineType(elem.getAttribute("name"));
                } else {
                    // clean up
                    PgenResource pgen = PgenUtil
                            .findPgenResource((AbstractEditor) editor);
                    if (pgen != null) {
                        pgen.removeGhostLine();
                        pgen.removeSelected();
                        pgen.deactivatePgenTools();
                    }

                    exeCommand(elem);

                    if (point.equals(ACTION_SECTION)) {
                        currentAction = elem.getAttribute("name");
                    }
                }
            } else if (point.equals(CLASS_SECTION)) {
                /*
                 * If a button in the "Class" section of the palette was
                 * pressed, unload the current set of buttons in the Object
                 * section, and load the object buttons registered as part of
                 * the class selected.
                 */

                // remove currently loaded buttons from the Object section
                org.eclipse.swt.widgets.Control[] kids = objectBox
                        .getChildren();
                for (int j = 0; j < kids.length; j++) {
                    kids[j].dispose();
                }

                // reset the previous category's button icon
                if (currentCategory != null) {
                    resetIcon(currentCategory);
                }

                currentCategory = elem.getAttribute("name");

                // display "active" icon on the current Class's button
                setActiveIcon(currentCategory);

                /*
                 * Loop threough each object registered with the current
                 * Class/Category
                 */
                for (String bname : getObjectNames(currentCategory)) {

                    IConfigurationElement element = itemMap.get(bname);

                    /*
                     * determine if button should be added to palette
                     */
                    if (buttonList != null) {
                        if (!buttonList.contains(bname))
                            continue;
                    }

                    Button item = new Button(objectBox, SWT.PUSH);

                    // Add button label
                    if (element.getAttribute("label") != null)
                        item.setToolTipText(element.getAttribute("label"));

                    /*
                     * create an icon image for the button, if an icon was
                     * specified in the registered item.
                     */
                    if (element.getAttribute("icon") != null) {

                        Image icon = getIcon(element.getAttribute("icon"));
                        item.setImage(icon);
                        item.addDisposeListener(this);

                    } else {

                        // No icon available. Set text to display on button
                        item.setText(element.getAttribute("name"));

                    }

                    // set the ConfigurationElement name in the button, add to
                    // map of currently
                    // displayed buttons
                    item.setData(element.getAttribute("name"));
                    item.addSelectionListener(this);
                    buttonMap.put(element.getAttribute("name"), item);

                    objectBox.setSize(objectBox.computeSize(SWT.DEFAULT,
                            SWT.DEFAULT, true));
                    objectBox.pack();
                    objectBox.layout(true);
                    objectBox.redraw();
                }

                /*
                 * if multiSelect is current tool, reload it now after category
                 * selection
                 */
                if (currentAction != null) {
                    if (currentAction.isEmpty())
                        currentAction = "Select";
                    if (currentAction.equalsIgnoreCase("Select")
                            || currentAction.equalsIgnoreCase("MultiSelect")
                            || currentAction.equalsIgnoreCase("Copy")
                            || currentAction.equalsIgnoreCase("Move")
                            || currentAction.equalsIgnoreCase("Modify")
                            || currentAction.equalsIgnoreCase("Connect")
                            || currentAction.equalsIgnoreCase("Rotate")
                            || currentAction.equalsIgnoreCase("Flip")
                            || currentAction.equalsIgnoreCase("Extrap")
                            || currentAction.equalsIgnoreCase("Interp")) {
                        elem = itemMap.get(currentAction);
                        if (elem != null)
                            exeCommand(elem);
                    }
                }

                Rectangle r = scroll.getClientArea();
                paletteComp.setSize(paletteComp.computeSize(r.width,
                        SWT.DEFAULT));
                paletteComp.layout();
            }
        } else {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            // The data not loaded yet
            MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);

            mb.setMessage("Pgen is not supported in this editor. Please select a mapEditor for Pgen to use first!");
            mb.open();
        }
    }

    public void widgetDefaultSelected(SelectionEvent se) {

    }

    /*
     * invoked when widget is disposed
     * 
     * @see
     * org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt
     * .events.DisposeEvent)
     */
    public void widgetDisposed(DisposeEvent event) {

        /*
         * If a button is being disposed, remove it from the map of currently
         * displayed items in the palette
         */
        if (event.getSource() instanceof Button) {

            Button btn = (Button) event.getSource();
            buttonMap.remove(btn.getData());

        }

    }

    /**
     * Called by the PgenCommandManager when its stack sizes change, when this
     * object is registered with the PgenCommandManager. Disables Undo and/or
     * Redo button when the stack is empty. Enables the button otherwise.
     */
    public void stacksUpdated(int undoSize, int redoSize) {

        if (undoButton != null) {
            if (undoSize <= 0)
                undoButton.setEnabled(false);
            else
                undoButton.setEnabled(true);
        }

        if (redoButton != null) {
            if (redoSize <= 0)
                redoButton.setEnabled(false);
            else
                redoButton.setEnabled(true);
        }

    }

    /**
     * Workbench part was activated. If it was an instance of NCMapEditor and
     * there is an instance of PgenResource for that editor, register it with
     * the PgenSession singleton.
     */
    @Override
    public void partActivated(IWorkbenchPartReference partRef) {
        IWorkbenchPart part = partRef.getPart(false);
        // System.out.println("Something Activated: "+part.getClass().getCanonicalName()
        // );
        // if ( part instanceof NCMapEditor &&((NCMapEditor)
        // part).getApplicationName().equals("NA")) {

        if (PgenUtil.isNatlCntrsEditor(part)) {

            PgenResource rsc = PgenUtil.findPgenResource((AbstractEditor) part);
            if ((rsc == null) && (PgenUtil.getPgenMode() == PgenMode.SINGLE))
                rsc = PgenUtil.createNewResource();
            if (rsc != null)
                rsc.setCatFilter(new CategoryFilter(
                        (currentCategory == null) ? "Any" : currentCategory));
            PgenSession.getInstance().setResource(rsc);

            AbstractEditor editor = (AbstractEditor) part;
            // if ( editor.getNumberofPanes() > 1 ) {
            if (PgenUtil.getNumberofPanes(editor) > 1) {
                currentIsMultiPane = editor;
                // editor.addSelectedPaneChangedListener( this );
                PgenUtil.addSelectedPaneChangedListener(editor, this);
            }
            activatePGENContext();
        }

        else if (part instanceof PgenPaletteWindow) {
            activatePGENContext();

            // found NCMapEditor
            // AbstractEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();
            AbstractEditor editor = PgenUtil.getActiveEditor();
            if (editor != null) {
                IRenderableDisplay display = editor.getActiveDisplayPane()
                        .getRenderableDisplay();
                ResourceList rscList = display.getDescriptor()
                        .getResourceList();

                for (ResourcePair rp : rscList) {

                    if (rp != null && rp.getResource() instanceof PgenResource) {
                        ((PgenResource) (rp.getResource())).setEditable(true);
                        if (!rp.getProperties().isVisible())
                            rp.getProperties().setVisible(true);
                    }
                }
                editor.refresh();
            }
        }

    }

    /**
     * Workbench part was brought on top. If it was an instance of NCMapEditor
     * and there is an instance of PgenResource for that editor, register it
     * with the PgenSession singleton.
     * 
     */
    @Override
    public void partBroughtToTop(IWorkbenchPartReference partRef) {
        IWorkbenchPart part = partRef.getPart(false);
        // System.out.println("Something BroughtToTop: "+part.getClass().getCanonicalName()
        // );
        partActivated(partRef);

        if (PgenUtil.isNatlCntrsEditor(part)) {
            AbstractEditor editor = (AbstractEditor) part;
            PgenResource rsc = PgenUtil.findPgenResource((AbstractEditor) part);

            if ((rsc != null) && (PgenUtil.getPgenMode() == PgenMode.SINGLE)
                    && (PgenUtil.doesLayerLink())) {

                NcDisplayName dispName = PgenUtil.getDisplayName(editor);

                if (dispName != null) { // sanity check
                    if (dispName.getId() > 0 && rsc != null) {

                        Product prod = rsc.getActiveProduct();
                        if (dispName.getId() <= prod.getLayers().size()) {
                            rsc.setActiveLayer(prod.getLayer(dispName.getId() - 1));
                        }
                    }
                }
            }

            // Open Product or Layer management dialog if necessary
            if (rsc != null)
                VizApp.runAsync(new ProductDialogStarter(rsc));
        }

    }

    @Override
    public void partClosed(IWorkbenchPartReference partRef) {
        IWorkbenchPart part = partRef.getPart(false);
        // System.out.println("Something Closed: "+part.getClass().getCanonicalName()
        // );

        if (part instanceof PgenPaletteWindow) {
            // if SINGLEMODE, foreach editor; remove pgen rsc
            if (PgenUtil.getPgenMode() == PgenMode.SINGLE) {
                PgenUtil.resetResourceData();
                if (VizPerspectiveListener.getCurrentPerspectiveManager() == null)
                    return; // workbench probably closing

                AbstractEditor[] editors = UiUtil.getEditors(PlatformUI
                        .getWorkbench().getActiveWorkbenchWindow(),
                        VizPerspectiveListener.getCurrentPerspectiveManager()
                                .getPerspectiveId());
                /*
                 * UiUtil.getEditors returns active editor first. Run through
                 * list in reverse so that active editor is processed last.
                 */
                for (int i = editors.length - 1; i >= 0; i--) {
                    unloadPgenResource(editors[i]);
                }
            }

            // if ( currentIsMultiPane != null )
            // currentIsMultiPane.removeSelectedPaneChangedListener( this );
            if (currentIsMultiPane != null) {
                PgenUtil.removeSelectedPaneChangedListener(currentIsMultiPane,
                        this);
            }
        } else if (PgenUtil.isNatlCntrsEditor(part)) {
            PgenResource pgen = PgenUtil
                    .findPgenResource((AbstractEditor) part);
            if (pgen != null) {
                pgen.closeDialogs();
            }
        }
    }

    @Override
    public void partDeactivated(IWorkbenchPartReference partRef) {
        IWorkbenchPart part = partRef.getPart(false);
        // System.out.println("Something Deactivated: "+part.getClass().getCanonicalName()
        // );

        if (PgenUtil.isNatlCntrsEditor(part)) {

            PgenResource pgen = PgenUtil
                    .findPgenResource((AbstractEditor) part);
            if (pgen != null) {

                // Comment out the following three lines to keep the drawing
                // tool and to keep the attribute dialog up
                // when user clicks on the blank space on PGEN pallete.
                // --bingfan 4/20/12

                // pgen.removeGhostLine();
                // pgen.removeSelected();
                // pgen.deactivatePgenTools();

                // not sure why closeDialogs() is put here and not sure why it's
                // commented out. --bingfan
                // pgen.closeDialogs();

                deactivatePGENContext();
                ((AbstractEditor) part).refresh();
            }

            AbstractEditor editor = (AbstractEditor) part;
            // if ( editor.getNumberofPanes() > 1 ) {
            if (PgenUtil.getNumberofPanes(editor) > 1) {
                currentIsMultiPane = null;
                // editor.removeSelectedPaneChangedListener( this );
                PgenUtil.removeSelectedPaneChangedListener(editor, this);
            }

        }

        else if (part instanceof PgenPaletteWindow) {
            deactivatePGENContext();
        }

    }

    @Override
    public void partOpened(IWorkbenchPartReference partRef) {
        // System.out.println("Something Opened: "+part.getClass().getCanonicalName()
        // );
        IWorkbenchPart part = partRef.getPart(false);
        if (part instanceof PgenPaletteWindow) {
            ((PgenPaletteWindow) part).setPartName("PGEN");
        }
    }

    @Override
    public void partHidden(IWorkbenchPartReference partRef) {
        IWorkbenchPart part = partRef.getPart(false);
        // System.out.println("Something Hidden: "+part.getClass().getCanonicalName()
        // );

        if (PgenUtil.isNatlCntrsEditor(part)) {
            PgenResource pgen = PgenUtil
                    .findPgenResource((AbstractEditor) part);
            if (pgen != null) {
                pgen.closeDialogs();
            }
        }
    }

    @Override
    public void partInputChanged(IWorkbenchPartReference partRef) {
        // TODO Auto-generated method stub
    }

    @Override
    public void partVisible(IWorkbenchPartReference partRef) {
        IWorkbenchPart part = partRef.getPart(false);
        // System.out.println("Something Opened: "+part.getClass().getCanonicalName()
        // );
        if (PgenUtil.isNatlCntrsEditor(part) && !PreloadGfaDataThread.loaded) {
            // preload the classes to reduce the first GFA format time
            new PreloadGfaDataThread().start();
        }

    }

    private void unloadPgenResource(AbstractEditor editor) {

        for (IRenderableDisplay display : UiUtil
                .getDisplaysFromContainer(editor)) {
            for (ResourcePair rp : display.getDescriptor().getResourceList()) {
                if (rp.getResource() instanceof PgenResource) {
                    PgenResource rsc = (PgenResource) rp.getResource();
                    rsc.unload();
                    display.getDescriptor().getResourceList()
                            .removePreRemoveListener(rsc);
                }
            }
        }
    }

    /**
     * 
     * @return the currently selected category on the palette
     */
    public String getCurrentCategory() {
        return currentCategory;
    }

    /*
     * Returns an icon image based on it's location
     */
    private Image getIcon(String iconLocation) {

        /*
         * If icon already loaded, use it
         */
        if (iconMap.containsKey(iconLocation))
            return iconMap.get(iconLocation);

        else {
            /*
             * load icon image from location specified.
             */
            ImageDescriptor id = Activator.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, iconLocation);
            if (id != null) {
                Image icon = id.createImage();
                // add it to the available icon map
                iconMap.put(iconLocation, icon);
                return icon;
            } else
                return null;
        }
    }

    public String getCurrentAction() {
        return currentAction;
    }

    /*
     * Finds the button with the given name, and sets its image to an "active"
     * version of the icon
     */
    public void setActiveIcon(String name) {

        // if name not recognized, do nothing
        if (!itemMap.containsKey(name))
            return;
        // if button not currently displayed on palette, do nothing
        if (!buttonMap.containsKey(name))
            return;

        String iconLocation = itemMap.get(name).getAttribute("icon");

        /*
         * If an active version of the icon exists, use it
         */
        if (activeIconMap.containsKey(iconLocation)) {
            Image im = activeIconMap.get(iconLocation);
            buttonMap.get(name).setImage(im);
        } else {
            /*
             * create an "active" version of the icon from the original.
             */
            Image im = iconMap.get(iconLocation);
            ImageData id = im.getImageData();

            for (int y = 0; y < id.height; y++) {
                for (int x = 0; x < id.width; x++) {
                    if (id.getPixel(x, y) == 0)
                        id.setPixel(x, y, fgcolor);
                    else
                        id.setPixel(x, y, bgcolor);
                }
            }

            // set "active" icon on button, and save it for later use.
            Image icon = new Image(im.getDevice(), id);
            buttonMap.get(name).setImage(icon);
            activeIconMap.put(iconLocation, icon);
        }

    }

    /*
     * Finds the button with the given name, and sets its image to the original
     * icon specified with the extension point
     */
    public void resetIcon(String name) {

        // if name not recognized, do nothing
        if (!itemMap.containsKey(name))
            return;
        // if button not currently displayed on palette, do nothing
        if (!buttonMap.containsKey(name))
            return;

        IConfigurationElement elem = itemMap.get(name);

        /*
         * reset to original icon
         */
        Image icon = getIcon(elem.getAttribute("icon"));
        if (icon != null) {
            buttonMap.get(name).setImage(icon);
        }
    }

    /*
     * Sets up an eclipse Command and ExecuteEvent for a registered commandId,
     * and then executes it.
     */
    private void exeCommand(IConfigurationElement elem) {

        // Get the commandId for this item
        String commandId = elem.getAttribute("commandId");
        /*
         * This code taken directly from
         * com.raytheon.viz.ui.glmap.actions.ClearAction
         * 
         * Finds the AbstractHandler currently registered with this commandId
         */
        IEditorPart part = VizWorkbenchManager.getInstance().getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service.getCommand(commandId);

        if (cmd != null) {

            try {

                /*
                 * Set up information to pass to the AbstractHandler
                 */
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", elem.getAttribute("name"));
                params.put("className", elem.getAttribute("className"));
                ExecutionEvent exec = new ExecutionEvent(cmd, params, null,
                        elem.getAttribute("name"));

                // Execute the handler
                cmd.executeWithChecks(exec);

                // Update the GUI elements on the menus and toolbars
                for (String toolbarID : NmapCommon
                        .getGUIUpdateElementCommands()) {
                    service.refreshElements(toolbarID, null);
                }

            } catch (Exception e) {
                // Error executing Handler

                e.printStackTrace();
                String msg = "Could not set PGEN drawing mode for the current map";
                ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                        "Error Activating PGEN" + " Tool", msg, new Status(
                                Status.ERROR, Activator.PLUGIN_ID, msg, e));
            }
        }

    }

    /**
     * 
     * @return A list of names for the available buttons in the Control Section
     *         of the Palette
     */
    public List<String> getControlNames() {
        return controlNames;
    }

    /**
     * 
     * @return A list of names for the available buttons in the Action Section
     *         of the Palette
     */
    public List<String> getActionNames() {
        return actionNames;
    }

    /**
     * 
     * @return A list of names for the available buttons in the Class Section of
     *         the Palette
     */
    public List<String> getClassNames() {
        return classNames;
    }

    /**
     * 
     * @return A list of names for the available buttons in the Object Section
     *         of the Palette
     */
    public List<String> getObjectNames() {
        return objectNames;
    }

    /**
     * @param Name
     *            of a category in the class section
     * @return A list of names for the available buttons in the Object Section
     *         of the Palette associated with the given Class/Category
     */
    public List<String> getObjectNames(String className) {
        ArrayList<String> objs = new ArrayList<String>();
        for (String name : getObjectNames()) {
            if (itemMap.get(name).getAttribute("className").equals(className))
                objs.add(name);
        }
        return objs;
    }

    /**
     * @param Name
     *            of a button in the palette
     * @return The icon image associated with the button
     */
    public Image getButtonImage(String bname) {

        return getIcon(itemMap.get(bname).getAttribute("icon"));

    }

    /**
     * @param Name
     *            of a button in the palette
     * @return The icon image associated with the button
     */
    public Image createNewImage(Image im, int fg, int bg) {

        /*
         * create an "active" version of the icon from the original.
         */
        ImageData id = im.getImageData();

        for (int ii = 0; ii < id.height; ii++) {
            for (int jj = 0; jj < id.width; jj++) {
                if (id.getPixel(jj, ii) == 0)
                    id.setPixel(jj, ii, fg);
                else
                    id.setPixel(jj, ii, bg);
            }
        }

        // create a new Image.
        return (new Image(im.getDevice(), id));
    }

    /**
     * @param none
     * @return itemMap
     */
    public HashMap<String, IConfigurationElement> getItemMap() {
        return itemMap;
    }

    private void deactivatePGENContext() {
        if (pgenContextActivation != null) {

            IContextService ctxSvc = (IContextService) PlatformUI
                    .getWorkbench().getService(IContextService.class);
            ctxSvc.deactivateContext(pgenContextActivation);
            // System.out.println("Deactivated " +
            // pgenContextActivation.getContextId());
            pgenContextActivation = null;
        }
    }

    private void activatePGENContext() {
        if (pgenContextActivation == null) {
            IContextService ctxSvc = (IContextService) PlatformUI
                    .getWorkbench().getService(IContextService.class);
            pgenContextActivation = ctxSvc
                    .activateContext("gov.noaa.nws.ncep.ui.pgen.pgenContext");
            // System.out.println("Activated " +
            // pgenContextActivation.getContextId());
        }
    }

    /**
     * Set PGEN default action as "Select"
     */
    public void setDefaultAction() {
        currentAction = "Select";
    }

    @Override
    public void selectedPanesChanged(String id, IDisplayPane[] pane) {
        // @Override
        // public void paneSelected(NCMapEditor editor, ArrayList<NCDisplayPane>
        // panes) {
        // TODO Auto-generated method stub
        // System.out.println("YOYOYYOYYOYYYOY");

        // PgenResource rsc = PgenUtil.findPgenResourceInPane(panes.get(0));
        // if ( rsc != null ) PgenSession.getInstance().setResource(rsc);
        // }
    }

    /**
     * @return the currentObject
     */
    public String getCurrentObject() {
        return currentObject;
    }
}
