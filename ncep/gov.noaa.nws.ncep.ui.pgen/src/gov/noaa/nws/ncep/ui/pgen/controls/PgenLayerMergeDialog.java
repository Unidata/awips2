/*
 * gov.noaa.nws.ncep.ui.pgen.controls.PgenLayerMergeDialog
 * 
 * July 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.controls;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * This class creates a dialog to allow the use import/merge layers from a
 * selected activity to the current activity's layers.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/12        #593     	J. Wu  		Initial Creation
 * 08/13		?			B. Yin		Merge outlook when layers are merged
 * 12/13        TTR776      J. Wu       Check default common action set in preference.
 * 
 * </pre>
 * 
 * @author J. Wu
 */

public class PgenLayerMergeDialog extends Dialog {

    private Composite top;

    /*
     * Available actions for each layer.
     */
    private static enum LayerActions {
        NO_ACTION, ADD_AS_NEW_LAYER, REPLACE_LIKE_NAME_LAYER, REPLACE_ACTIVE_LAYER, REPLACE_RENAME_ACTIVE_LAYER, MERGE_INTO_LIKE_NAME_LAYER, MERGE_INTO_ACTIVE_LAYER, MERGE_RENAME_ACTIVE_LAYER
    };

    /*
     * Bulk actions names and definitions.
     */
    private static String[] BulkActionNames = new String[] {
            "Take no action",
            "Reset all layers to 'Take no action'",
            "Set all layers to 'Add as a new layer'",
            "Set all layers to 'Replace content of like-named layers', if available",
            "Set all layers to 'Merge content into like-named layers', if available",
            "Set all layers to 'Merge into ActiveLayer'" };

    private final static int BULK_NO_ACTION = 0;

    private final static int BULK_RESET_ALL_NO_ACTION = 1;

    private final static int BULK_ADD_ALL_AS_NEW = 2;

    private final static int BULK_REPLACE_ALL_LIKE_NAME_LAYER = 3;

    private final static int BULK_MERGE_ALL_LIKE_NAME_LAYER = 4;

    private final static int BULK_MERGE_ALL_INTO_ACTIVE_LAYER = 5;

    /*
     * Others.
     */
    protected PgenResource drawingLayer = null;

    protected Product incomingActivity = null;

    protected String incomingFile = null;

    private LinkedHashMap<Layer, Combo> layerComboMap = null;

    private static Point shellLocation;

    /**
     * AttrDlg constructor
     * 
     * @param parShell
     * @throws VizException
     */
    public PgenLayerMergeDialog(Shell parShell, Product prodIn, String fileIn)
            throws VizException {

        super(parShell);
        this.setShellStyle(SWT.TITLE | SWT.PRIMARY_MODAL);

        drawingLayer = PgenSession.getInstance().getPgenResource();
        incomingActivity = prodIn;
        incomingFile = fileIn;

        layerComboMap = new LinkedHashMap<Layer, Combo>();

    }

    @Override
    public void createButtonsForButtonBar(Composite parent) {
        super.createButtonsForButtonBar(parent);
    }

    @Override
    public Control createButtonBar(Composite parent) {

        Control bar = super.createButtonBar(parent);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        bar.setLayoutData(gd);
        return bar;

    }

    /*
     * Called when "X" button on window is clicked.
     * 
     * @see org.eclipse.jface.window.Window#handleShellCloseEvent()
     */
    @Override
    public void handleShellCloseEvent() {
        super.handleShellCloseEvent();
        PgenUtil.setSelectingMode();
    }

    /**
     * Set the location of the dialog
     */
    public int open() {

        if (this.getShell() == null) {
            this.create();
        }

        if (shellLocation == null) {
            this.getShell().setLocation(
                    this.getShell().getParent().getLocation());
        } else {
            getShell().setLocation(shellLocation);
        }

        return super.open();

    }

    /**
     * Save location of the dialog.
     */
    public boolean close() {
        if (getShell() != null) {
            Rectangle bounds = getShell().getBounds();
            shellLocation = new Point(bounds.x, bounds.y);
        }
        return super.close();
    }

    /**
     * Creates the dialog area
     */
    @Override
    public Control createDialogArea(Composite parent) {

        top = (Composite) super.createDialogArea(parent);
        this.getShell().setText("Contours Attributes");

        // Create the main layout for the dialog.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 5;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);

        // Initialize all of the menus, controls, and layouts
        initializeComponents();

        return top;
    }

    /**
     * Creates buttons, menus, and other controls in the dialog area
     */
    private void initializeComponents() {

        this.getShell().setText("Import/Merge Layers");

        Composite topComp = new Composite(top, SWT.NONE);
        topComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        GridLayout layout0 = new GridLayout(1, false);
        topComp.setLayout(layout0);

        Label advLbl = new Label(topComp, SWT.CENTER);
        advLbl.setText("ADVANCED");

        // Present basic info for current activity/layer.
        Composite curActComp = new Composite(top, SWT.NONE);
        curActComp.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, true,
                false));

        GridLayout layout = new GridLayout(2, true);
        curActComp.setLayout(layout);

        Group curActGrp = new Group(curActComp, SWT.SHADOW_ETCHED_OUT);
        GridLayout layout1 = new GridLayout(1, false);
        curActGrp.setLayout(layout1);

        curActGrp.setText("Current Activity");

        Label lbl1 = new Label(curActGrp, SWT.NONE);
        lbl1.setText(drawingLayer.getActiveProduct().getName());

        Group curLyrGrp = new Group(curActComp, SWT.SHADOW_ETCHED_OUT);
        GridLayout layout2 = new GridLayout(1, false);
        curLyrGrp.setLayout(layout2);

        curLyrGrp.setText("Active Layer");

        Label lbl2 = new Label(curLyrGrp, SWT.NONE);
        lbl2.setText(drawingLayer.getActiveLayer().getName());

        // Present basic info for selected file.
        Composite selActComp = new Composite(top, SWT.NONE);
        selActComp.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, true,
                false));

        GridLayout layout3 = new GridLayout(1, false);
        selActComp.setLayout(layout3);

        Group selActGrp = new Group(selActComp, SWT.SHADOW_ETCHED_OUT);
        GridLayout layout4 = new GridLayout(1, false);
        selActGrp.setLayout(layout4);

        selActGrp.setText("Selected File Activity and Filename");

        Label lbl3 = new Label(selActGrp, SWT.NONE);
        lbl3.setText(incomingActivity.getName() + " { " + incomingFile + " }");

        // Present actions for each incoming layer
        Composite layersComp = new Composite(top, SWT.NONE);
        layersComp.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, true,
                false));

        GridLayout layout5 = new GridLayout(1, false);
        layersComp.setLayout(layout5);

        Group layersGrp = new Group(layersComp, SWT.SHADOW_ETCHED_OUT);
        GridLayout layout6 = new GridLayout(2, false);
        layersGrp.setLayout(layout6);

        layersGrp.setText("Incoming Layers");

        for (Layer lyr : incomingActivity.getLayers()) {
            Label lbl4 = new Label(layersGrp, SWT.NONE);
            lbl4.setText(lyr.getName());

            Combo layerActCombo = new Combo(layersGrp, SWT.DROP_DOWN
                    | SWT.READ_ONLY);

            boolean sameLayerExist = layerExist(lyr.getName());
            layerActCombo
                    .add("Take no action                                                                                   ");
            layerActCombo.add("Add as layer '" + lyr.getName() + "'");

            if (sameLayerExist) {
                layerActCombo.add("Replace content of '" + lyr.getName() + "'");
            }

            layerActCombo.add("Replace content of 'ActiveLayer'");
            layerActCombo
                    .add("Replace content of 'ActiveLayer' and rename to '"
                            + lyr.getName() + "'");

            if (sameLayerExist) {
                layerActCombo.add("Merge content into '" + lyr.getName() + "'");
            }

            layerActCombo.add("Merge content into 'ActiveLayer'");
            layerActCombo
                    .add("Merge content into 'ActiveLayer' and rename to '"
                            + lyr.getName() + "'");

            layerActCombo.select(0);

            layerComboMap.put(lyr, layerActCombo);
        }

        // Add bulk actions if there are two or more layers.
        if (incomingActivity.getLayers().size() > 1) {
            Label lbl4 = new Label(layersGrp, SWT.NONE);
            lbl4.setText("Bulk Action");

            Combo bulkCombo = new Combo(layersGrp, SWT.DROP_DOWN
                    | SWT.READ_ONLY);

            for (String str : BulkActionNames) {
                bulkCombo.add(str);
            }

            bulkCombo.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    performBulkActions(((Combo) e.widget).getSelectionIndex());
                }

            });

            bulkCombo.select(PgenUtil.getLayerMergeOption());
        }

        /*
         * Set the default for each layer to the common action set in PGEN
         * preference.
         */
        performBulkActions(PgenUtil.getLayerMergeOption());

    }

    /*
     * Check if a layer of the given layer existing in the current activity.
     */
    private boolean layerExist(String layerName) {
        return (drawingLayer.getActiveProduct().getLayer(layerName) != null);
    }

    /*
     * Perform selected "Bulk" action to each layer.
     */
    private void performBulkActions(int actionNum) {
        switch (actionNum) {
        case BULK_NO_ACTION:
            break;

        case BULK_RESET_ALL_NO_ACTION:
            for (Combo cmb : layerComboMap.values()) {
                cmb.select(0);
            }
            break;

        case BULK_ADD_ALL_AS_NEW:
            for (Combo cmb : layerComboMap.values()) {
                cmb.select(1);
            }

            break;

        case BULK_REPLACE_ALL_LIKE_NAME_LAYER:
            for (Combo cmb : layerComboMap.values()) {
                if (cmb.getItemCount() > 6) {
                    cmb.select(2);
                }
            }

            break;
        case BULK_MERGE_ALL_LIKE_NAME_LAYER:
            for (Combo cmb : layerComboMap.values()) {
                if (cmb.getItemCount() > 6) {
                    cmb.select(5);
                }
            }

            break;
        case BULK_MERGE_ALL_INTO_ACTIVE_LAYER:
            for (Combo cmb : layerComboMap.values()) {
                if (cmb.getItemCount() > 6) {
                    cmb.select(6);
                } else {
                    cmb.select(4);
                }
            }

            break;

        default:
            break;

        }

    }

    /**
     * Add a horizontal separator to the display.
     */
    public static void addSeparator(Composite top) {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(top, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Perform selected actions for each layer & redraws.
     */
    public void okPressed() {

        if (conflictActions()) {
            return;
        }

        Product activeAct = drawingLayer.getActiveProduct();
        Layer activeLayer = drawingLayer.getActiveLayer();

        for (Layer lyr : layerComboMap.keySet()) {

            int index = getLayerActionIndex(layerComboMap.get(lyr));

            switch (LayerActions.values()[index]) {

            case NO_ACTION:
                break;

            case ADD_AS_NEW_LAYER:
                addLayer(activeAct, lyr);
                break;

            case REPLACE_LIKE_NAME_LAYER:
                replaceMergeLayer(activeAct.getLayer(lyr.getName()), lyr, true,
                        null);
                break;

            case REPLACE_ACTIVE_LAYER:
                replaceMergeLayer(activeLayer, lyr, true, null);
                break;

            case REPLACE_RENAME_ACTIVE_LAYER:
                replaceMergeLayer(activeLayer, lyr, true, activeAct);
                break;

            case MERGE_INTO_LIKE_NAME_LAYER:
                replaceMergeLayer(activeAct.getLayer(lyr.getName()), lyr,
                        false, null);
                break;

            case MERGE_INTO_ACTIVE_LAYER:
                replaceMergeLayer(drawingLayer.getActiveLayer(), lyr, false,
                        null);
                break;

            case MERGE_RENAME_ACTIVE_LAYER:
                replaceMergeLayer(activeLayer, lyr, false, activeAct);
                break;

            default:
                break;

            }

        }

        this.close();

    }

    /**
     * closes the dialog
     */
    public void cancelPressed() {
        super.cancelPressed();
    }

    /*
     * Add input layer to an activity - make sure the layer name won't conflict
     * with existing layers.
     */
    private void addLayer(Product prd, Layer newLayer) {

        if (prd != null && newLayer != null) {
            newLayer.setName(findUniqueLayerName(prd, newLayer));
            prd.addLayer(newLayer);
        }

    }

    /*
     * Build a layer name that is will unique in the activity.
     */
    private String findUniqueLayerName(Product prd, Layer newLayer) {

        String lyrName = "New Layer";

        if (prd != null && newLayer != null) {
            lyrName = newLayer.getName();
            int ii = 1;
            while (prd.getLayer(lyrName) != null) {
                lyrName = new String(lyrName + " " + ii);
            }
        }

        return lyrName;

    }

    /*
     * Replace or merge the content of one layer with the content from another
     * layer
     * 
     * Note: when "prd" is not null, it means a "rename" is required.
     */
    private void replaceMergeLayer(Layer existingLayer, Layer newLayer,
            boolean replace, Product prd) {

        if (existingLayer != null && newLayer != null) {

            if (replace)
                existingLayer.clear();

            existingLayer.add(newLayer.getDrawables());
            mergeOutlooks(existingLayer);

            if (prd != null) {
                existingLayer.setName(findUniqueLayerName(prd, newLayer));
            }

        }

    }

    /*
     * Get the layer action index.
     */
    private int getLayerActionIndex(Combo lyrCombo) {
        int index = lyrCombo.getSelectionIndex();
        if (lyrCombo.getItemCount() < LayerActions.values().length) {
            if (index == 2 || index == 3) {
                index++;
            } else if (index > 3) {
                index += 2;
            }
        }

        return index;
    }

    /*
     * Check if if there are any conflicts for the user-selected actions:
     * 
     * 1. One existing layer can receive only one "Replace" action from incoming
     * layers. 2. One existing layer can receive only one "Rename" action from
     * incoming layers.
     * 
     * Note that only "active layer" might receive such conflicting actions. .
     */
    private boolean conflictActions() {

        boolean conflict = false;

        ArrayList<String> replacingLayers = new ArrayList<String>();
        ArrayList<String> renamingLayers = new ArrayList<String>();

        for (Layer lyr : layerComboMap.keySet()) {

            int index = getLayerActionIndex(layerComboMap.get(lyr));

            switch (LayerActions.values()[index]) {

            case NO_ACTION:
                break;

            case ADD_AS_NEW_LAYER:
                break;

            case REPLACE_LIKE_NAME_LAYER:
                if (drawingLayer.getActiveLayer() == drawingLayer
                        .getActiveProduct().getLayer(lyr.getName())) {
                    replacingLayers.add(lyr.getName());
                }
                break;

            case REPLACE_ACTIVE_LAYER:
                replacingLayers.add(lyr.getName());

                break;

            case REPLACE_RENAME_ACTIVE_LAYER:

                replacingLayers.add(lyr.getName());
                renamingLayers.add(lyr.getName());

                break;

            case MERGE_INTO_LIKE_NAME_LAYER:
                break;
            case MERGE_INTO_ACTIVE_LAYER:
                break;

            case MERGE_RENAME_ACTIVE_LAYER:
                renamingLayers.add(lyr.getName());

                break;

            default:
                break;

            }

        }

        String msg = "The following conflicts have been found for the active layer '"
                + drawingLayer.getActiveLayer().getName()
                + "'. \nPlease fix before proceed:\n\n";
        if (replacingLayers.size() > 1) {
            conflict = true;
            msg += "1. " + replacingLayers.size() + " layers (";
            for (String st : replacingLayers) {
                msg += st;
                if (st != replacingLayers.get(replacingLayers.size() - 1)) {
                    msg += ", ";
                }
            }

            msg += ") are selected to replace the content of the active layer.\n\n";
            msg += "But only one layer is allowed and others should use 'Merge'.\n\n";
        }

        if (renamingLayers.size() > 1) {
            conflict = true;
            msg += "2. Layers (";
            for (String st : renamingLayers) {
                msg += st;
                if (st != renamingLayers.get(renamingLayers.size() - 1)) {
                    msg += ", ";
                }
            }

            msg += ") are selected to rename the active layer.\n\n";
            msg += "But only one layer is allowed to be selected to do so.\n\n";
        }

        if (conflict) {
            MessageDialog confirmOpen = new MessageDialog(PlatformUI
                    .getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Conflicting actions to import layers", null, msg,
                    MessageDialog.WARNING, new String[] { "Ok" }, 0);

            confirmOpen.open();
        }

        return conflict;

    }

    /**
     * Merges outlook in the specified layer. Outlook of the same type needs to
     * put into one outlook in order to make the 'set continue' and format work.
     * 
     * @param layer
     */
    private void mergeOutlooks(Layer layer) {

        ArrayList<ArrayList<Outlook>> otlkList = new ArrayList<ArrayList<Outlook>>();

        // loop through DEs in the layer and put outlooks in the list
        // The outer list is a list of different types outlook list.
        // The inner list is a list of same type of outlook.
        for (AbstractDrawableComponent adc : layer.getDrawables()) {
            if (adc instanceof Outlook) {
                Outlook look = (Outlook) adc;

                boolean found = false;
                for (ArrayList<Outlook> aList : otlkList) {
                    if (look.getOutlookType().equalsIgnoreCase(
                            aList.get(0).getOutlookType())) {
                        aList.add(look);
                        found = true;
                    }
                }

                if (!found) {
                    ArrayList<Outlook> oList = new ArrayList<Outlook>();
                    oList.add(look);
                    otlkList.add(oList);
                }
            }
        }

        // Merge outlook. Keep the first outlook for one type and add components
        // of other outlooks
        // of the same type into the first one.
        for (ArrayList<Outlook> tList : otlkList) {
            if (tList.size() > 1) {
                for (int ii = 1; ii < tList.size(); ii++) {
                    Iterator<AbstractDrawableComponent> it = tList.get(ii)
                            .getComponentIterator();
                    while (it.hasNext()) {
                        tList.get(0).add(it.next());
                    }
                    layer.remove(tList.get(ii));
                }
            }
        }

    }
}
