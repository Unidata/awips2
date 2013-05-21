/*
 * gov.noaa.nws.ncep.ui.pgen.controls.RetrieveActivityDialog
 * 
 * 29 March 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

import gov.noaa.nws.ncep.common.dataplugin.pgen.PgenRecord;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.sigmet.VaaInfo;
import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenSnapJet;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * A dialog to Retrieve PGEN activities from EDEX.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	-----------------------------------
 * 03/13		#977		S.gilbert	Modified from PgenFileManageDialog1
 * 
 * </pre>
 * 
 * @author
 * @version 1
 */
public class RetrieveActivityDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrieveActivityDialog.class);

    /*
     * Internal class used to hold some characteristics of an Activity
     */
    class ActivityElement {
        String dataURI;

        String activityType;

        String activityLabel;

        Date refTime;
    }

    /*
     * Used to compare Activity's reference times.
     */
    class ActivityTimeComparator extends ViewerComparator {

        @Override
        public int compare(Viewer viewer, Object e1, Object e2) {
            Date elem1 = ((ActivityElement) e1).refTime;
            Date elem2 = ((ActivityElement) e2).refTime;
            return -1 * elem1.compareTo(elem2); // multiply by -1 to reverse
                                                // ordering
        }

    }

    private String title = null;

    private Shell shell;

    private Button sortByNameBtn = null;

    private Button sortByDateBtn = null;

    private List dirList = null;

    private ListViewer fileListViewer = null;

    private Button browseBtn = null;

    private Button autoSaveOffBtn = null;

    private Button autoSaveOnBtn = null;

    private static final int ADD_ID = IDialogConstants.CLIENT_ID + 7587;

    private static final String ADD_LABEL = "Add";

    private static final int REPLACE_ID = IDialogConstants.CLIENT_ID + 7586;

    private static final String REPLACE_LABEL = "Replace";

    private static final int ADVANCE_ID = IDialogConstants.CLIENT_ID + 7588;

    private static final String ADVANCE_LABEL = "Advanced";

    private static final int CLOSE_ID = IDialogConstants.CLIENT_ID + 7590;

    private static final String CLOSE_LABEL = "Close";

    private Button replaceBtn = null;

    private Button addBtn = null;

    private Button appendBtn = null;

    private Button cancelBtn = null;

    private Map<String, java.util.List<ActivityElement>> activityMap;

    private static String selectedDir = null;

    private static String fullName = null;

    /*
     * Constructor
     */
    public RetrieveActivityDialog(Shell parShell, String btnName)
            throws VizException {

        super(parShell);

        setTitle(btnName);

    }

    /*
     * Set up the file mode.
     */
    private void setTitle(String btnName) {

        if (btnName.equals("Open")) {
            title = "Rretrive a PGEN Activity";
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        this.setShellStyle(SWT.RESIZE | SWT.PRIMARY_MODAL);

        this.shell = shell;
        if (title != null) {
            shell.setText(title);
        }
    }

    /**
     * (non-Javadoc) Create all of the widgets on the Dialog
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {

        Composite dlgAreaForm = (Composite) super.createDialogArea(parent);

        Composite topForm = new Composite(dlgAreaForm, SWT.NONE);
        topForm.setLayout(new FormLayout());

        /*
         * Create a label and two radio buttons - how to sort the files
         */
        Composite sortForm = new Composite(topForm, SWT.NONE);
        sortForm.setLayout(new FormLayout());

        sortByNameBtn = new Button(sortForm, SWT.RADIO);
        sortByNameBtn.setText("Sort Alphabetically");

        FormData layoutData1 = new FormData(370, 25);
        layoutData1.top = new FormAttachment(0, 0);
        layoutData1.left = new FormAttachment(0, 0);

        sortByNameBtn.setLayoutData(layoutData1);

        sortByNameBtn.setSelection(true);

        /*
         * Sort the files by name. when the corresponding radio button is
         * selected
         */
        sortByNameBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent ev) {
                fileListViewer.setComparator(new ViewerComparator());
                fileListViewer.refresh(true);

            }
        });

        sortByDateBtn = new Button(sortForm, SWT.RADIO);
        sortByDateBtn.setText("Sort By Date");

        FormData layoutData3 = new FormData();
        layoutData3.top = new FormAttachment(sortByNameBtn, 5, SWT.BOTTOM);
        layoutData3.left = new FormAttachment(sortByNameBtn, 0, SWT.LEFT);
        sortByDateBtn.setLayoutData(layoutData3);

        /*
         * Sort the files by date. when the corresponding radio button is
         * selected
         */
        sortByDateBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent ev) {
                fileListViewer.setComparator(new ActivityTimeComparator());
                fileListViewer.refresh(true);

            }
        });

        /*
         * Create a list of directories to select from
         */
        Label dirLbl = new Label(topForm, SWT.NONE);
        dirLbl.setText("Select Activity Type:");

        FormData layoutData5 = new FormData();
        layoutData5.top = new FormAttachment(sortForm, 15, SWT.BOTTOM);
        layoutData5.left = new FormAttachment(sortForm, 0, SWT.LEFT);

        dirLbl.setLayoutData(layoutData5);

        dirList = new List(topForm, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL);

        // for (String str : dirTableMap.keySet()) {
        activityMap = getActivityMap();
        for (String str : activityMap.keySet()) {
            dirList.add(str);
        }

        FormData layoutData6 = new FormData(350, 200);
        layoutData6.top = new FormAttachment(dirLbl, 5, SWT.BOTTOM);
        layoutData6.left = new FormAttachment(dirLbl, 0, SWT.LEFT);
        dirList.setLayoutData(layoutData6);

        dirList.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event e) {
                if (dirList.getSelectionCount() > 0) {

                    selectedDir = dirList.getSelection()[0];
                    fileListViewer.setInput(activityMap.get(selectedDir));
                    fileListViewer.getList().setToolTipText(null);
                    fileListViewer.refresh();

                    // Update the full file name with the new path
                    fullName = null;
                }
            }
        });

        /*
         * Create a list to display the product files in a selected directory
         */
        Label fileLbl = new Label(topForm, SWT.NONE);
        fileLbl.setText("Select an Activity Label:");

        FormData layoutData8 = new FormData();
        layoutData8.top = new FormAttachment(dirList, 20, SWT.BOTTOM);
        layoutData8.left = new FormAttachment(dirList, 0, SWT.LEFT);

        fileLbl.setLayoutData(layoutData8);

        fileListViewer = new ListViewer(topForm, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL);

        FormData layoutData9 = new FormData(350, 200);
        layoutData9.top = new FormAttachment(fileLbl, 5, SWT.BOTTOM);
        layoutData9.left = new FormAttachment(fileLbl, 0, SWT.LEFT);

        fileListViewer.getList().setLayoutData(layoutData9);

        fileListViewer.setContentProvider(ArrayContentProvider.getInstance());
        if (sortByNameBtn.getSelection()) {
            fileListViewer.setComparator(new ViewerComparator());
        } else {
            fileListViewer.setComparator(new ActivityTimeComparator());
        }

        fileListViewer.setLabelProvider(new LabelProvider() {

            @Override
            public String getText(Object element) {
                if (element instanceof ActivityElement)
                    return ((ActivityElement) element).activityLabel;
                else
                    return super.getText(element);
            }

        });

        fileListViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        IStructuredSelection selection = (IStructuredSelection) event
                                .getSelection();
                        if (selection.getFirstElement() instanceof ActivityElement) {
                            ActivityElement elem = (ActivityElement) selection
                                    .getFirstElement();
                            // System.out.println(elem.dataURI);
                            // System.out.println(elem.activityType);
                            // System.out.println(elem.activityLabel);
                            // System.out.println(elem.refTime);
                            fileListViewer.getList().setToolTipText(
                                    elem.dataURI);
                        } else
                            System.out.println("GOT??? "
                                    + selection.getFirstElement().getClass()
                                            .getCanonicalName());
                    }
                });

        /**
         * Create a browse button to open a file dialog to navigate & select a
         * file.
         */
        browseBtn = new Button(topForm, SWT.PUSH);

        FormData layoutData10 = new FormData(355, 25);
        layoutData10.top = new FormAttachment(fileListViewer.getList(), 20,
                SWT.BOTTOM);
        layoutData10.left = new FormAttachment(fileListViewer.getList(), 0,
                SWT.LEFT);

        browseBtn.setLayoutData(layoutData10);
        browseBtn.setSize(330, 20);
        browseBtn.setText("Browse");
        browseBtn.setEnabled(false);

        /*
         * Create a label and two radio buttons to turn "auto save" on or off.
         */
        Label autoSaveLbl = new Label(topForm, SWT.NONE);
        autoSaveLbl.setText("Auto Save:");

        FormData layoutData11 = new FormData();
        layoutData11.top = new FormAttachment(browseBtn, 20, SWT.BOTTOM);
        layoutData11.left = new FormAttachment(browseBtn, 0, SWT.LEFT);
        autoSaveLbl.setLayoutData(layoutData11);

        autoSaveOffBtn = new Button(topForm, SWT.RADIO);
        autoSaveOffBtn.setText("Off");
        autoSaveOffBtn.setSelection(true);

        FormData layoutData12 = new FormData();
        layoutData12.top = new FormAttachment(autoSaveLbl, 0, SWT.TOP);
        layoutData12.left = new FormAttachment(autoSaveLbl, 10, SWT.RIGHT);
        autoSaveOffBtn.setLayoutData(layoutData12);

        autoSaveOnBtn = new Button(topForm, SWT.RADIO);
        autoSaveOnBtn.setText("On");

        FormData layoutData13 = new FormData();
        layoutData13.top = new FormAttachment(autoSaveOffBtn, 0, SWT.TOP);
        layoutData13.left = new FormAttachment(autoSaveOffBtn, 10, SWT.RIGHT);
        autoSaveOnBtn.setLayoutData(layoutData13);

        return dlgAreaForm;
    }

    private Map<String, java.util.List<ActivityElement>> getActivityMap() {

        Map<String, java.util.List<ActivityElement>> activityMap = new HashMap<String, java.util.List<ActivityElement>>();

        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(PgenRecord.class.getName());
        request.addRequestField(PgenRecord.ACTIVITY_TYPE);
        request.addRequestField(PgenRecord.ACTIVITY_LABEL);
        request.addRequestField(PgenRecord.DATAURI);
        request.addRequestField(PgenRecord.REF_TIME);
        request.setOrderByField(PgenRecord.ACTIVITY_TYPE);

        DbQueryResponse response;
        try {
            response = (DbQueryResponse) ThriftClient.sendRequest(request);
            for (Map<String, Object> result : response.getResults()) {
                ActivityElement elem = new ActivityElement();
                elem.activityType = (String) result
                        .get(PgenRecord.ACTIVITY_TYPE);
                elem.activityLabel = (String) result
                        .get(PgenRecord.ACTIVITY_LABEL);
                elem.dataURI = (String) result.get(PgenRecord.DATAURI);
                elem.refTime = (Date) result.get(PgenRecord.REF_TIME);

                if (activityMap.containsKey(elem.activityType)) {
                    ((java.util.List<ActivityElement>) activityMap
                            .get(elem.activityType)).add(elem);
                } else {
                    java.util.List<ActivityElement> elist = new ArrayList<ActivityElement>();
                    elist.add(elem);
                    activityMap.put(elem.activityType, elist);
                }
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return activityMap;
    }

    /**
     * Create Replace/Append/Cancel button for "Open" a product file or
     * Save/Cancel button for "Save" a product file.
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {

        addBtn = createButton(parent, ADD_ID, ADD_LABEL, true);
        replaceBtn = createButton(parent, REPLACE_ID, REPLACE_LABEL, true);
        appendBtn = createButton(parent, ADVANCE_ID, ADVANCE_LABEL, true);

        replaceBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                openProducts(true);
            }
        });

        addBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                openProducts(false);
            }
        });

        appendBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                appendProducts();
            }
        });

        cancelBtn = createButton(parent, CLOSE_ID, CLOSE_LABEL, true);
        cancelBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                close();
            }
        });

    }

    /**
     * Retrieve an activity to replace or append to the current product list in
     * the current PGEN session.
     */
    private void openProducts(boolean replace) {

        ActivityElement elem = getActivitySelection();
        if (elem == null) {
            return;
        }
        fullName = elem.activityLabel;

        java.util.List<Product> pgenProds = null;
        try {
            pgenProds = StorageUtils.retrieveProduct(elem.dataURI);
        } catch (PgenStorageException e) {
            StorageUtils.showError(e);
        }

        /*
         * some Volcano Products are pure texts: TEST/RESUME and cannot be
         * drawn.
         */
        if (VaaInfo.isNoneDrawableTxt(pgenProds)) {
            VaaInfo.openMsgDlg(VaaInfo.NONE_DRAWABLE_MSG);
            return;
        }

        /*
         * Confirm the action
         */
        PgenResource pgen = PgenSession.getInstance().getPgenResource();

        // Force all product/layer display onOff flag to be false at the start.
        /*
         * for ( gov.noaa.nws.ncep.ui.pgen.elements.Product prd : pgenProds ) {
         * prd.setOnOff( false ); for ( gov.noaa.nws.ncep.ui.pgen.elements.Layer
         * lyr : prd.getLayers() ) { lyr.setOnOff( false ); } }
         */
        if (replace) {
            MessageDialog confirmOpen = new MessageDialog(PlatformUI
                    .getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Confirm File Replace", null, "Replace Activity <"
                            + pgen.getActiveProduct().getType()
                            + "> with New Activity <"
                            + pgenProds.get(0).getType() + "> ?",
                    MessageDialog.INFORMATION, new String[] { "Yes", "No" }, 0);

            confirmOpen.open();

            if (confirmOpen.getReturnCode() != MessageDialog.OK) {
                return;
            }

        }

        pgen.setAutosave(autoSaveOnBtn.getSelection());
        if (fullName.endsWith(".lpf")) {
            pgen.setAutoSaveFilename(fullName.replace(".lpf", "xml"));
        } else {
            pgen.setAutoSaveFilename(fullName);
        }

        pgenProds.get(0).setInputFile(fullName);

        this.setJetTool(pgenProds);

        close();

        /*
         * Replace the active product or add the product to the end
         */
        if (replace) {
            // Reset the output file name.
            for (gov.noaa.nws.ncep.ui.pgen.elements.Product pp : pgenProds) {
                pp.setOutputFile(null);
            }

            PgenFileNameDisplay.getInstance().setFileName(fullName);

            pgen.replaceProduct(pgenProds);
        } else {
            if (pgen.getActiveProduct() == null) {
                PgenFileNameDisplay.getInstance().setFileName(fullName);
            }

            pgen.addProduct(pgenProds);
        }

        PgenUtil.refresh();

    }

    /**
     * Append the products in a file with those in the current product list.
     */
    private void appendProducts() {

        ActivityElement elem = getActivitySelection();
        if (elem == null) {
            return;
        }
        fullName = elem.activityLabel;

        java.util.List<Product> pgenProds = null;
        try {
            pgenProds = StorageUtils.retrieveProduct(elem.dataURI);
        } catch (PgenStorageException e) {
            StorageUtils.showError(e);
        }

        /*
         * some Volcano Products are pure texts: TEST/RESUME and cannot be
         * drawn.
         */
        if (VaaInfo.isNoneDrawableTxt(pgenProds)) {
            VaaInfo.openMsgDlg(VaaInfo.NONE_DRAWABLE_MSG);
            return;
        }

        PgenResource pgen = PgenSession.getInstance().getPgenResource();

        PgenLayerMergeDialog layerMergeDlg = null;
        try {
            layerMergeDlg = new PgenLayerMergeDialog(shell, pgenProds.get(0),
                    fullName);
        } catch (Exception e) {
            e.printStackTrace();
        }

        if (layerMergeDlg != null) {

            layerMergeDlg.open();
            if (layerMergeDlg.getReturnCode() == MessageDialog.OK) {

                pgen.setAutosave(autoSaveOnBtn.getSelection());
                if (fullName.endsWith(".lpf")) {
                    pgen.setAutoSaveFilename(fullName.replace(".lpf", "xml"));
                } else {
                    pgen.setAutoSaveFilename(fullName);
                }

                this.setJetTool(pgenProds);

                close();

                pgen.getResourceData().startProductManage();
            }
        }

    }

    private ActivityElement getActivitySelection() {
        ActivityElement elem = null;

        if (!fileListViewer.getSelection().isEmpty()) {
            IStructuredSelection sel = (IStructuredSelection) fileListViewer
                    .getSelection();
            elem = (ActivityElement) sel.getFirstElement();
            // System.out.println(elem.dataURI);
            // System.out.println(elem.activityType);
            // System.out.println(elem.activityLabel);
        } else {

            MessageDialog confirmDlg = new MessageDialog(PlatformUI
                    .getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Invalid PGEN Selection", null,
                    "Please select an Activity from the Activity Label list.",
                    MessageDialog.INFORMATION, new String[] { "OK" }, 0);

            confirmDlg.open();

            return null;
        }

        return elem;
    }

    /**
     * Sets the jet snap tool in order to zoom tghe jet correctly.
     * 
     * @param prods
     */
    private void setJetTool(
            java.util.List<gov.noaa.nws.ncep.ui.pgen.elements.Product> prods) {

        PgenSnapJet st = new PgenSnapJet(PgenSession.getInstance()
                .getPgenResource().getDescriptor(), PgenUtil.getActiveEditor(),
                null);

        for (gov.noaa.nws.ncep.ui.pgen.elements.Product prod : prods) {
            for (Layer layer : prod.getLayers()) {

                Iterator<AbstractDrawableComponent> iterator = layer
                        .getComponentIterator();
                while (iterator.hasNext()) {
                    AbstractDrawableComponent adc = iterator.next();
                    if (adc instanceof Jet) {
                        ((Jet) adc).setSnapTool(st);
                        // st.snapJet((Jet)adc);
                    }
                }
            }
        }

    }

}
