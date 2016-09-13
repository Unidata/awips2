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

package com.raytheon.viz.hydro.datatrashcan;

import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.IFontProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.IGetSortType;
import com.raytheon.viz.hydrocommon.data.DataTrashCanData;
import com.raytheon.viz.hydrocommon.datamanager.DataTrashCanDataManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * This class displays the Data Trash Can dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 18 JUL 2010  2110       mpduff      Tweaked list box labels
 * 05 FEB 2013  1578       rferrel     Made dialog non-blocking.
 * 15 APR 2016  5483       dgilling    Refactor based on CaveJFACEDialog, 
 *                                     remove fixed layouts, use TableViewer
 *                                     for primary control.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class DataTrashCanDlg extends CaveJFACEDialog implements IGetSortType {

    private static abstract class MonospaceColumnLabelProvider extends
            ColumnLabelProvider implements IFontProvider {

        @Override
        public Font getFont(Object element) {
            return JFaceResources.getTextFont();
        }

        @Override
        public abstract String getText(Object element);
    }

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String MOVE_BUTTON_LABEL = "Move Selected to Data Tables";

    private static final int MOVE_BUTTON_ID = IDialogConstants.CLIENT_ID + 1;

    private static final String DELETE_SELECTED_BUTTON_LABEL = "Delete Selected";

    private static final int DELETE_SELECTED_BUTTON_ID = IDialogConstants.CLIENT_ID + 2;

    private static final String DELETE_ALL_BUTTON_LABEL = "Delete All";

    private static final int DELETE_ALL_BUTTON_ID = IDialogConstants.CLIENT_ID + 3;

    private static DateFormat TIME_FORMAT = new SimpleDateFormat("MM/dd HH:mm") {
        {
            setTimeZone(TimeUtil.GMT_TIME_ZONE);
        }
    };

    private final NumberFormat VALUE_FORMAT;

    /**
     * Location check box.
     */
    private Button locationChk;

    /**
     * Location text control.
     */
    private Text locationTF;

    /**
     * Physical element check box/
     */
    private Button peChk;

    /**
     * Reject type combo box.
     */
    private Combo rejectTypeCbo;

    /**
     * Physical element data list control.
     */
    private List peDataList;

    /**
     * Sort by combo list.
     */
    private Combo sortByCbo;

    /**
     * Data list control.
     */
    private TableViewer dataTable;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public DataTrashCanDlg(Shell parent) {
        super(parent);
        setShellStyle(SWT.DIALOG_TRIM);
        setBlockOnOpen(false);

        this.VALUE_FORMAT = NumberFormat.getNumberInstance();
        this.VALUE_FORMAT.setMinimumFractionDigits(2);
        this.VALUE_FORMAT.setMaximumFractionDigits(2);
        this.VALUE_FORMAT.setGroupingUsed(false);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Data Trash Can");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        createTopGroupCotrols(composite);
        createDataListControl(composite);
        return composite;
    }

    /**
     * Create the controls located at the top of the dialog.
     */
    private void createTopGroupCotrols(Composite parent) {
        Group topGroup = new Group(parent, SWT.NONE);
        topGroup.setLayout(new GridLayout(3, false));
        topGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // -----------------------------------------
        // Create the left side of the top section
        // -----------------------------------------
        Composite leftComp = new Composite(topGroup, SWT.NONE);
        leftComp.setLayout(new GridLayout(3, false));
        leftComp.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, false, true));

        Label filterByLbl = new Label(leftComp, SWT.NONE);
        filterByLbl.setText("Filter By:");
        filterByLbl
                .setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));

        locationChk = new Button(leftComp, SWT.CHECK);
        locationChk.setText("Location:");
        locationChk.setSelection(false);
        locationChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                locationTF.setEnabled(locationChk.getSelection());
                dataTable.refresh();
            }
        });
        locationChk
                .setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));

        locationTF = new Text(leftComp, SWT.BORDER);
        locationTF.setEnabled(false);
        locationTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });
        locationTF.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                dataTable.refresh();
            }
        });
        GC gc = new GC(locationTF);
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();
        Rectangle trim = locationTF.computeTrim(0, 0, charWidth * 8,
                locationTF.getLineHeight());
        GridData gd = new GridData(SWT.LEFT, SWT.TOP, false, false);
        gd.widthHint = trim.width;
        locationTF.setLayoutData(gd);

        Label rejectLbl = new Label(leftComp, SWT.NONE);
        rejectLbl.setText("Reject Type:");
        rejectLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, true));

        rejectTypeCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        rejectTypeCbo.setItems(new String[] { "All", "Auto", "Manual" });
        rejectTypeCbo.select(0);
        rejectTypeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataTable.refresh();
            }
        });
        rejectTypeCbo
                .setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, true));

        // -----------------------------------------
        // Create the list control in the center of
        // the top section
        // -----------------------------------------
        Composite centerComp = new Composite(topGroup, SWT.NONE);
        centerComp.setLayout(new GridLayout(2, false));
        centerComp
                .setLayoutData(new GridData(SWT.CENTER, SWT.FILL, false, true));

        peChk = new Button(centerComp, SWT.CHECK);
        peChk.setText("Phys. Element:");
        peChk.setSelection(false);
        peChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                peDataList.setEnabled(peChk.getSelection());
                dataTable.refresh();
            }
        });
        peChk.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, true));

        peDataList = new List(centerComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        peDataList.setFont(JFaceResources.getTextFont());
        peDataList.setEnabled(false);
        String[] listItems = getPhyElemListData();
        peDataList.setItems(listItems);
        peDataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataTable.refresh();
            }
        });
        gc = new GC(peDataList);
        charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();
        int maxWidth = -Integer.MAX_VALUE;
        for (String item : listItems) {
            maxWidth = Math.max(maxWidth, item.length());
        }
        trim = peDataList.computeTrim(0, 0, charWidth * maxWidth,
                peDataList.getItemHeight() * 10);
        gd = new GridData(SWT.RIGHT, SWT.FILL, false, true);
        gd.heightHint = trim.height;
        gd.widthHint = trim.width;
        peDataList.setLayoutData(gd);

        // -----------------------------------------
        // Create the sort control on the right side
        // of the top section
        // -----------------------------------------
        Composite rightComp = new Composite(topGroup, SWT.NONE);
        rightComp.setLayout(new GridLayout(2, false));
        rightComp.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, false, true));

        Label sortByLbl = new Label(rightComp, SWT.NONE);
        sortByLbl.setText("Sort By:");
        sortByLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, true));

        sortByCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        sortByCbo.setItems(new String[] { "Location", "Time" });
        sortByCbo.select(0);
        sortByCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataTable.refresh();
            }
        });
        sortByCbo.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, true));
    }

    /**
     * Create the data list control.
     */
    private void createDataListControl(Composite parent) {
        dataTable = new TableViewer(parent, SWT.MULTI | SWT.FULL_SELECTION
                | SWT.V_SCROLL | SWT.BORDER);
        final Table table = dataTable.getTable();
        table.setHeaderVisible(true);
        table.setLinesVisible(false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = table.getHeaderHeight() + (table.getItemHeight() * 20);
        dataTable.getControl().setLayoutData(gd);

        GC gc = new GC(dataTable.getControl());
        gc.setFont(JFaceResources.getTextFont());
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();

        TableViewerColumn colLocation = new TableViewerColumn(dataTable,
                SWT.NONE);
        colLocation.getColumn().setText("Location");
        colLocation.getColumn().setWidth(9 * charWidth);
        colLocation.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return data.getLid();
            }
        });

        TableViewerColumn colName = new TableViewerColumn(dataTable, SWT.NONE);
        colName.getColumn().setText("Name");
        colName.getColumn().setWidth(22 * charWidth);
        colName.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return data.getName();
            }
        });

        TableViewerColumn colPhysElement = new TableViewerColumn(dataTable,
                SWT.NONE);
        colPhysElement.getColumn().setText("PE");
        colPhysElement.getColumn().setWidth(4 * charWidth);
        colPhysElement.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return data.getPe();
            }
        });

        TableViewerColumn colDuration = new TableViewerColumn(dataTable,
                SWT.NONE);
        colDuration.getColumn().setText("Dur");
        colDuration.getColumn().setWidth(6 * charWidth);
        colDuration.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return Integer.toString(data.getDur());
            }
        });

        TableViewerColumn colTs = new TableViewerColumn(dataTable, SWT.NONE);
        colTs.getColumn().setText("TS");
        colTs.getColumn().setWidth(4 * charWidth);
        colTs.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return data.getTs();
            }
        });

        TableViewerColumn colExtremum = new TableViewerColumn(dataTable,
                SWT.NONE);
        colExtremum.getColumn().setText("Ext");
        colExtremum.getColumn().setWidth(4 * charWidth);
        colExtremum.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return data.getExtremum();
            }
        });

        TableViewerColumn colValue = new TableViewerColumn(dataTable, SWT.RIGHT);
        colValue.getColumn().setText("Value");
        colValue.getColumn().setWidth(11 * charWidth);
        colValue.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return VALUE_FORMAT.format(data.getValue());
            }
        });

        TableViewerColumn colObsTime = new TableViewerColumn(dataTable,
                SWT.NONE);
        colObsTime.getColumn().setText("ObsTime");
        colObsTime.getColumn().setWidth(13 * charWidth);
        colObsTime.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return TIME_FORMAT.format(data.getValidTime());
            }
        });

        TableViewerColumn colBasisTime = new TableViewerColumn(dataTable,
                SWT.NONE);
        colBasisTime.getColumn().setText("BasisTime");
        colBasisTime.getColumn().setWidth(13 * charWidth);
        colBasisTime.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return TIME_FORMAT.format(data.getBasisTime());
            }
        });

        TableViewerColumn colRevision = new TableViewerColumn(dataTable,
                SWT.NONE);
        colRevision.getColumn().setText("RV");
        colRevision.getColumn().setWidth(4 * charWidth);
        colRevision.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return (data.getRevision() == 1) ? "T" : "F";
            }
        });

        TableViewerColumn colShefQualCode = new TableViewerColumn(dataTable,
                SWT.NONE);
        colShefQualCode.getColumn().setText("SQ");
        colShefQualCode.getColumn().setWidth(4 * charWidth);
        colShefQualCode.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return data.getShefQualCode();
            }
        });

        TableViewerColumn colQualCode = new TableViewerColumn(dataTable,
                SWT.NONE);
        colQualCode.getColumn().setText("QC");
        colQualCode.getColumn().setWidth(4 * charWidth);
        colQualCode.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return data.getQualityCodeSymbol();
            }
        });

        TableViewerColumn colUser = new TableViewerColumn(dataTable, SWT.NONE);
        colUser.getColumn().setText("User");
        colUser.getColumn().setWidth(10 * charWidth);
        colUser.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return data.getUserID();
            }
        });

        TableViewerColumn colType = new TableViewerColumn(dataTable, SWT.NONE);
        colType.getColumn().setText("Type");
        colType.getColumn().setWidth(6 * charWidth);
        colType.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return (data.getRejectType().equalsIgnoreCase("A")) ? "Auto"
                        : "Man";
            }
        });

        TableViewerColumn colPostTime = new TableViewerColumn(dataTable,
                SWT.NONE);
        colPostTime.getColumn().setText("PostTime");
        colPostTime.getColumn().setWidth(13 * charWidth);
        colPostTime.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return TIME_FORMAT.format(data.getPostingTime());
            }
        });

        TableViewerColumn colProduct = new TableViewerColumn(dataTable,
                SWT.NONE);
        colProduct.getColumn().setText("Product");
        colProduct.getColumn().setWidth(12 * charWidth);
        colProduct.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                return data.getProductID();
            }
        });

        TableViewerColumn colProdTime = new TableViewerColumn(dataTable,
                SWT.NONE);
        colProdTime.getColumn().setText("ProdTime");
        colProdTime.getColumn().setWidth(13 * charWidth);
        colProdTime.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                DataTrashCanData data = (DataTrashCanData) element;
                Date productTime = data.getProductTime();
                return (productTime != null) ? TIME_FORMAT.format(data
                        .getProductTime()) : "";
            }
        });

        dataTable.setContentProvider(ArrayContentProvider.getInstance());
        dataTable.setInput(getTrashData());
        dataTable.setComparator(new ViewerComparator() {

            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
                DataTrashCanData data1 = (DataTrashCanData) e1;
                DataTrashCanData data2 = (DataTrashCanData) e2;
                return data1.compareTo(data2);
            }
        });

        dataTable.addFilter(new ViewerFilter() {

            @Override
            public boolean select(Viewer viewer, Object parentElement,
                    Object element) {
                if ((locationTF.isEnabled())
                        && (!locationTF.getText().trim().isEmpty())) {
                    String searchString = locationTF.getText().trim()
                            .toUpperCase();
                    DataTrashCanData data = (DataTrashCanData) element;
                    return data.getLid().contains(searchString);
                }

                return true;
            }
        });
        dataTable.addFilter(new ViewerFilter() {

            @Override
            public boolean select(Viewer viewer, Object parentElement,
                    Object element) {
                String rejectType = rejectTypeCbo.getItem(rejectTypeCbo
                        .getSelectionIndex());
                if (!rejectType.equals("All")) {
                    DataTrashCanData data = (DataTrashCanData) element;
                    return (((rejectType.equals("Auto")) && (data
                            .getRejectType().equalsIgnoreCase("A"))) || ((rejectType
                            .equals("Manual")) && (data.getRejectType()
                            .equalsIgnoreCase("M"))));
                }

                return true;
            }
        });
        dataTable.addFilter(new ViewerFilter() {

            @Override
            public boolean select(Viewer viewer, Object parentElement,
                    Object element) {
                if (peChk.getSelection()) {
                    Collection<String> peSelections = new HashSet<>();
                    int[] selectedInd = peDataList.getSelectionIndices();
                    for (int i : selectedInd) {
                        peSelections.add(peDataList.getItem(i).split(" ")[0]
                                .toUpperCase());
                    }

                    DataTrashCanData data = (DataTrashCanData) element;
                    return peSelections.contains(data.getPe().toUpperCase());
                }

                return true;
            }
        });
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, MOVE_BUTTON_ID, MOVE_BUTTON_LABEL, false);
        createButton(parent, DELETE_SELECTED_BUTTON_ID,
                DELETE_SELECTED_BUTTON_LABEL, false);
        createButton(parent, DELETE_ALL_BUTTON_ID, DELETE_ALL_BUTTON_LABEL,
                false);
        createButton(parent, IDialogConstants.CLOSE_ID,
                IDialogConstants.CLOSE_LABEL, false);
    }

    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case MOVE_BUTTON_ID:
            try {
                moveToDataTables();
            } catch (VizException e) {
                statusHandler.error("Error moving data: ", e);
            }
            break;
        case DELETE_SELECTED_BUTTON_ID:
            deleteSelected();
            break;
        case DELETE_ALL_BUTTON_ID:
            deleteAll();
            break;
        case IDialogConstants.CLOSE_ID:
            close();
            break;
        default:
            statusHandler.warn(String.format(
                    "Unrecognized button [%d] pressed.", buttonId));
            break;
        }
    }

    /**
     * Get the data from the DB
     */
    private Collection<DataTrashCanData> getTrashData() {
        Collection<DataTrashCanData> trashData = Collections.emptyList();
        try {
            trashData = DataTrashCanDataManager.getInstance()
                    .getDataTrashCanData(this);
        } catch (VizException e) {
            statusHandler.error("Problem getting trash data: ", e);
        }
        return trashData;
    }

    /**
     * Populate the physical elements list.
     */
    private String[] getPhyElemListData() {
        Collection<String> peList = Collections.emptyList();
        try {
            peList = DataTrashCanDataManager.getInstance().getPEList();
        } catch (VizException e) {
            statusHandler.error("Problem getting PE values: ", e);
        }
        return peList.toArray(new String[0]);
    }

    /**
     * Called by the "Delete Selected" button. Deletes only the selected records
     * from the RejectedData table.
     */
    private void deleteSelected() {
        ISelection sel = dataTable.getSelection();
        if ((sel != null) && (sel instanceof IStructuredSelection)) {
            IStructuredSelection selection = (IStructuredSelection) dataTable
                    .getSelection();
            if (!selection.isEmpty()) {
                String prompt = "Do you wish to delete "
                        + ((selection.size() > 1) ? "these " + selection.size()
                                + " records?" : "this record?");
                boolean delete = MessageDialog.openConfirm(getShell(),
                        "Delete Confirmation", prompt);
                if (delete) {
                    deleteRecords(selection.toList());
                }
            }
        }
    }

    /**
     * Called by the "Delete All" button. Removes all records from the
     * RejectedData table.
     */
    private void deleteAll() {
        if (MessageDialog.openConfirm(getShell(), "Empty Confirmation",
                "Do you wish to delete ALL records in the Trash Bin?")) {
            // Delete All records from RejectedData
            deleteRecords((java.util.List<DataTrashCanData>) dataTable
                    .getInput());
        }
    }

    /**
     * Performs the actual deletion of records via calls to the data manager.
     * 
     * @param dataToDelete
     */
    private void deleteRecords(java.util.List<DataTrashCanData> dataToDelete) {
        // Have DM delete records
        try {
            DataTrashCanDataManager.getInstance().deleteTrashRecords(
                    dataToDelete);
        } catch (VizException e) {
            MessageDialog.openError(getShell(), "Unable to delete records.",
                    "Unable to delete records.");
        }

        // Refresh data records
        dataTable.setInput(getTrashData());
    }

    /**
     * Repost selected data to the correct PE table and remove it from the
     * rejected data table.
     * 
     * @throws VizException
     */
    private void moveToDataTables() throws VizException {
        ISelection sel = dataTable.getSelection();
        if ((sel != null) && (sel instanceof IStructuredSelection)) {
            IStructuredSelection selection = (IStructuredSelection) dataTable
                    .getSelection();
            if (!selection.isEmpty()) {
                // Repost to PE Table and Delete from Trash Table via DM
                DataTrashCanDataManager.getInstance().repostData(
                        selection.toList());
                dataTable.setInput(getTrashData());
            }
        }
    }

    @Override
    public String getSortType() {
        return sortByCbo.getItem(sortByCbo.getSelectionIndex());
    }
}
