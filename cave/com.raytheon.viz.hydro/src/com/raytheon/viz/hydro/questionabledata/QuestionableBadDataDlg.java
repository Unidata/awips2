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

package com.raytheon.viz.hydro.questionabledata;

import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.IFontProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDlg;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * This class displays the Questionable and Bad Data dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 09 sep 2010  5399	   lbousaidi   changed constructor for both
 * 							 		   openTabularTimeSeries and openGraphTimeSeries
 * 05 Feb 2013  1578       rferrel     Changes for non-blocking singleton TimeSeriesDlg.
 *                                     Changes for non-blocking dialog.
 * 06 May 2016  5483       dgilling    Refactor based on CaveJFACEDialog, fix
 *                                     hi-dpi layout issues, code cleanup.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class QuestionableBadDataDlg extends CaveJFACEDialog {

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

    private static final String TABULAR_TS_BUTTON_LABEL = "Tabular Time Series";

    private static final int TABULAR_TS_BUTTON_ID = IDialogConstants.CLIENT_ID + 1;

    private static final String GRAPH_TS_BUTTON_LABEL = "Graph Time Series";

    private static final int GRAPH_TS_BUTTON_ID = IDialogConstants.CLIENT_ID + 2;

    private static final String SET_MISSING_BUTTON_LABEL = "Set Missing";

    private static final int SET_MISSING_BUTTON_ID = IDialogConstants.CLIENT_ID + 3;

    private static final String DELETE_SELECTED_BUTTON_LABEL = "Delete Selected";

    private static final int DELETE_SELECTED_BUTTON_ID = IDialogConstants.CLIENT_ID + 4;

    private static final String[] PHYSICAL_ELEMENTS = { "Agriculture",
            "Discharge", "Evaporation", "FishCount", "Gate Dam", "Ground",
            "Height (Stage)", "Ice", "Lake", "Moisture", "Power",
            "Precipitation (PC)", "Precipitation (PP)",
            "Precipitation (Other)", "Pressure", "Radiation", "Snow",
            "Temperature", "WaterQuality", "Weather", "Wind", "YUnique" };

    private static DateFormat OBS_TIME_FORMAT = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm") {
        {
            setTimeZone(TimeUtil.GMT_TIME_ZONE);
        }
    };

    private static DateFormat TIME_FORMAT = new SimpleDateFormat("MM-dd HH:mm") {
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
     * Physical element combo box.
     */
    private Combo physicalElemCbo;

    /**
     * Spinner indicating how many days back to display data.
     */
    private Spinner daysBackSpnr;

    /**
     * Sort by location radio button.
     */
    private Button locationRdo;

    /**
     * Sort by time radio button.
     */
    private Button timeRdo;

    /**
     * Sort by SHEF quality radio button.
     */
    private Button shefQualityRdo;

    /**
     * Sort by quality code radio button.
     */
    private Button qualityCodeRdo;

    /**
     * List control displaying the bad data.
     */
    private TableViewer dataTable;

    /**
     * QC description text control.
     */
    private Text qcDescriptionTF;

    /**
     * Stack composite.
     */
    private Composite stackComposite;

    /**
     * Stack layout.
     */
    private StackLayout stackLayout;

    /**
     * Table composite.
     */
    private Composite tableComp;

    /**
     * No data composite.
     */
    private Composite noDataComp;

    private Label noDataLabel;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public QuestionableBadDataDlg(Shell parentShell) {
        super(parentShell);
        setBlockOnOpen(false);
        setShellStyle(SWT.DIALOG_TRIM);

        this.VALUE_FORMAT = NumberFormat.getNumberInstance();
        this.VALUE_FORMAT.setMinimumFractionDigits(0);
        this.VALUE_FORMAT.setMaximumFractionDigits(8);
        this.VALUE_FORMAT.setMaximumIntegerDigits(8);
        this.VALUE_FORMAT.setMinimumFractionDigits(1);
        this.VALUE_FORMAT.setGroupingUsed(false);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Questionable and Bad Data");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        createTopControls(composite);
        createStackComposite(composite);
        createDescriptionControl(composite);
        return composite;
    }

    /**
     * Create the Filter and Sort By controls located at the top of the dialog.
     * 
     * @param parent
     */
    private void createTopControls(Composite parent) {
        Composite topComp = new Composite(parent, SWT.NONE);
        topComp.setLayout(new GridLayout(12, false));
        topComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label filterLbl = new Label(topComp, SWT.NONE);
        filterLbl.setText("Filter By:");
        filterLbl
                .setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, true));

        locationChk = new Button(topComp, SWT.CHECK);
        locationChk.setText("Location");
        locationChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                locationTF.setEnabled(locationChk.getSelection());
                updateDisplayList();
            }
        });
        locationChk.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        locationTF = new Text(topComp, SWT.BORDER);
        locationTF.setEnabled(false);
        locationTF.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                updateDisplayList();
            }
        });
        GC gc = new GC(locationTF);
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        int charHeight = gc.getFontMetrics().getHeight();
        gc.dispose();
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = locationTF.computeTrim(0, 0, charWidth * 8, charHeight).width;
        locationTF.setLayoutData(gd);

        physicalElemCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        physicalElemCbo.setItems(PHYSICAL_ELEMENTS);
        physicalElemCbo.select(6);
        physicalElemCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                dataTable.setInput(getData());
                updateStackComposite();
            }
        });
        physicalElemCbo.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER,
                false, true));

        Label lastLbl = new Label(topComp, SWT.RIGHT);
        lastLbl.setText("Last");
        lastLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, true));

        daysBackSpnr = new Spinner(topComp, SWT.BORDER);
        daysBackSpnr.setDigits(0);
        daysBackSpnr.setIncrement(1);
        daysBackSpnr.setPageIncrement(5);
        daysBackSpnr.setSelection(1);
        daysBackSpnr.setMinimum(0);
        daysBackSpnr.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER, false,
                true));
        daysBackSpnr.addFocusListener(new FocusAdapter() {

            @Override
            public void focusLost(FocusEvent e) {
                dataTable.setInput(getData());
                updateStackComposite();
            }
        });

        Label daysLbl = new Label(topComp, SWT.NONE);
        daysLbl.setText("days");
        daysLbl.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, true));

        Label sortByLbl = new Label(topComp, SWT.RIGHT);
        sortByLbl.setText("Sort By:");
        sortByLbl
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, true));

        locationRdo = new Button(topComp, SWT.RADIO);
        locationRdo.setText("Location");
        locationRdo.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER, false,
                true));
        locationRdo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDisplayList();
            }
        });

        timeRdo = new Button(topComp, SWT.RADIO);
        timeRdo.setText("Time");
        timeRdo.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER, false, true));
        timeRdo.setSelection(true);
        timeRdo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDisplayList();
            }
        });

        shefQualityRdo = new Button(topComp, SWT.RADIO);
        shefQualityRdo.setText("Shef Quality");
        shefQualityRdo.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER,
                false, true));
        shefQualityRdo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDisplayList();
            }
        });

        qualityCodeRdo = new Button(topComp, SWT.RADIO);
        qualityCodeRdo.setText("Quality Code");
        qualityCodeRdo.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER,
                false, true));
        qualityCodeRdo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDisplayList();
            }
        });
    }

    /**
     * Create stack composite that will contain the table of QuestionableData
     * records or the label saying no records matched the query parameters.
     * 
     * @param parent
     */
    private void createStackComposite(Composite parent) {
        stackComposite = new Composite(parent, SWT.NONE);
        stackLayout = new StackLayout();
        stackComposite.setLayout(stackLayout);

        // create the composites in the stack here
        tableComp = createDataListControl(stackComposite);
        noDataComp = createNoDataComposite(stackComposite);

        stackLayout.topControl = tableComp;
        stackComposite.layout();
    }

    /**
     * Create the "questionable and bad" data list control.
     * 
     * @param parent
     */
    private Composite createDataListControl(Composite parent) {
        Composite comp = new Composite(parent, SWT.NONE);
        comp.setLayout(new GridLayout(1, false));
        comp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        dataTable = new TableViewer(comp, SWT.MULTI | SWT.FULL_SELECTION
                | SWT.V_SCROLL | SWT.BORDER);
        final Table table = dataTable.getTable();
        table.setHeaderVisible(true);
        table.setLinesVisible(false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = table.getHeaderHeight() + (table.getItemHeight() * 17);
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
                QuestionableData data = (QuestionableData) element;
                return data.getLid();
            }
        });

        TableViewerColumn colName = new TableViewerColumn(dataTable, SWT.NONE);
        colName.getColumn().setText("Name");
        colName.getColumn().setWidth(22 * charWidth);
        colName.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                QuestionableData data = (QuestionableData) element;
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
                QuestionableData data = (QuestionableData) element;
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
                QuestionableData data = (QuestionableData) element;
                return Integer.toString(data.getDur());
            }
        });

        TableViewerColumn colTs = new TableViewerColumn(dataTable, SWT.NONE);
        colTs.getColumn().setText("TS");
        colTs.getColumn().setWidth(4 * charWidth);
        colTs.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                QuestionableData data = (QuestionableData) element;
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
                QuestionableData data = (QuestionableData) element;
                return data.getExtremum();
            }
        });

        TableViewerColumn colValue = new TableViewerColumn(dataTable, SWT.RIGHT);
        colValue.getColumn().setText("Value");
        colValue.getColumn().setWidth(10 * charWidth);
        colValue.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                QuestionableData data = (QuestionableData) element;
                return VALUE_FORMAT.format(data.getValue());
            }
        });

        TableViewerColumn colObsTime = new TableViewerColumn(dataTable,
                SWT.NONE);
        colObsTime.getColumn().setText("Observation Time");
        colObsTime.getColumn().setWidth(18 * charWidth);
        colObsTime.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                QuestionableData data = (QuestionableData) element;
                return OBS_TIME_FORMAT.format(data.getObstime());
            }
        });

        TableViewerColumn colRevision = new TableViewerColumn(dataTable,
                SWT.NONE);
        colRevision.getColumn().setText("RV");
        colRevision.getColumn().setWidth(4 * charWidth);
        colRevision.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                QuestionableData data = (QuestionableData) element;
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
                QuestionableData data = (QuestionableData) element;
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
                QuestionableData data = (QuestionableData) element;
                return data.getQualityCodeSymbol();
            }
        });

        TableViewerColumn colProduct = new TableViewerColumn(dataTable,
                SWT.NONE);
        colProduct.getColumn().setText("Product");
        colProduct.getColumn().setWidth(12 * charWidth);
        colProduct.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                QuestionableData data = (QuestionableData) element;
                return data.getProductID();
            }
        });

        TableViewerColumn colProdTime = new TableViewerColumn(dataTable,
                SWT.NONE);
        colProdTime.getColumn().setText("Time");
        colProdTime.getColumn().setWidth(13 * charWidth);
        colProdTime.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                QuestionableData data = (QuestionableData) element;
                Date productTime = data.getProductTime();
                return (productTime != null) ? TIME_FORMAT.format(productTime)
                        : "";
            }
        });

        TableViewerColumn colPostTime = new TableViewerColumn(dataTable,
                SWT.NONE);
        colPostTime.getColumn().setText("Posted");
        colPostTime.getColumn().setWidth(13 * charWidth);
        colPostTime.setLabelProvider(new MonospaceColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                QuestionableData data = (QuestionableData) element;
                Date postingTime = data.getPostingTime();
                return (postingTime != null) ? TIME_FORMAT.format(postingTime)
                        : "";
            }
        });

        dataTable.setContentProvider(ArrayContentProvider.getInstance());
        dataTable.setInput(getData());

        dataTable.addFilter(new ViewerFilter() {

            @Override
            public boolean select(Viewer viewer, Object parentElement,
                    Object element) {
                if ((locationTF.isEnabled())
                        && (!locationTF.getText().trim().isEmpty())) {
                    String searchString = locationTF.getText().trim()
                            .toUpperCase();
                    QuestionableData data = (QuestionableData) element;
                    return data.getLid().contains(searchString);
                }

                return true;
            }
        });

        dataTable.setComparator(new ViewerComparator() {

            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
                QuestionableData data1 = (QuestionableData) e1;
                QuestionableData data2 = (QuestionableData) e2;

                if (locationRdo.getSelection()) {
                    int compare = data1.getLid().compareTo(data2.getLid());
                    if (compare != 0) {
                        return compare;
                    }

                    return -data1.getObstime().compareTo(data2.getObstime());
                } else if (timeRdo.getSelection()) {
                    int compare = -data1.getObstime().compareTo(
                            data2.getObstime());
                    if (compare != 0) {
                        return compare;
                    }

                    return data1.getLid().compareTo(data2.getLid());
                } else if (shefQualityRdo.getSelection()) {
                    int compare = data1.getShefQualCode().compareTo(
                            data2.getShefQualCode());
                    if (compare != 0) {
                        return compare;
                    }

                    compare = data1.getLid().compareTo(data2.getLid());
                    if (compare != 0) {
                        return compare;
                    }

                    return -data1.getObstime().compareTo(data2.getObstime());
                } else {
                    int compare = -Integer.compare(data1.getQualityCode(),
                            data2.getQualityCode());
                    if (compare != 0) {
                        return compare;
                    }

                    compare = data1.getLid().compareTo(data2.getLid());
                    if (compare != 0) {
                        return compare;
                    }

                    return -data1.getObstime().compareTo(data2.getObstime());
                }
            }
        });

        dataTable.addSelectionChangedListener(new ISelectionChangedListener() {

            @Override
            public void selectionChanged(SelectionChangedEvent event) {
                updateDescription(event);
            }
        });

        return comp;
    }

    /**
     * Creates the {@link Composite} containing the label that will be displayed
     * if no data matches the user's query parameters.
     * 
     * @param parent
     * @return
     */
    private Composite createNoDataComposite(Composite parent) {
        Composite comp = new Composite(parent, SWT.NONE);
        comp.setLayout(new FillLayout());

        noDataLabel = new Label(comp, SWT.WRAP);
        noDataLabel.setText("Placeholder");

        return comp;
    }

    /**
     * Create the QC description label and text control.
     * 
     * @param parent
     */
    private void createDescriptionControl(Composite parent) {
        Composite qcComp = new Composite(parent, SWT.NONE);
        qcComp.setLayout(new GridLayout(2, false));
        qcComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label qcLbl = new Label(qcComp, SWT.NONE);
        qcLbl.setText("QC Description");
        qcLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));

        qcDescriptionTF = new Text(qcComp, SWT.BORDER);
        qcDescriptionTF.setEditable(false);
        qcDescriptionTF.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, TABULAR_TS_BUTTON_ID, TABULAR_TS_BUTTON_LABEL,
                false);
        createButton(parent, GRAPH_TS_BUTTON_ID, GRAPH_TS_BUTTON_LABEL, false);
        createButton(parent, SET_MISSING_BUTTON_ID, SET_MISSING_BUTTON_LABEL,
                false);
        createButton(parent, DELETE_SELECTED_BUTTON_ID,
                DELETE_SELECTED_BUTTON_LABEL, false);
        createButton(parent, IDialogConstants.CLOSE_ID,
                IDialogConstants.CLOSE_LABEL, false);
    }

    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case TABULAR_TS_BUTTON_ID:
            openTabularTimeSeries(getCurrentlySelectedData(dataTable
                    .getSelection()));
            break;
        case GRAPH_TS_BUTTON_ID:
            openGraphTimeSeries(getCurrentlySelectedData(dataTable
                    .getSelection()));
            break;
        case SET_MISSING_BUTTON_ID:
            setMissing(getCurrentlySelectedRange());
            break;
        case DELETE_SELECTED_BUTTON_ID:
            deleteRecords(getCurrentlySelectedRange());
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
     * Retrieves the sorted data from the database
     * 
     * @return list of sorted {@link QuestionableData} records matching the
     *         user's query parameters from the UI.
     */
    private Collection<QuestionableData> getData() {
        String sortCriteria = "";
        if (timeRdo.getSelection()) {
            sortCriteria = timeRdo.getText();
        } else if (locationRdo.getSelection()) {
            sortCriteria = locationRdo.getText();
        } else if (shefQualityRdo.getSelection()) {
            sortCriteria = shefQualityRdo.getText();
        } else if (qualityCodeRdo.getSelection()) {
            sortCriteria = qualityCodeRdo.getText();
        }

        String physElement = physicalElemCbo.getItem(physicalElemCbo
                .getSelectionIndex());

        int daysBack = daysBackSpnr.getSelection();

        // Pass table, days, sort order
        return QuestionableDataManager.getInstance().getQuestionableData(
                physElement, daysBack, sortCriteria);
    }

    /**
     * Refreshes the records displayed
     */
    private void updateDisplayList() {
        dataTable.refresh();
        updateStackComposite();
    }

    private void updateStackComposite() {
        if (dataTable.getTable().getItemCount() > 0) {
            stackLayout.topControl = tableComp;
        } else {
            StringBuilder showErrorMsg = new StringBuilder();
            showErrorMsg
                    .append("No Questionable or Bad data found within the past ");
            showErrorMsg.append(daysBackSpnr.getSelection());
            showErrorMsg.append(" days in table ");
            showErrorMsg.append(physicalElemCbo.getItem(physicalElemCbo
                    .getSelectionIndex()));
            if (locationChk.getSelection()) {
                showErrorMsg.append(" for location ");
                showErrorMsg.append(locationTF.getText());
            }

            stackLayout.topControl = noDataComp;
            noDataLabel.setText(showErrorMsg.toString());
        }

        stackComposite.layout();
    }

    /**
     * Updated the QC description for the selected data.
     * 
     * @param event
     *            {@link SelectionChangedEvent} event containing the
     *            newly-selected record that forced the update to the QC
     *            description control.
     */
    private void updateDescription(SelectionChangedEvent event) {
        String newDescription = "";
        QuestionableData selected = getCurrentlySelectedData(event
                .getSelection());
        if (selected != null) {
            newDescription = QuestionableDataManager.getInstance()
                    .getDescription(selected);
        }
        qcDescriptionTF.setText(newDescription);
    }

    /**
     * Remove the selected data records.
     */
    private void deleteRecords(List<QuestionableData> recordsToDelete) {
        try {
            String physElement = physicalElemCbo.getItem(physicalElemCbo
                    .getSelectionIndex());
            QuestionableDataManager.getInstance().deleteRecords(
                    recordsToDelete, physElement);
            dataTable.setInput(getData());
            updateStackComposite();
        } catch (VizException e) {
            MessageDialog.openConfirm(getShell(), "Unable to delete records.",
                    "Unable to delete records.");
            statusHandler.error("Unable to delete records.", e);
        }
    }

    /**
     * Display the time series graph for the selected record.
     */
    private void openGraphTimeSeries(QuestionableData currData) {
        if (currData != null) {
            getShell().setCursor(
                    getShell().getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
            TimeSeriesDlg.getInstance().updateAndOpen(currData.getLid(),
                    currData.getPe(), currData.getTs(), true);
            getShell().setCursor(null);
        }
    }

    /**
     * Open time series tabular information for the selected record.
     */
    private void openTabularTimeSeries(QuestionableData currData) {
        if (currData != null) {
            getShell().setCursor(
                    getShell().getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
            TimeSeriesDlg.getInstance().updateAndOpen(currData.getLid(),
                    currData.getPe(), currData.getTs(), false);
            getShell().setCursor(null);
        }
    }

    /**
     * The the data record for the selected line.
     * 
     * @param selection
     *            Current selection from the {@code dataTable}.
     * 
     * @return questionableData
     */
    private QuestionableData getCurrentlySelectedData(ISelection selection) {
        QuestionableData rval = null;

        if ((selection instanceof IStructuredSelection)
                && (((IStructuredSelection) selection).size() > 0)) {
            rval = (QuestionableData) ((IStructuredSelection) selection)
                    .getFirstElement();
        }

        return rval;
    }

    /**
     * Get a list of records for the selected line(s).
     * 
     * @return list
     */
    private List<QuestionableData> getCurrentlySelectedRange() {
        List<QuestionableData> rval = Collections.emptyList();

        ISelection selection = dataTable.getSelection();
        if ((selection instanceof IStructuredSelection)
                && (((IStructuredSelection) selection).size() > 0)) {
            rval = ((IStructuredSelection) selection).toList();
        }

        return rval;
    }

    private void setMissing(List<QuestionableData> recordsToSetMissing) {
        try {
            QuestionableDataManager.getInstance().setMissing(
                    recordsToSetMissing);
            dataTable.setInput(getData());
            updateStackComposite();
        } catch (VizException e) {
            statusHandler.error("Unable to set record as missing.", e);
        }
    }
}
