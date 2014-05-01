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

package com.raytheon.viz.hydro.pointdatacontrol;

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.pointdatacontrol.data.PointControlPeTs;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydrocommon.HydroDataCache;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays a common dialog used for Type/Source, Service Area, or
 * Data Source filter from the Point Data Control dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 25 JAN 2011  7625       bkowal      The dialog will now be a modal dialog
 *                                     and it will include a title bar and
 *                                     close button.
 * 07 FEB 2013  1578       rferrel     Change for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class FilteringDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FilteringDlg.class);

    /** Type sources to always include. */
    private static final String[] HARD_CODED_TYPE_SOURCES = { "RG", "RP", "RM",
            "RR", "RZ" };

    /**
     * List data control.
     */
    private List dataList;

    /**
     * Point Data Control Data Manager.
     */
    private PDCDataManager dataManager;

    /**
     * The PE Selection from the main PDC dialog
     */
    private String peSelection = null;

    /**
     * Dialog Type enumeration.
     * 
     * @author lvenable
     * 
     */
    public static enum DialogType {
        TYPE_SOURCE, SERVICE_AREA, DATA_SOURCE
    };

    /**
     * Instance of dialog type.
     */
    private DialogType dialogType;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param title
     *            Dialog title.
     * @param dialogType
     *            Type of dialog that will be created.
     */
    public FilteringDlg(Shell parent, String title, DialogType dialogType) {
        this(parent, title, dialogType, null);
    }

    /**
     * Second Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param title
     *            Dialog title.
     * @param dialogType
     *            Type of dialog that will be created.
     * @param peSelection
     *            Physical Element selection.
     */
    public FilteringDlg(Shell parent, String title, DialogType dialogType,
            String peSelection) {
        super(parent, SWT.DIALOG_TRIM | SWT.SYSTEM_MODAL, CAVE.DO_NOT_BLOCK);
        setText(title);

        this.dialogType = dialogType;
        dataManager = PDCDataManager.getInstance();
        this.peSelection = peSelection;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 5;
        mainLayout.marginWidth = 5;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        createDataListBox();
        createBottomButtons();
        populateList();
    }

    /**
     * Create the data list control.
     */
    private void createDataListBox() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.heightHint = 220;
        gd.widthHint = 275;
        dataList = new List(shell, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
    }

    /**
     * Create the bottom dialog buttons.
     */
    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(2, true));

        gd = new GridData(80, SWT.DEFAULT);
        Button applyBtn = new Button(buttons, SWT.PUSH);
        applyBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleApply();
                setReturnValue(true);
                close();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                close();
            }
        });

    }

    /**
     * Populate the list.
     */
    private void populateList() {
        /*
         * Get options data to see which data source items are preselected
         */
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        try {
            if (dialogType == DialogType.DATA_SOURCE) {
                PointControlPeTs pcPeTs = pdcManager.getPCPeTsData();

                String[] dataSourceArray = pcPeTs.getAdhocDataSrcBuf();
                for (int i = 0; i < dataSourceArray.length; i++) {
                    dataList.add(dataSourceArray[i]);
                }

                /*
                 * Highlight any data source ids that were in the pc_options
                 * data source list.
                 */
                if ((pcOptions.getDataSourcesChosen() != null)
                        && (pcOptions.getDataSourceChosenCount() > 0)) {
                    ArrayList<Integer> al = new ArrayList<Integer>();
                    for (int i = 0; i < pcOptions.getDataSourcesChosen().length; i++) {
                        for (int j = 0; j < dataList.getItemCount(); j++) {
                            if (dataSourceArray[j].equalsIgnoreCase(pcOptions
                                    .getDataSourcesChosen()[i])) {
                                al.add(j);
                                break;
                            }
                        }
                    }

                    int[] indices = new int[al.size()];
                    for (int i = 0; i < al.size(); i++) {
                        indices[i] = al.get(i);
                    }
                    dataList.setSelection(indices);
                }
            } else if (dialogType == DialogType.SERVICE_AREA) {
                ArrayList<String> hsaList = dataManager.getHsaList("");

                for (int i = 0; i < hsaList.size(); i++) {
                    dataList.add(hsaList.get(i));
                }

                /* Highlight any hsa ids that were in the pc_options hsa list. */
                if ((pcOptions.getHsaList() != null)
                        && (pcOptions.getHsaList().size() > 0)) {
                    ArrayList<Integer> al = new ArrayList<Integer>();
                    for (int i = 0; i < pcOptions.getHsaList().size(); i++) {
                        for (int j = 0; j < dataList.getItemCount(); j++) {
                            if (hsaList.get(j).equalsIgnoreCase(
                                    pcOptions.getHsaList().get(i))) {
                                al.add(j);
                                break;
                            }
                        }
                    }

                    int[] indices = new int[al.size()];
                    for (int i = 0; i < al.size(); i++) {
                        indices[i] = al.get(i);
                    }
                    dataList.setSelection(indices);
                }

            } else if (dialogType == DialogType.TYPE_SOURCE) {
                PointControlPeTs pcPeTs = pdcManager.getPCPeTsData();
                HydroDataCache cache = HydroDataCache.getInstance();
                ArrayList<String> items = new ArrayList<String>();
                boolean peIsPrimary = false;
                boolean includeInTsList = false;
                StringBuilder buf = new StringBuilder();
                int count = 0;
                int queryMode = pcOptions.getQueryMode();

                if (peSelection.equals("Primary")) {
                    peIsPrimary = true;
                }

                if (queryMode == 0) { // Ad Hoc Mode
                    ArrayList<String[]> peTsBuf = pcPeTs.getOrgBuf();
                    for (int i = 0; i < peTsBuf.size(); i++) {

                        buf.setLength(0);
                        includeInTsList = false;

                        /* check if the PE part matches */
                        if ((peSelection.split(" ")[0].equals(pcPeTs
                                .getOrgBuf().get(i)[0]))
                                || (peIsPrimary && (pcPeTs.getOrgBuf().get(i)[0]
                                        .equalsIgnoreCase("HG") || pcPeTs
                                        .getOrgBuf().get(i)[0]
                                        .equalsIgnoreCase("QR")))) {
                            buf.append(pcPeTs.getOrgBuf().get(i)[1]);
                            includeInTsList = false;

                            /*
                             * if treating processed data with regular obs, then
                             * any ts in a pets combination that matches the pe
                             * is considered. the processed table should not
                             * even be allowed to be selected in this case, so
                             * we don't need to check for that.
                             */

                            if (pcOptions.getProcessMode() == 1) {
                                includeInTsList = true;
                                /*
                                 * if the processed data are not comingled with
                                 * obs, i.e. it has its own place as one of the
                                 * 'other' tables.
                                 */
                            } else {
                                /*
                                 * if the processed table is the selected table
                                 * and the type source is a P typesource, then
                                 * consider it.
                                 */
                                // if
                                // (pcOptions.getElementType().equals(AdHocDataElementType.OTHER_AD_HOC_TYPE)
                                // &&
                                if ((pcOptions.getElementType() == 4)
                                        && (pcOptions.getProcessSelected() == 1)) {

                                    if (buf.toString().startsWith("P")) {
                                        includeInTsList = true;
                                    }
                                } else {
                                    /*
                                     * if the processed table is not the
                                     * selected table, then only consider it if
                                     * it is not a P typesource
                                     */
                                    if (!buf.toString().startsWith("P")) {
                                        includeInTsList = true;
                                    }
                                }
                            }

                            if (includeInTsList) {
                                // Add "TS  Name" to the adhoctypesourcebuffer
                                if (!items.contains(buf.toString() + "  "
                                        + cache.getTSDesc(buf.toString()))) {
                                    items.add(buf.toString() + "  "
                                            + cache.getTSDesc(buf.toString()));
                                    count++;
                                }
                            }
                        }
                    }
                    String[] itemArray = items
                            .toArray(new String[items.size()]);
                    Arrays.sort(itemArray);

                    pcPeTs.setAdhocTypeSourceBuffer(itemArray);
                    dataList.setItems(pcPeTs.getAdhocTypeSourceBuffer());
                } else { // Time Step mode
                    String where = " where ts like 'R%%' or ts like 'P%%' order by 1";
                    ArrayList<Object[]> rs = dataManager.getUnique("ts",
                            "ingestFilter", where);
                    ArrayList<String> tsList = new ArrayList<String>();

                    // add all of the hard_coded_type_sources
                    for (String ts : HARD_CODED_TYPE_SOURCES) {
                        tsList.add(ts);
                    }

                    if ((rs != null) && (rs.size() > 0)) {
                        for (Object[] oa : rs) {
                            String ts = (String) oa[0];

                            // determine if this TS should be added to the list
                            // of type sources
                            // we don't want to add a type/source that was
                            // already hardcoded
                            if (!tsList.contains(ts)) {
                                tsList.add(ts);
                            }
                        }
                    }

                    for (String s : tsList) {
                        String desc = cache.getTSDesc(s);
                        if (desc == null) {
                            items.add(s);
                        } else {
                            items.add(s + "  " + desc);
                        }
                    }

                    String[] itemArray = items
                            .toArray(new String[items.size()]);
                    Arrays.sort(itemArray);

                    pcPeTs.setTimestepTypeSourceBuffer(itemArray);
                    dataList.setItems(pcPeTs.getTimestepTypeSourceBuffer());
                }

                /*
                 * Highlight any TS selections that were in the pc_options data
                 * source list.
                 */
                if (pcOptions.getTypeSourceChosenList() != null) {
                    ArrayList<Integer> al = new ArrayList<Integer>();
                    for (int i = 0; i < pcOptions.getTypeSourceChosenList()
                            .size(); i++) {
                        for (int j = 0; j < dataList.getItemCount(); j++) {
                            String[] sa = dataList.getItem(j).split(" ", 2);
                            if (sa[0].equalsIgnoreCase(pcOptions
                                    .getTypeSourceChosenList().get(i))) {
                                al.add(j);
                                break;
                            }
                        }
                    }

                    int[] indices = new int[al.size()];
                    for (int i = 0; i < al.size(); i++) {
                        indices[i] = al.get(i);
                    }
                    dataList.setSelection(indices);
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error populating list: "
                    + e);
        }
    }

    /**
     * Handle the Apply callback.
     */
    private void handleApply() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        int[] selectionIndices = dataList.getSelectionIndices();
        ArrayList<String> tsList = new ArrayList<String>();
        String ts;
        if (selectionIndices.length > 0) {
            ArrayList<String> al = new ArrayList<String>();
            for (int i = 0; i < selectionIndices.length; i++) {
                al.add(dataList.getItem(selectionIndices[i]));
            }

            if (dialogType == DialogType.DATA_SOURCE) {
                pcOptions
                        .setDataSourcesChosen(al.toArray(new String[al.size()]));
                pcOptions.setDataSourceChosenCount(al.size());
            } else if (dialogType == DialogType.SERVICE_AREA) {
                /* find out which WFOs where selected from the WFO scrolled list */
                pcOptions.setHsaList(al);
                pdcManager.setServiceBackupInfo(al);
            } else if (dialogType == DialogType.TYPE_SOURCE) {
                for (int i = 0; i < al.size(); i++) {
                    String[] sa = al.get(i).split(" ", 2);
                    ts = sa[0];
                    tsList.add(ts);
                }
                pcOptions.setTypeSourceChosenList(tsList);
                pcOptions.setTypeSourceChosenCount(tsList.size());
            }
        }
    }
}