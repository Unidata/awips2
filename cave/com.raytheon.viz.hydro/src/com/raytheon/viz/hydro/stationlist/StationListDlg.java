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

package com.raytheon.viz.hydro.stationlist;

import java.util.Iterator;
import java.util.SortedMap;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.pdc.PointDataControlManager;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDlg;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.events.MapUpdateEvent;
import com.raytheon.viz.hydrocommon.events.StationSelectionChangeEvent;
import com.raytheon.viz.hydrocommon.listeners.MapUpdateListener;
import com.raytheon.viz.hydrocommon.listeners.StationSelectionChangeListener;
import com.raytheon.viz.hydrocommon.util.HydroDialogStatus;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Station List dialog for HydroView.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation.
 * 10/2/2008    1555       grichard    Support station selection. 
 * 12/21/2008              mduff       Implement single and double clicks.
 * 01/10/2008   1802       askripsk    Finished single and double clicks.
 * 07/03/2010   5906       mduff       Fixed the list to match the data.
 * 02/05/2013   1578       rferrel     Changes for non-blocking singleton TimeSeriesDlg.
 * 03/29/2013   1790       rferrel     Make dialog non-blocking.
 * 04/09/2015   4215       mduff       Fixed IndexOutOfBounds Exception.
 * May 03, 2016 5623       bkowal      Removed usage of unimplemented method. Cleanup.
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class StationListDlg extends CaveSWTDialog
        implements MapUpdateListener, StationSelectionChangeListener {

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Font used for the list controls.
     */
    private Font font;

    /**
     * Search text field.
     */
    private Text searchTF;

    /**
     * Map of station list data.
     */
    private final SortedMap<String, GageData> dataMap = new TreeMap<>();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public StationListDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Station List");
        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        manager.addMapUpdateListener(this);
        manager.addStationSelectionListener(this);
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
        HydroDialogStatus.stationListDlgOpen = false;
        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        manager.removeMapUpdateListener(this);
        manager.removeStationSelectionListener(this);

    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        // Initialize all of the controls and layouts
        createDataListLabels();
        createStationListControl();
        createSearchControl();
        addSeparator();
        createBottomControl();

        populateStationList();

        HydroDialogStatus.stationListDlgOpen = true;
    }

    /**
     * Create the labels for the data list.
     */
    private void createDataListLabels() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        labelComp.setLayout(gl);

        GridData gd = new GridData(75, SWT.DEFAULT);
        Label idLbl = new Label(labelComp, SWT.NONE);
        idLbl.setLayoutData(gd);
        idLbl.setText("Id");

        Label nameLbl = new Label(labelComp, SWT.NONE);
        nameLbl.setText("Name");
    }

    /**
     * Create the station list control.
     */
    private void createStationListControl() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 265;
        gd.heightHint = 350;
        dataList = new List(shell,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL);
        dataList.setLayoutData(gd);

        dataList.setFont(font);

        dataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectSite();
            }
        });

        dataList.addMouseListener(new MouseListener() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                // Open TimeSeries on Double click
                String selection = dataList
                        .getItem(dataList.getSelectionIndex());
                String[] parts = selection.trim().split("\\s+");
                GageData data = dataMap.get(parts[0]);
                if (data != null) {
                    shell.setCursor(
                            getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
                    HydroDisplayManager.getInstance().setCurrentData(data);
                    TimeSeriesDlg.getInstance().updateAndOpen(data.getLid(),
                            false);
                    shell.setCursor(null);
                }
            }

            @Override
            public void mouseDown(MouseEvent e) {
            }

            @Override
            public void mouseUp(MouseEvent e) {
            }
        });

    }

    /**
     * Create the search text control.
     */
    private void createSearchControl() {
        Composite searchComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        searchComp.setLayout(gl);
        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        searchComp.setLayoutData(gd);

        Label searchLbl = new Label(searchComp, SWT.NONE);
        searchLbl.setText("Search: ");

        gd = new GridData(100, SWT.DEFAULT);
        searchTF = new Text(searchComp, SWT.BORDER);
        searchTF.setLayoutData(gd);
        searchTF.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                // intentionally left blank
            }

            @Override
            public void keyReleased(KeyEvent e) {
                // When the user types in the search text control,
                // check that list of station data and highlight
                // the matched entry in the list.
                String tmp = searchTF.getText().toUpperCase();
                int i = 0;
                String[] listItems = dataList.getItems();
                for (String item : listItems) {
                    if (item.startsWith(tmp)) {
                        dataList.select(i);
                        selectSite();
                        break;
                    }
                    i++;
                }
            }
        });
    }

    /**
     * Add a horizontal separator bar to the dialog.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create the cancel button at the bottom of the dialog.
     */
    private void createBottomControl() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        Button cancelBtn = new Button(centeredComp, SWT.NONE);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Prepare the display of the stations.
     */
    private void populateStationList() {
        java.util.List<GageData> gageDataList = HydroDisplayManager
                .getInstance().getObsReportList();

        if (gageDataList != null) {
            String format = "%5s %s  [%3.2f %3.2f]";
            dataList.removeAll();
            dataMap.clear();

            // populate the dataMap
            for (int i = 0; i < gageDataList.size(); i++) {
                GageData data = gageDataList.get(i);
                dataMap.put(data.getLid(), data);
            }

            // Populate the list widget
            Iterator<GageData> iter = dataMap.values().iterator();
            while (iter.hasNext()) {
                GageData data = iter.next();
                double lat = data.getLat();
                double lon = data.getLon();
                if (lon != HydroConstants.MISSING_VALUE) {
                    lon *= -1;
                }
                String name = data.getName();
                if ((lat > 0) && (lon > 0)) {
                    if (name == null) {
                        name = "     ";
                    }
                    String displayString = String.format(format, data.getLid(),
                            name, lat, lon);
                    dataList.add(displayString);
                }
            }
        }
        setSelection();
    }

    /**
     * Get site for the current selection and update the perspective's map to
     * have the site slected.
     */
    private void selectSite() {
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();
        if (dataList.getSelectionIndex() == -1) {
            return;
        }

        String selection = dataList.getItem(dataList.getSelectionIndex());
        String[] parts = selection.trim().split("\\s+");
        if (dataMap != null) {
            GageData data = dataMap.get(parts[0]);
            if (data != null) {
                displayManager.setCurrentData(data);

                PointDataControlManager pdcManager = PointDataControlManager
                        .getInstance();
                pdcManager.setRedraw(true);
            }
        }
    }

    /**
     * Set the list to the manager's current station.
     */
    private void setSelection() {
        // Select the previously selected station
        GageData currStation = HydroDisplayManager.getInstance()
                .getCurrentData();

        if (currStation != null) {
            String lid = currStation.getLid();
            String listLid = "";
            if (dataList.getItemCount() > 0) {
                for (int i = 0; i < dataList.getItemCount(); i++) {
                    String entry = dataList.getItem(i).trim();
                    if (entry.length() > 0) {
                        listLid = entry.split("\\s+")[0];
                    }
                    if (listLid.equals(lid)) {
                        dataList.select(i);
                        dataList.showSelection();
                        break;
                    }
                }
            }
        } else {
            dataList.deselectAll();
        }
    }

    @Override
    public void notifyUpdate(MapUpdateEvent mue) {
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                dataList.removeAll();
                populateStationList();
            }
        });
    }

    @Override
    public void notifyUpdate(StationSelectionChangeEvent sdue) {
        setSelection();
    }
}
