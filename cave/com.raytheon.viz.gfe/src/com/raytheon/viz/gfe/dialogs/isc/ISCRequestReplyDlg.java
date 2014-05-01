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
package com.raytheon.viz.gfe.dialogs.isc;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The ISC Request dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/20/09      1995       lvenable    Initial port
 * 10/24/2008   1287        rferrel     Made dialog non-blocking.
 * 12/28/2012   DR15587     jzeng       Query weather elements from fcst DB 
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class ISCRequestReplyDlg extends CaveSWTDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ISCRequestReplyDlg.class);

    private ListManager domainList;

    private ListManager gridSrcList;

    private ListManager weatherElemList;

    private Label gridSrcLbl;

    private DataManager dataMgr;

    private List<String> weList;

    private Map<String, Map<String, List<Map<String, String>>>> domainDict;

    private Map<String, Map<String, String>> serverDictT2S;

    private String xml;

    private boolean iscAvailable;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public ISCRequestReplyDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        setText("ISC Request/Reply");

        dataMgr = DataManager.getCurrentInstance();
        iscAvailable = dataMgr.requestISC();
    }

    @Override
    protected void initializeComponents(Shell shell) {

        createLabelsAndLists();
        createBottomButtons();
        if (iscAvailable) {
            initializeData();
            populateWEList();
        } else {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Server Problem: Unable to get server info from iscRequestQuery -IRT Address/requestISC not enabled. No ISC data available");
        }
        populateDomainList();
    }

    private void createLabelsAndLists() {
        final Composite listComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 15;
        listComp.setLayout(gl);
        listComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label domainLbl = new Label(listComp, SWT.CENTER);
        domainLbl.setText("Domain");
        domainLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridSrcLbl = new Label(listComp, SWT.CENTER);
        gridSrcLbl.setText("Grid Source");
        gridSrcLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label weatherLbl = new Label(listComp, SWT.CENTER);
        weatherLbl.setText("Weather Elements");
        weatherLbl.setLayoutData(gd);

        domainList = new ListManager(listComp) {
            @Override
            protected void addListListener() {
                dataList.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        updateListIndexes();
                        gridSrcList.clearList();
                        List<String> selectedDomains = domainList
                                .getSelectedItems();
                        for (String d : selectedDomains) {
                            for (String str : serverDictT2S.keySet()) {
                                if (str.startsWith(d)) {
                                    gridSrcList.addItemToList(str);
                                }
                            }
                        }
                        shell.setBounds(shell.getBounds().x,
                                shell.getBounds().y,
                                shell.getBounds().width + 500,
                                shell.getBounds().height);
                        shell.pack();
                    }
                });
            }
        };

        gridSrcList = new ListManager(listComp);

        weatherElemList = new ListManager(listComp);

        if (!iscAvailable) {
            listComp.setEnabled(false);

        }

    }

    private void createBottomButtons() {
        addSeparator(shell);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 120;

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button makeRequestBtn = new Button(buttonComp, SWT.PUSH);
        makeRequestBtn.setText("Make Request");
        makeRequestBtn.setLayoutData(gd);
        makeRequestBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                makeRequest();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("&Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });
        if (!iscAvailable) {
            makeRequestBtn.setEnabled(false);
        }
    }

    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    @SuppressWarnings("unchecked")
    private void initializeData() {
        Object[] response = dataMgr.doIscRequestQuery();
        this.xml = (String) response[0];
        this.weList = (List<String>) response[1];
        Collections.sort(this.weList);
        
        /*
         * If the weList is empty, get it from database
         */
        if (this.weList.isEmpty() ){
            String query = "Select distinct (parmname) from awips.gfe";
            List<Object[]> list = null;
            try {
            	list = DirectDbQuery.executeQuery(query, "metadata",
                    QueryLanguage.SQL);
            	for (Object[] we : list){
                    weList.add(we[0].toString());
            	}
            } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error querying database", e);
            }
        }

        
        domainDict = (Map<String, Map<String, List<Map<String, String>>>>) response[2];
        serverDictT2S = (Map<String, Map<String, String>>) response[4];

        // output the list of servers and their priority
        String s = "\n";
        for (String key : this.domainDict.keySet()) {
            s += "DOMAIN=" + key + "\n";
            ArrayList<Map<String, String>> servers = (ArrayList<Map<String, String>>) this.domainDict
                    .get(key);
            for (Map<String, String> server : servers) {
                s += "  mhs=" + server.get("mhsid") + " host="
                        + server.get("host") + " port=" + server.get("port")
                        + "\n";
            }

        }
        Activator
                .getDefault()
                .getLog()
                .log(new Status(IStatus.INFO, Activator.PLUGIN_ID,
                        "DomainDict servers:" + s));

    }

    private void populateDomainList() {
        this.domainList.clearList();
        if (iscAvailable) {
            domainList.addItemToList(domainDict.keySet().toArray(
                    new String[] {}));
        } else {
            domainList.addItemToList("ISC Request/Reply not avaliable");
        }
        domainList.deselectAll();

    }

    private void populateWEList() {
        this.weatherElemList.clearList();
        this.weatherElemList.addItemToList(weList.toArray(new String[] {}));
        this.weatherElemList.selectAll();
        this.weatherElemList.deselectAll();
    }

    public void makeRequest() {
        if (iscAvailable) {
            try {
                List<String> weatherElements = null;
                if (this.domainList.getSelectedItems().isEmpty()) {
                    statusHandler.handle(Priority.PROBLEM,
                            "No Domain has been selected");
                    return;
                }
                if (this.gridSrcList.getSelectedItems().isEmpty()) {
                    statusHandler.handle(Priority.PROBLEM,
                            "No Grid Source has been selected");
                    return;
                }
                if (this.weatherElemList.getSelectedItems().isEmpty()) {
                    weatherElements = this.weatherElemList.getItems();
                } else {
                    weatherElements = this.weatherElemList.getSelectedItems();
                }
                String response = dataMgr.getClient().iscGetRequestXML(xml,
                        this.gridSrcList.getSelectedItems(), weatherElements);
                dataMgr.makeISCRequest(response);
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Server Problem: Error making request", e);
            }
        }
    }
}
