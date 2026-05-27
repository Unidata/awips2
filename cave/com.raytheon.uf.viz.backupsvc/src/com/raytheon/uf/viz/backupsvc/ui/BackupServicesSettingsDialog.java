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
package com.raytheon.uf.viz.backupsvc.ui;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.resource.FontDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.backupsvc.request.GetBackupConfigFileRequest;
import com.raytheon.uf.common.backupsvc.response.BackupConfigFileResponse;
import com.raytheon.uf.common.dataplugin.backupsvc.constants.BackupServicesConstants;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.site.requests.GetPrimarySiteRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Setting Dialog for the main Backup Services Dialog. This is a Backup Services
 * Dialog to view only configuration in backupSvc.xml file.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/15/2021   84638     Amanuel.Challa    Initial creation
 * Jul 27, 2021 84654     Robert.Blum       Added domains to settings dialog.
 * Aug 30, 2021 93179     Amanuel Challa    Added edex cutoff version to settings dialog.
 * Oct 08, 2021 97253     Robert.Blum       Add purge config value and fixed formatting.
 *
 * </pre>
 *
 * @author Amanuel.Challa
 * @version 1.0
 */
public class BackupServicesSettingsDialog extends CaveSWTDialog {
    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BackupServicesSettingsDialog.class);

    private static final String OVERVIEW_TEXT = "This is a Backup Services Dialog to "
            + "view only configuration in backupSvc.xml file.\n"
            + "The Dialog includes files intended for backup, as well as files that are blacklisted,\n"
            + "Backup Host information, PollIntervalSeconds,RateLimitKBps and JobSize information.";

    private String excludedFiles;

    private String includedeFiles;

    private int pollIntervalSeconds;

    private int jobSize;

    private int rateLimitKBps;

    private Table hostTable;

    private String localDomains;

    private String edexVersion;

    private int purgeTimePeriodDays;

    private BackupConfigFileResponse configFileResponseInfo;

    protected BackupServicesSettingsDialog(Shell parent) {
        super(parent, SWT.SHELL_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK
                | CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT);
        setText("Backup Services Config");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.widthHint = 700;
        mainComp.setLayoutData(gridData);
        mainComp.setLayout(new GridLayout(1, true));
        try {
            loadConfig();
            createOverviewGroup(mainComp);
        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }

        Button closeButton = new Button(mainComp, SWT.PUSH);
        gridData = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        closeButton.setLayoutData(gridData);
        closeButton.setText("Close");
        closeButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Send Request to Edex using GetBackupConfigFileRequest and Receives a
     * response with the contents of backupSvc.xml file.
     *
     * @return BackupConfigFileResponse
     *
     *         a response object with backupSvc.xml file content
     */
    public BackupConfigFileResponse makeRequest() {
        BackupConfigFileResponse response = null;
        GetBackupConfigFileRequest request = null;
        request = new GetBackupConfigFileRequest();
        try {
            response = (BackupConfigFileResponse) RequestRouter.route(request);
        } catch (Exception e) {
            statusHandler.error(
                    "Unable to send request to get backupSvc.xml file contents. ",
                    e);
        }
        return response;
    }

    /**
     * Extract the Contents of BackupConfigFileResponse and store them into
     * Global Variables to be used for displaying in settings Dialog.
     *
     * @throws Exception
     */
    public void loadConfig() throws Exception {
        configFileResponseInfo = makeRequest();
        StringBuilder exFileText = new StringBuilder();
        StringBuilder inFileText = new StringBuilder();
        // validate if all data in backupSvc.xml file are present
        if ((configFileResponseInfo != null)
                && (configFileResponseInfo.getExceptions() == null
                        || configFileResponseInfo.getExceptions().getMessage()
                                .isEmpty())) {
            if (configFileResponseInfo.getExcludeFileList().size() == 0) {
                excludedFiles = exFileText.append(
                        "Excluded Backup Files have not been configured.")
                        .toString();
            } else {
                for (String file : configFileResponseInfo
                        .getExcludeFileList()) {
                    exFileText.append(file);
                    excludedFiles = exFileText.toString();
                }
            }
            if (configFileResponseInfo.getIncludeFileList().size() == 0) {
                includedeFiles = inFileText.append(
                        "Included Backup Files have not been configured.")
                        .toString();
            } else {
                for (String file : configFileResponseInfo
                        .getIncludeFileList()) {
                    inFileText.append(file);
                    includedeFiles = inFileText.toString();
                }
            }
            if (configFileResponseInfo.getLocalDomains().isEmpty()) {
                localDomains = "Only files for the local Edex Site will be sent via Backup Services.";
            } else {
                Set<String> domains = new HashSet<>();
                domains.addAll(configFileResponseInfo.getLocalDomains());

                GetPrimarySiteRequest edexSiteRequest = new GetPrimarySiteRequest();
                try {
                    String edexSite = (String) ThriftClient
                            .sendRequest(edexSiteRequest);
                    domains.add(edexSite);
                } catch (VizException e) {
                    statusHandler.error("Error getting primary EDEX site!", e);
                }
                localDomains = "Files for these domains will be sent via Backup Services: "
                        + domains + ".";
            }
            pollIntervalSeconds = configFileResponseInfo
                    .getPollIntervalSeconds();
            jobSize = configFileResponseInfo.getBigJobSize();
            edexVersion = configFileResponseInfo.getEdexCutoffVersion();
            rateLimitKBps = configFileResponseInfo.getRateLimitKBps();
            purgeTimePeriodDays = configFileResponseInfo
                    .getPurgeTimePeriodDays();
        } else {
            statusHandler.error("Error in BackupConfigFileResponse From Edex:",
                    configFileResponseInfo.getExceptions());
            throw new Exception("Error in BackupConfigFileResponse From Edex:",
                    configFileResponseInfo.getExceptions());
        }
    }

    /**
     * Creates the Groups,Table and all containing widgets for Backup Services
     * Settings Dialog
     *
     * @param parent
     *            The parent Composite
     */
    public void createOverviewGroup(Composite parent) {

        createGroupsandLabel(parent, "", OVERVIEW_TEXT);
        // Display local domain/site info for Backup
        createGroupsandLabel(parent, "Local Domains", localDomains);

        // Create the Table to display Backup Hosts Information
        Composite hostGrp = createGroup(parent, "Host Information");
        createHostTable(hostGrp);
        // Display Excluded and Included files for Backup
        createGroupsandLabel(parent, "Includede Files", includedeFiles);
        createGroupsandLabel(parent, "Excluded Files", excludedFiles);
        // Display other miscellaneous information
        Group miscInfoGrp = createGroup(parent, "Misc Info");
        createLabel(miscInfoGrp, String.format("Poll Interval: %,d seconds",
                pollIntervalSeconds));
        createLabel(miscInfoGrp,
                String.format("Big Job Size: %,d bytes", jobSize));
        createLabel(miscInfoGrp,
                String.format("Rate Limit: %,d KBps", rateLimitKBps));
        createLabel(miscInfoGrp, "Purge Time Period (Accepted/Rejected Jobs): "
                + String.valueOf(purgeTimePeriodDays) + " days");
        createLabel(miscInfoGrp, "EDEX Cutoff Version: " + edexVersion);
    }

    /**
     * Creates the table that displays Backup Host information
     *
     * @param parent
     *            Composite used as the parent
     */
    public void createHostTable(Composite parent) {
        Label hostText = new Label(parent, SWT.NONE);
        hostText.setText(
                "Filter Domains are the local domains that will be sent to that host.");

        Composite hostTableComp = new Composite(parent, SWT.BORDER);
        hostTableComp
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        hostTableComp.setLayout(new GridLayout(1, false));
        hostTable = new Table(hostTableComp, SWT.V_SCROLL | SWT.H_SCROLL);
        hostTable.setHeaderVisible(true);
        hostTable.setLinesVisible(true);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.horizontalSpan = 1;
        hostTable.setLayoutData(gridData);
        // Set table column titles
        String[] titles = { "Site", "Filter Domains", "Host", "Port" };
        for (String title : titles) {
            TableColumn hostTableColumn = new TableColumn(hostTable, SWT.NULL);
            hostTableColumn.setText(title);
            hostTableColumn.pack();
            hostTableColumn.setWidth(200);
        }
        populateTable();
    }

    /**
     * Clears and populates the table with Backup Host Information.
     */
    protected void populateTable() {
        if (hostTable != null && !hostTable.isDisposed()) {
            // Clear the tree
            hostTable.removeAll();
            if (configFileResponseInfo.getHostInfoList().size() == 0) {
                TableItem item = new TableItem(hostTable, SWT.NULL);
                item.setText(0, "No Site Info");
                item.setText(1, "No Filter Domain Info");
                item.setText(2, "No Host Info");
                item.setText(3, "No Port Info");
                item.setData(configFileResponseInfo);
                packColumns();
            } else {
                for (String host : configFileResponseInfo.getHostInfoList()) {
                    TableItem item = new TableItem(hostTable, SWT.NULL);
                    String[] hostInfo = host
                            .split(BackupServicesConstants.COLON_SEPARATOR);
                    item.setText(0, hostInfo[0]);
                    item.setText(1, hostInfo[1]);
                    item.setText(2, hostInfo[2]);
                    item.setText(3, hostInfo[3]);
                    item.setData(host);
                }
                packColumns();
            }
        }
    }

    /**
     * Goes through each table column and packs it.
     */
    protected void packColumns() {
        for (int i = 0; i < hostTable.getColumnCount(); i++) {
            hostTable.getColumn(i).pack();
        }
    }

    /**
     * Create a Group for Backup Services Settings Dialog
     *
     * @param parent
     *            The parent Composite
     * @param grpName
     *            The Group name
     * @return Group
     */
    public Group createGroup(Composite parent, String grpName) {
        Group mainGrp = new Group(parent, SWT.None);
        FontDescriptor boldDescriptor = FontDescriptor
                .createFrom(mainGrp.getFont()).setStyle(SWT.BOLD);
        Font boldFont = boldDescriptor.createFont(mainGrp.getDisplay());
        mainGrp.setFont(boldFont);
        mainGrp.setText(grpName);
        GridData GroupData = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainGrp.setLayoutData(GroupData);
        mainGrp.setLayout(new GridLayout(1, true));
        return mainGrp;
    }

    /**
     * Create Groups and Labels for Backup Services Settings Dialog
     *
     * @param parent
     *            The parent Group
     * @param grpName
     *            The Group name
     * @param displyTxt
     *            The Label name
     */
    public void createGroupsandLabel(Composite parent, String grpName,
            String displyTxt) {
        Group mainGrp = createGroup(parent, grpName);
        createLabel(mainGrp, displyTxt);
    }

    /**
     * Create Labels for Backup Services Settings Dialog
     *
     * @param mainGrp
     *            The parent Group
     * @param displyTxt
     *            The Group name
     */
    public void createLabel(Group mainGrp, String displyTxt) {
        Label mainGrpText = new Label(mainGrp, SWT.LEFT);
        mainGrpText.setText(String.valueOf(displyTxt));
    }
}
