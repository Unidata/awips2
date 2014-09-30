/*
 * gov.noaa.nws.ncep.viz.ui.remotescript.dialog.RemoteScriptDialog
 * 
 * 26 March 2014
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.ui.remotescript.dialog;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.ui.remotescript.job.IRemoteJobObserver;
import gov.noaa.nws.ncep.viz.ui.remotescript.job.JobsModelProvider;
import gov.noaa.nws.ncep.viz.ui.remotescript.job.RemoteScriptJob;
import gov.noaa.nws.ncep.viz.ui.remotescript.job.RemoteScriptJob.JobStatus;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.remote.script.RemoteScriptListRequest;
import com.raytheon.uf.common.remote.script.RemoteScriptListResponse;
import com.raytheon.uf.common.remote.script.RemoteScriptRunRequest;
import com.raytheon.uf.common.remote.script.RemoteScriptRunResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Dialog for remote script execution
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/14        ?           B. Yin  Initial Creation.
 * 
 * </pre>
 * 
 * @author byin
 * 
 */

public class RemoteScriptDialog extends Dialog implements IRemoteJobObserver {

    // singleton instance
    private static RemoteScriptDialog INSTANCE;

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RemoteScriptDialog.class);

    private final IUser user = UserController.getUserObject();

    // last location of the dialog
    private Point lastLocation = null;

    // last size
    private Point lastSize = null;

    // List viewer for available scripts
    private ListViewer scriptListViewer = null;

    // Table viewer for submitted jobs
    private TableViewer jobTableViewer;

    private Listener scrollBarListener;

    // Text field for script selected from the list
    private Text scriptSelected;

    private Button goBtn;

    private Text scriptUsage;

    private Text scriptOutput;

    // Available scripts and their localization contexts
    private Map<String, List<LocalizationContext>> scriptMap;

    // private static final String CHECKSUM_FILE_EXTENSION = ".md5";

    private static final String SCRIPT_HELP_ARGUMENT = "--help";

    public RemoteScriptDialog(Shell parent) {
        super(parent);
        setShellStyle(SWT.TITLE | SWT.CLOSE | SWT.MAX | SWT.MIN | SWT.RESIZE
                | SWT.MODELESS);

        JobsModelProvider.INSTANCE.registerJobObserver(this);
    }

    /**
     * Creates the dialog if the dialog does not exist and returns the instance.
     * If the dialog exists, return the instance.
     * 
     * @param parShell
     * @return
     */
    public static RemoteScriptDialog getInstance(Shell parShell) {

        if (INSTANCE == null) {
            INSTANCE = new RemoteScriptDialog(parShell);
        }
        return INSTANCE;

    }

    /**
     * Creates the dialog area
     */
    @Override
    public Control createDialogArea(Composite parent) {

        Composite top = (Composite) super.createDialogArea(parent);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        top.setLayout(mainLayout);

        // Initialize all of the controls, and layouts
        initializeComponents(top);

        return top;

    }

    /**
     * Creates buttons, menus, and other controls in the dialog area
     * 
     */
    private void initializeComponents(Composite parent) {

        getShell().setText("Remote Script Execution");

        // show scroll bars when needed
        scrollBarListener = new Listener() {
            @Override
            public void handleEvent(Event event) {
                Text txt = (Text) event.widget;
                Rectangle r1 = txt.getClientArea();
                Rectangle r2 = txt.computeTrim(r1.x, r1.y, r1.width, r1.height);
                Point p = txt.computeSize(SWT.DEFAULT, SWT.DEFAULT, true);
                txt.getHorizontalBar().setVisible(r2.width <= p.x);
                txt.getVerticalBar().setVisible(r2.height <= p.y);
                if (event.type == SWT.Modify) {
                    txt.getParent().layout(true);
                    txt.showSelection();
                }
            }
        };

        // Create tab folder
        TabFolder tabFolder = new TabFolder(parent, SWT.RESIZE);
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        tabFolder.setLayoutData(gd);

        // Create submit tab
        final TabItem submitTabItem = new TabItem(tabFolder, SWT.RESIZE);
        submitTabItem.setText("Submit");

        Composite submitComp = new Composite(tabFolder, SWT.TOP | SWT.RESIZE);
        submitComp.setLayout(new GridLayout(2, false));
        createSubmitTabControls(submitComp);
        submitTabItem.setControl(submitComp);

        // Create status tab
        final TabItem statusTabItem = new TabItem(tabFolder, SWT.NONE);
        statusTabItem.setText("Status");

        Composite statusComp = new Composite(tabFolder, SWT.TOP | SWT.RESIZE);
        statusComp.setLayout(new GridLayout(1, false));
        createStatusTabControls(statusComp);
        statusTabItem.setControl(statusComp);

        tabFolder.pack();
    }

    @Override
    /**
     * Do not create Ok/Cancel buttons for this dialog.
     */
    public Control createButtonBar(Composite parent) {
        return null;
    }

    /**
     * Creates controls in the submit tab
     * 
     * @param parent
     */
    private void createSubmitTabControls(Composite parent) {

        SashForm sashForm = new SashForm(parent, SWT.HORIZONTAL);
        GridData sashGd = new GridData(SWT.FILL, SWT.FILL, true, true);
        sashForm.setLayoutData(sashGd);
        sashForm.setSashWidth(3);

        createLeftSubmitPanel(sashForm);
        createRightSubmitPanel(sashForm);
        sashForm.setWeights(new int[] { 3, 7 });

    }

    /**
     * Creates Site, Desk, and the script list in the submit tab
     * 
     * @param parent
     */
    private void createLeftSubmitPanel(Composite parent) {

        Composite leftComp = new Composite(parent, SWT.TOP);
        leftComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        leftComp.setLayout(new FormLayout());

        // Composite for site and desk
        Composite siteDeskComp = new Composite(leftComp, SWT.None);

        FormData fd1 = new FormData();
        fd1.top = new FormAttachment(0, 0);
        fd1.left = new FormAttachment(0, 0);

        siteDeskComp.setLayoutData(fd1);
        siteDeskComp.setLayout(new GridLayout(2, false));

        // Site combo
        Label siteLbl = new Label(siteDeskComp, SWT.LEFT);
        siteLbl.setText("Site:");
        final Combo siteCombo = new Combo(siteDeskComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);

        String curSite = LocalizationManager.getInstance().getCurrentSite();

        int ii = 0;
        for (String site : PathManagerFactory.getPathManager().getContextList(
                LocalizationLevel.SITE)) {
            siteCombo.add(site);
            if (site.equals(curSite)) {
                siteCombo.select(ii);
            }
            ii++;
        }

        // Desk combo
        Label deskLbl = new Label(siteDeskComp, SWT.LEFT);
        deskLbl.setText("Desk:");
        final Combo deskCombo = new Combo(siteDeskComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        String curDesk = NcPathManager.getInstance().getDeskContext()
                .getContextName();

        int jj = 0;
        for (String desk : PathManagerFactory.getPathManager().getContextList(
                NcPathManager.getInstance().getDeskLevel())) {
            deskCombo.add(desk);
            if (desk.equals(curDesk)) {
                deskCombo.select(jj);
            }
            jj++;
        }

        deskCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptListViewer.setInput(getScriptList(siteCombo.getText(),
                        deskCombo.getText()));
                scriptListViewer.refresh(true);
                scriptSelected.setText("");
            }
        });

        siteCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptListViewer.setInput(getScriptList(siteCombo.getText(),
                        deskCombo.getText()));
                scriptListViewer.refresh(true);
                scriptSelected.setText("");
            }
        });

        // Available script list group
        Group listGrp = new Group(leftComp, SWT.None);
        listGrp.setText("Available Scripts");

        FormData fd2 = new FormData();
        fd2.top = new FormAttachment(siteDeskComp, 5, SWT.BOTTOM);
        fd2.bottom = new FormAttachment(100, 0);

        fd2.left = new FormAttachment(0, 0);
        fd2.right = new FormAttachment(100, 0);

        listGrp.setLayoutData(fd2);
        listGrp.setLayout(new FormLayout());

        scriptListViewer = new ListViewer(listGrp, SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
        FormData fd3 = new FormData();
        fd3.top = new FormAttachment(0, 10);
        fd3.bottom = new FormAttachment(100, 0);
        fd3.height = 400;
        fd3.left = new FormAttachment(0, 0);
        fd3.right = new FormAttachment(100, 0);

        scriptListViewer.getList().setLayoutData(fd3);

        scriptListViewer.getList().setBackground(listGrp.getBackground());

        scriptListViewer.setContentProvider(new IStructuredContentProvider() {

            @Override
            public Object[] getElements(Object inputElement) {
                return (Object[]) inputElement;
                // return getScriptList(siteCombo.getText(),
                // deskCombo.getText());
            }

            @Override
            public void dispose() {
            }

            @Override
            public void inputChanged(Viewer viewer, Object oldInput,
                    Object newInput) {
            }
        });

        scriptListViewer.setInput(getScriptList(siteCombo.getText(),
                deskCombo.getText()));

        scriptListViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        String scripteWithContext = scriptListViewer.getList()
                                .getSelection()[0];
                        String script = scripteWithContext.substring(0,
                                scripteWithContext.indexOf(" ("));
                        scriptSelected.setText(script + " ");

                        scriptUsage.setText(getScriptUsage(script,
                                getScriptContext(script, scripteWithContext)));
                        if (scriptUsage.getText().contains(
                                "Not an executable script")) {
                            goBtn.setEnabled(false);
                        } else {
                            goBtn.setEnabled(true);
                        }
                    }
                });
    }

    /**
     * Creates text field for selected script, GO button and usage text field.
     * 
     * @param parent
     */
    private void createRightSubmitPanel(Composite parent) {

        Composite rightComp = new Composite(parent, SWT.TOP | SWT.RESIZE);
        rightComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        rightComp.setLayout(new FormLayout());

        // Text for selected script and the GO button
        Composite textComp = new Composite(rightComp, SWT.TOP | SWT.RESIZE);
        FormData fd1 = new FormData();
        fd1.top = new FormAttachment(0, 0);
        fd1.left = new FormAttachment(0, 0);
        fd1.right = new FormAttachment(100, 0);

        textComp.setLayoutData(fd1);
        textComp.setLayout(new FormLayout());

        scriptSelected = new Text(textComp, SWT.SINGLE | SWT.BORDER
                | SWT.RESIZE);
        FormData fd2 = new FormData();
        fd2.top = new FormAttachment(0, 5);
        fd2.bottom = new FormAttachment(100, 0);
        fd2.left = new FormAttachment(0, 0);
        fd2.right = new FormAttachment(90, 0);

        scriptSelected.setLayoutData(fd2);

        scriptSelected.setBackground(this.getShell().getDisplay()
                .getSystemColor(SWT.COLOR_GRAY));
        scriptSelected.addListener(SWT.Verify, new Listener() {
            @Override
            public void handleEvent(Event event) {
                // Make the script name in the text field NOT editable.
                if (event.widget instanceof Text) {
                    if (getSelectedScript().isEmpty() && event.text.isEmpty()) {
                        event.doit = true;
                    } else if (event.text.equals(getSelectedScript() + " ")
                            && event.start == 0) {
                        // event sent from the list viewer
                        event.doit = true;
                    } else if (event.start <= getSelectedScript().length()) {
                        event.doit = false;
                    }
                }
            }
        });

        goBtn = new Button(textComp, SWT.PUSH);
        goBtn.setText("GO");
        FormData fd3 = new FormData();
        fd3.top = new FormAttachment(0, 5);
        fd3.left = new FormAttachment(scriptSelected, 10, SWT.RIGHT);
        goBtn.setLayoutData(fd3);

        goBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                if (scriptListViewer.getList().getSelectionCount() != 0) {
                    String scriptWithContext = scriptListViewer.getList()
                            .getSelection()[0];
                    String script = getSelectedScript();
                    String arguments = getArguments();

                    RemoteScriptJob job = new RemoteScriptJob(user, script,
                            arguments, getScriptContext(script,
                                    scriptWithContext), Calendar.getInstance(),
                            JobStatus.IN_PROGRESS);
                    JobsModelProvider.INSTANCE.addRemoteScriptJob(job);
                    // JobsModelProvider.INSTANCE.runJob(job);
                }
            }
        });

        Group usageGrp = new Group(rightComp, SWT.RESIZE);
        FormData fd4 = new FormData();
        fd4.top = new FormAttachment(textComp, 15, SWT.BOTTOM);
        fd4.bottom = new FormAttachment(100, 0);

        fd4.left = new FormAttachment(0, 0);
        fd4.right = new FormAttachment(100, 0);

        usageGrp.setLayoutData(fd4);
        usageGrp.setLayout(new FormLayout());

        usageGrp.setText("Script Usage");

        scriptUsage = new Text(usageGrp, SWT.MULTI | SWT.READ_ONLY | SWT.RESIZE
                | SWT.H_SCROLL | SWT.V_SCROLL);
        FormData fd5 = new FormData();
        fd5.top = new FormAttachment(0, 10);
        fd5.bottom = new FormAttachment(100, 0);

        fd5.left = new FormAttachment(0, 0);
        fd5.right = new FormAttachment(100, 0);

        scriptUsage.setLayoutData(fd5);
        scriptUsage.setBackground(usageGrp.getBackground());

        scriptUsage.addListener(SWT.Resize, scrollBarListener);
        scriptUsage.addListener(SWT.Modify, scrollBarListener);

    }

    /**
     * Creates controls in status tab
     * 
     * @param parent
     */
    private void createStatusTabControls(Composite parent) {

        // Sash form to hold the job table and the job output field.
        SashForm sashForm = new SashForm(parent, SWT.VERTICAL);
        GridData sashGd = new GridData(SWT.FILL, SWT.FILL, true, true);
        sashForm.setLayoutData(sashGd);
        sashForm.setSashWidth(3);

        // Create the job table viewer
        jobTableViewer = new TableViewer(sashForm, SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.FULL_SELECTION);

        // Sort the job by time submitted
        jobTableViewer.setComparator(new ViewerComparator() {
            @Override
            public int compare(Viewer viewer, Object obj1, Object obj2) {
                RemoteScriptJob rsj1 = (RemoteScriptJob) obj1;
                RemoteScriptJob rsj2 = (RemoteScriptJob) obj2;

                return rsj2.getTmSubmitted().compareTo(rsj1.getTmSubmitted());
            }
        });

        Table jobTable = jobTableViewer.getTable();

        // Add a column header named "Job" that's left justified
        TableViewerColumn column = createTableViewerColumn(jobTableViewer,
                "Job", 200, 0);

        column.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                RemoteScriptJob rsj = (RemoteScriptJob) element;
                return (rsj.getRemoteScriptName() + " ("
                        + getLocalizationName(rsj.getContext()) + ")");
            }
        });

        // Add a column header named "Time Submitted" that's left justified
        TableViewerColumn column2 = createTableViewerColumn(jobTableViewer,
                "Time Submitted", 200, 1);

        column2.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                RemoteScriptJob rsj = (RemoteScriptJob) element;
                SimpleDateFormat sdf = new SimpleDateFormat(
                        "yyyy-MM-dd HH:mm:ss");
                return sdf.format(rsj.getTmSubmitted().getTime());
            }
        });

        // Add a column header named "Status/Return" that's left justified
        TableViewerColumn column3 = createTableViewerColumn(jobTableViewer,
                "Status/Return", 200, 2);

        column3.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                RemoteScriptJob rsj = (RemoteScriptJob) element;
                return rsj.getStatus().toString();
            }
        });

        jobTableViewer.setContentProvider(new ArrayContentProvider());
        jobTableViewer.setInput(JobsModelProvider.INSTANCE
                .getRemoteScriptJobs());

        setItemBackgroundColor(jobTableViewer);

        jobTableViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        scriptOutput.setText(((RemoteScriptJob) jobTableViewer
                                .getTable().getSelection()[0].getData())
                                .getOutput());
                    }
                });

        // Show the column headers
        jobTable.setHeaderVisible(true);

        // Create the job output field
        Group outputGrp = new Group(sashForm, SWT.RESIZE);
        outputGrp.setLayout(new FormLayout());

        outputGrp.setText("Job Output");

        scriptOutput = new Text(outputGrp, SWT.MULTI | SWT.READ_ONLY
                | SWT.RESIZE | SWT.H_SCROLL | SWT.V_SCROLL);
        FormData fd = new FormData();
        fd.top = new FormAttachment(0, 0);
        fd.bottom = new FormAttachment(100, 0);

        fd.left = new FormAttachment(0, 0);
        fd.right = new FormAttachment(100, 0);

        scriptOutput.setLayoutData(fd);
        scriptOutput.setBackground(outputGrp.getBackground());
        jobTable.setBackground(outputGrp.getBackground());

        scriptOutput.addListener(SWT.Resize, scrollBarListener);
        scriptOutput.addListener(SWT.Modify, scrollBarListener);

        sashForm.setWeights(new int[] { 5, 1 });

    }

    /**
     * Set the location and size of the dialog
     */
    @Override
    public int open() {

        if (this.getShell() == null) {
            this.create();
        }

        Point size = getInitialSize();
        getShell().setSize(size);
        getShell().setLocation(getInitialLocation(size));

        return super.open();
    }

    /**
     * Get the last used size of the dialog.
     * 
     * @return
     */
    @Override
    protected Point getInitialSize() {
        return (lastSize == null) ? super.getInitialSize() : lastSize;
    }

    /**
     * Get the last used location of the dialog.
     * 
     * @param size
     * @return
     */
    @Override
    protected Point getInitialLocation(Point size) {
        return (lastLocation == null) ? super.getInitialLocation(size)
                : lastLocation;
    }

    /**
     * Save location and size of the dialog.
     */
    public boolean close() {
        if (getShell() != null && !getShell().isDisposed()) {
            Rectangle bounds = getShell().getBounds();
            lastLocation = new Point(bounds.x, bounds.y);
            lastSize = getShell().getSize();
        }
        return super.close();
    }

    /**
     * Create a column of a table viewer
     * 
     * @param tv
     *            - table viewer
     * @param title
     *            - column title
     * @param bound
     *            - column width
     * @param colNumber
     *            - column number
     * @return table viewer column
     */
    private TableViewerColumn createTableViewerColumn(TableViewer tv,
            String title, int bound, final int colNumber) {
        final TableViewerColumn viewerColumn = new TableViewerColumn(tv,
                SWT.NONE);
        final TableColumn column = viewerColumn.getColumn();
        column.setText(title);
        column.setWidth(bound);
        column.setResizable(true);
        column.setMoveable(true);
        return viewerColumn;
    }

    /**
     * Set color for each row of the table
     * 
     * @param tv
     */
    private void setItemBackgroundColor(TableViewer tv) {
        Table tbl = tv.getTable();
        TableItem items[] = tbl.getItems();

        for (int ii = 0; ii < items.length; ii++) {
            setItemBackgroundColor((RemoteScriptJob) items[ii].getData());
        }

    }

    /**
     * Set the color of the job if it in the table. FAIL or TIME_OUT: red OK :
     * green IN_PROGRESS : grey
     * 
     * @param job
     */
    private void setItemBackgroundColor(RemoteScriptJob job) {

        TableItem item = findTableItem(job);
        JobStatus status = job.getStatus();
        if (item != null) {
            if (status.equals(JobStatus.FAIL)
                    || status.equals(JobStatus.TIME_OUT)) {
                this.getShell().getDisplay();
                item.setBackground(new Color(Display.getCurrent(), 225, 50, 30));
            } else if (status.equals(JobStatus.OK)) {
                item.setBackground(new Color(Display.getCurrent(), 70, 180, 30));
            } else {
                item.setBackground(this.getShell().getDisplay()
                        .getSystemColor(SWT.COLOR_GRAY));
            }
        }
    }

    /**
     * Create a String array of available scripts with their localization
     * levels.
     * 
     * @param site
     * @param desk
     * @return script name array
     */
    @SuppressWarnings("unchecked")
    private String[] getScriptList(String site, String desk) {

        List<String> ret = new ArrayList<String>();

        getScriptMap(site, desk);

        if (scriptMap != null) {
            Iterator<?> it = scriptMap.entrySet().iterator();

            while (it.hasNext()) {
                Entry<?, ?> entry = (Entry<?, ?>) it.next();

                for (LocalizationContext lc : (List<LocalizationContext>) entry
                        .getValue()) {
                    ret.add((String) entry.getKey() + " ("
                            + getLocalizationName(lc) + ")");
                }
            }
            Collections.sort(ret);
            return ret.toArray(new String[ret.size()]);
        } else {
            return new String[] {};
        }
    }

    /**
     * Query the EDEX server and get the available scripts for the specified
     * site and desk.
     * 
     * @param site
     * @param desk
     */
    private void getScriptMap(String site, String desk) {

        if (desk == null || desk.isEmpty()) {
            desk = "None";
        }
        IPathManager manager = PathManagerFactory.getPathManager();
        LocalizationContext baseContext = manager.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext siteContext = manager.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.SITE);
        siteContext.setContextName(site);

        LocalizationContext deskContext = manager.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                NcPathManager.getInstance().getDeskLevel());
        deskContext.setContextName(desk);

        LocalizationContext userContext = manager.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.USER);

        RemoteScriptListRequest request = new RemoteScriptListRequest(user
                .uniqueId().toString(), new LocalizationContext[] {
                baseContext, siteContext, deskContext, userContext });
        request.setUser(user);

        try {
            RemoteScriptListResponse response = (RemoteScriptListResponse) ThriftClient
                    .sendPrivilegedRequest(request);
            if (response != null) {
                scriptMap = response.getScripts();
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Run the selected script with "--help" argument to get the usage info.
     * 
     * @param script
     * @param context
     * @return
     */
    private String getScriptUsage(String script, LocalizationContext context) {

        RemoteScriptRunRequest runRequest = new RemoteScriptRunRequest(user
                .uniqueId().toString(), script, context);
        runRequest.setUser(user);

        runRequest.addScriptArgument(SCRIPT_HELP_ARGUMENT);

        String usageInfo = "";
        try {
            RemoteScriptRunResponse runResponse = (RemoteScriptRunResponse) ThriftClient
                    .sendPrivilegedRequest(runRequest);
            if (runResponse != null) {
                usageInfo += runResponse.getOutput();
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        if (usageInfo.isEmpty()) {
            usageInfo = "No usage infomation!";
        }
        return usageInfo;
    }

    /**
     * Get the arguments user types in
     * 
     * @return
     */
    private String getArguments() {
        String script = getSelectedScript();
        return scriptSelected.getText().substring(script.length() + 1);
    }

    /**
     * Get the localization context of the selected script.
     * 
     * @param script
     * @param scripteWithContext
     * @return - localization context of the script
     */
    protected LocalizationContext getScriptContext(String script,
            String scriptWithContext) {
        String contextName = scriptWithContext.substring(
                scriptWithContext.indexOf(" (") + 2,
                scriptWithContext.indexOf(")"));

        LocalizationContext context = null;

        if (contextName.equalsIgnoreCase("BASE")) {
            IPathManager manager = PathManagerFactory.getPathManager();
            context = manager.getContext(
                    LocalizationContext.LocalizationType.COMMON_STATIC,
                    LocalizationContext.LocalizationLevel.BASE);
        } else {
            for (LocalizationContext lc : scriptMap.get(script)) {
                if (!lc.getLocalizationLevel().equals(
                        LocalizationContext.LocalizationLevel.BASE)
                        && lc.getContextName().equalsIgnoreCase(contextName)) {
                    context = lc;
                }
            }
        }

        return context;
    }

    /**
     * Get the selected script name (without the localization context string)
     * from the script list.
     * 
     * @return script name
     */
    private String getSelectedScript() {
        if (scriptListViewer.getList().getSelectionCount() == 0) {
            return "";
        } else {
            String scripteWithContext = scriptListViewer.getList()
                    .getSelection()[0];
            return scripteWithContext.substring(0,
                    scripteWithContext.indexOf(" ("));
        }
    }

    /**
     * Update status, color, output of the specified job.
     */
    @Override
    public void updateRemoteJob(RemoteScriptJob job) {
        jobTableViewer.update(job, null);
        setItemBackgroundColor(job);

        // update output
        if (jobTableViewer.getTable().getSelection().length > 0
                && jobTableViewer.getTable().getSelection()[0].getData()
                        .equals(job)) {
            String output = job.getOutput();
            if (!job.getError().isEmpty()) {
                output += "\nError message:\n" + job.getError();
            }
            scriptOutput.setText(output);
        }
    }

    /**
     * Add a new job to the job status table.
     */
    @Override
    public void addRemoteJob(RemoteScriptJob job) {
        jobTableViewer.add(job);
        setItemBackgroundColor(job);
    }

    /**
     * Find the table item of the specified job.
     * 
     * @param job
     * @return
     */
    private TableItem findTableItem(RemoteScriptJob job) {
        Table tbl = jobTableViewer.getTable();
        TableItem items[] = tbl.getItems();

        for (int ii = 0; ii < items.length; ii++) {
            if (items[ii].getData().equals(job)) {
                return items[ii];
            }
        }
        return null;
    }

    /**
     * Get the name of the localization context.
     * 
     * @param lc
     * @return localization context name
     */
    private String getLocalizationName(LocalizationContext lc) {
        if (lc.getLocalizationLevel().equals(
                LocalizationContext.LocalizationLevel.BASE)) {
            return "base";
        } else {
            return lc.getContextName();
        }
    }
}
