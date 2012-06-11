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
package com.raytheon.viz.texteditor.alarmalert.dialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.services.textdbsrv.IQueryTransport;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.alarmalert.util.AlarmAlertFunctions;
import com.raytheon.viz.texteditor.alarmalert.util.AlarmAlertLists;
import com.raytheon.viz.texteditor.alarmalert.util.CurrentAlarmEvent;
import com.raytheon.viz.texteditor.alarmalert.util.ICurrentAlarmListener;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.CommandFailedException;
import com.raytheon.viz.texteditor.command.ICommand;
import com.raytheon.viz.texteditor.msgs.IAfosBrowserCallback;
import com.raytheon.viz.texteditor.util.TextEditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ModeListener;

/**
 * The Current Alarm dialog for the alert alarm functionality of text
 * workstation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 9, 2009             mnash       Initial creation
 * Apr 14, 2010 4734       mhuang      Corrected StdTextProduct import 
 *                                      dependency
 * Jun 29, 2010 5466       cjeanbap    Add SWT.RESIZE type to shell.
 * Oct 29, 2010 7375       cjeanbap    Moved set window size to opened();
 *                                      removed shellListener().
 * Jun 03, 2011 9681       cjeanbap    Changed list style.
 * Aug 12, 2011 10045      rferrel     Now remembers screen location
 * Mar 19, 2012 14624      mhuang      Fixed problem of always retrieval of latest 
 *                                      product when a number of alarm products
 *                                      with same afos pil but different issue
 *                                      times showed up in the product list of
 *                                      current alarm queue window.
 * May 23, 2012 14952      rferrel     Now use refTime/createtime to display
 *                                      selected product
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CurrentAlarmQueue extends CaveSWTDialog implements
        IAfosBrowserCallback, ICurrentAlarmListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CurrentAlarmQueue.class);

    private Font font;

    private AlarmAlertDlg dlg = null;

    protected AlarmDisplayWindow alarmDisplayDlg;

    private Composite shellComp = null;

    /**
     * The alarm queue list.
     */
    private List list = null;

    /**
     * A list of reference times maintained in the same order as the list
     * entries.
     */
    private java.util.List<Date> listDates;

    private Button displayAll;

    private IQueryTransport queryTransport = null;

    private java.util.List<StdTextProduct> prodList = null;

    private static CurrentAlarmQueue INSTANCE;

    private static Point closeLocation;

    /**
     * Redraw flag indicating if the window should redraw on a resize.
     */
    private boolean canRedraw = true;

    private static final int MIN_WIDTH = 350;

    /**
     * @param parentShell
     * @param style
     */
    private CurrentAlarmQueue(Shell parentShell) {
        super(parentShell, SWT.RESIZE, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("Current Alarm Queue");
    }

    public static CurrentAlarmQueue getInstance(Shell parentShell) {
        if (INSTANCE == null || INSTANCE.getParent().isDisposed()) {
            INSTANCE = new CurrentAlarmQueue(parentShell);
        }
        return INSTANCE;
    }

    /**
     * Opens the dialog without ever displaying it, and does all the
     * initialization necessary to get alarms/alerts up and running without the
     * user ever having to do more than open the text workstation.
     */
    public void openInvisible() {
        Shell parent = getParent();

        shell = new Shell(parent, getStyle());

        shell.setText(getText());

        if (doesNotHaveAttribute(CAVE.MODE_INDEPENDENT)) {
            new ModeListener(shell);
        }

        // Create the main layout for the shell.
        shell.setLayout(constructShellLayout());
        shell.setLayoutData(constructShellLayoutData());
        shell.setVisible(false);

        // Initialize all of the controls and layouts
        initializeComponents(shell);

        // pack and open the dialog
        if (doesNotHaveAttribute(CAVE.NO_PACK)) {
            shell.pack();
            shell.setVisible(false);
        }

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                disposed();
            }
        });

        preOpened();

        opened();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        font.dispose();
        AlarmAlertLists.getInstance().removeListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(final Shell shell) {
        setReturnValue(false);

        shell.setMinimumSize(MIN_WIDTH, 200);
        // Create the main layout for the shell.

        font = new Font(shell.getDisplay(), "Helvetica", 11, SWT.BOLD);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        shellComp = new Composite(shell, SWT.NONE);
        shellComp.setLayout(constructShellLayout());
        shellComp.setLayoutData(gd);

        shell.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                if (canRedraw == false) {
                    return;
                }

                final Shell resizedShell = (Shell) e.getSource();
                final Point point = resizedShell.getSize();

                if (point.x != MIN_WIDTH) {
                    canRedraw = false;
                    Display.getDefault().asyncExec(new Runnable() {
                        @Override
                        public void run() {
                            // resizedShell.setMinimumSize(MIN_WIDTH, point.y);
                            canRedraw = true;
                        }
                    });
                }
            }
        });

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initializes each component of the shell
     */
    private void initializeComponents() {
        createButtons();
        createTextArea();
        addQueueListener();
        populate();

        dlg = new AlarmAlertDlg(shell);
        dlg.openInvisible();

    }

    /**
     * Creates the area that will post the log lines
     */
    private void createTextArea() {
        Composite textComp = new Composite(shellComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        textComp.setLayout(gl);
        GridData textData = new GridData(SWT.FILL, SWT.FILL, true, true);
        textComp.setLayoutData(textData);
        listDates = new ArrayList<Date>();
        list = new List(textComp, SWT.BORDER | SWT.V_SCROLL | SWT.SINGLE);
        list.setLayoutData(textData);
        list.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                displayList();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });
    }

    /**
     * Create the top buttons for the current alarm queue dialog
     */
    private void createButtons() {
        Composite buttonComp = new Composite(shellComp, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.marginHeight = 2;
        buttonComp.setLayout(gl);
        // opens the alarm display dialog, checks for open shells
        Button openDisplay = new Button(buttonComp, SWT.PUSH);
        openDisplay.setText("Open Display");
        openDisplay.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayList();
            }
        });

        displayAll = new Button(buttonComp, SWT.PUSH);
        displayAll.setText("Display All");
        displayAll.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayAll();
            }
        });

        // opens the product list dialog, checks for disposed shells
        Button productList = new Button(buttonComp, SWT.PUSH);
        productList.setText("Product List");
        productList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (dlg == null) {
                    dlg = new AlarmAlertDlg(shell);
                    dlg.open();
                } else {
                    if (dlg.getShell() == null || dlg.getShell().isDisposed()) {
                        dlg = new AlarmAlertDlg(shell);
                        dlg.open();
                    } else {
                        dlg.open();
                    }
                }
            }
        });
    }

    /**
     * Display the selected product the current alarm queue list.
     */
    private void displayList() {
        String command = "";
        Date refDate = null;
        if (list != null && list.getItemCount() > 0
                && list.getSelectionCount() > 0 && list.getSelection() != null) {
            command = list.getSelection()[0].split(" ")[0];
            refDate = listDates.get(list.getSelectionIndex());
            AlarmAlertLists.getInstance().getCurrentAlarms()
                    .remove(list.getSelectionIndex());
            listDates.remove(list.getSelectionIndex());
            list.remove(list.getSelectionIndex());
            if (list.getItemCount() == 0) {
                AlarmAlertFunctions.getAlarmalertbell().close();
            }
        }

        java.util.List<StdTextProduct> prods = null;
        if (command != "" && refDate != null) {
            prods = produceTextProduct(command, refDate.getTime());
        }

        if (alarmDisplayDlg == null) {
            alarmDisplayDlg = new AlarmDisplayWindow(shell, prods);
            alarmDisplayDlg.open();
            if (list != null && !list.isDisposed() && list.getItemCount() == 0) {
                close();
            }
        } else {
            if (alarmDisplayDlg.getShell() == null
                    || alarmDisplayDlg.getShell().isDisposed()) {
                alarmDisplayDlg = new AlarmDisplayWindow(shell, prods);
                alarmDisplayDlg.open();
                if (list != null && !list.isDisposed()
                        && list.getItemCount() == 0) {
                    close();
                }
            } else {
                alarmDisplayDlg.setProds(prods);
                alarmDisplayDlg.setDialogFocus();
            }
        }
        if (list != null && !list.isDisposed()) {
            if (list.getItemCount() == 0) {
                displayAll.setEnabled(false);
            }
        }
    }

    /**
     * Display all the products in the alarm queue list and clear the list.
     */
    private void displayAll() {
        String[] command = null;
        if (list != null) {
            command = new String[list.getItemCount()];
            for (int i = 0; i < list.getItemCount(); i++) {
                command[i] = list.getItems()[i].split(" ")[0];
            }
            // Do a count of how many instances of each command are in
            // the queue.
            Map<String, Integer> counter = new HashMap<String, Integer>();
            for (int i = 0; i < command.length; ++i) {
                if (counter.get(command[i]) == null) {
                    counter.put(command[i], 0);
                } else {
                    counter.put(command[i], (counter.get(command[i]) + 1));
                }
            }
            // For each command, see how far back it needs to go to
            // account for multiple instances, and construct the
            // appropriate AFOS command.
            for (int j = 0; j < command.length; ++j) {
                Integer count = counter.get(command[j]);
                if (count > 0) {
                    String newCom = "-" + count.toString() + ":" + command[j];
                    counter.put(command[j], (count - 1));
                    command[j] = newCom;
                }
            }
            AlarmAlertLists.getInstance().getCurrentAlarms().clear();
            listDates.clear();
            list.removeAll();
            AlarmAlertFunctions.getAlarmalertbell().close();
        }
        if (alarmDisplayDlg == null) {
            java.util.List<StdTextProduct> prods = new ArrayList<StdTextProduct>();
            if (command.length > 0) {
                for (int i = 0; i < command.length; i++) {
                    prods.addAll(produceTextProduct(command[i]));
                }
            }
            alarmDisplayDlg = new AlarmDisplayWindow(shell, prods);
            alarmDisplayDlg.setAccumulate(true);
            alarmDisplayDlg.open();
        } else {
            if (alarmDisplayDlg.getShell() == null
                    || alarmDisplayDlg.getShell().isDisposed()) {
                java.util.List<StdTextProduct> prods = new ArrayList<StdTextProduct>();
                if (command.length > 0) {
                    for (int i = 0; i < command.length; i++) {
                        prods.addAll(produceTextProduct(command[i]));
                    }
                }
                alarmDisplayDlg = new AlarmDisplayWindow(shell, prods);
                alarmDisplayDlg.setAccumulate(true);
                alarmDisplayDlg.open();
            } else {
                alarmDisplayDlg.setAccumulate(true);
                java.util.List<StdTextProduct> prods = new ArrayList<StdTextProduct>();
                if (command.length > 0) {
                    for (int i = 0; i < command.length; i++) {
                        prods.addAll(produceTextProduct(command[i]));
                    }
                }
                alarmDisplayDlg.setProds(prods);
                alarmDisplayDlg.setAccumulate(true);
                alarmDisplayDlg.setDialogFocus();
            }
        }
        displayAll.setEnabled(false);
    }

    /**
     * Add a line to list that contains the afosPil and a date displayed as a
     * local time string.
     * 
     * @param afosPil
     * @param date
     */
    public void addToQueue(String afosPil, Date date) {
        SimpleDateFormat formatter = new SimpleDateFormat("HH:mm:ss");
        String s = formatter.format(date);
        String lineText = afosPil + " alert message received at " + s;
        if (!list.isDisposed()) {
            displayAll.setEnabled(true);
            listDates.add(date);
            list.add(lineText);
            list.select(0);
        }
    }

    private void addQueueListener() {
        AlarmAlertLists.getInstance().addListener(this);
    }

    /**
     * Grab the current alarms from the alarm log file
     */
    private void populate() {
        // TODO populate with the current alerts
        java.util.List<AlarmAlertProduct> alarms = AlarmAlertLists
                .getInstance().getCurrentAlarms();
        CAVEMode mode = CAVEMode.getMode();
        for (int i = 0; i < alarms.size(); i++) {
            AlarmAlertProduct aap = alarms.get(i);
            if ((CAVEMode.OPERATIONAL.equals(mode) || CAVEMode.TEST
                    .equals(mode)) && aap.getOperationalMode()) {
                addToQueue(aap.getProductId(), aap.getDateReceived());
            }
        }
        if (list.getItemCount() == 0) {
            displayAll.setEnabled(false);
        }
    }

    public void setDialogFocus() {
        if (shell != null && !shell.isDisposed()) {
            shell.setActive();
        }
    }

    public java.util.List<StdTextProduct> produceTextProduct(String command) {
        ICommand cmd = CommandFactory.getAfosCommand(command);
        executeCommand(cmd);
        return prodList;
    }

    /**
     * Get the product for the given AFOS command and reference time.
     * 
     * @param command
     * @param refTime
     * @return prodList
     */
    private java.util.List<StdTextProduct> produceTextProduct(String command,
            Long refTime) {
        ICommand cmd = CommandFactory.getAfosCommand(command, refTime);
        executeCommand(cmd);
        return prodList;
    }

    /*
     * get text product using wmo command (DR_14624)
     */
    public java.util.List<StdTextProduct> getAwipsTextProduct(String awipsId,
            String wmoId, String site, String hdrTime, String bbb) {
        ICommand cmd = CommandFactory.getAwipsCommand(awipsId, wmoId, site,
                hdrTime, bbb);
        executeCommand(cmd);
        return prodList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IAfosBrowserCallback#executeCommand(
     * com.raytheon.viz.texteditor.command.ICommand)
     */
    @Override
    public void executeCommand(ICommand command) {
        queryTransport = TextEditorUtil.getTextDbsrvTransport();
        try {
            prodList = command.executeCommand(queryTransport);
            if (prodList == null || prodList.size() <= 0) {
                prodList = new ArrayList<StdTextProduct>();
            }
        } catch (CommandFailedException e) {
            statusHandler.handle(Priority.PROBLEM, "Error retrieving metadata",
                    e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IAfosBrowserCallback#setAfosCmdField
     * (java.lang.String)
     */
    @Override
    public void setAfosCmdField(String cmd) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.alarmalert.dialogs.ICurrentAlarmListener#
     * currentAlarmChanged()
     */
    @Override
    public void currentAlarmChanged(CurrentAlarmEvent event) {
        AlarmAlertProduct aap = (AlarmAlertProduct) event.getSource();
        if (!shell.isDisposed() && shell != null) {
            CAVEMode mode = CAVEMode.getMode();
            if ((CAVEMode.OPERATIONAL.equals(mode) || CAVEMode.TEST
                    .equals(mode)) && aap.getOperationalMode()) {
                addToQueue(aap.getProductId(), aap.getDateReceived());
            }
        }
    }

    @Override
    protected void opened() {
        // shell.setSize(600, 300);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                closeLocation = getShell().getLocation();
            }
        });

        if (closeLocation != null) {
            getShell().setLocation(closeLocation);
        }
    }
}
