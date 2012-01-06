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
package com.raytheon.viz.gfe.dialogs;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.viz.gfe.core.script.IScriptUtil;
import com.raytheon.viz.gfe.core.script.action.NewAction;
import com.raytheon.viz.gfe.procedures.ProcedureCatalog;
import com.raytheon.viz.gfe.procedures.ProcedureMouseListener;
import com.raytheon.viz.gfe.procedures.util.ProcedureUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog for the define procedures action. Derived from the
 * DefineTextProductsDialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sept 18, 2008            askripsk     Initial creation.
 * Oct 15, 2009             wldougher    Derived from DefineTextProductsDialog
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class DefineProceduresDialog extends CaveJFACEDialog {

    public static final int CLOSE_ID = 2;

    private static final int HEIGHT = 330;

    private String title;

    private Shell shell;

    private List proceduresList;

    private Label proceduresLabel;

    private Composite comp;

    private MenuItem newItem;

    private MenuItem newProcedureItem;

    // private ITextProductListChangedListener listListener;

    @SuppressWarnings("unused")
    private Button closeButton;

    private ProcedureCatalog catalog;

    private ILocalizationFileObserver observer;

    /**
     * Constructor
     * 
     * @param parentShell
     *            the shell in which the dialog should appear.
     * @param dialogTitle
     *            The text to display in the dialog titlebar.
     */
    public DefineProceduresDialog(Shell parentShell) {
        super(parentShell);

        this.title = "Define Procedures";
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);

        catalog = new ProcedureCatalog();
        observer = new ILocalizationFileObserver() {

            /**
             * Update proceduresList when scripts are added or deleted.
             * 
             * @see com.raytheon.uf.common.localization.ILocalizationFileObserver#fileUpdated(com.raytheon.uf.common.localization.FileUpdatedMessage)
             */
            @Override
            public void fileUpdated(FileUpdatedMessage message) {
                shell.getDisplay().asyncExec(new Runnable() {
                    @Override
                    public void run() {
                        loadProcedures();
                    }
                });
            }

        };
        catalog.addObserver(observer);
    }

    @Override
    public boolean close() {
        catalog.removeObserver(observer);
        observer = null;
        catalog = null;
        return super.close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == CLOSE_ID) {
            close();
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
        if (title != null) {
            shell.setText(title);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        GridData data = new GridData(GridData.HORIZONTAL_ALIGN_CENTER
                | GridData.VERTICAL_ALIGN_CENTER);
        parent.setLayoutData(data);
        this.closeButton = createButton(parent, CLOSE_ID, "Close", true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createContents(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createContents(Composite parent) {
        Control contents = super.createContents(parent);

        getShell().setLocation(getInitialLocation(getShell().getSize()));

        return contents;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createDialogArea(final Composite parent) {

        Composite composite = (Composite) super.createDialogArea(parent);

        createMenuBar(parent);

        comp = new Composite(composite, SWT.NONE);

        comp.setLayout(new GridLayout(1, false));
        comp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // Setup list labels
        initProceduresLabel();

        // Create Lists
        initProceduresList();

        applyDialogFont(composite);

        return composite;
    }

    /**
     * Create the menu bar. The menubar has one option ("File"), and allows the
     * user to create a new procedure or close this dialog.
     * 
     * @param parent
     */
    protected void createMenuBar(Composite parent) {
        shell = parent.getShell();
        Menu menuBar = new Menu(shell, SWT.BAR);
        shell.setMenuBar(menuBar);
        shell.setSize(250, HEIGHT);

        // file menu item
        MenuItem file = new MenuItem(menuBar, SWT.CASCADE);
        file.setText("File");
        Menu fileMenu = new Menu(shell, SWT.DROP_DOWN);
        file.setMenu(fileMenu);

        newItem = new MenuItem(fileMenu, SWT.CASCADE);
        newItem.setText("New Item In Window...");
        // TODO: add selection listener

        new MenuItem(fileMenu, SWT.SEPARATOR);
        MenuItem closeItem = new MenuItem(fileMenu, SWT.PUSH);
        closeItem.setText("Close");
        closeItem.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }

        });

        Menu newMenu = new Menu(newItem);

        newProcedureItem = new MenuItem(newMenu, SWT.PUSH);
        newProcedureItem.setText("Procedures");
        newProcedureItem.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                IScriptUtil util = new ProcedureUtil();
                Action anAction = new NewAction(util);
                anAction.run();
            }
        });
        newItem.setMenu(newMenu);

    }

    /**
     * Initialize the proceduresLabel variable.
     */
    private void initProceduresLabel() {
        proceduresLabel = new Label(comp, SWT.NONE);
        proceduresLabel.setText("Procedures");
        proceduresLabel.setLayoutData(new GridData(SWT.CENTER, SWT.NONE, false,
                false));
    }

    /**
     * Create proceduresList, fill it with the procedure names, and connect it
     * to the mouse listener for performing actions (modify, delete, etc.).
     */
    private void initProceduresList() {
        proceduresList = new List(comp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.heightHint = proceduresList.getItemHeight() * 10;
        layoutData.widthHint = 150;
        proceduresList.setLayoutData(layoutData);

        loadProcedures();
        proceduresList.addMouseListener(new ProcedureMouseListener());
    }

    /**
     * Set the contents of proceduresList to all the Python scripts in the GFE
     * procedures directory.
     */
    private void loadProcedures() {
        proceduresList.removeAll();

        Set<String> procs = new HashSet<String>();
        procs.addAll(catalog.getNames());
        String[] procedures = procs.toArray(new String[0]);
        Arrays.sort(procedures, String.CASE_INSENSITIVE_ORDER);

        for (String procedure : procedures) {
            proceduresList.add(procedure);
        }
    }
}