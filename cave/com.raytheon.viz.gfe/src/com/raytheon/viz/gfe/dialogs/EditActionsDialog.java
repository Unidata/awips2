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

import java.util.Date;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISmartToolInventoryChanged;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.smarttool.SmartToolMouseListener;
import com.raytheon.viz.gfe.smarttool.SmartUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog for the smart tools edit action
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2007            njensen     Initial creation.	
 * 05Aug2008    #1407       ebabin      Dim delta button for wx/discrete types.
 * Oct 25, 2012 #1287      rferrel     Code clean up part of non-blocking dialog.
 * Oct 13, 2012 #1298      rferrel     Changes for non-blocking SetDeltaDialog.
 *                                      Changes for non-blocking SetValueDialog.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class EditActionsDialog extends CaveJFACEDialog implements
        ISmartToolInventoryChanged {

    private final String XCOORD_SETTING = "EditActionDialog_Xcoord";

    private final String YCOORD_SETTING = "EditActionDialog_Ycoord";

    private final int PICKUP_ID = 2;

    private final int DELTA_ID = 3;

    private final int HEIGHT = 330;

    private String title;

    private Shell shell;

    private List toolsList;

    private Label toolsLabel;

    private Composite comp;

    private MenuItem screenItem;

    private SmartToolMouseListener listener = new SmartToolMouseListener();

    private IActivatedParmChangedListener parmChanged;

    private DataManager dataManager;

    private Parm parm;

    private Button pickupButton;

    private Button deltaButton;

    /**
     * Constructor
     * 
     * @param parentShell
     */
    public EditActionsDialog(Shell parentShell) {
        super(parentShell);
        title = "Edit Actions";
        setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS | SWT.RESIZE);

        dataManager = DataManager.getCurrentInstance();
        parm = dataManager.getSpatialDisplayManager().getActivatedParm();

        parmChanged = new IActivatedParmChangedListener() {

            @Override
            public void activatedParmChanged(final Parm newParm) {
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        handleParmChange(newParm);
                    }
                });
            }
        };

        dataManager.getSpatialDisplayManager().addActivatedParmChangedListener(
                parmChanged);

        dataManager.getSmartToolInterface().addListener(this);
    }

    @Override
    public boolean close() {
        dataManager.getSpatialDisplayManager()
                .removeActivatedParmChangedListener(parmChanged);
        dataManager.getSmartToolInterface().removeListener(this);

        return super.close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == DELTA_ID) {
            showDeltaDialog();
        }
        if (buttonId == PICKUP_ID) {
            showPickupDialog();
        } else {
            super.buttonPressed(buttonId);
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
        pickupButton = createButton(parent, PICKUP_ID, "PickUp...", false);
        deltaButton = createButton(parent, DELTA_ID, "Delta...", false);
        if (parm != null) {
            if ((parm.getGridInfo().getGridType() == GridType.DISCRETE)
                    || (parm.getGridInfo().getGridType() == GridType.WEATHER)) {
                deltaButton.setEnabled(false);
            }
        }
    }

    @Override
    protected Control createContents(Composite parent) {
        Control contents = super.createContents(parent);

        toolsLabel.setVisible(true);
        toolsList.setVisible(true);

        shell.setSize(shell.computeSize(SWT.DEFAULT, HEIGHT));

        handleParmChange(parm);

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

        initToolsLabel();
        initToolsList();

        applyDialogFont(composite);

        if (!SmartUtil.isLdPreloadSet()) {
            String warning = "Environment variable LD_PRELOAD is not set.  It should be set to the Python library (e.g. /usr/lib/libpython2.7.so) for smart tools to work correctly.";
            MessageDialog.openWarning(shell, "Warning", warning);
        }

        return composite;
    }

    protected void createMenuBar(Composite parent) {
        shell = parent.getShell();
        Menu menuBar = new Menu(shell, SWT.BAR);
        shell.setMenuBar(menuBar);

        // file menu item
        MenuItem file = new MenuItem(menuBar, SWT.CASCADE);
        file.setText("File");
        Menu fileMenu = new Menu(shell, SWT.DROP_DOWN);
        file.setMenu(fileMenu);

        screenItem = new MenuItem(fileMenu, SWT.CHECK);
        screenItem.setText("Screen Smart Tools");
        screenItem.setSelection(true);
        screenItem.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (toolsList != null) {
                    toolsList.setItems(getSmartTools(parm));
                }
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);
        MenuItem closeItem = new MenuItem(fileMenu, SWT.PUSH);
        closeItem.setText("Close");
        closeItem.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * @param newParm
     */
    private void handleParmChange(Parm newParm) {
        parm = newParm;
        if (toolsList != null) {
            toolsList.setItems(getSmartTools(newParm));
        }
        IGridData grid = null;
        if (newParm != null) {
            Date time = dataManager.getSpatialDisplayManager()
                    .getSpatialEditorTime();
            grid = newParm.overlappingGrid(time);
        }
        pickupButton.setEnabled(grid != null);
        deltaButton.setEnabled(grid != null);
        if (parm != null) {
            if ((parm.getGridInfo().getGridType() == GridType.DISCRETE)
                    || (parm.getGridInfo().getGridType() == GridType.WEATHER)) {
                deltaButton.setEnabled(false);
            }
        }
    }

    private void initToolsLabel() {
        toolsLabel = new Label(comp, SWT.NONE);
        toolsLabel.setLayoutData(new GridData(SWT.CENTER, SWT.NONE, false,
                false));
        toolsLabel.setText("Smart Tools");
    }

    private void initToolsList() {
        toolsList = new List(comp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        toolsList.setItems(getSmartTools(parm));
        toolsList.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        toolsList.addMouseListener(listener);
    }

    public String[] getSmartTools(Parm parm) {
        return dataManager.getSmartToolInterface().listTools(
                screenItem.getSelection() ? parm : null);
    }

    private void showDeltaDialog() {
        SetDeltaDialog.openDialog();
    }

    private void showPickupDialog() {
        SetValueDialog.openDialog();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#getInitialLocation(org.eclipse.swt.graphics
     * .Point)
     */
    @Override
    protected Point getInitialLocation(Point initialSize) {
        Point result = super.getInitialLocation(initialSize);
        if (GFEPreference.contains(XCOORD_SETTING)) {
            result.x = GFEPreference.getIntPreference(XCOORD_SETTING);
        }
        if (GFEPreference.contains(YCOORD_SETTING)) {
            result.y = GFEPreference.getIntPreference(YCOORD_SETTING);
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.msgs.ISmartToolInventoryChanged#
     * smartToolInventoryChanged()
     */
    @Override
    public void smartToolInventoryChanged() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                toolsList.setItems(getSmartTools(parm));
            }
        });
    }
}
