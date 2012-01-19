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
package com.raytheon.uf.viz.app.launcher.dialogs;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Resource;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.app.launcher.dialogs.widgets.AppTextWidget;

/**
 * Implements a dialog that queries the user for one or more application arguments.
 * When the Run button is clicked, the values are obtained from the text entry fields
 * and returned in a String[] wrapped as an object. The arguments can be made optional.
 * If the arguments are not optional, the dialog can only be canceled until values for
 * all arguments are provided. When the arguments are optional, individual arguments
 * may be left blank.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2009 2081       mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0
 */
public class AppArgsDlg extends Dialog {
    /** dialog shell */
    private Shell shell = null;
    /** SWT/OS display */
    private Display display = null;
    /** return value from open method */
    private Object returnObject = null;
    /** font for controls -- must be disposed */
    private Font controlFont = null;
    
    /** name of the application being launched*/
    private final String appName;
    /** number of arguments to obtain*/
    private final int args;
    /** flag to indicate if the arguments are optional */
    private final boolean optional;
    /** the test entry widgets */
    private List<AppTextWidget> argEntry = new ArrayList<AppTextWidget>();
    /** buttons that may be disabled */
    private List<Button> dimables = new ArrayList<Button>();
    /**
     * Constructor. Creates the dialog for the specified application.
     * The dialog has a single argument entry box. The argument is
     * required.
     * 
     * @param parent parent of this dialog
     * @param appName name of the application to launch
     */
    public AppArgsDlg(Shell parent,String appName) {
        this(parent,appName,1);
    }
    /**
     * Constructor. Creates a dialog for the specified application.
     * The dialog has text entry boxes for the specified number of
     * arguments. The arguments are required. 
     * 
     * @param parent parent of this dialog
     * @param appName name of the application to launch
     * @param args number of arguments to request
     */
    public AppArgsDlg(Shell parent,String appName, int args) {
        this(parent,appName,args,false);
    }
    /**
     * Constructor. Creates a dialog for the specified application.
     * The dialog has text entry boxes for the specified number of
     * arguments. The arguments may be required or optional depending
     * on the value of the {@code optional} flag.
     * 
     * @param parent parent of this dialog
     * @param appName name of the application to launch
     * @param args number of arguments to request
     * @param optional true if the arguments are optional
     */
    public AppArgsDlg(Shell parent, String appName, int args, boolean optional) {
        super(parent,SWT.NONE);
        this.appName = appName;
        this.args = args;
        this.optional = optional;
    }
    /**
     * Displays the dialog and manages its life cycle. When the dialog
     * is closed, a object containing the arguments is returned to the
     * client.
     * 
     * @return contains the arguments
     */
    public Object open() {
        createDialogShell();
        initializeResources();
        createDataEntryArea();
        shell.pack();
        shell.open();
        
        manageEvents();
        disposeResources();
        return returnObject;
    }
    /**
     * Creates the data entry area for the dialog.
     */
    private void createDataEntryArea() {
        
        Label lbl = new Label(shell,SWT.NONE);
        lbl.setText("Enter arguments for " + appName + ":");
        lbl.setFont(controlFont);
        
        createTextEntryBoxes(shell, argEntry, 300, args);
        createDialogButtons(shell,dimables,80);
        setButtonEnabledState();
    }
    /**
     * Creates the argument entry boxes for the dialog. Each argument entry
     * box has an associated label containing "Arg(x):". A ModifyListener that
     * calls {@code setButtonEnabledState()} is added to each entry box.
     * 
     * @param parent the container for the argument entry boxes
     * @param argList list to contain the argument entry boxes 
     * @param width width of each argument entry box
     * @param count number of argument entry boxes to create
     */
    private void createTextEntryBoxes(Composite parent, List<AppTextWidget> argList, int width, int count) {
        for (int i = 0; i < count; i++) {
            String name = String.format("Arg(%d):", i);
            AppTextWidget wgt = new AppTextWidget(parent,name,width);
            wgt.create();
            wgt.addModifyListener(new ModifyListener() {
                @Override
                public void modifyText(ModifyEvent e) {
                    setButtonEnabledState();
                }
            });
            argList.add(wgt);
            wgt = null;
        }
    }
    /**
     * Creates the action buttons for the dialog. The basic properties of
     * the buttons are defined in the {@link ActionButton} enumeration. The
     * buttons are displayed in a right justified row at the bottom of the
     * dialog.
     * 
     * @param parent the container for the action buttons
     * @param dimables list containing buttons that may be disabled
     * @param btnWidth desired width of each button
     */
    private void createDialogButtons(Composite parent,List<Button> dimables, int btnWidth) {
        GridData gd = null;
        Composite comp = null;
        
        gd = new GridData(SWT.RIGHT,SWT.DEFAULT,true,true);
        comp = new Composite(parent,SWT.NONE);
        comp.setLayout(new GridLayout(ActionButton.buttons.length,true));
        comp.setLayoutData(gd);
        
        String[] names = ActionButton.getButtonNames();
        for(String name : names) {
            Button btn = createActionBtn(comp,name,btnWidth);
            if (ActionButton.translate(name).isDimable()) {
                dimables.add(btn);
            }
        }
        
    }
    /**
     * Creates a single action button with the specified name and preferred
     * width. Each button has a SelectionListener which calls 
     * {@code processRequest(Event)} when the button is pressed.
     * 
     * @param parent container for the button
     * @param label text to display on the button
     * @param width preferred width of the button
     * 
     * @return the newly created button
     */
    private Button createActionBtn(Composite parent, String label, int width) {
        GridData gd = null;
        Button btn = null;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        gd.widthHint = width;
        btn = new Button(parent,SWT.PUSH);
        btn.setText(label);
        btn.setLayoutData(gd);
        btn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                processRequest(event);
            }
        });
        return btn;
    }
    /**
     * Called each time the text changes in one of the argument
     * entry fields on the dialog.
     * <P>
     * Sets the enabled property of the dimable buttons based on
     * the state of the argument entry fields of the dialog. The
     * dimable buttons are disabled when
     * <OL>
     * <LI>the application arguments are not optional, and
     * <LI>not all argument entry fields contain text
     * </OL>
     */
    private void setButtonEnabledState() {
        boolean enabled = true;
        if (!this.optional) {
            for (AppTextWidget text : argEntry) {
                if (text.getText().trim().length() == 0) {
                    enabled = false;
                    break;
                }
            }
        }
        for (Button btn : dimables) {
            btn.setEnabled(enabled);
        }
    }
    /**
     * Called when one of the action buttons on the dialog is clicked.
     * <P>
     * Obtains the name of the button and triggers the command handler.
     * 
     * @param event the SWT event that triggers the call back
     */
    private void processRequest(SelectionEvent event) {
        Object obj = event.getSource();
        if (obj instanceof Button) {
            Button btn = (Button)obj;
            String command = btn.getText();
            trigger(command);
        }
    }
    /**
     * Executes the processing handler associated with the command.
     * 
     * @param command the command to execute
     */
    private void trigger(String command) {
        ActionButton action = ActionButton.translate(command);
        switch (action) {
        case RUN_BUTTON:
            execute(false);
            break;
        case CANX_BUTTON:
            execute(true);
            break;
        case CLR_BUTTON:
            clear();
            break;
        default:
            // intentionally empty
        }
    }
    /**
     * Executes the main functionality of the dialog. Called when
     * either the <em>Run</em> or <em>Cancel</em> button is clicked.
     * The argument determines which button was clicked.
     * <P>
     * When the <em>Run</em> button is clicked, obtains the contents
     * of the argument entry fields and places them in a String[]
     * object to return to the client.
     * <P>
     * For both <em>Run</em> and <em>Cancel</em>, the dialog is closed
     * by calling the Shell's dispose method.
     * 
     * @param canx true for cancel, false for run
     */
    private void execute(boolean canx) {
        if (!canx) {
            ArrayList<String> args = new ArrayList<String>();
            for (AppTextWidget atb : argEntry) {
                args.add(atb.getText());
            }
            returnObject = args.toArray(new String[] {});
        }
        shell.dispose();
    }
    /**
     * Clears the argument entry fields on the dialog. Called when
     * the <em>Clear</em> button is clicked.
     */
    private void clear() {
        for (AppTextWidget atb : argEntry) {
            atb.clear();
        }
    }
    /**
     * Initializes all disposable resources -- Colors, Fonts, etc --
     * used by the dialog.
     * <P>
     * <B>Important:</B> All disposable resources must be initialized
     * here and disposed in {@link #disposeResources()} to prevent resource
     * leaks. 
     */
    private void initializeResources() {
        controlFont = new Font(shell.getDisplay(),"Monospace",12,SWT.NORMAL);
    }
    /**
     * Disposes all disposable resources -- Colors, Fonts, etc --
     * used by the dialog.
     * <P>
     * <B>Important:</B> All disposable resources must be initialized
     * in {@link #initializeResources()} and disposed of here to prevent
     * resource leaks.
     */
    private void disposeResources() {
        disposeResource(controlFont);
    }
    /**
     * Safely disposes a single resource.
     * 
     * @param resource the resource to dispose
     */
    private void disposeResource(Resource resource) {
        if (resource != null) {
            resource.dispose();
            resource = null;
        }
    }
    /**
     * Provides the event management loop for the dialog.
     */
    private void manageEvents() {
        while(!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
    }
    /**
     * Creates the shell for the dialog.
     */
    private void createDialogShell() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent,SWT.DIALOG_TRIM | SWT.RESIZE);
        shell.setText(this.appName + ": Specify App Arguments");
        
        GridLayout layout = new GridLayout(1,false);
        layout.marginHeight = 2;
        layout.marginHeight = 2;
        layout.verticalSpacing = 1;
        shell.setLayout(layout);
    }
    /**
     * Enumeration defining the buttons for this widget. The commands Map
     * defines the actions available to the dialog; the buttons array defines
     * the buttons to create.
     * <P>
     * Implementations notes: Each enum value needs to have an entry in the
     * commands Map; the key is the lower case of the text on the button.
     * Each button to be created must have an entry in the buttons array; there
     * must be a corresponding entry in the commands Map.
     */
    private enum ActionButton {
        RUN_BUTTON(true), CANX_BUTTON(false),CLR_BUTTON(false);
        /** define the commands this dialog can respond to */
        private static final Map<String, ActionButton> commands = new HashMap<String, ActionButton>() {
            private static final long serialVersionUID = 1l;
            {
                put("run",RUN_BUTTON);
                put("cancel",CANX_BUTTON);
                put("clear",CLR_BUTTON);
            }
        };
        /** define the buttons this dialog will have */
        private static String[] buttons = {"Run","Clear","Cancel"};
        /** true if the button can be disabled */
        private final boolean dimable;
        /**
         * Constructor. Set the argument to {@code true} if the
         * button can be disabled.
         * 
         * @param dimable true if the buttons can be disabled, false otherwise
         */
        private ActionButton(boolean dimable)
        {
            this.dimable = dimable;
        }
        /**
         * returns true if the button can be disabled.
         */
        public boolean isDimable() {
            return this.dimable;
        }
        /**
         * Returns the enum value associated with the specified
         * action.
         * @param action the action to translate
         * @return the enum value representing the action
         */
        public static final ActionButton translate(String action) {
            return commands.get(action.toLowerCase());
        }
        /**
         * returns an array of button names.
         */
        public static final String[] getButtonNames() {
            return buttons;
        }
    }
}
