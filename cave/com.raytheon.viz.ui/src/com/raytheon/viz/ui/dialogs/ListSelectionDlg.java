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
package com.raytheon.viz.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

/**
 * Dialog allowing the user to select one or more items from the provided list.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 09, 2014   2864     mpduff      Initial creation
 * Apr 22, 2014   3053     lvenable    Updated to be more configurable.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ListSelectionDlg extends CaveSWTDialog {
    /** Stings to populate the list */
    private final String[] textChoices;

    /** Single/Multiple select flag */
    private final boolean singleSelect;

    /** The list widget */
    private List selectList;

    /** Label above the list control. */
    private String listLblText;

    /** Default minimum list width. */
    private int defaultMinListWidth = 225;

    /** Default Minimum list height. */
    private int defaultMinListHeight = 275;

    /** Minimum list width. */
    private int minListWidth = defaultMinListWidth;

    /** Minimum list height. */
    private int minListHeight = defaultMinListHeight;

    /** Text to put in the action button. */
    private String actionButtonText;

    /**
     * Enumeration to determine how the selection from the list will be
     * returned.
     */
    public enum ReturnArray {
        ARRAY_STRING_ITEMS, ARRAY_INDEXES
    };

    /** Variable to determine how the data is returned. */
    private ReturnArray returnAsArray = ReturnArray.ARRAY_STRING_ITEMS;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param textChoices
     *            Array of items to be put in the list.
     * @param singleSelect
     *            True for single selection, false for multiple selection.
     * @param returnAs
     *            Determine how the selection is returned.
     * @param actionButtonText
     *            Text for the action button.
     */
    public ListSelectionDlg(Shell parent, String[] textChoices,
            boolean singleSelect, ReturnArray returnAs, String actionButtonText) {
        this(parent, textChoices, singleSelect, returnAs, actionButtonText,
                null, null);
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param textChoices
     *            Array of items to be put in the list.
     * @param singleSelect
     *            True for single selection, false for multiple selection.
     * @param returnAs
     *            Determine how the selection is returned.
     * @param actionButtonText
     *            Text for the action button.
     * @param title
     *            Dialog title.
     * @param listMsg
     *            Text displayed in the label above the list control.
     */
    public ListSelectionDlg(Shell parent, String[] textChoices,
            boolean singleSelect, ReturnArray returnAs,
            String actionButtonText, String title, String listMsg) {
        this(parent, textChoices, singleSelect, returnAs, actionButtonText,
                title, listMsg, 225, 275);
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param textChoices
     *            Array of items to be put in the list.
     * @param singleSelect
     *            True for single selection, false for multiple selection.
     * @param returnAs
     *            Determine how the selection is returned.
     * @param actionButtonText
     *            Text for the action button.
     * @param title
     *            Dialog title.
     * @param listMsg
     *            Text displayed in the label above the list control.
     * @param minWidth
     *            Minimum list control width (minimum default value is 225).
     * @param minHeight
     *            Minimum list control height (minimum default value is 275).
     */
    public ListSelectionDlg(Shell parent, String[] textChoices,
            boolean singleSelect, ReturnArray returnAs,
            String actionButtonText, String title, String listMsg,
            int minWidth, int minHeight) {
        super(parent, SWT.TITLE | SWT.RESIZE | SWT.APPLICATION_MODAL,
                CAVE.DO_NOT_BLOCK);

        this.textChoices = textChoices;
        this.singleSelect = singleSelect;
        this.returnAsArray = returnAs;

        // Set the minimum width & height for the list control.
        this.minListWidth = (defaultMinListWidth < minWidth ? defaultMinListWidth
                : minWidth);
        this.minListHeight = (defaultMinListHeight < minHeight ? defaultMinListHeight
                : minHeight);

        // Set the action button text.
        if (actionButtonText == null) {
            this.actionButtonText = "Select";
        } else {
            this.actionButtonText = actionButtonText;
        }

        // Set the dialog title.
        if (title == null) {
            setText("Selection");
        } else {
            setText(title);
        }

        // Set the text in the label above the list control.
        if (listMsg == null) {
            if (singleSelect) {
                listLblText = "Select an item:";
            } else {
                listLblText = "Select item(s):";
            }
        } else {
            listLblText = listMsg;
        }
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(1, false));
        mainComp.setLayoutData(gd);

        Label listLbl = new Label(mainComp, SWT.NONE);
        listLbl.setText(listLblText);

        int style = SWT.SINGLE;
        if (!this.singleSelect) {
            style = SWT.MULTI;
        }

        style |= SWT.V_SCROLL | SWT.H_SCROLL;

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        selectList = new List(mainComp, SWT.BORDER | style);
        selectList.setLayoutData(gd);
        selectList.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                action();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite btnComp = new Composite(shell, SWT.NONE);
        btnComp.setLayout(new GridLayout(2, false));
        btnComp.setLayoutData(gd);

        int buttonWidth = 0;

        Button selectBtn = new Button(btnComp, SWT.PUSH);
        buttonWidth = calculateButtonWidth(selectBtn, actionButtonText);
        selectBtn.setLayoutData(new GridData(buttonWidth, SWT.DEFAULT));
        selectBtn.setText(actionButtonText);
        selectBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                action();
            }
        });

        Button cancelBtn = new Button(btnComp, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(buttonWidth, SWT.DEFAULT));
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

        selectList.setItems(textChoices);
        this.shell.setMinimumSize(minListWidth, minListHeight);
    }

    /**
     * Calculate the button width based on the text to be displayed.
     * 
     * @param btn
     *            Button control.
     * @param text
     *            Text to be displayed.
     * @return The calculated button width.
     */
    private int calculateButtonWidth(Button btn, String text) {
        int rv = 0;
        int extentLength = 0;
        int defaultButtonWidth = 75;
        int textBufferWidth = 15;

        // Get the length of the text in pixels that will be displayed in the
        // button.
        GC gc = new GC(btn);
        extentLength = gc.stringExtent(text).x;
        rv = (defaultButtonWidth > extentLength) ? defaultButtonWidth
                : extentLength;
        gc.dispose();

        /*
         * Return the lenght of the text and the added buffer that accounts for
         * the button edges.
         */
        return rv + textBufferWidth;
    }

    /**
     * Action handler.
     */
    private void action() {

        if (selectList.getSelectionCount() == 0) {
            int choice = displayConfirmationBox();
            if (choice == SWT.CANCEL) {
                return;
            }
        }

        if (returnAsArray == ReturnArray.ARRAY_STRING_ITEMS) {
            setReturnValue(selectList.getSelection());
        } else if (returnAsArray == ReturnArray.ARRAY_INDEXES) {
            setReturnValue(selectList.getSelectionIndices());
        } else {
            setReturnValue(null);
        }

        close();
    }

    /**
     * Display a confirmation dialog to the user.
     * 
     * @return SWT.OK or SWT.CANCEL
     */
    private int displayConfirmationBox() {
        String itemTxt = null;

        if (singleSelect) {
            itemTxt = "No item is selected.";
        } else {
            itemTxt = "No items are selected.";
        }

        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Confirmation");
        mb.setMessage(itemTxt + "  Do you wish to continue?");
        int val = mb.open();
        return val;
    }
}
