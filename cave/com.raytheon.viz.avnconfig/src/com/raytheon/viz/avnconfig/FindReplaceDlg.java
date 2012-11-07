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
package com.raytheon.viz.avnconfig;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * FindReplaceDlg class displays the Find & Replace dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 6/19/2008    937        grichard    Implemented 'replace all'.
 * 10/11/2012   1229       rferrel     Made dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class FindReplaceDlg extends CaveSWTDialog {

    /**
     * Search for text field.
     */
    private Text findWhatTF;

    /**
     * Replace with text field.
     */
    private Text replaceByTF;

    /**
     * Find button.
     */
    private Button findBtn;

    /**
     * Replace button.
     */
    private Button replaceBtn;

    /**
     * Replace All button.
     */
    private Button replaceAllBtn;

    /**
     * Cancel button.
     */
    private Button undoBtn;

    /**
     * Text Editor from the main display.
     */
    private StyledText textEditor;

    /**
     * Caret position.
     */
    private int caretPosition = -1;

    /**
     * Text Editor buffer with everything upper-case.
     */
    private StringBuffer editorTextBufUpcase;

    /**
     * Number of characters.
     */
    private int characterCount = -9999;

    /**
     * Found word index.
     */
    private int foundIndex = -9999;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param textEditor
     *            Text editor containing the text to be searched.
     */
    public FindReplaceDlg(Shell parent, StyledText textEditor) {
        super(parent, SWT.NONE, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("Find and Replace");

        this.textEditor = textEditor;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createSearchTextField();
        createReplaceTextField();
        createControlButtons();
        addSeparator();
        createCloseButton();
    }

    /**
     * Create the "Find what" text field.
     */
    private void createSearchTextField() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Composite findComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        gridLayout.horizontalSpacing = 20;
        findComp.setLayout(gridLayout);
        findComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Label findWhatLbl = new Label(findComp, SWT.NONE);
        findWhatLbl.setText("Find what: ");
        findWhatLbl.setLayoutData(gd);

        gd = new GridData(GridData.FILL_HORIZONTAL);
        findWhatTF = new Text(findComp, SWT.BORDER | SWT.MULTI);
        findWhatTF.setLayoutData(gd);
    }

    /**
     * Create the "Replace by" text field.
     */
    private void createReplaceTextField() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Composite replaceComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        gridLayout.horizontalSpacing = 20;
        replaceComp.setLayout(gridLayout);
        replaceComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Label replaceLbl = new Label(replaceComp, SWT.NONE);
        replaceLbl.setText("Replace by: ");
        replaceLbl.setLayoutData(gd);

        gd = new GridData(GridData.FILL_HORIZONTAL);
        replaceByTF = new Text(replaceComp, SWT.BORDER | SWT.MULTI);
        replaceByTF.setLayoutData(gd);
    }

    /**
     * Create the buttons to control how the search will be conducted.
     */
    private void createControlButtons() {
        Composite topBtnRowComp = new Composite(shell, SWT.NONE);
        RowLayout rowLayout = new RowLayout();
        rowLayout.marginLeft = 1;
        rowLayout.spacing = 1;
        rowLayout.marginTop = 1;
        topBtnRowComp.setLayout(rowLayout);

        // Create the Search button.
        RowData rd = new RowData(80, SWT.DEFAULT);
        findBtn = new Button(topBtnRowComp, SWT.PUSH);
        findBtn.setText("Find");
        findBtn.setLayoutData(rd);
        findBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                findWhatText();
            }
        });

        // Create the Replace button.
        rd = new RowData(80, SWT.DEFAULT);
        replaceBtn = new Button(topBtnRowComp, SWT.PUSH);
        replaceBtn.setText("Replace");
        replaceBtn.setLayoutData(rd);
        replaceBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                replaceText();
            }
        });

        // Create the Replace All button.
        rd = new RowData(100, SWT.DEFAULT);
        replaceAllBtn = new Button(topBtnRowComp, SWT.PUSH);
        replaceAllBtn.setText("Replace All");
        replaceAllBtn.setLayoutData(rd);
        replaceAllBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                replaceAll();
            }
        });

        // Create the Cancel button.
        rd = new RowData(80, SWT.DEFAULT);
        undoBtn = new Button(topBtnRowComp, SWT.PUSH);
        undoBtn.setText("Undo");
        undoBtn.setLayoutData(rd);
        undoBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            }
        });
    }

    /**
     * Create the Close button.
     */
    private void createCloseButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Add a horizontal separator to the display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 4;
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Check if there has been updates to the text editor.
     */
    private void checkForUpdates() {
        if (characterCount != textEditor.getText().length()) {
            characterCount = textEditor.getText().length();
            editorTextBufUpcase = new StringBuffer(textEditor.getText()
                    .toUpperCase());
        }

        if (caretPosition != textEditor.getCaretOffset()) {
            caretPosition = textEditor.getCaretOffset();
            foundIndex = caretPosition;
        }
    }

    /**
     * Search for the text in the text editor.
     */
    private boolean findWhatText() {
        checkForUpdates();

        ++foundIndex;

        int findTextLength = findWhatTF.getText().length();
        if (findTextLength == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Find Warning");
            mb.setMessage("You do not have text in the 'Find what' field.");
            mb.open();
            return false;
        }

        if (editorTextBufUpcase.indexOf(findWhatTF.getText().toUpperCase()) == -1) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Warning");
            mb.setMessage("\"" + findWhatTF.getText() + "\"" + " not found.");
            mb.open();
            return false;
        }

        int tmpIndex = foundIndex;
        foundIndex = editorTextBufUpcase.indexOf(findWhatTF.getText()
                .toUpperCase(), foundIndex);

        if (foundIndex == -1) {
            MessageBox searchTopMB = new MessageBox(shell, SWT.ICON_ERROR
                    | SWT.YES | SWT.NO);
            searchTopMB.setText("Warning");
            searchTopMB.setMessage("\"" + findWhatTF.getText() + "\""
                    + " not found.\n" + "Start from the top of the product?");
            int result = searchTopMB.open();

            if (result == SWT.NO) {
                foundIndex = tmpIndex;
                return false;
            }

            foundIndex = 0;

            foundIndex = editorTextBufUpcase.indexOf(findWhatTF.getText()
                    .toUpperCase(), foundIndex);

            if (foundIndex == -1) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Warning");
                mb.setMessage("\"" + findWhatTF.getText() + "\""
                        + " not found.");
                mb.open();
                return false;
            }
        }

        textEditor.setSelection(foundIndex, foundIndex + findTextLength);
        return true;
    }

    /**
     * Replace the text in the text editor with the text in the Replace With
     * text field.
     * 
     * @return True if text was replaced, false if not.
     */
    private boolean replaceText() {
        if (textEditor.getSelectionCount() == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
            mb.setText("Sorry!");
            mb.setMessage("Nothing to replace.");
            mb.open();
            return false;
        }

        textEditor.replaceTextRange(foundIndex, textEditor.getSelectionCount(),
                replaceByTF.getText());
        return true;
    }

    /**
     * Replace all text matching the "Find what" text with the text in the
     * "Replace by" text field
     */
    private void replaceAll() {
        // TODO change to: while(findWhatText() { replaceText();}
        while (1 == 1) {
            if (findWhatText()) {
                replaceText();
            } else {
                break;
            }
        }
    }
}
