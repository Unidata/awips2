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

package com.raytheon.viz.texteditor.dialogs;

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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Search and Replace dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 9/13/07      368         lvenable    Initial creation.
 * 10/11/2007   482         grichard    Reformatted file.
 * 1/8/2008     663         grichard    Implemented 'replace all'.
 * 25SEP2012    1196        lvenable    Refactor dialogs to prevent blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 */
public class SearchReplaceDlg extends CaveSWTDialog {

    /**
     * Search for text field.
     */
    private Text searchForTF;

    /**
     * Replace with text field.
     */
    private Text replaceWithTF;

    /**
     * Search button.
     */
    private Button searchBtn;

    /**
     * Replace button.
     */
    private Button replaceBtn;

    /**
     * Replace and Search button.
     */
    private Button replaceSearchBtn;

    /**
     * Replace All button.
     */
    private Button replaceAllBtn;

    /**
     * Cancel button.
     */
    private Button cancelBtn;

    /**
     * Button height.
     */
    private final int BUTTON_HEIGHT = 27;

    /**
     * Text Editor from the main display.
     */
    private StyledText textEditor;

    /**
     * Caret position.
     */
    private int caretPosition = -1;

    /**
     * Text Editor buffer with everything uppercase.
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
     * Search Offset.
     */
    private int searchOffset = 0;

    /**
     * Flag indicating if the text editor is in edit mode.
     */
    private boolean inEditMode = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param textEditor
     *            Text editor containing the text to be searched.
     * @param inEditMode
     *            Edit mode flag.
     */
    public SearchReplaceDlg(Shell parent, StyledText textEditor,
            boolean inEditMode) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("Search and Replace");

        this.textEditor = textEditor;
        this.inEditMode = inEditMode;
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

        setEditMode(inEditMode);
    }

    /**
     * Create the "Search For" text field.
     */
    private void createSearchTextField() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Composite searchComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        gridLayout.horizontalSpacing = 20;
        searchComp.setLayout(gridLayout);
        searchComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Label searchLbl = new Label(searchComp, SWT.NONE);
        searchLbl.setText("Search For: ");
        searchLbl.setLayoutData(gd);

        gd = new GridData(GridData.FILL_HORIZONTAL);
        searchForTF = new Text(searchComp, SWT.BORDER | SWT.SINGLE);
        searchForTF.setLayoutData(gd);
    }

    /**
     * Create the "Replace With" text field.
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
        replaceLbl.setText("Replace With: ");
        replaceLbl.setLayoutData(gd);

        gd = new GridData(GridData.FILL_HORIZONTAL);
        replaceWithTF = new Text(replaceComp, SWT.BORDER | SWT.SINGLE);
        replaceWithTF.setLayoutData(gd);
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
        RowData rd = new RowData(80, BUTTON_HEIGHT);
        searchBtn = new Button(topBtnRowComp, SWT.PUSH);
        searchBtn.setText("Search");
        searchBtn.setLayoutData(rd);
        searchBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                searchForText();
            }
        });

        // Create the Replace button.
        rd = new RowData(80, BUTTON_HEIGHT);
        replaceBtn = new Button(topBtnRowComp, SWT.PUSH);
        replaceBtn.setText("Replace");
        replaceBtn.setLayoutData(rd);
        replaceBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                replaceText();
            }
        });

        // Create the Replace and Search Again button.
        rd = new RowData(SWT.DEFAULT, BUTTON_HEIGHT);
        replaceSearchBtn = new Button(topBtnRowComp, SWT.PUSH);
        replaceSearchBtn.setText("Replace && Search Again");
        replaceSearchBtn.setLayoutData(rd);
        replaceSearchBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                replaceAndSearchAgain();
            }
        });

        // Create the Replace All button.
        rd = new RowData(100, BUTTON_HEIGHT);
        replaceAllBtn = new Button(topBtnRowComp, SWT.PUSH);
        replaceAllBtn.setText("Replace All");
        replaceAllBtn.setLayoutData(rd);
        replaceAllBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                replaceAll();
            }
        });

        // Create the Cancel button.
        rd = new RowData(80, BUTTON_HEIGHT);
        cancelBtn = new Button(topBtnRowComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(rd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.setVisible(false);
            }
        });
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
            foundIndex = -1;
            searchOffset = caretPosition;
        }
    }

    /**
     * Search for the text in the text editor.
     */
    private boolean searchForText() {
        checkForUpdates();
        int searchTextLength = searchForTF.getText().length();
        if (searchTextLength == 0) {
            TextWSMessageBox.open(shell, "Search Warning",
                    "You do not have text in the 'Search For' field.");
            return false;
        }

        int tmpIndex = foundIndex;
        foundIndex = editorTextBufUpcase.indexOf(searchForTF.getText()
                .toUpperCase(), searchOffset++);

        if (foundIndex == -1) {
            int result = TextWSMessageBox.open(shell, "Warning", "\""
                    + searchForTF.getText() + "\"" + " not found.\n"
                    + "Start from the top of the product?", SWT.ICON_ERROR
                    | SWT.YES | SWT.NO);

            if (result == SWT.NO || searchForTF.isDisposed()) {
                foundIndex = tmpIndex;
                return false;
            }

            searchOffset = 0;

            foundIndex = editorTextBufUpcase.indexOf(searchForTF.getText()
                    .toUpperCase(), searchOffset++);

            if (foundIndex == -1) {
                TextWSMessageBox.open(shell, "Warning",
                        "\"" + searchForTF.getText() + "\"" + " not found.");
                return false;
            }
        }

        textEditor.setSelection(foundIndex, foundIndex + searchTextLength);
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
            TextWSMessageBox.open(shell, "Sorry!",
                    "Please select 'search' before 'replace.'",
                    SWT.ICON_INFORMATION | SWT.OK);
            return false;
        }

        textEditor.replaceTextRange(foundIndex, textEditor.getSelectionCount(),
                replaceWithTF.getText());
        return true;
    }

    /**
     * Replace text and search again.
     */
    private void replaceAndSearchAgain() {
        if (replaceText()) {
            searchForText();
        }
    }

    /**
     * Replace all text matching the "Search For" text with the text in the
     * "Replace With" text field
     */
    private void replaceAll() {
        while (1 == 1) {
            if (searchForText()) {
                replaceText();
            } else {
                break;
            }
        }
    }

    /**
     * Set the edit mode which will enable/disable button depending on the flag
     * passed in.
     * 
     * @param inEditMode
     *            True in the editor is in edit mode, false otherwise.
     */
    public void setEditMode(boolean inEditMode) {
        if (!this.isDisposed()) {
            if (inEditMode == true) {
                replaceBtn.setEnabled(true);
                replaceSearchBtn.setEnabled(true);
                replaceAllBtn.setEnabled(true);
                replaceWithTF.setEnabled(true);
            } else {
                replaceBtn.setEnabled(false);
                replaceSearchBtn.setEnabled(false);
                replaceAllBtn.setEnabled(false);
                replaceWithTF.setEnabled(false);
            }
        }
    }
}
