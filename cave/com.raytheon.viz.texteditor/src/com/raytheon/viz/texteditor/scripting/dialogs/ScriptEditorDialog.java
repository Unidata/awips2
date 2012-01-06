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
package com.raytheon.viz.texteditor.scripting.dialogs;

import java.io.File;
import java.io.IOException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Resource;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.texteditor.Activator;
import com.raytheon.viz.texteditor.dialogs.SearchReplaceDlg;
import com.raytheon.viz.texteditor.scripting.dialogs.HelpRequestDlg.EnumHelpTypes;
import com.raytheon.viz.texteditor.scripting.dialogs.util.FileUtilities;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Main dialog for the Text WS Script Editor. Will always be launched from the
 * Text Editor Window.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2009            mfegan     Initial creation
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class ScriptEditorDialog extends CaveSWTDialog implements IScriptEditor {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ScriptEditorDialog.class);
    /* strings for dialog title creation */
    private static final String FILE_DEFAULT = "untitled";

    private static final String FMT_TITLE = "Text %s Script Editor (%s)";

    @SuppressWarnings("unused")
    private static final String HELLO_WORLD = "repeat(5,\"print 'Hello World!'\")\n"
            + "wait()\n" + "print 'After Loop!'";

    /** the observer to pass to results from a running script */
    private final IScriptEditorObserver observer;

    /** the script output window */
    ScriptOutputDlg scriptOutput = null;

    /** the text editor ID token */
    private final String token;

    /** the name of the file */
    private String fileName = FILE_DEFAULT;

    private String filePath = "";

    /** flag to indicate if an initial script is to be loaded */
    private boolean loadScript = false;

    /** the script editor box */
    private StyledText scriptEditor = null;

    /** editor "dirty" flag */
    private boolean scriptEditorDirty = false;

    /** the 'run' menu item */
    private MenuItem miRunScript;

    /** the 'continue' menu item */
    private MenuItem miContinue;

    /** the 'skip wait' menu item */
    private MenuItem miSkipWait;

    /** the 'cancel' menu item */
    private MenuItem miCancel;

    /** the undelete character menu item */
    private MenuItem miUndelCharacter;

    /** the undelete word menu item */
    private MenuItem miUndelWord;

    /** the undelete line menu item */
    private MenuItem miUndelLine;

    /** the option->overstrike menu item */
    private MenuItem miOverstrike;

    /** the 'show output' menu item */
    private MenuItem showOutput = null;

    /**
     * determines if the 'Show Output' menu item should be selected when the
     * dialog is first opened.
     */
    private final boolean outputState;

    /** tracks if the text editor is in overwrite mode */
    private boolean overwriteMode = false;

    /**
     * Deleted character stored as a string.
     */
    private String deletedChar = "";

    /**
     * Deleted word stored as a string.
     */
    private String deletedWord = "";

    /**
     * Deleted line stored as a string.
     */
    private String deletedLine = "";

    /**
     * Search and replace dialog.
     */
    private SearchReplaceDlg searchReplaceDlg = null;

    /* the fonts */
    private Font smlFont = null;

    private Font medFont = null;

    private Font lrgFont = null;

    /**
     * An enumeration of the font sizes.
     */
    private enum EnumFontSize {
        SMALL(10), MEDIUM(12), LARGE(16);
        private final int size;

        private EnumFontSize(int points) {
            this.size = points;
        }

        public int getSize() {
            return this.size;
        }
    }

    /**
     * Constructor. Creates the class that supports the dialog.
     * 
     * @param parent
     *            The parent shell for this dialog
     * @param observer
     *            the observer for this dialog
     * @param token
     *            text editor dialog identifier token.
     */
    public ScriptEditorDialog(Shell parent, IScriptEditorObserver observer,
            String token) {
        this(parent, observer, token, false);
    }

    /**
     * Constructor. Creates the class that supports the dialog. This version
     * allows the client class to set the state of the 'Show Output' menu item.
     * 
     * @param parent
     *            the parent shell for this dialog
     * @param observer
     *            the observer for this dialog
     * @param token
     *            the text editor dialog identifier token
     * @param outputState
     *            the desired state of the 'Show Output' menu item
     */
    public ScriptEditorDialog(Shell parent, IScriptEditorObserver observer,
            String token, boolean outputState) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.NO_PACK);
        this.observer = observer;
        this.token = token;
        this.outputState = outputState;

        setTitle();
    }

    /**
     * Opens the dialog. This version loads the specified script file.
     * 
     * @param filename
     *            name of the file to load
     * 
     * @return object populated as a result of user actions
     */
    public Object open(String filePath, String filename) {
        // this will need to handle operations involving having a script
        // provided
        this.fileName = filename;
        this.filePath = filePath;
        this.loadScript = true;
        return open();
    }

    /**
     * Sets the dialog title based on the Editor ID token and the currently
     * displayed script filename.
     */
    private void setTitle() {
        setText(String.format(FMT_TITLE, token, fileName));
    }

    /* file I/O utilities */
    private boolean loadFile(String path, String file) {
        return loadFile(path + File.separator + file);
    }

    private boolean loadFile(String path) {
        if (scriptEditor != null) {
            // load the file...
            try {
                String contents = FileUtilities.loadFileToString(path);
                scriptEditor.setText(contents);
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, "Unable to save load file \""
                                + fileName + "\"", e);
                return false;
            }
            return true;
        }
        return false;
    }

    private void saveFile(String path, String file, String contents)
            throws IOException {
        saveFile(path + File.separator + file, contents);
    }

    private void saveFile(String path, String contents) throws IOException {
        FileUtilities.writeStringToFile(path, contents);
    }

    @Override
    protected Layout constructShellLayout() {
        return new FillLayout();
    }

    @Override
    protected void disposed() {
        System.out.println("shell is disposed...");
        dirtyFileHelper("before closing");
        disposeResource(smlFont);
        disposeResource(medFont);
        disposeResource(lrgFont);
        if (observer != null) {
            observer.windowClosing();
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Point size = new Point(500, 500);
        shell.setSize(size);
        shell.layout();
        smlFont = new Font(shell.getDisplay(), "Monospace", EnumFontSize.SMALL
                .getSize(), SWT.NORMAL);
        medFont = new Font(shell.getDisplay(), "Monospace", EnumFontSize.MEDIUM
                .getSize(), SWT.NORMAL);
        lrgFont = new Font(shell.getDisplay(), "Monospace", EnumFontSize.LARGE
                .getSize(), SWT.NORMAL);

        createMenuBar();
        createClientArea();
    }

    /**
     * Safely disposes a single resource.
     * 
     * @param resource
     *            the resource to dispose
     */
    private void disposeResource(Resource resource) {
        if (resource != null) {
            resource.dispose();
            resource = null;
        }
    }

    /**
     * Top level method for creating the main menu.
     */
    private void createMenuBar() {
        Menu menu = new Menu(shell, SWT.BAR);
        shell.setMenuBar(menu);
        createFileMenu(menu);
        createEditMenu(menu);
        createOptionsMenu(menu);
        createExecuteMenu(menu);
        createHelpMenu(menu);
    }

    /**
     * Top level method for creating the client area
     */
    private void createClientArea() {
        Composite comp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(GridData.FILL_BOTH);
        GridLayout gl = new GridLayout(1, false);
        comp.setLayout(gl);
        comp.setLayoutData(gd);
        scriptEditor = new StyledText(comp, SWT.BORDER | SWT.MULTI
                | SWT.H_SCROLL | SWT.V_SCROLL);
        scriptEditor.setLayoutData(gd);

        scriptEditor.setFont(medFont);
        scriptEditor.addVerifyKeyListener(new VerifyKeyListener() {
            @Override
            public void verifyKey(VerifyEvent event) {
                if (event.keyCode == SWT.DEL || event.character == SWT.BS
                        || event.keyCode == SWT.SHIFT) {
                    // need to capture the Delete, Backspace and Shift keys
                } else if (event.keyCode > 500) {
                    // capture non-alphanumeric editing keys
                } else if (overwriteMode
                        && scriptEditor.getCaretOffset() < scriptEditor
                                .getCharCount()) {
                    scriptEditor.replaceTextRange(
                            scriptEditor.getCaretOffset(), 1, String
                                    .valueOf(event.character));
                    scriptEditor
                            .setCaretOffset(scriptEditor.getCaretOffset() + 1);
                    event.doit = false;
                }

            }
        });
        scriptEditor.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                scriptEditorDirty = true;
            }
        });

        if (loadScript) {
            loadFile(this.filePath, this.fileName);
        } else {
            scriptEditor.setText("");
        }
        scriptEditorDirty = false;
    }

    /**
     * creates the file menu
     * 
     * @param bar
     *            the main menu bar
     */
    private void createFileMenu(Menu bar) {
        /* the selection listener */
        FileMenuSelectionListener listener = new FileMenuSelectionListener();
        MenuItem item = new MenuItem(bar, SWT.CASCADE);
        item.setText("File");
        Menu menu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(menu);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("New\tCtrl+T");
        item.setAccelerator(SWT.CTRL + 'T');
        item.addSelectionListener(listener);
        item.setData(FileMenuSelection.FILE_NEW);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Open ...\tCtrl+O");
        item.setAccelerator(SWT.CTRL + 'O');
        item.addSelectionListener(listener);
        item.setData(FileMenuSelection.FILE_OPEN);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Save\tCtrl+S");
        item.setAccelerator(SWT.CTRL + 'S');
        item.addSelectionListener(listener);
        item.setData(FileMenuSelection.FILE_SAVE);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Save As ...\tF3");
        item.setAccelerator(SWT.F3);
        item.addSelectionListener(listener);
        item.setData(FileMenuSelection.FILE_SAVE_AS);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Rename ...");
        item.addSelectionListener(listener);
        item.setData(FileMenuSelection.FILE_RENAME);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Delete ...");
        item.addSelectionListener(listener);
        item.setData(FileMenuSelection.FILE_DELETE);

        new MenuItem(menu, SWT.SEPARATOR);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Print\tCtrl+P");
        item.setAccelerator(SWT.CTRL + 'P');
        item.addSelectionListener(listener);
        item.setData(FileMenuSelection.FILE_PRINT);

        new MenuItem(menu, SWT.SEPARATOR);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Close\tAlt+F4");
        item.setAccelerator(SWT.ALT + SWT.F4);
        item.addSelectionListener(listener);
        item.setData(FileMenuSelection.FILE_CLOSE);

    }

    /**
     * creates the Edit drop down on the main menu
     * 
     * @param bar
     *            the main menu bar
     */
    private void createEditMenu(Menu bar) {
        /* the menu selection handler */
        EditMenuSelectionListener listener = new EditMenuSelectionListener();
        /* create the Edit drop down */
        MenuItem item = new MenuItem(bar, SWT.CASCADE);
        item.setText("Edit");
        Menu menu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(menu);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Cut\tCrtl+X");
        item.setData(EditMenuSelection.CUT);
        item.addSelectionListener(listener);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Copy\tCtrl+C");
        item.setData(EditMenuSelection.COPY);
        item.addSelectionListener(listener);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Paste\tCtrl+V");
        item.setData(EditMenuSelection.PASTE);
        item.addSelectionListener(listener);

        new MenuItem(menu, SWT.SEPARATOR);

        /* create the 'select' sub-menu */
        item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Select");
        Menu subMenu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(subMenu);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To previous word\tCtrl+Shift+Left");
        item.setData(EditMenuSelection.SEL_PREVIOUS_WORD);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To next word\tCtrl+Shift+Right");
        item.setData(EditMenuSelection.SEL_NEXT_WORD);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To beginning of line\tShift+Home");
        item.setData(EditMenuSelection.SEL_BEGINNING_LINE);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To end of line\tShift+End");
        item.setData(EditMenuSelection.SEL_END_LINE);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To previous page\tShift+Page Up");
        item.setData(EditMenuSelection.SEL_PREVIOUS_PAGE);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To next page\tShift+Page Down");
        item.setData(EditMenuSelection.SEL_NEXT_PAGE);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To top of Product\tCtrl+Shift+Home");
        item.setData(EditMenuSelection.SEL_TOP_PRODUCT);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To end of Product\tCtrl+Shift+End");
        item.setData(EditMenuSelection.SEL_END_PRODUCT);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("All\tCtrl+A");
        item.setData(EditMenuSelection.SEL_ALL);
        item.addSelectionListener(listener);

        /* create the 'delete' sub-menu */
        item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Delete");
        subMenu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(subMenu);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("Character\tF6");
        item.setAccelerator(SWT.F6);
        item.setData(EditMenuSelection.DEL_CHARACTER);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("Word\tF7");
        item.setAccelerator(SWT.F7);
        item.setData(EditMenuSelection.DEL_WORD);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("Line\tF7");
        item.setAccelerator(SWT.F8);
        item.setData(EditMenuSelection.DEL_LINE);
        item.addSelectionListener(listener);

        /* create the 'undelete' sub-menu */
        item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Undelete");
        subMenu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(subMenu);

        miUndelCharacter = new MenuItem(subMenu, SWT.PUSH);
        miUndelCharacter.setText("Character\tShift+F6");
        miUndelCharacter.setAccelerator(SWT.SHIFT + SWT.F6);
        miUndelCharacter.setData(EditMenuSelection.UNDEL_CHARACTER);
        miUndelCharacter.setEnabled(false);
        miUndelCharacter.addSelectionListener(listener);

        miUndelWord = new MenuItem(subMenu, SWT.PUSH);
        miUndelWord.setText("Word\tShift+F7");
        miUndelWord.setAccelerator(SWT.SHIFT + SWT.F7);
        miUndelWord.setData(EditMenuSelection.UNDEL_WORD);
        miUndelWord.setEnabled(false);
        miUndelWord.addSelectionListener(listener);

        miUndelLine = new MenuItem(subMenu, SWT.PUSH);
        miUndelLine.setText("Line\tShift+F8");
        miUndelLine.setAccelerator(SWT.SHIFT + SWT.F8);
        miUndelLine.setData(EditMenuSelection.UNDEL_LINE);
        miUndelLine.setEnabled(false);
        miUndelLine.addSelectionListener(listener);

        new MenuItem(menu, SWT.SEPARATOR);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Search ...\tF2");
        item.setAccelerator(SWT.F2);
        item.setData(EditMenuSelection.SEARCH);
        item.addSelectionListener(listener);

    }

    /**
     * Sets up the options menu.
     * 
     * @param bar
     *            the main menu
     */
    private void createOptionsMenu(Menu bar) {
        /* the menu selection handler */
        OptionMenuSelectionListener listener = new OptionMenuSelectionListener();

        MenuItem item = new MenuItem(bar, SWT.CASCADE);
        item.setText("Options");
        Menu menu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(menu);

        /* create the 'font size' sub-menu */
        item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Font Size");
        Menu subMenu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(subMenu);

        item = new MenuItem(subMenu, SWT.RADIO);
        item.setText("Small");
        item.setData(OptionMenuSelection.OPTION_FONT_SMALL);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.RADIO);
        item.setText("Medium");
        item.setSelection(true);
        item.setData(OptionMenuSelection.OPTION_FONT_MEDIUM);
        item.addSelectionListener(listener);

        item = new MenuItem(subMenu, SWT.RADIO);
        item.setText("Large");
        item.setData(OptionMenuSelection.OPTION_FONT_LARGE);
        item.addSelectionListener(listener);

        miOverstrike = new MenuItem(menu, SWT.CHECK);
        miOverstrike.setText("Overstrike Mode\tIns");
        miOverstrike.setAccelerator(SWT.INSERT);
        miOverstrike.setData(OptionMenuSelection.OPTION_OVERSTRIKE);
        miOverstrike.addSelectionListener(listener);
    }

    /**
     * Creates the 'execute' menu.
     * 
     * @param bar
     *            the main menu
     */
    private void createExecuteMenu(Menu bar) {
        /* the selection listener */
        ExecuteMenuSelectionListener listener = new ExecuteMenuSelectionListener();
        MenuItem item = new MenuItem(bar, SWT.CASCADE);
        item.setText("Execute");
        Menu menu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(menu);

        miRunScript = new MenuItem(menu, SWT.PUSH);
        miRunScript.setText("Run");
        miRunScript.setData(ExecuteMenuSelection.EXECUTE_RUN);
        miRunScript.addSelectionListener(listener);

        showOutput = new MenuItem(menu, SWT.CHECK);
        showOutput.setText("Show Output");
        showOutput.setSelection(outputState);
        showOutput.setData(ExecuteMenuSelection.EXECUTE_SHOW_OUTPUT);
        showOutput.addSelectionListener(listener);

        new MenuItem(menu, SWT.SEPARATOR);

        miContinue = new MenuItem(menu, SWT.PUSH);
        miContinue.setText("Continue");
        miContinue.setData(ExecuteMenuSelection.EXECUTE_CONTINUE);
        miContinue.addSelectionListener(listener);

        miSkipWait = new MenuItem(menu, SWT.PUSH);
        miSkipWait.setText("Skip Wait");
        miSkipWait.setData(ExecuteMenuSelection.EXECUTE_SKIP_WAIT);
        miSkipWait.addSelectionListener(listener);

        miCancel = new MenuItem(menu, SWT.PUSH);
        miCancel.setText("Cancel");
        miCancel.setData(ExecuteMenuSelection.EXECUTE_CANCEL);
        miCancel.addSelectionListener(listener);
        setExecuteInterruptStates(false, false, false);
    }

    /**
     * Create the help menu.
     * 
     * @param bar
     *            the main menu
     */
    private void createHelpMenu(Menu bar) {
        MenuItem item = new MenuItem(bar, SWT.CASCADE);
        item.setText("Help");
        Menu menu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(menu);

        item = new MenuItem(menu, SWT.PUSH);
        item.setData(EnumHelpTypes.BASIC);
        item.setText(EnumHelpTypes.BASIC.getName());
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleHelpRequest(e);
            }
        });

        item = new MenuItem(menu, SWT.PUSH);
        item.setData(EnumHelpTypes.ADVANCED);
        item.setText(EnumHelpTypes.ADVANCED.getName());
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleHelpRequest(e);
            }
        });
    }

    private void setExecuteInterruptStates(boolean cont, boolean skipWait,
            boolean canx) {
        if (!miContinue.isDisposed()) {
            miContinue.setEnabled(cont);
        }
        if (!miSkipWait.isDisposed()) {
            miSkipWait.setEnabled(skipWait);
        }
        if (!miCancel.isDisposed()) {
            miCancel.setEnabled(canx);
        }
    }

    /* menu call backs */

    /* file menu call backs */
    /**
     * Provides actions for printing editor contents to system printer.
     */
    private void handleFilePrint() {
        if (scriptEditor.getText().isEmpty()) {
            return;
        }
        scriptEditor.print();
    }

    /**
     * Provides actions for deleting an existing script file.
     */
    private void handleFileDelete() {
        FileDialog fd = new FileDialog(shell, SWT.OPEN);
        fd.setText("Delete Script File");
        fd.setFilterExtensions(SCRIPT_EXTNS);
        String result = fd.open();
        if (result == null) {
            return;
        }
        File file = new File(result);
        try {
            if (!file.delete()) {
                statusHandler.handle(Priority.PROBLEM, "Unable to delete \""
                                + result + "\"");
            }
        } catch (SecurityException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to delete \"" + result
                            + "\"", e);

        }
    }

    /**
     * Provides actions for renaming an existing script file.
     */
    private void handleFileRename() {
        // 1) get the name/location of the file to rename
        FileDialog fd = new FileDialog(shell, SWT.OPEN);
        fd.setText("Rename Script File");
        fd.setFilterExtensions(SCRIPT_EXTNS);
        if (!"".equals(filePath)) {
            fd.setFilterPath(filePath);
        }
        String result = fd.open();
        if (result == null) {
            return;
        }
        String fileToRename = result;

        // 2) get the new name for the file
        fd = new FileDialog(shell, SWT.SAVE);
        if (!"".equals(filePath)) {
            fd.setFilterPath(filePath);
        }
        fd.setFilterExtensions(SCRIPT_EXTNS);
        fd.setText("Rename Script File to");
        result = fd.open();
        if (result == null) {
            return;
        }
        String newFileName = result;
        // 3) rename the file
        try {
            File file = new File(fileToRename);
            File dest = new File(newFileName);
            if (!file.renameTo(dest)) {
                statusHandler.handle(Priority.PROBLEM,
                                "Unable to rename script file \""
                                        + fileToRename + "\"");
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to rename script file \""
                            + fileToRename + "\"", e);
        }
    }

    /**
     * Provides actions needed to save editor contents to a file.
     * 
     * @param saveAs
     *            flag to indicate if this is a 'save' or 'save as' action
     */
    private void handleFileSave(boolean saveAs) {
        if (scriptEditor.getText().isEmpty()) {
            return;
        }
        if (saveAs || FILE_DEFAULT.equalsIgnoreCase(this.fileName)) {
            FileDialog fd = new FileDialog(shell, SWT.SAVE);
            if (!"".equals(filePath)) {
                fd.setFilterPath(filePath);
            }
            fd.setOverwrite(true);
            fd.setFilterExtensions(SCRIPT_EXTNS);
            fd.setText("Save Script File" + (saveAs ? " As" : ""));
            String selected = fd.open();
            if (selected != null) {
                filePath = fd.getFilterPath();
                fileName = fd.getFileName();
            } else {
                return;
            }
        }
        try {
            saveFile(filePath, fileName, scriptEditor.getText());
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to save script file \""
                            + fileName + "\"", e);
            return;
        }
        setTitle();
        scriptEditorDirty = false;
    }

    /**
     * Provides actions needed by the File-->New menu item.
     */
    private void handleFileNew() {
        dirtyFileHelper("before deleting");
        this.scriptEditor.setText("");
        this.fileName = FILE_DEFAULT;
        this.filePath = "";
        scriptEditorDirty = false;
        setTitle();
    }

    /**
     * Provides actions needed to open a new script.
     */
    private void handleFileOpen() {
        dirtyFileHelper("before opening new script");
        FileDialog fd = new FileDialog(shell, SWT.OPEN);
        fd.setText("Open Script File");
        fd.setFilterExtensions(SCRIPT_EXTNS);
        fd.setFilterNames(SCRIPT_NAMES);
        if (!"".equals(filePath)) {
            fd.setFilterPath(filePath);
        }
        String result = fd.open();
        if (result == null) {
            return;
        }
        fileName = fd.getFileName();
        filePath = fd.getFilterPath();
        if (!loadFile(filePath, fileName)) {
            return;
        }
        scriptEditorDirty = false;
        setTitle();
    }

    /**
     * Helper method providing a standard check for a dirty file. A file is
     * dirty if it hasn't been saved since it was last modified. This method
     * manages the user prompt and the file save.
     * 
     * @param condition
     *            additional text for the input dialog
     */
    private void dirtyFileHelper(String condition) {
        boolean newFile = FILE_DEFAULT.equalsIgnoreCase(this.fileName);
        if (scriptEditorDirty || (newFile && !scriptEditor.getText().isEmpty())) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.YES
                    | SWT.NO);
            mb.setText("File Open");
            mb.setMessage("Save the changes to document\n\"" + this.fileName
                    + "\" " + condition + "?");
            int response = mb.open();
            if (response == SWT.YES) {
                handleFileSave(newFile);
            }
        }
    }

    /**
     * Provides actions needed to close the dialog.
     */
    private void handleFileClose() {
        shell.dispose();
    }

    /**
     * Runs the script in the current editor window.
     * 
     * @param event
     *            the menu selection event
     */
    protected void handleRunScript() {
        miRunScript.setEnabled(false);
        // short circuit -- quit early if no script
        String script = scriptEditor.getText();
        if ("".equals(script)) {
            statusHandler.handle(Priority.PROBLEM,
                            "No script to execute (SCRP)\nEnter/load a script and try again");
            miRunScript.setEnabled(true);
            return;
        }
        // set interrupt flags
        setExecuteInterruptStates(true, true, true);
        try {
            String scriptPath = writeScriptToTempFile(script);
            observer.executeTextScript(scriptPath);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to execute script (SCRP)", e);
            setExecuteInterruptStates(false, false, false);
        } finally {
            // intentionally empty, for now
        }
    }

    /**
     * Provides the actions needed change font sizes.
     * 
     * @param size
     *            desired size
     */
    private void handleFontSizeChange(EnumFontSize size) {
        switch (size) {
        case SMALL:
            scriptEditor.setFont(smlFont);
            break;
        case MEDIUM:
            scriptEditor.setFont(medFont);
            break;
        case LARGE:
            scriptEditor.setFont(lrgFont);
            break;
        }
    }

    private void handleHelpRequest(SelectionEvent event) {
        Object obj = event.getSource();
        if (!(obj instanceof MenuItem)) {
            return;
        }
        Object data = ((MenuItem) obj).getData();
        HelpRequestDlg dlg = new HelpRequestDlg(shell, (EnumHelpTypes) data,
                token);
        dlg.open();
    }

    /**
     * Writes the script to a file.
     * 
     * @param script
     *            the script to write to the file
     * @return the path to the output file
     * 
     * @throws Exception
     *             if an error occurs
     */
    private String writeScriptToTempFile(String script) throws Exception {
        String path = System.getProperty("java.io.tmpdir") + File.separator
                + "temp-script-text" + token + ".py";
        File file = new File(path);
        FileUtilities.writeStringToFile(file, script);
        return path;
    }

    public void setScriptOutputState(boolean state) {
        if (showOutput != null && !showOutput.isDisposed()) {
            showOutput.setSelection(state);
        }
    }

    @Override
    public void scriptComplete() {
        setExecuteInterruptStates(false, false, false);
        if (!miRunScript.isDisposed()) {
            miRunScript.setEnabled(true);
        }
    }

    @Override
    public void setScriptControls(boolean cont, boolean skip) {
        if (miCancel.isDisposed()) {
            setExecuteInterruptStates(cont, skip, false);
        } else {
            setExecuteInterruptStates(cont, skip, miCancel.getEnabled());
        }
    }

    /* helpers to handle edit menu delete/restore sub menus. */
    /**
     * Performs the single character delete/undelete. Pass in {@code true} to
     * delete. {@code false} to undelete.
     * 
     * @param delete
     *            operation flag
     */
    private void handleCharacterEdit(boolean delete) {
        int offset = scriptEditor.getCaretOffset();
        int length = scriptEditor.getCharCount();
        if (delete) {
            if (offset < length) {
                miUndelCharacter.setEnabled(delete);
                deletedChar = scriptEditor.getTextRange(offset, 1);
                scriptEditor.replaceTextRange(offset, 1, "");
            }
        } else {
            miUndelCharacter.setEnabled(delete);
            if (!overwriteMode || offset == length) {
                scriptEditor.insert(deletedChar);
            } else {
                scriptEditor.replaceTextRange(offset, 1, deletedChar);
            }
        }
    }

    /**
     * Performs the single word delete/undelete. Pass in {@code true} to delete.
     * {@code false} to undelete.
     * 
     * @param delete
     *            operation flag
     */
    private void handleWordEdit(boolean delete) {
        int offset = scriptEditor.getCaretOffset();
        int length = scriptEditor.getCharCount();
        if (delete) {
            if (offset < length) {
                scriptEditor.invokeAction(SWT.CTRL | SWT.SHIFT
                        | SWT.ARROW_RIGHT);
                deletedWord = scriptEditor.getSelectionText();
                scriptEditor.invokeAction(SWT.DEL);
                miUndelWord.setEnabled(delete);
            }
        } else {
            if (!overwriteMode || offset == length) {
                scriptEditor.insert(deletedWord);
            } else {
                scriptEditor.replaceTextRange(offset, deletedWord.length(),
                        deletedWord);
            }
            miUndelWord.setEnabled(delete);
        }

    }

    /**
     * Performs the single line delete/undelete. Pass in {@code true} to delete.
     * {@code false} to undelete.
     * 
     * @param delete
     *            operation flag
     */
    private void handleLineEdit(boolean delete) {
        int offset = scriptEditor.getCaretOffset();
        int length = scriptEditor.getCharCount();
        if (delete) {
            if (offset < length) {
                scriptEditor.invokeAction(SWT.SHIFT | SWT.END);
                deletedLine = scriptEditor.getSelectionText();
                scriptEditor.invokeAction(SWT.DEL);
                miUndelLine.setEnabled(delete);
            }
        } else {
            if (!overwriteMode || offset == length) {
                scriptEditor.insert(deletedLine);
            } else {
                scriptEditor.replaceTextRange(offset, deletedLine.length(),
                        deletedLine);
            }
            miUndelLine.setEnabled(delete);
        }
    }

    /**
     * An enumeration of the functions provided by the edit menu.
     */
    private enum EditMenuSelection {
        NONE, CUT, COPY, PASTE, SEL_PREVIOUS_WORD, SEL_NEXT_WORD, SEL_BEGINNING_LINE, SEL_END_LINE, SEL_PREVIOUS_PAGE, SEL_NEXT_PAGE, SEL_TOP_PRODUCT, SEL_END_PRODUCT, SEL_ALL, DEL_CHARACTER, DEL_WORD, DEL_LINE, UNDEL_CHARACTER, UNDEL_WORD, UNDEL_LINE, SEARCH;
    }

    /**
     * Provides a single selection listener for the edit menu.
     */
    private class EditMenuSelectionListener extends SelectionAdapter {
        @Override
        public void widgetSelected(SelectionEvent event) {
            Object obj = event.getSource();
            if (!(obj instanceof MenuItem)) {
                // should not happen
                return;
            }
            MenuItem item = (MenuItem) obj;
            obj = item.getData();
            if (!(obj instanceof EditMenuSelection)) {
                obj = EditMenuSelection.NONE;
            }
            handleEditMenu((EditMenuSelection) obj);
        }
    }

    /**
     * implements the top level logic for the edit menu.
     * 
     * @param selection
     *            identifies the menu selection
     */
    private void handleEditMenu(EditMenuSelection selection) {
        switch (selection) {
        case NONE:
            break;
        case CUT:
            this.scriptEditor.cut();
            break;
        case COPY:
            this.scriptEditor.copy();
            break;
        case PASTE:
            this.scriptEditor.paste();
            break;
        case SEL_PREVIOUS_WORD:
            scriptEditor.invokeAction(SWT.CTRL | SWT.SHIFT | SWT.ARROW_LEFT);
            break;
        case SEL_NEXT_WORD:
            scriptEditor.invokeAction(SWT.CTRL | SWT.SHIFT | SWT.ARROW_RIGHT);
            break;
        case SEL_BEGINNING_LINE:
            scriptEditor.invokeAction(SWT.SHIFT | SWT.HOME);
            break;
        case SEL_END_LINE:
            scriptEditor.invokeAction(SWT.SHIFT | SWT.END);
            break;
        case SEL_PREVIOUS_PAGE:
            scriptEditor.invokeAction(SWT.SHIFT | SWT.PAGE_UP);
            break;
        case SEL_NEXT_PAGE:
            scriptEditor.invokeAction(SWT.SHIFT | SWT.PAGE_DOWN);
            break;
        case SEL_TOP_PRODUCT:
            scriptEditor.invokeAction(SWT.CTRL | SWT.SHIFT | SWT.HOME);
            break;
        case SEL_END_PRODUCT:
            scriptEditor.invokeAction(SWT.CTRL | SWT.SHIFT | SWT.END);
            break;
        case SEL_ALL:
            this.scriptEditor.selectAll();
            break;
        case DEL_CHARACTER:
            handleCharacterEdit(true);
            break;
        case UNDEL_CHARACTER:
            handleCharacterEdit(false);
            break;
        case DEL_WORD:
            handleWordEdit(true);
            break;
        case UNDEL_WORD:
            handleWordEdit(false);
            break;
        case DEL_LINE:
            handleLineEdit(true);
            break;
        case UNDEL_LINE:
            handleLineEdit(false);
            break;
        case SEARCH:
            searchReplaceDlg = new SearchReplaceDlg(shell, scriptEditor, true);
            searchReplaceDlg.open();
            searchReplaceDlg = null;
            break;
        default:
            System.out.println("Option " + selection + " not yet implemented");
        }
    }

    /**
     * An enumeration of the functions provided by the file menu.
     */
    private enum FileMenuSelection {
        NONE, FILE_NEW, FILE_OPEN, FILE_SAVE, FILE_SAVE_AS, FILE_RENAME, FILE_DELETE, FILE_PRINT, FILE_CLOSE;
    }

    /**
     * Provides a single selection listener for the file menu.
     */
    private class FileMenuSelectionListener extends SelectionAdapter {
        @Override
        public void widgetSelected(SelectionEvent event) {
            Object obj = event.getSource();
            if (!(obj instanceof MenuItem)) {
                // should not happen
                return;
            }
            MenuItem item = (MenuItem) obj;
            obj = item.getData();
            if (!(obj instanceof FileMenuSelection)) {
                obj = FileMenuSelection.NONE;
            }
            handleFileMenu((FileMenuSelection) obj);
        }
    }

    /**
     * Implements the top level logic of the file menu.
     * 
     * @param selection
     *            identifies the menu selection
     */
    private void handleFileMenu(FileMenuSelection selection) {
        switch (selection) {
        case FILE_NEW:
            handleFileNew();
            break;
        case FILE_OPEN:
            handleFileOpen();
            break;
        case FILE_SAVE:
            handleFileSave(false);
            break;
        case FILE_SAVE_AS:
            handleFileSave(true);
            break;
        case FILE_RENAME:
            handleFileRename();
            break;
        case FILE_DELETE:
            handleFileDelete();
            break;
        case FILE_PRINT:
            handleFilePrint();
            break;
        case FILE_CLOSE:
            handleFileClose();
            break;
        default:
            System.out.println("Option " + selection + " not yet implemented");
        }
    }

    /**
     * An enumeration of the functions provided by the options menu.
     */
    private enum OptionMenuSelection {
        NONE, OPTION_FONT_SMALL, OPTION_FONT_MEDIUM, OPTION_FONT_LARGE, OPTION_OVERSTRIKE;
    }

    /**
     * Provides a single selection listener for the options menu.
     */
    private class OptionMenuSelectionListener extends SelectionAdapter {
        @Override
        public void widgetSelected(SelectionEvent event) {
            Object obj = event.getSource();
            if (!(obj instanceof MenuItem)) {
                // should not happen
                return;
            }
            MenuItem item = (MenuItem) obj;
            obj = item.getData();
            if (!(obj instanceof OptionMenuSelection)) {
                obj = OptionMenuSelection.NONE;
            }
            handleOptionMenu((OptionMenuSelection) obj);
        }
    }

    /**
     * Implements the top level logic of the option menu.
     * 
     * @param selection
     *            identifies the menu selection
     */
    private void handleOptionMenu(OptionMenuSelection selection) {
        switch (selection) {
        case OPTION_FONT_SMALL:
            handleFontSizeChange(EnumFontSize.SMALL);
            break;
        case OPTION_FONT_MEDIUM:
            handleFontSizeChange(EnumFontSize.MEDIUM);
            break;
        case OPTION_FONT_LARGE:
            handleFontSizeChange(EnumFontSize.LARGE);
            break;
        case OPTION_OVERSTRIKE:
            overwriteMode = miOverstrike.getSelection();
            break;
        default:
            System.out.println("Option " + selection + " not yet implemented");
        }
    }

    /**
     * An enumeration of the functions provided by the execute menu.
     */
    private enum ExecuteMenuSelection {
        NONE, EXECUTE_RUN, EXECUTE_SHOW_OUTPUT, EXECUTE_CONTINUE, EXECUTE_SKIP_WAIT, EXECUTE_CANCEL;
    }

    /**
     * Provides a single selection listener for the execute menu.
     */
    private class ExecuteMenuSelectionListener extends SelectionAdapter {
        public void widgetSelected(SelectionEvent event) {
            Object obj = event.getSource();
            if (!(obj instanceof MenuItem)) {
                // should not happen
                return;
            }
            MenuItem item = (MenuItem) obj;
            obj = item.getData();
            if (!(obj instanceof ExecuteMenuSelection)) {
                obj = ExecuteMenuSelection.NONE;
            }
            handleExecuteMenu((ExecuteMenuSelection) obj);
        }

    }

    /**
     * Implements the top level logic of the execute menu.
     * 
     * @param selection
     *            identifies the menu selection
     */
    private void handleExecuteMenu(ExecuteMenuSelection selection) {
        switch (selection) {
        case EXECUTE_RUN:
            handleRunScript();
            break;
        case EXECUTE_SHOW_OUTPUT:
            if (observer != null) {
                observer.manageScriptOutputWindow(showOutput.getSelection());
            }
            break;
        case EXECUTE_CONTINUE:
            if (observer != null) {
                observer.onContinueScript();
            }
            break;
        case EXECUTE_SKIP_WAIT:
            if (observer != null) {
                observer.onSkipWait();
            }
            break;
        case EXECUTE_CANCEL:
            if (observer != null) {
                observer.onCancelScript();
            }
            break;
        default:
            System.out.println("Option " + selection + " not yet implemented");
        }

    }
}
