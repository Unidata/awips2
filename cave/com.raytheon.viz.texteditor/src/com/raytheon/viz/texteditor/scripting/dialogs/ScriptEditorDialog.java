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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.texteditor.Activator;
import com.raytheon.viz.texteditor.dialogs.SearchReplaceDlg;
import com.raytheon.viz.texteditor.dialogs.TextWSMessageBox;
import com.raytheon.viz.texteditor.scripting.dialogs.HelpRequestDlg.EnumHelpTypes;
import com.raytheon.viz.texteditor.scripting.runner.ITextWsScriptController;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Main dialog for the Text WS Script Editor. Will always be launched from the
 * Text Editor Window.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 29, 2009           mfegan    Initial creation
 * Sep 25, 2012  1196     lvenable  Refactor dialogs to prevent blocking.
 * Sep 26, 2012  1196     lvenable  Dialog refactor to not block.
 * Feb 26, 2019  7746     randerso  Change to use Path instead of String for
 *                                  TextWS script path.
 * Mar 04, 2019  7601     tgurney   Only allow running script if script is not
 *                                  already running. Cleanup.
 * Oct 28, 2019  7601     tgurney   Fix Run menu item tooltip
 *
 * </pre>
 *
 * @author mfegan
 */

public class ScriptEditorDialog extends CaveSWTDialog {

    private static final int SMALL_FONT_SIZE = 10;

    private static final int MEDIUM_FONT_SIZE = 12;

    private static final int LARGE_FONT_SIZE = 16;

    /**
     * array of script extension information
     */
    public static final String[] SCRIPT_EXTNS = { "[^\\.]*", "*.py", "*.tcl",
    "*.txt" };

    /**
     * array of script names
     */
    public static final String[] SCRIPT_NAMES = { "All Scripts",
            "Python Scripts", "TCL Scripts", "Text Files" };

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScriptEditorDialog.class);

    /* strings for dialog title creation */
    private static final String FILE_DEFAULT = "untitled";

    private static final String FMT_TITLE = "Text %s Script Editor (%s%s)";

    @SuppressWarnings("unused")
    private static final String HELLO_WORLD = "repeat(5,\"print 'Hello World!'\")\n"
            + "wait()\n" + "print 'After Loop!'";

    /** the observer to pass to results from a running script */
    private final IScriptEditorObserver observer;

    /** the text editor ID token */
    private final String token;

    /** the fully qualified path of the script */
    private Path scriptPath;

    /** flag to indicate if an initial script is to be loaded */
    private boolean loadScript = false;

    /** the script editor box */
    private StyledText scriptEditor = null;

    /** editor "dirty" flag */
    private boolean scriptEditorDirty = false;

    /** the 'run' menu item */
    private MenuItem miRunScript;

    /** the 'continue/skip wait' menu item */
    private MenuItem miContinue;

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

    private Font font = null;

    private ITextWsScriptController runningScript;

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
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.NO_PACK | CAVE.DO_NOT_BLOCK);
        this.observer = observer;
        this.token = token;
        this.outputState = outputState;

        this.scriptPath = Activator.getDefault().getMostRecentDir()
                .resolve(FILE_DEFAULT);

        updateTitle();
    }

    /**
     * Sets the dialog title based on the Editor ID token and the currently
     * displayed script filename.
     */
    private void updateTitle() {
        setText(String.format(FMT_TITLE, token, scriptEditorDirty ? "*" : "",
                scriptPath.getFileName()));
    }

    private void setFontSize(int pt) {
        Font oldFont = font;
        font = new Font(shell.getDisplay(), "Monospace", pt, SWT.NORMAL);
        scriptEditor.setFont(font);
        if (oldFont != null) {
            oldFont.dispose();
        }
    }

    private boolean loadFile(Path path) {
        try {
            String contents = new String(Files.readAllBytes(path));
            scriptEditor.setText(contents);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to load file \"" + path + "\"", e);
            return false;
        }
        return true;
    }

    private void saveFile(Path path, String contents) throws IOException {
        Files.write(path, contents.getBytes());
    }

    @Override
    protected Layout constructShellLayout() {
        return new FillLayout();
    }

    @Override
    protected void disposed() {
        promptToSave("before closing");
        if (font != null) {
            font.dispose();
        }
        if (observer != null) {
            observer.scriptEditorClosed();
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Point size = new Point(500, 500);
        shell.setSize(size);
        shell.layout();

        createMenuBar();
        createClientArea();
        setFontSize(MEDIUM_FONT_SIZE);
        updateRunScriptEnabled();
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
        scriptEditor = new StyledText(comp,
                SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
        scriptEditor.setLayoutData(gd);

        scriptEditor.addVerifyKeyListener(event -> {
            if (event.keyCode == SWT.DEL || event.character == SWT.BS
                    || event.keyCode == SWT.SHIFT) {
                // need to capture the Delete, Backspace and Shift keys
            } else if (event.keyCode > 500) {
                // capture non-alphanumeric editing keys
            } else if (overwriteMode && scriptEditor
                    .getCaretOffset() < scriptEditor.getCharCount()) {
                scriptEditor.replaceTextRange(scriptEditor.getCaretOffset(),
                        1, String.valueOf(event.character));
                scriptEditor
                .setCaretOffset(scriptEditor.getCaretOffset() + 1);
                event.doit = false;
            }

        });
        scriptEditor.addModifyListener(e -> setDirty(true));

        if (loadScript) {
            loadFile(this.scriptPath);
        } else {
            scriptEditor.setText("");
        }
        setDirty(false);
    }

    private void setDirty(boolean dirty) {
        scriptEditorDirty = dirty;
        updateTitle();
        updateRunScriptEnabled();
    }

    private void updateRunScriptEnabled() {
        boolean enabled = true;
        String toolTipText = null;
        if (scriptEditorDirty || getText().endsWith("(" + FILE_DEFAULT + ")")) {
            toolTipText = "Script must be saved before running";
            enabled = false;
        } else if (runningScript != null) {
            toolTipText = "Script is already running";
            enabled = false;
        }
        miRunScript.setEnabled(enabled);
        miRunScript.setToolTipText(toolTipText);
    }

    /**
     * creates the file menu
     *
     * @param bar
     *            the main menu bar
     */
    private void createFileMenu(Menu bar) {
        /* the selection listener */
        MenuItem item = new MenuItem(bar, SWT.CASCADE);
        item.setText("File");
        Menu menu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(menu);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("New\tCtrl+T");
        item.setAccelerator(SWT.CTRL + 'T');
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleFileNew();
            }
        });

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Open ...\tCtrl+O");
        item.setAccelerator(SWT.CTRL + 'O');
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleFileOpen();
            }
        });

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Save\tCtrl+S");
        item.setAccelerator(SWT.CTRL + 'S');
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleFileSave(false);
            }
        });

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Save As ...\tF3");
        item.setAccelerator(SWT.F3);
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleFileSave(true);
            }
        });

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Rename ...");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleFileRename();
            }
        });

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Delete ...");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleFileDelete();
            }
        });

        new MenuItem(menu, SWT.SEPARATOR);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Print\tCtrl+P");
        item.setAccelerator(SWT.CTRL + 'P');
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (!scriptEditor.getText().isEmpty()) {
                    scriptEditor.print();
                }
            }
        });

        new MenuItem(menu, SWT.SEPARATOR);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Close\tAlt+F4");
        item.setAccelerator(SWT.ALT + SWT.F4);
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

    }

    /**
     * creates the Edit drop down on the main menu
     *
     * @param bar
     *            the main menu bar
     */
    private void createEditMenu(Menu bar) {
        /* create the Edit drop down */
        MenuItem item = new MenuItem(bar, SWT.CASCADE);
        item.setText("Edit");
        Menu menu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(menu);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Cut\tCrtl+X");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor.cut();
            }
        });

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Copy\tCtrl+C");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor.copy();
            }
        });

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Paste\tCtrl+V");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor.paste();
            }
        });

        new MenuItem(menu, SWT.SEPARATOR);

        /* create the 'select' sub-menu */
        item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Select");
        Menu subMenu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(subMenu);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To previous word\tCtrl+Shift+Left");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor
                .invokeAction(SWT.CTRL | SWT.SHIFT | SWT.ARROW_LEFT);
            }
        });

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To next word\tCtrl+Shift+Right");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor
                .invokeAction(SWT.CTRL | SWT.SHIFT | SWT.ARROW_RIGHT);
            }
        });

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To beginning of line\tShift+Home");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor.invokeAction(SWT.SHIFT | SWT.HOME);
            }
        });

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To end of line\tShift+End");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor.invokeAction(SWT.SHIFT | SWT.END);
            }
        });

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To previous page\tShift+Page Up");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor.invokeAction(SWT.SHIFT | SWT.PAGE_UP);
            }
        });

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To next page\tShift+Page Down");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor.invokeAction(SWT.SHIFT | SWT.PAGE_DOWN);
            }
        });

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To top of Product\tCtrl+Shift+Home");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor.invokeAction(SWT.CTRL | SWT.SHIFT | SWT.HOME);
            }
        });

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("To end of Product\tCtrl+Shift+End");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor.invokeAction(SWT.CTRL | SWT.SHIFT | SWT.END);
            }
        });

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("All\tCtrl+A");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scriptEditor.selectAll();
            }
        });

        /* create the 'delete' sub-menu */
        item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Delete");
        subMenu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(subMenu);

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("Character\tF6");
        item.setAccelerator(SWT.F6);
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleCharacterEdit(true);
            }
        });

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("Word\tF7");
        item.setAccelerator(SWT.F7);
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleWordEdit(true);
            }
        });

        item = new MenuItem(subMenu, SWT.PUSH);
        item.setText("Line\tF8");
        item.setAccelerator(SWT.F8);
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleLineEdit(true);
            }
        });

        /* create the 'undelete' sub-menu */
        item = new MenuItem(menu, SWT.CASCADE);
        item.setText("Undelete");
        subMenu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(subMenu);

        miUndelCharacter = new MenuItem(subMenu, SWT.PUSH);
        miUndelCharacter.setText("Character\tShift+F6");
        miUndelCharacter.setAccelerator(SWT.SHIFT + SWT.F6);
        miUndelCharacter.setEnabled(false);
        miUndelCharacter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleCharacterEdit(false);
            }
        });

        miUndelWord = new MenuItem(subMenu, SWT.PUSH);
        miUndelWord.setText("Word\tShift+F7");
        miUndelWord.setAccelerator(SWT.SHIFT + SWT.F7);
        miUndelWord.setEnabled(false);
        miUndelWord.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleWordEdit(false);
            }
        });

        miUndelLine = new MenuItem(subMenu, SWT.PUSH);
        miUndelLine.setText("Line\tShift+F8");
        miUndelLine.setAccelerator(SWT.SHIFT + SWT.F8);
        miUndelLine.setEnabled(false);
        miUndelLine.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleLineEdit(false);
            }
        });

        new MenuItem(menu, SWT.SEPARATOR);

        item = new MenuItem(menu, SWT.PUSH);
        item.setText("Search ...\tF2");
        item.setAccelerator(SWT.F2);
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (searchReplaceDlg == null || searchReplaceDlg.isDisposed()) {
                    searchReplaceDlg = new SearchReplaceDlg(shell, scriptEditor,
                            true);
                    searchReplaceDlg.open();
                } else {
                    searchReplaceDlg.bringToTop();
                }
            }
        });

    }

    /**
     * Sets up the options menu.
     *
     * @param bar
     *            the main menu
     */
    private void createOptionsMenu(Menu bar) {
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
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setFontSize(SMALL_FONT_SIZE);
            }
        });

        item = new MenuItem(subMenu, SWT.RADIO);
        item.setText("Medium");
        item.setSelection(true);
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setFontSize(MEDIUM_FONT_SIZE);
            }
        });

        item = new MenuItem(subMenu, SWT.RADIO);
        item.setText("Large");
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setFontSize(LARGE_FONT_SIZE);
            }
        });

        miOverstrike = new MenuItem(menu, SWT.CHECK);
        miOverstrike.setText("Overstrike Mode\tIns");
        miOverstrike.setAccelerator(SWT.INSERT);
        miOverstrike.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                overwriteMode = miOverstrike.getSelection();
            }
        });
    }

    /**
     * Creates the 'execute' menu.
     *
     * @param bar
     *            the main menu
     */
    private void createExecuteMenu(Menu bar) {
        MenuItem item = new MenuItem(bar, SWT.CASCADE);
        item.setText("Execute");
        Menu menu = new Menu(shell, SWT.DROP_DOWN);
        item.setMenu(menu);

        miRunScript = new MenuItem(menu, SWT.PUSH);
        miRunScript.setText("Run");
        miRunScript.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                runningScript = observer.executeTextScript(scriptPath);
            }
        });

        showOutput = new MenuItem(menu, SWT.CHECK);
        showOutput.setText("Show Output");
        showOutput.setSelection(outputState);
        showOutput.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (observer != null) {
                    observer.setShowScriptOutput(showOutput.getSelection());
                }
            }
        });

        new MenuItem(menu, SWT.SEPARATOR);

        miContinue = new MenuItem(menu, SWT.PUSH);
        miContinue.setText("Continue/Skip Wait");
        miContinue.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (runningScript != null) {
                    runningScript.doContinue();
                }
            }
        });
        miContinue.setEnabled(false);

        miCancel = new MenuItem(menu, SWT.PUSH);
        miCancel.setText("Cancel");
        miCancel.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (runningScript != null) {
                    runningScript.cancel();
                }
            }
        });
        miCancel.setEnabled(false);
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

    public void setContinueEnabled(boolean enabled) {
        if (!shell.isDisposed()) {
            miContinue.setEnabled(enabled);
        }
    }

    /**
     * Provides actions for deleting an existing script file.
     */
    private void handleFileDelete() {
        FileDialog fd = new FileDialog(shell, SWT.OPEN);
        fd.setText("Delete Script File");
        fd.setFilterExtensions(SCRIPT_EXTNS);
        fd.setFilterPath(Activator.getDefault().getMostRecentDir().toString());
        String result = fd.open();
        if (result == null) {
            return;
        }
        Path path = Paths.get(result);
        try {
            Files.delete(path);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to delete \"" + path + "\"", e);

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
        fd.setFilterPath(Activator.getDefault().getMostRecentDir().toString());
        fd.setFilterPath(scriptPath.getParent().toString());
        String result = fd.open();
        if (result == null) {
            return;
        }
        Path source = Paths.get(result);

        // 2) get the new name for the file
        fd = new FileDialog(shell, SWT.SAVE);
        fd.setFilterPath(scriptPath.getParent().toString());
        fd.setFilterExtensions(SCRIPT_EXTNS);
        fd.setText("Rename Script File to");
        fd.setFilterPath(source.getParent().toString());
        result = fd.open();
        if (result == null) {
            return;
        }
        Path target = Paths.get(result);
        // 3) rename the file
        try {
            Files.move(source, target, StandardCopyOption.REPLACE_EXISTING);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to rename script file \"" + source + "\"", e);
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
        Path dest;
        if (saveAs || FILE_DEFAULT
                .equalsIgnoreCase(scriptPath.getFileName().toString())) {
            FileDialog fd = new FileDialog(shell, SWT.SAVE);
            if (scriptPath == null) {
                fd.setFilterPath(
                        Activator.getDefault().getMostRecentDir().toString());
            } else {
                fd.setFilterPath(scriptPath.getParent().toString());
            }
            fd.setOverwrite(true);
            fd.setFilterExtensions(SCRIPT_EXTNS);
            fd.setText("Save Script File" + (saveAs ? " As" : ""));
            String selected = fd.open();
            if (selected == null) {
                return;
            }
            dest = Paths.get(selected);
        } else {
            dest = scriptPath;
        }

        try {
            saveFile(dest, scriptEditor.getText());

            scriptPath = dest;
            Activator.getDefault().saveMostRecentDir(scriptPath);

            updateTitle();
            setDirty(false);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to save script file \"" + dest + "\"", e);
        }
    }

    /**
     * Provides actions needed by the File-->New menu item.
     */
    private void handleFileNew() {
        promptToSave("before deleting");

        scriptPath = Activator.getDefault().getMostRecentDir()
                .resolve(FILE_DEFAULT);
        setDirty(false);
        if (!scriptEditor.isDisposed()) {
            this.scriptEditor.setText("");
            updateTitle();
        }
    }

    /**
     * Provides actions needed to open a new script.
     */
    private void handleFileOpen() {
        promptToSave("before opening new script");
        if (shell.isDisposed()) {
            return;
        }
        FileDialog fd = new FileDialog(shell, SWT.OPEN);
        fd.setText("Open Script File");
        fd.setFilterExtensions(SCRIPT_EXTNS);
        fd.setFilterNames(SCRIPT_NAMES);
        fd.setFilterPath(Activator.getDefault().getMostRecentDir().toString());
        String result = fd.open();
        if (result == null) {
            return;
        }

        scriptPath = Paths.get(result);
        Activator.getDefault().saveMostRecentDir(scriptPath);

        if (!loadFile(scriptPath)) {
            return;
        }
        setDirty(false);
        updateTitle();
    }

    /**
     * Open dialog asking to save the file if it is dirty. Save the file if the
     * user says yes.
     *
     * @param extraDialogText
     *            additional text for the input dialog
     */
    private void promptToSave(String extraDialogText) {
        String fileName = this.scriptPath.getFileName().toString();
        boolean newFile = FILE_DEFAULT.equalsIgnoreCase(fileName);
        if (scriptEditorDirty
                || newFile && !scriptEditor.getText().isEmpty()) {
            int response = TextWSMessageBox.open(shell, "File Open",
                    "Save the changes to document\n\"" + fileName + "\" "
                            + extraDialogText + "?",
                            SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            if (response == SWT.YES) {
                handleFileSave(newFile);
            }
        }
    }

    private void handleHelpRequest(SelectionEvent event) {
        Object obj = event.getSource();
        if (obj instanceof MenuItem) {
            Object data = ((MenuItem) obj).getData();
            HelpRequestDlg dlg = new HelpRequestDlg(shell, (EnumHelpTypes) data,
                    token);
            dlg.open();
        }
    }

    public void setScriptOutputState(boolean state) {
        if (showOutput != null && !showOutput.isDisposed()) {
            showOutput.setSelection(state);
        }
    }

    public void scriptStarted() {
        if (!shell.isDisposed()) {
            updateRunScriptEnabled();
            miCancel.setEnabled(true);
        }
    }

    public void scriptComplete() {
        runningScript = null;
        if (!shell.isDisposed()) {
            updateRunScriptEnabled();
            miCancel.setEnabled(false);
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
                scriptEditor
                .invokeAction(SWT.CTRL | SWT.SHIFT | SWT.ARROW_RIGHT);
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
}
