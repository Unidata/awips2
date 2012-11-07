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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ExtendedModifyEvent;
import org.eclipse.swt.custom.ExtendedModifyListener;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * A simple text Editor dialog allowing the user to modify a localized file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation
 *  9 JUL 2010  5078       rferrel     Handle File Not Found.
 *  7 DEC 2010  7621       rferrel     Modified constructor to take a
 *                                     LocalizedFile and added preOpen() to
 *                                     open the LocalizedFile.
 * 11 OCT 2012  1229       rferrel     Changes for non-blocking FindReplaceDlg.
 * 15 OCT 2012  1229       rferrel     Made dialog non-blocking.
 * 15 OCT 2012  1229       rferrel     Changes for non-blocking HelpUsageDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TextEditorSetupDlg extends CaveSWTDialog {

    /**
     * Composite containing message status controls.
     */
    private MessageStatusComp msgStatusComp;

    /**
     * Label containing the file and path of the open configuration file.
     */
    private Label filePathLbl;

    /**
     * Background color of the style text control.
     */
    private Color styledTextBackgroundColor;

    /**
     * Font for the styled text control.
     */
    private Font textFont;

    /**
     * Styled text editor control.
     */
    private StyledText editorStTxt;

    /**
     * Insert button.
     */
    private Button insertBtn;

    /**
     * Wrap button.
     */
    private Button wrapBtn;

    /**
     * TAF template flag.
     */
    private boolean tafTemplate;

    private LocalizationFile openedFile;

    /**
     * Popup menu for cut, copy, paste, undo, and redo.
     */
    private Menu popupMenu;

    private ArrayList<HashMap<String, Object>> undoStack;

    private ArrayList<HashMap<String, Object>> redoStack;

    private final int UNDO_STACK_SIZE = 10;

    public boolean modifyFlag = true;

    private String template;

    private FindReplaceDlg findDlg;

    private OpenDlg openDlg;

    private HelpUsageDlg usageDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public TextEditorSetupDlg(Shell parent) {
        this(parent, null);
    }

    /**
     * Construction.
     * 
     * @param parent
     *            Parent shell
     * @param lFile
     *            When not null a localized file to load into the editor.
     */
    public TextEditorSetupDlg(Shell parent, LocalizationFile lFile) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Text Editor");

        this.template = null;
        this.openedFile = lFile;
        tafTemplate = false;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        mainLayout.verticalSpacing = 5;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        styledTextBackgroundColor.dispose();
        textFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all of the controls and layouts
        textFont = new Font(getDisplay(), "Monospace", 10, SWT.NORMAL);
        styledTextBackgroundColor = new Color(getDisplay(), 82, 107, 129);

        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
        createMenus();

        createTopControls();

        createTextControl();

        createBottomMessageControls();

        if (template != null) {
            editorStTxt.setText(template);
        }
    }

    /**
     * Create the menus at the top of the display.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createEditMenu(menuBar);
        createHelpMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the File menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createFileMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // -------------------------------------------------
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        // Print menu item
        MenuItem printMI = new MenuItem(fileMenu, SWT.NONE);
        printMI.setText("&Print");
        printMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PrintDialog pd = new PrintDialog(shell);
                pd.open();
            }
        });

        // Separator bar
        new MenuItem(fileMenu, SWT.SEPARATOR);

        // Close menu item
        MenuItem closeMI = new MenuItem(fileMenu, SWT.NONE);
        closeMI.setText("&Close");
        closeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Create the Edit menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createEditMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem editMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        editMenuItem.setText("&Edit");

        // Create the File menu item with a File "dropdown" menu
        Menu editMenu = new Menu(menuBar);
        editMenuItem.setMenu(editMenu);

        // -------------------------------------------------
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        // Cut menu item
        MenuItem cutMI = new MenuItem(editMenu, SWT.NONE);
        cutMI.setText("&Cut");
        cutMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                editorStTxt.cut();
            }
        });

        // Copy menu item
        MenuItem copyMI = new MenuItem(editMenu, SWT.NONE);
        copyMI.setText("C&opy");
        copyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                editorStTxt.copy();
            }
        });

        // Paste menu item
        MenuItem pasteMI = new MenuItem(editMenu, SWT.NONE);
        pasteMI.setText("&Paste");
        pasteMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                editorStTxt.paste();
            }
        });

        // Find menu item
        MenuItem findMI = new MenuItem(editMenu, SWT.NONE);
        findMI.setText("&Find...");
        findMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Use the AvnFPS Find/Replace dialog
                if (mustCreate(findDlg)) {
                    findDlg = new FindReplaceDlg(shell, editorStTxt);
                    findDlg.open();
                } else {
                    findDlg.bringToTop();
                }
            }
        });

        // Undo menu item
        MenuItem undoMI = new MenuItem(editMenu, SWT.NONE);
        undoMI.setText("&Undo");
        undoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            }
        });

        // Redo menu item
        MenuItem redoMI = new MenuItem(editMenu, SWT.NONE);
        redoMI.setText("&Redo");
        redoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            }
        });
    }

    /**
     * Create the Help menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createHelpMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Help menu
        // -------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        // Create the File menu item with a File "dropdown" menu
        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        // -------------------------------------------------
        // Create all the items in the Help dropdown menu
        // -------------------------------------------------

        // About menu item
        MenuItem keyBindingsMI = new MenuItem(helpMenu, SWT.NONE);
        keyBindingsMI.setText("&Key Bindings...");
        keyBindingsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            }
        });

        // Usage menu item
        MenuItem usageMI = new MenuItem(helpMenu, SWT.NONE);
        usageMI.setText("&Usage...");
        usageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String description = "Text Editor Help";
                    String helpText = "This is a generic text editor. Edit any text file via the file\n"
                            + "selection dialog that is invoked by pressing the 'Open' button.\n"
                            + "\n"
                            + "Menu items:\n"
                            + "   File:\n"
                            + "        Print invokes print dialog\n"
                            + "   Edit:\n"
                            + "        provides the usual editing functions (i.e. Cut, Copy, Paste,\n"
                            + "        and Find/Replace). The menu can be also invoked by pressing\n"
                            + "        right mouse button within the text window area.\n"
                            + "\n"
                            + "Buttons:\n"
                            + "   Clear: clears text window.\n"
                            + "   Open:  invokes file selection dialog\n"
                            + "   Save:  saves content of the text window\n"
                            + "   Save as:  invokes file selection dialog\n"
                            + "\n"
                            + "Toggles:\n"
                            + "   Insert: toggles insert/overwrite mode\n"
                            + "   Wrap:   toggles word wrap\n"
                            + "\n"
                            + "The currently loaded (if any) file name is displayed in a label above the\n"
                            + "text window.";
                    usageDlg = new HelpUsageDlg(shell, description, helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the top control buttons.
     */
    private void createTopControls() {
        // ------------------------------------------
        // Create the composite for the controls.
        // ------------------------------------------
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        controlComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        controlComp.setLayoutData(gd);

        // -------------------------------------------
        // Create a button composite for the buttons
        // -------------------------------------------
        Composite buttonComp = new Composite(controlComp, SWT.NONE);
        gl = new GridLayout(4, true);
        buttonComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        int buttonWidth = 120;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button clearBtn = new Button(buttonComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setToolTipText("Clear text area");
        clearBtn.setLayoutData(gd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                editorStTxt.setText("");
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button openBtn = new Button(buttonComp, SWT.PUSH);
        openBtn.setText("Open");
        openBtn.setToolTipText("Open file");
        openBtn.setLayoutData(gd);
        openBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openFile();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setToolTipText("Save file with current name");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (tafTemplate) {
                    String template = editorStTxt.getText();
                    String siteId = template.substring(0, 4);
                    String startHour = template.substring(7, 9);
                    try {
                        ITafSiteConfig config = TafSiteConfigFactory
                                .getInstance();
                        config.saveTafTemplate(siteId, startHour, template);
                        msgStatusComp.setMessageText(
                                "File saved successfully.", new RGB(0, 255, 0));
                    } catch (FileNotFoundException e) {
                        msgStatusComp.setMessageText(e.getMessage(), new RGB(
                                255, 0, 0));
                    } catch (ConfigurationException e) {
                        msgStatusComp
                                .setMessageText(
                                        "An error occured when saving the TAF template.",
                                        new RGB(255, 0, 0));
                    } catch (LocalizationOpFailedException e) {
                        msgStatusComp
                                .setMessageText(
                                        "An error occured when saving the TAF template.",
                                        new RGB(255, 0, 0));
                    }
                } else {
                    saveFile();
                }
            }
        });

        // mmaron DR 2928
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button saveAsBtn = new Button(buttonComp, SWT.PUSH);
        saveAsBtn.setText("Save As");
        saveAsBtn.setToolTipText("Save file with new name");
        saveAsBtn.setLayoutData(gd);
        saveAsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveFileAs();
            }
        });

        // ----------------------------------------------------
        // Create a label & check composite for the buttons
        // ----------------------------------------------------
        Composite labelCheckComp = new Composite(controlComp, SWT.NONE);
        gl = new GridLayout(3, false);
        labelCheckComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        labelCheckComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        filePathLbl = new Label(labelCheckComp, SWT.BORDER);
        filePathLbl.setLayoutData(gd);

        insertBtn = new Button(labelCheckComp, SWT.CHECK);
        insertBtn.setText("Insert");
        insertBtn.setSelection(true);

        wrapBtn = new Button(labelCheckComp, SWT.CHECK);
        wrapBtn.setText("Wrap");
        wrapBtn.setSelection(true);
    }

    /**
     * Create the styled text control.
     */
    private void createTextControl() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        gd.widthHint = 800;
        gd.heightHint = 500;

        editorStTxt = new StyledText(shell, SWT.BORDER | SWT.MULTI
                | SWT.H_SCROLL | SWT.V_SCROLL);
        editorStTxt.setWordWrap(wrapBtn.getSelection());
        editorStTxt.setFont(textFont);
        editorStTxt.setEditable(true);
        editorStTxt.setBackground(styledTextBackgroundColor);
        editorStTxt.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        editorStTxt.setLayoutData(gd);

        undoStack = new ArrayList<HashMap<String, Object>>(UNDO_STACK_SIZE);
        redoStack = new ArrayList<HashMap<String, Object>>(UNDO_STACK_SIZE);

        editorStTxt.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 3) {
                    popupMenu.setVisible(true);
                }
            }

        });

        editorStTxt.addExtendedModifyListener(new ExtendedModifyListener() {
            @Override
            public void modifyText(ExtendedModifyEvent e) {
                // Check the modifyFlag, it will be set to false if we're
                // currently in the process of an undo or redo action, it should
                // be true otherwise.
                if (modifyFlag) {
                    int start = e.start;
                    int length = e.length;
                    String replacedText = e.replacedText;
                    HashMap<String, Object> undoData = new HashMap<String, Object>();
                    undoData.put("start", start);
                    undoData.put("length", length);
                    undoData.put("replacedText", replacedText);

                    if (undoStack.size() == UNDO_STACK_SIZE) {
                        undoStack.remove(0);
                    }

                    undoStack.add(undoData);
                    redoStack.clear();
                }
            }
        });

        createEditorPopupMenu(shell);
    }

    private void createEditorPopupMenu(Composite editorTextComp) {
        popupMenu = new Menu(editorTextComp);

        MenuItem cutMI = new MenuItem(popupMenu, SWT.NONE);
        cutMI.setText("Cut");
        cutMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                editorStTxt.cut();
            }
        });

        MenuItem copyMI = new MenuItem(popupMenu, SWT.NONE);
        copyMI.setText("Copy");
        copyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                editorStTxt.copy();
            }
        });

        MenuItem pasteMI = new MenuItem(popupMenu, SWT.NONE);
        pasteMI.setText("Paste");
        pasteMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                editorStTxt.paste();
            }
        });

        MenuItem undoMI = new MenuItem(popupMenu, SWT.NONE);
        undoMI.setText("Undo");
        undoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                undoText();
            }
        });

        MenuItem redoMI = new MenuItem(popupMenu, SWT.NONE);
        redoMI.setText("Redo");
        redoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                redoText();
            }
        });

        editorTextComp.setMenu(popupMenu);
    }

    /**
     * The call back action for the Undo button.
     */
    private void undoText() {
        if (undoStack.size() > 0) {
            HashMap<String, Object> undoData = undoStack.remove(undoStack
                    .size() - 1);
            int start = (Integer) undoData.get("start");
            int length = (Integer) undoData.get("length");
            String text = (String) undoData.get("replacedText");
            undoData.clear();
            undoData.put("start", start);
            undoData.put("length", text.length());
            undoData.put("replacedText",
                    editorStTxt.getTextRange(start, length));
            redoStack.add(undoData);
            // Set the modifyFlag to false so the action of undoing the last
            // modification does not update the undoStack.
            modifyFlag = false;
            editorStTxt.replaceTextRange(start, length, text);
            editorStTxt.setCaretOffset(start + text.length());
            // Reset the modifyFlag so that subsequent modifications do update
            // the undoStack.
            modifyFlag = true;
        }
    }

    /**
     * Call back action for the Redo button.
     */
    private void redoText() {
        if (redoStack.size() > 0) {
            HashMap<String, Object> redoData = redoStack.remove(redoStack
                    .size() - 1);
            int start = (Integer) redoData.get("start");
            int length = (Integer) redoData.get("length");
            String text = (String) redoData.get("replacedText");
            redoData.clear();
            redoData.put("start", start);
            redoData.put("length", text.length());
            redoData.put("replacedText",
                    editorStTxt.getTextRange(start, length));
            undoStack.add(redoData);
            // Set the modifyFlag to false so the action of redoing the last
            // modification does not update the undoStack
            modifyFlag = false;
            editorStTxt.replaceTextRange(start, length, text);
            editorStTxt.setCaretOffset(start + text.length());
            // Reset the modifyFlag so that subsequent modifications do update
            // the undoStack.
            modifyFlag = true;
        }
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {
        msgStatusComp = new MessageStatusComp(shell, null, null);
    }

    /**
     * Show the 'Save' file dialog.
     */
    private void saveFile() {
        if (openedFile == null) {
            return;
        }

        LocalizationLevel level = openedFile.getContext()
                .getLocalizationLevel();
        if (level == LocalizationLevel.BASE) {
            openedFile.getContext()
                    .setLocalizationLevel(LocalizationLevel.SITE);
        }
        openedFile.getContext().setContextName(
                LocalizationManager.getInstance().getCurrentSite());

        File fn = openedFile.getFile();

        if (fn != null) {
            try {

                if (fn.exists() == false) {
                    File parentFile = fn.getParentFile();

                    if (parentFile.mkdirs() == false) {
                        System.out.println("Could not create directories...");
                    }

                    if (fn.createNewFile() == false) {
                        System.out.println("Cannot create file...");
                    }
                }

                if (editorStTxt.getText().isEmpty()) {
                    msgStatusComp.setMessageText("Unable to save empty file + "
                            + fn, new RGB(255, 0, 0));
                    return;
                }

                BufferedWriter output = new BufferedWriter(new FileWriter(fn));
                output.write(editorStTxt.getText());
                output.close();

                openedFile.save();

                filePathLbl.setText(fn.toString());

                msgStatusComp.setMessageText("File " + fn
                        + " saved successfully.", new RGB(0, 255, 0));
                TafSiteConfigFactory.clearInstance();
            } catch (FileNotFoundException e) {
                e.printStackTrace();
                msgStatusComp.setMessageText("Unable to open file " + fn
                        + " for writing.", new RGB(255, 0, 0));
            } catch (IOException e) {
                e.printStackTrace();
                msgStatusComp.setMessageText(
                        "An error occured while saving file " + fn, new RGB(
                                255, 0, 0));
            } catch (LocalizationOpFailedException e) {
                msgStatusComp.setMessageText(
                        "An error occured while saving file " + fn, new RGB(
                                255, 0, 0));
                e.printStackTrace();
            }
        }
    }

    // mmaron DR 2928
    // saves to the same dir as the opened file - according to njensen
    private void saveFileAs() {
        FileDialog dlg = new FileDialog(shell, SWT.SAVE);
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE), "aviation").getAbsolutePath();
        if (openedFile == null) {
            msgStatusComp.setMessageText(
                    "No file opened.  Please open file first.", new RGB(255, 0,
                            0));
            return;
        }
        path = openedFile.getFile().getAbsolutePath();
        System.out.println(path);
        path = path.substring(1, path.lastIndexOf('/'));

        dlg.setFilterPath(path);
        String fn = dlg.open();
        if (fn != null) {
            try {
                BufferedWriter output = new BufferedWriter(new FileWriter(
                        new File(fn)));
                output.write(editorStTxt.getText());
                output.close();
                msgStatusComp.setMessageText("File " + fn
                        + " saved successfully.", new RGB(0, 255, 0));

                TafSiteConfigFactory.clearInstance();
            } catch (FileNotFoundException e) {
                msgStatusComp.setMessageText("Unable to open file " + fn
                        + " for writing.", new RGB(255, 0, 0));
            } catch (IOException e) {
                msgStatusComp.setMessageText(
                        "An error occured while saving file " + fn, new RGB(
                                255, 0, 0));
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    public void preOpened() {
        if (openedFile != null) {
            openFile(openedFile);
        }
    }

    /**
     * Show the 'Open' file dialog.
     */
    private void openFile() {
        if (mustCreate(openDlg)) {
            openDlg = new OpenDlg(shell);
            openDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    LocalizationFile selectedFile = null;
                    if (returnValue instanceof LocalizationFile) {
                        selectedFile = (LocalizationFile) returnValue;
                    }
                    openFile(selectedFile);
                }
            });
            openDlg.open();
        } else {
            openDlg.bringToTop();
        }
    }

    /**
     * Get the contents of a localized file and populates the editor.
     * 
     * @param lFile
     */
    private void openFile(LocalizationFile lFile) {
        openedFile = lFile;

        if (openedFile != null) {
            try {
                StringBuilder contents = new StringBuilder();
                BufferedReader input = new BufferedReader(new FileReader(
                        new File(openedFile.getFile().getAbsolutePath())));
                String line = null;

                while ((line = input.readLine()) != null) {
                    contents.append(line);
                    contents.append(System.getProperty("line.separator"));
                }

                editorStTxt.setText(contents.toString());

                input.close();
                msgStatusComp.setMessageText("File " + openedFile
                        + " opened successfully.", new RGB(0, 255, 0));
                filePathLbl.setText(openedFile.getFile().toString());
            } catch (FileNotFoundException e) {
                msgStatusComp.setMessageText("File " + openedFile
                        + " not found.", new RGB(255, 0, 0));
            } catch (IOException e) {
                msgStatusComp.setMessageText(
                        "An error occured while opening file " + openedFile,
                        new RGB(255, 0, 0));
            }
        }
    }
}
