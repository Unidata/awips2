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
package com.raytheon.viz.hydrocommon.texteditor;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Text Editor dialog used for editing ASCII text.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 9, 2008				lvenable	Initial creation
 * Sep 18, 2009 2772        mpduff      Fixed NullPointer when opening in Read only.
 * Jul 15, 2013 2088        rferrel     Changes for non-blocking FindReplaceDlg.
 *                                      Make dialog non-blocking.
 * May 27, 2014 3133        njensen     Removed references to Activator
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class TextEditorDlg extends CaveSWTDialog {

    private static final String PLUGIN = "com.raytheon.viz.hydrocommon";

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Read-only flag that hides controls from the user.
     */
    private boolean readOnly = true;

    /**
     * Text file to view/edit.
     */
    private File textFile;

    /**
     * Styled text control used for editing the text.
     */
    private StyledText textST;

    /**
     * Original text data (before editing).
     */
    private StringBuffer originalTextData;

    /**
     * The find and replace dialog.
     */
    private FindReplaceDlg findReplaceDlg;

    /**
     * Undo/Redo edit array.
     */
    private ArrayList<String> undoRedoArray;

    /**
     * Undo toolbar item.
     */
    private ToolItem undoToolBtn;

    /**
     * Redo toolbar item.
     */
    private ToolItem redoToolBtn;

    /**
     * Undo menu item.
     */
    private MenuItem undoMI;

    /**
     * Redo menu item.
     */
    private MenuItem redoMI;

    /**
     * Undo/Redo action flag.
     */
    private boolean undoRedoAction = false;

    /**
     * Undo/Redo caret index.
     */
    private int undoRedoIndex = 1;

    /**
     * Save image.
     */
    private Image saveImage;

    /**
     * Cut image.
     */
    private Image cutImage;

    /**
     * Copy image.
     */
    private Image copyImage;

    /**
     * Paste image.
     */
    private Image pasteImage;

    /**
     * Select All image.
     */
    private Image selectAllImage;

    /**
     * Undo image.
     */
    private Image undoImage;

    /**
     * Redo image.
     */
    private Image redoImage;

    /**
     * Find & Replace image.
     */
    private Image findReplaceImage;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param readOnly
     *            Read only flag.
     * @param file
     *            File to view/edit.
     */
    public TextEditorDlg(Shell parent, boolean readOnly, File file) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        textFile = file;
        this.readOnly = readOnly;

        if (textFile != null) {
            StringBuilder sb = new StringBuilder("Text Editor - ");
            sb.append(textFile.getName());
            if (readOnly == true) {
                sb.append(" (Read Only)");
            }
            setText(sb.toString());
        }

        setReturnValue(file);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        controlFont.dispose();
        saveImage.dispose();
        cutImage.dispose();
        copyImage.dispose();
        pasteImage.dispose();
        selectAllImage.dispose();
        findReplaceImage.dispose();
        undoImage.dispose();
        redoImage.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        undoRedoArray = new ArrayList<String>();

        getImages();

        createMenus();
        createToolBar();
        createTextControl();

        // Add the text data to the text control.
        textST.setText(originalTextData.toString());

        textST.setFocus();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#shouldOpen()
     */
    @Override
    protected boolean shouldOpen() {
        // Verify file name
        if (textFile.exists() == false) {
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("File Error");
            mb.setMessage("The following file does not exist:\n"
                    + textFile.getAbsolutePath());
            mb.open();
            return false;
        }

        if (readFile() == false) {
            return false;
        }
        return true;
    }

    /**
     * Create the menu bar and menus.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createEditMenu(menuBar);
        createOptionsMenu(menuBar);

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

        if (readOnly == false) {
            // Save menu item
            MenuItem saveMI = new MenuItem(fileMenu, SWT.NONE);
            saveMI.setText("&Save\tCtrl+S");
            saveMI.setAccelerator(SWT.CTRL + 'S');
            saveMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    saveFile();
                }
            });

            // Save As menu item
            MenuItem saveAsMI = new MenuItem(fileMenu, SWT.NONE);
            saveAsMI.setText("Save &As...");
            saveAsMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    fileSaveAs();
                }
            });

            new MenuItem(fileMenu, SWT.SEPARATOR);
        }

        // Exit menu item
        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("E&xit");
        exitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
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
        // Create the Edit menu
        // -------------------------------------
        MenuItem editMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        editMenuItem.setText("&Edit");

        // Create the Edit menu item with a Edit "dropdown" menu
        Menu editMenu = new Menu(menuBar);
        editMenuItem.setMenu(editMenu);

        // -------------------------------------------------
        // Create all the items in the Edit dropdown menu
        // -------------------------------------------------

        if (readOnly == false) {
            // Undo menu item
            undoMI = new MenuItem(editMenu, SWT.NONE);
            undoMI.setText("&Undo\tCtrl+Z");
            undoMI.setAccelerator(SWT.CTRL + 'Z');
            undoMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    undoText();
                }
            });

            // Redo menu item
            redoMI = new MenuItem(editMenu, SWT.NONE);
            redoMI.setText("&Redo\tCtrl+Y");
            redoMI.setAccelerator(SWT.CTRL + 'Y');
            redoMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    redoText();
                }
            });

            new MenuItem(editMenu, SWT.SEPARATOR);

            // Cut menu item
            MenuItem cutMI = new MenuItem(editMenu, SWT.NONE);
            cutMI.setText("Cu&t\tCtrl+X");
            cutMI.setAccelerator(SWT.CTRL + 'X');
            cutMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    cutText();
                }
            });
        }

        // Copy menu item
        MenuItem copyMI = new MenuItem(editMenu, SWT.NONE);
        copyMI.setText("&Copy\tCtrl+C");
        copyMI.setAccelerator(SWT.CTRL + 'C');
        copyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                copyText();
            }
        });

        if (readOnly == false) {
            // Paste menu item
            MenuItem pasteMI = new MenuItem(editMenu, SWT.NONE);
            pasteMI.setText("&Paste\tCtrl+V");
            pasteMI.setAccelerator(SWT.CTRL + 'V');
            pasteMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    pasteText();
                }
            });

        }

        new MenuItem(editMenu, SWT.SEPARATOR);

        // Select All menu item
        MenuItem selectAllMI = new MenuItem(editMenu, SWT.NONE);
        selectAllMI.setText("Select &All\tCtrl+A");
        selectAllMI.setAccelerator(SWT.CTRL + 'A');
        selectAllMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectAllText();
            }
        });

        new MenuItem(editMenu, SWT.SEPARATOR);

        // Select All menu item
        MenuItem findReplaceMI = new MenuItem(editMenu, SWT.NONE);
        findReplaceMI.setText("&Find/Replace...\tCtrl+F");
        findReplaceMI.setAccelerator(SWT.CTRL + 'F');
        findReplaceMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                findReplaceText();
            }
        });
    }

    /**
     * Create the Options menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createOptionsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Options menu
        // -------------------------------------
        MenuItem optionsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        optionsMenuItem.setText("&Edit");

        // Create the Options menu item with a Options "dropdown" menu
        Menu optionsMenu = new Menu(menuBar);
        optionsMenuItem.setMenu(optionsMenu);

        // -------------------------------------------------
        // Create all the items in the Options dropdown menu
        // -------------------------------------------------

        // Word wrap menu item
        final MenuItem wordWrapMI = new MenuItem(optionsMenu, SWT.CHECK);
        wordWrapMI.setText("&Word Wrap");
        wordWrapMI.setSelection(true);
        wordWrapMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                textST.setWordWrap(wordWrapMI.getSelection());
            }
        });
    }

    /**
     * Create the tool bar and tool bar items.
     */
    private void createToolBar() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite toolbarComp = new Composite(shell, SWT.NONE);
        toolbarComp.setLayout(new GridLayout(1, false));
        toolbarComp.setLayoutData(gd);

        ToolBar toolBar = new ToolBar(toolbarComp, SWT.HORIZONTAL);

        if (readOnly == false) {
            ToolItem saveToolBtn = new ToolItem(toolBar, SWT.PUSH);
            saveToolBtn.setImage(saveImage);
            saveToolBtn.setToolTipText("Save");
            saveToolBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    saveFile();
                }
            });

            new ToolItem(toolBar, SWT.SEPARATOR);

            ToolItem cutToolBtn = new ToolItem(toolBar, SWT.PUSH);
            cutToolBtn.setImage(cutImage);
            cutToolBtn.setToolTipText("Cut");
            cutToolBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    cutText();
                }
            });

        }

        ToolItem copyToolBtn = new ToolItem(toolBar, SWT.PUSH);
        copyToolBtn.setImage(copyImage);
        copyToolBtn.setToolTipText("Copy");
        copyToolBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                copyText();
            }
        });

        if (readOnly == false) {
            ToolItem pasteToolBtn = new ToolItem(toolBar, SWT.PUSH);
            pasteToolBtn.setImage(pasteImage);
            pasteToolBtn.setToolTipText("Paste");
            pasteToolBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    pasteText();
                }
            });

        }

        new ToolItem(toolBar, SWT.SEPARATOR);

        ToolItem searchToolBtn = new ToolItem(toolBar, SWT.PUSH);
        searchToolBtn.setImage(findReplaceImage);
        searchToolBtn.setToolTipText("Search");
        searchToolBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                findReplaceText();
            }
        });

        if (readOnly == false) {
            new ToolItem(toolBar, SWT.SEPARATOR);

            undoToolBtn = new ToolItem(toolBar, SWT.PUSH);
            undoToolBtn.setImage(undoImage);
            undoToolBtn.setToolTipText("Undo");
            undoToolBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    undoText();
                }
            });

            redoToolBtn = new ToolItem(toolBar, SWT.PUSH);
            redoToolBtn.setImage(redoImage);
            redoToolBtn.setToolTipText("Redo");
            redoToolBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    redoText();
                }
            });
        }

        new ToolItem(toolBar, SWT.SEPARATOR);

        ToolItem selectAllToolBtn = new ToolItem(toolBar, SWT.PUSH);
        selectAllToolBtn.setImage(selectAllImage);
        selectAllToolBtn.setToolTipText("Select All");
        selectAllToolBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectAllText();
            }
        });
    }

    /**
     * Create the text control used for viewing/editing.
     */
    private void createTextControl() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite textComp = new Composite(shell, SWT.NONE);
        textComp.setLayout(new GridLayout(1, false));
        textComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 800;
        gd.heightHint = 600;
        textST = new StyledText(textComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL);
        textST.setWordWrap(true);
        textST.setFont(controlFont);
        textST.setLayoutData(gd);

        if (readOnly == true) {
            textST.setEditable(false);
        }

        textST.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                updateText();
            }
        });
    }

    /**
     * Read in the file.
     * 
     * @return True if the file could be read in.
     */
    private boolean readFile() {
        originalTextData = new StringBuffer();
        String str;
        FileReader fr;

        try {
            fr = new FileReader(textFile);

            BufferedReader br = new BufferedReader(fr);

            boolean firstLine = true;

            while ((str = br.readLine()) != null) {
                if (firstLine == false) {
                    originalTextData.append("\n");
                }

                originalTextData.append(str);
                firstLine = false;
            }
            br.close();
            fr.close();
        } catch (Exception e) {
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("File Error");
            mb.setMessage("An error occurred reading the file:\n"
                    + textFile.getAbsolutePath()
                    + "\n\nPlease see the log file for more information.");
            mb.open();
            return false;
        }

        return true;
    }

    /**
     * Cut the selected text from the text control.
     */
    private void cutText() {
        textST.cut();
    }

    /**
     * Copy the selected text from the text control.
     */
    private void copyText() {
        textST.copy();
    }

    /**
     * Paste text from the clipboard to the text control.
     */
    private void pasteText() {
        textST.paste();
    }

    /**
     * Selected all of the text in the text control.
     */
    private void selectAllText() {
        textST.selectAll();
    }

    /**
     * Undo the last change to the text.
     */
    private void undoText() {
        if (undoRedoIndex > 1) {
            --undoRedoIndex;
        }

        int caretPos = textST.getCaretOffset();

        undoRedoAction = true;
        textST.setText(undoRedoArray.get(undoRedoIndex - 1));
        textST.setSelection(caretPos, caretPos);

        undoRedoAction = false;

        enableDisableUndoRedo();
    }

    /**
     * Redo the last change to the text.
     */
    private void redoText() {
        if (undoRedoIndex < undoRedoArray.size()) {
            ++undoRedoIndex;
        } else {
            redoMI.setEnabled(false);
            redoToolBtn.setEnabled(false);
        }

        int caretPos = textST.getCaretOffset();

        undoRedoAction = true;
        textST.setText(undoRedoArray.get(undoRedoIndex - 1));
        textST.setSelection(caretPos, caretPos);

        undoRedoAction = false;

        enableDisableUndoRedo();
    }

    /**
     * Display the find and replace dialog.
     */
    private void findReplaceText() {
        if (findReplaceDlg == null || findReplaceDlg.isDisposed()) {
            findReplaceDlg = new FindReplaceDlg(shell, textST);
            findReplaceDlg.open();
        } else {
            findReplaceDlg.bringToTop();
        }
    }

    /**
     * Save the file.
     */
    private void saveFile() {
        FileWriter fw;

        try {
            fw = new FileWriter(textFile, false);

            fw.write(textST.getText());
            fw.flush();
            fw.close();
        } catch (IOException e) {
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("File Error");
            mb.setMessage("Unable to save file:\n" + textFile.getAbsolutePath()
                    + "\n" + e.getMessage());
            mb.open();
        }
    }

    /**
     * Save the file as a different filename.
     */
    private void fileSaveAs() {
        FileDialog dlg = new FileDialog(shell, SWT.SAVE);

        dlg.setFilterPath(textFile.getParent() + "/");
        String fn = dlg.open();

        if (fn == null) {
            return;
        }

        textFile = new File(fn);

        saveFile();
    }

    /**
     * Update the undo/redo as the text control has been updated.
     */
    private void updateText() {
        if (undoRedoAction == true) {
            return;
        }

        if (undoRedoArray.size() > 50) {
            for (int i = 0; i < 30; i++) {
                undoRedoArray.remove(0);
            }
        }

        if (undoRedoIndex != undoRedoArray.size()) {
            int diff = undoRedoArray.size() - undoRedoIndex;
            for (int i = 0; i < diff; i++) {
                undoRedoArray.remove(undoRedoIndex);
            }
        }

        undoRedoArray.add(textST.getText());
        undoRedoIndex = undoRedoArray.size();

        enableDisableUndoRedo();
    }

    /**
     * Enable/Disable the undo/redo buttons & menu items.
     */
    private void enableDisableUndoRedo() {
        if (readOnly == false) {
            if (undoRedoIndex > 1) {
                undoMI.setEnabled(true);
                undoToolBtn.setEnabled(true);
            } else {
                undoMI.setEnabled(false);
                undoToolBtn.setEnabled(false);
            }

            if (undoRedoIndex < undoRedoArray.size()) {
                redoMI.setEnabled(true);
                redoToolBtn.setEnabled(true);
            } else {
                redoMI.setEnabled(false);
                redoToolBtn.setEnabled(false);
            }
        }
    }

    /**
     * Get the images for the toolbar buttons.
     */
    private void getImages() {
        ImageDescriptor id;

        id = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN,
                "icons/media-floppy.png");
        saveImage = id.createImage();

        id = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN,
                "icons/edit-cut.png");
        cutImage = id.createImage();

        id = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN,
                "icons/edit-copy.png");
        copyImage = id.createImage();

        id = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN,
                "icons/edit-paste.png");
        pasteImage = id.createImage();

        id = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN,
                "icons/edit-find-replace.png");
        findReplaceImage = id.createImage();

        id = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN,
                "icons/edit-select-all.png");
        selectAllImage = id.createImage();

        id = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN,
                "icons/edit-undo.png");
        undoImage = id.createImage();

        id = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN,
                "icons/edit-redo.png");
        redoImage = id.createImage();
    }

    public void showDialog() {
        shell.setVisible(true);
        shell.setFocus();
    }
}
