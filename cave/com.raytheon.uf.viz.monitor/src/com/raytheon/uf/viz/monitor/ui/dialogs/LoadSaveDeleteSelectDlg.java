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
package com.raytheon.uf.viz.monitor.ui.dialogs;

import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * File Load/Save/Delete/Select Dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 5, 2012  #1351      skorolev    Cleaned code
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class LoadSaveDeleteSelectDlg extends CaveSWTDialog {
    /**  **/
    public static enum DialogType {
        OPEN, SAVE_AS, DELETE, SELECT_DEFAULT
    };

    /** Dialog Type **/
    private DialogType dialogType;

    /** Font **/
    private Font controlFont;

    /** Configuration File List **/
    private List cfgFileList;

    /** Selected File **/
    private LocalizationFile selectedFile;

    /** Localization Files **/
    private LocalizationFile[] locFiles;

    /** Localization Files Map **/
    private TreeMap<String, LocalizationFile> locFileMap;

    /** New file name **/
    private Text newFileNameTF;

    /** Action Button **/
    private Button actionBtn;

    /** File path **/
    private String fileNamePath;

    /** Excluded Name For Saving **/
    private String excludedNameForSaving = "";

    /**
     * Constructor
     * 
     * @param parent
     * @param type
     *            Dialog type
     * @param fileNamePath
     * @param excludedNameForSaving
     */
    public LoadSaveDeleteSelectDlg(Shell parent, DialogType type,
            String fileNamePath, String excludedNameForSaving) {
        super(parent, SWT.TITLE);
        // TODO add Cave.DO_NOT_BLOCK once all dialogs using this class have
        // been converted to use close call.
        if (type == DialogType.OPEN) {
            setText("Load");
        } else if (type == DialogType.SAVE_AS) {
            setText("Save");
        } else if (type == DialogType.SELECT_DEFAULT) {
            setText("Select Default");
        } else if (type == DialogType.DELETE) {
            setText("Delete");
        }
        dialogType = type;
        this.fileNamePath = fileNamePath;
        if (excludedNameForSaving != null) {
            this.excludedNameForSaving = excludedNameForSaving;
        }
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
        setReturnValue(selectedFile);
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
        locFileMap = new TreeMap<String, LocalizationFile>();
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        createListControl();
        // Create the buttons at the bottom of the display.
        createBottomButtons();
        getAvailableConfigFiles();
    }

    /**
     * Creates List Control.
     */
    private void createListControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(1, false));
        controlComp.setLayoutData(gd);
        Label listLbl = new Label(controlComp, SWT.NONE);
        listLbl.setText("Available Files:");
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 400;
        gd.heightHint = 400;
        gd.horizontalSpan = 2;
        cfgFileList = new List(controlComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL);
        cfgFileList.setLayoutData(gd);
        cfgFileList.setFont(controlFont);
        cfgFileList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (dialogType == DialogType.SAVE_AS) {
                    String selItem = cfgFileList.getItem(cfgFileList
                            .getSelectionIndex());
                    int idx = selItem.lastIndexOf("/");
                    String newStr = selItem.substring(idx + 1);
                    newFileNameTF.setText(newStr);
                }
            }
        });
        if (dialogType == DialogType.SAVE_AS) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            gd.horizontalSpan = ((GridLayout) controlComp.getLayout()).numColumns;
            Label sepLbl = new Label(controlComp, SWT.SEPARATOR
                    | SWT.HORIZONTAL);
            sepLbl.setLayoutData(gd);
            Label newFileLbl = new Label(controlComp, SWT.NONE);
            newFileLbl.setText("Enter file name:");
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            newFileNameTF = new Text(controlComp, SWT.BORDER);
            newFileNameTF.setLayoutData(gd);
        }
    }

    /**
     * Creates Bottom Buttons.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        actionBtn = new Button(buttonComp, SWT.PUSH);
        actionBtn.setLayoutData(gd);

        if (dialogType == DialogType.OPEN) {
            actionBtn.setText("Open");
            actionBtn.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    openDeleteSelectAction();
                }
            });
        } else if (dialogType == DialogType.DELETE) {
            actionBtn.setText("Delete");
            actionBtn.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    openDeleteSelectAction();
                }
            });
        } else if (dialogType == DialogType.SAVE_AS) {
            actionBtn.setText("Save");
            actionBtn.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    if (validateFileName() == true) {
                        saveAction();
                        shell.dispose();
                    }
                }
            });
        } else if (dialogType == DialogType.SELECT_DEFAULT) {
            actionBtn.setText("Select");
            actionBtn.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    openDeleteSelectAction();
                }
            });
        }

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                selectedFile = null;
                shell.dispose();
            }
        });
    }

    /**
     * Opens Delete confirmation message.
     */
    private void openDeleteSelectAction() {
        int selectedIndex = cfgFileList.getSelectionIndex();
        String str = cfgFileList.getItem(selectedIndex);
        if (dialogType == DialogType.DELETE) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.YES
                    | SWT.NO);
            mb.setText("Delete");
            mb.setMessage("You are about to delete the file:\n\n" + str
                    + "\n\nDo you wish to continue?");
            int result = mb.open();
            if (result == SWT.NO) {
                return;
            }
        }
        selectedFile = locFileMap.get(str);
        shell.dispose();
    }

    /**
     * Save action.
     */
    private void saveAction() {
        String fileName = newFileNameTF.getText();
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        String newFileName = fileNamePath + fileName;
        selectedFile = pm.getLocalizationFile(context, newFileName);
        shell.dispose();
    }

    /**
     * Validates File Name.
     * 
     * @return True/False
     */
    private boolean validateFileName() {
        StringBuffer strBuf = new StringBuffer(newFileNameTF.getText().trim());
        newFileNameTF.setText(strBuf.toString());
        if (strBuf.length() == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Warning");
            mb.setMessage("No file name has been entered.");
            mb.open();
            return false;
        }
        if (strBuf.toString().matches("[A-Za-z0-9._-]+") == false) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Warning");
            mb.setMessage("File name contains invalid charaters.  The file name can only\n"
                    + "contain A-Z, a-z, 0-9, or periods, underscores, or dashes.");
            mb.open();
            return false;
        }
        if (strBuf.toString().compareTo(excludedNameForSaving) == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Warning");
            mb.setMessage("Cannot save to the base default file name.\n"
                    + "Please choose anther file name.");
            mb.open();
            return false;
        }
        String[] listItems = cfgFileList.getItems();
        for (String listItem : listItems) {
            int idx = listItem.lastIndexOf("/");
            String fn = listItem.substring(idx + 1);
            if (fn.compareTo(strBuf.toString()) == 0) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.YES
                        | SWT.NO);
                mb.setText("Warning");
                mb.setMessage("File name already exists.  Do you wish to overwrite\n"
                        + "the existing file?.");
                int result = mb.open();

                if (result == SWT.NO) {
                    return false;
                }
            }
        }
        if (strBuf.toString().endsWith(".xml") == false) {
            strBuf.append(".xml");
            newFileNameTF.setText(strBuf.toString().trim());
        }
        return true;
    }

    /**
     * Gets Available Configuration Files.
     */
    private void getAvailableConfigFiles() {
        String[] extensions = new String[] { ".xml" };
        locFiles = PathManagerFactory.getPathManager().listStaticFiles(
                fileNamePath, extensions, false, true);
        if (locFiles == null) {
            return;
        }
        for (int i = 0; i < locFiles.length; i++) {
            /*
             * Add the available files to the list as long as the file name does
             * not match the "exclude file name".
             */
            if (dialogType == DialogType.SAVE_AS
                    || dialogType == DialogType.DELETE) {
                if (excludedNameForSaving.compareTo(locFiles[i].getFile()
                        .getName()) != 0) {
                    locFileMap
                            .put(locFiles[i].getFile().getName(), locFiles[i]);
                }
            } else {
                locFileMap.put(locFiles[i].getFile().getName(), locFiles[i]);
            }
        }
        for (String str : locFileMap.keySet()) {
            cfgFileList.add(str);
        }
        if (cfgFileList.getSelectionCount() > 0) {
            cfgFileList.setSelection(0);
        }
        if (cfgFileList.getItemCount() == 0) {
            if (dialogType == DialogType.DELETE
                    || dialogType == DialogType.SELECT_DEFAULT) {
                actionBtn.setEnabled(false);
            }
        }
    }

    /**
     * Gets Selected File
     * 
     * @return selectedFile
     */
    public LocalizationFile getSelectedFile() {
        return selectedFile;
    }
}
