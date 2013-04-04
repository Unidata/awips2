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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.util.ArrayList;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
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
 * Display FFMP Basin Table's Load/Save dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Dec 6, 2012  1353       rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class LoadSaveConfigDlg extends CaveSWTDialog {

    public static enum DialogType {
        OPEN("Load Configuration"), SAVE_AS("Save Configuration");

        private final String title;

        public String getTitle() {
            return title;
        }

        DialogType(String title) {
            this.title = title;
        }
    };

    private final DialogType dialogType;

    private Font controlFont;

    private List cfgFileList;

    private LocalizationFile[] locFiles;

    private TreeMap<String, LocalizationFile> locFileMap;

    private Text newFileNameTF;

    private Button actionBtn;

    private Button siteBtn;

    private Button userBtn;

    public LoadSaveConfigDlg(Shell parent, DialogType type) {
        super(parent, SWT.TITLE, CAVE.DO_NOT_BLOCK);
        setText(type.getTitle());
        dialogType = type;
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
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        locFileMap = new TreeMap<String, LocalizationFile>();
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListControl();

        // Create the buttons at the bottom of the display.
        createBottomButtons();

        getAvailableConfigFiles();
    }

    private void createListControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(1, false));
        controlComp.setLayoutData(gd);

        Label listLbl = new Label(controlComp, SWT.NONE);
        listLbl.setText("Available Config Files:");

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
                    if (selItem
                            .startsWith(LocalizationContext.LocalizationLevel.SITE
                                    .name())
                            || selItem
                                    .startsWith(LocalizationContext.LocalizationLevel.BASE
                                            .name())) {
                        siteBtn.setSelection(true);
                        userBtn.setSelection(false);
                    } else {
                        userBtn.setSelection(true);
                        siteBtn.setSelection(false);
                    }
                }
            }
        });

        cfgFileList.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                if ((e.button == 1) && (dialogType == DialogType.OPEN)) {
                    if (cfgFileList.getSelectionIndex() >= 0) {
                        openAction();
                    }
                }
            }
        });

        if (dialogType == DialogType.SAVE_AS) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            gd.horizontalSpan = ((GridLayout) controlComp.getLayout()).numColumns;
            Label sepLbl = new Label(controlComp, SWT.SEPARATOR
                    | SWT.HORIZONTAL);
            sepLbl.setLayoutData(gd);

            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            userBtn = new Button(controlComp, SWT.RADIO);
            userBtn.setText("User");
            userBtn.setLayoutData(gd);

            siteBtn = new Button(controlComp, SWT.RADIO);
            siteBtn.setText("Site");
            siteBtn.setLayoutData(gd);
            siteBtn.setSelection(true);

            Label newFileLbl = new Label(controlComp, SWT.NONE);
            newFileLbl.setText("Enter file name:");

            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            newFileNameTF = new Text(controlComp, SWT.BORDER);
            newFileNameTF.setLayoutData(gd);
        }
    }

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
                @Override
                public void widgetSelected(SelectionEvent event) {
                    openAction();
                }
            });
        } else if (dialogType == DialogType.SAVE_AS) {
            actionBtn.setText("Save");
            actionBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    if (validateFileName() == true) {
                        saveAction();
                        shell.dispose();
                    }
                }
            });
        }

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    private void openAction() {
        int selectedIndex = cfgFileList.getSelectionIndex();
        String str = cfgFileList.getItem(selectedIndex);
        setReturnValue(locFileMap.get(str));
        close();
    }

    private void saveAction() {
        String fileName = newFileNameTF.getText();

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationLevel level = LocalizationLevel.SITE;
        if (userBtn.getSelection()) {
            level = LocalizationLevel.USER;
        }
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, level);
        String newFileName = "ffmp/guiConfig/" + fileName;
        LocalizationFile selectedFile = pm.getLocalizationFile(context,
                newFileName);

        FFMPConfig.getInstance().saveFFMPBasinConfig(selectedFile);
        setReturnValue(selectedFile);
        close();
    }

    private boolean validateFileName() {
        StringBuffer strBuf = new StringBuffer(newFileNameTF.getText().trim());

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

        if (strBuf.toString().endsWith(".xml") == false) {
            strBuf.append(".xml");
            newFileNameTF.setText(strBuf.toString().trim());
        }

        return true;
    }

    private void getAvailableConfigFiles() {
        String[] extensions = new String[] { ".xml" };
        IPathManager pm = PathManagerFactory.getPathManager();

        ArrayList<LocalizationContext> contextList = new ArrayList<LocalizationContext>();
        contextList.add(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE));
        contextList.add(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.SITE));
        contextList.add(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER));
        locFiles = pm.listFiles(contextList
                .toArray(new LocalizationContext[contextList.size()]),
                "ffmp/guiConfig", extensions, false, true);

        if (locFiles == null) {
            return;
        }

        for (int i = 0; i < locFiles.length; i++) {
            locFileMap.put(locFiles[i].getContext().getLocalizationLevel()
                    + ":" + locFiles[i].getName(), locFiles[i]);
        }

        for (String str : locFileMap.keySet()) {
            cfgFileList.add(str);
        }

        if (cfgFileList.getSelectionCount() > 0) {
            cfgFileList.setSelection(0);
        }
    }
}
