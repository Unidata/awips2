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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;

public class OpenSaveDlg extends Dialog {
    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    public static enum DialogType {
        OPEN, SAVE_AS
    };

    private DialogType dialogType;

    private Font controlFont;

    private List cfgFileList;

    private LocalizationFile selectedFile;

    private LocalizationFile[] locFiles;

    private Map<String, LocalizationFile> locFileMap;

    public OpenSaveDlg(Shell parent, DialogType type) {
        super(parent, 0);

        dialogType = type;
    }

    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.TITLE);
        shell.setText("Open Configuration File");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();
        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        controlFont.dispose();

        return null;
    }

    private void initializeComponents() {
        locFileMap = new TreeMap<String, LocalizationFile>();
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListControl();

        // Create the buttons at the bottom of the display.
        createBottomButtons();

        getAvailableConfigFiles();
    }

    private void createListControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        listComp.setLayoutData(gd);

        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText("Available Config Files:");

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 400;
        gd.heightHint = 400;
        gd.horizontalSpan = 2;
        cfgFileList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
        cfgFileList.setLayoutData(gd);
        cfgFileList.setFont(controlFont);
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

        if (dialogType == DialogType.OPEN) {
            gd = new GridData(100, SWT.DEFAULT);
            Button openBtn = new Button(buttonComp, SWT.PUSH);
            openBtn.setText("Open");
            openBtn.setLayoutData(gd);
            openBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    int selectedIndex = cfgFileList.getSelectionIndex();
                    String str = cfgFileList.getItem(selectedIndex);
                    selectedFile = locFileMap.get(str);
                    shell.dispose();
                }
            });
        } else if (dialogType == DialogType.SAVE_AS) {
            gd = new GridData(100, SWT.DEFAULT);
            Button saveBtn = new Button(buttonComp, SWT.PUSH);
            saveBtn.setText("Save");
            saveBtn.setLayoutData(gd);
            saveBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    selectedFile = null;
                    shell.dispose();
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
                shell.dispose();
            }
        });
    }

    private void getAvailableConfigFiles() {
        String[] extensions = new String[] { ".xml", ".cfg" };
        LocalizationType[] types = new LocalizationType[] {
                LocalizationType.CAVE_CONFIG, LocalizationType.CAVE_STATIC,
                LocalizationType.COMMON_STATIC };
        ArrayList<LocalizationFile> localFiles = new ArrayList<LocalizationFile>();
        for (LocalizationType type : types) {
            LocalizationContext[] contexts = PathManagerFactory
                    .getPathManager().getLocalSearchHierarchy(type);
            for (LocalizationContext context : contexts) {
                localFiles.addAll(Arrays.asList(PathManagerFactory
                        .getPathManager().listFiles(context, "aviation",
                                extensions, true, true)));
            }
        }
        locFiles = localFiles.toArray(new LocalizationFile[0]);
        if (locFiles == null) {
            return;
        }

        for (int i = 0; i < locFiles.length; i++) {
            if (locFiles[i].getName().startsWith("aviation/avnsetup") == false) {
                String contextName = locFiles[i].getContext().getContextName();
                if (contextName == null) {
                    contextName = " - " + "BASE";
                } else {
                    contextName = " - " + contextName;
                }
                locFileMap
                        .put(locFiles[i].getName() + contextName, locFiles[i]);
            }
        }

        for (String str : locFileMap.keySet()) {
            cfgFileList.add(str);
        }

        if (cfgFileList.getSelectionCount() > 0) {
            cfgFileList.setSelection(0);
        }
    }

    public LocalizationFile getSelectedFile() {
        return selectedFile;
    }
}
