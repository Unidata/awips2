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
package com.raytheon.viz.texteditor.alarmalert.dialogs;

import java.io.File;
import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.texteditor.alarmalert.util.AlarmAlertFunctions;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2009            mnash     Initial creation
 * ======================================
 * AWIPS2 DR Work
 * 07/25/2012          953 jkorman     Modified file "search" to return LocalizationFile
 * instead of File so references are deleted in all locations.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlarmAlertSaveLoadDlg extends CaveSWTDialog {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlarmAlertSaveLoadDlg.class);

    private Font font;

    private Composite shellComp;

    private static AlarmAlertSaveLoadDlg saveLoadDlg = null;

    private static String fileName = "";

    private SaveOrLoad saveLoad;

    private Text textBox;

    protected static enum SaveOrLoad {
        SAVE, LOAD
    }

    /**
     * @param parentShell
     */
    protected AlarmAlertSaveLoadDlg(Shell parentShell, SaveOrLoad saveLoad) {
        super(parentShell, SWT.APPLICATION_MODAL | SWT.CLOSE | SWT.TITLE,
                CAVE.PERSPECTIVE_INDEPENDENT);
        if (saveLoad == SaveOrLoad.SAVE) {
            setText("Save Lists As...");
        } else {
            setText("Load Lists");
        }

        this.saveLoad = saveLoad;
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(fileName);

        // make a composite that covers the entire shell
        shellComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        shellComp.setLayout(constructShellLayout());
        shellComp.setLayoutData(gd);

        font = new Font(shell.getDisplay(), "Helvetica", 10, SWT.BOLD);

        // Initialize all of the controls and layouts
        shell.setMinimumSize(350, 150);
        if (saveLoad == SaveOrLoad.SAVE) {
            createSave();
        } else {
            createLoad();
        }
    }

    /**
     * 
     */
    private void createLoad() {
        Composite listComp = new Composite(shellComp, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout layout = new GridLayout(1, true);
        listComp.setLayout(layout);
        listComp.setLayoutData(gd);

        Label listLabel = new Label(listComp, SWT.BOLD);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        listLabel.setLayoutData(gd);
        listLabel.setAlignment(SWT.CENTER);
        listLabel.setText("User File Names");

        final List lists = new List(listComp, SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        lists.setLayoutData(gd);
        LocalizationContext lc = AlarmAlertFunctions.initUserLocalization();
        
        // Get a list of localization files!
        LocalizationFile[] fList = PathManagerFactory.getPathManager()
                .listFiles(lc, "alarms", new String[] { "xml" }, false, true);

        final java.util.List<LocalizationFile> fileList = new ArrayList<LocalizationFile>();
        for (LocalizationFile locFile : fList) {
            // We only want the filename in the display list.
            String[] s = locFile.getName().split("/");
            // Make sure we have some data!
            if (s.length > 0) {
                // The last element is the filename.
                lists.add(s[s.length - 1]);
                // Complete file reference here.
                fileList.add(locFile);
            }
        }

        Composite ackButtons = new Composite(shell, SWT.NONE);
        GridLayout ackButtonLayout = new GridLayout(3, true);
        ackButtons.setLayout(ackButtonLayout);
        ackButtons.setLayoutData(new GridData(SWT.DEFAULT, SWT.DEFAULT, true,
                false));

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        Button loadButton = new Button(ackButtons, SWT.PUSH);
        loadButton.setText("Load");
        loadButton.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        Button deleteButton = new Button(ackButtons, SWT.PUSH);
        deleteButton.setText("Delete");
        deleteButton.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        Button cancelButton = new Button(ackButtons, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.setLayoutData(gd);

        // loads the
        loadButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                // Set the filename to be returned through getFileName()
                fileName = lists.getSelection()[0];
                shell.close();
            }
        });

        // delete the file from the display list and from the file system
        deleteButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                int num = lists.getSelectionIndex();
                LocalizationFile f = fileList.get(num);
                try {
                    if (!f.delete()) {
                        String msg = String.format(
                                "ALARM/ALERT:Failed deleting file %s", f
                                        .getFile().getPath());
                        statusHandler.handle(Priority.PROBLEM, msg);
                    }
                } catch (LocalizationOpFailedException e) {
                    String msg = String.format(
                            "ALARM/ALERT:Failed deleting file %s", f.getFile()
                                    .getPath());
                    statusHandler.handle(Priority.PROBLEM, msg, e);
                }
                lists.remove(num);
            }
        });

        // close the display without doing anything else
        cancelButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.close();
            }
        });

    }

    /**
     * 
     */
    private void createSave() {
        Composite listComp = new Composite(shellComp, SWT.NONE);
        GridLayout layout = new GridLayout(1, true);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        listComp.setLayout(layout);
        listComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        Label listLabel = new Label(listComp, SWT.BOLD);
        listLabel.setAlignment(SWT.CENTER);
        listLabel.setText("User File Names");
        listLabel.setLayoutData(gd);

        final List lists = new List(listComp, SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        lists.setLayoutData(gd);
        LocalizationContext lc = AlarmAlertFunctions.initUserLocalization();
        LocalizationFile fileDir = PathManagerFactory.getPathManager()
                .getLocalizationFile(lc, "alarms");
        File file = fileDir.getFile();
        final File[] fileList = file.listFiles();
        for (File locFile : fileList) {
            if (locFile.getName().endsWith(".xml")) {
                lists.add(locFile.getName());
            }
        }

        // Text box
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        textBox = new Text(listComp, SWT.BORDER | SWT.SINGLE);
        textBox.setLayoutData(gd);

        Composite ackButtons = new Composite(shell, SWT.NONE);
        GridLayout ackButtonLayout = new GridLayout(3, true);
        ackButtons.setLayout(ackButtonLayout);
        ackButtons.setLayoutData(new GridData(SWT.DEFAULT, SWT.DEFAULT, true,
                false));

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        Button saveButton = new Button(ackButtons, SWT.PUSH);
        saveButton.setText("Save");
        saveButton.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        Button cancelButton = new Button(ackButtons, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.setLayoutData(gd);

        // get the file name
        saveButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                // Set the filename to be returned through getFileName()
                fileName = textBox.getText();
                shell.close();
            }
        });

        // close the display without doing anything else
        cancelButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.close();
            }
        });

        lists.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                textBox.setText(lists.getSelection()[0]);
            }
        });

    }

    public void setDialogFocus() {
        shell.setFocus();
    }

    public String getFileName() {
        return fileName;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        saveLoadDlg = new AlarmAlertSaveLoadDlg(new Shell(), SaveOrLoad.LOAD);
        saveLoadDlg.open();
    }
}
