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
package com.raytheon.uf.viz.collaboration.ui.session;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.google.common.collect.Lists;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.collaboration.ui.notifier.Notifier;
import com.raytheon.uf.viz.collaboration.ui.notifier.NotifierTask;
import com.raytheon.uf.viz.collaboration.ui.notifier.NotifierTools;
import com.raytheon.uf.viz.core.sounds.SoundUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Add Notifier Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2014    2632    mpduff      Initial creation
 * Mar 05, 2014    2632    mpduff      Changed task set to map of user->task.
 * Mar 27, 2014    2632    mpduff      Sorted users in combo box, changed how Add action works.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class AddNotifierDlg extends CaveSWTDialog {

    /** Array of userIds */
    private final String[] userIds;

    /** Map of buttons to Notifiers */
    private final Map<Button, Notifier> buttonMap = new HashMap<Button, Notifier>();

    /** Set of NotifierTask objects */
    private final Map<String, NotifierTask> taskMap = new HashMap<String, NotifierTask>();

    /** The user select Combo box */
    private Combo userCbo;

    /** The sound file path text widget */
    private Text soundTxt;

    /** Recurring radio button */
    private Button recurringRdo;

    /** The non-recurring radio button */
    private Button singleRdo;

    /** Close callback */
    private final ICloseCallback callback;

    /** The class return value */
    private boolean returnValue = false;

    /**
     * Constructor.
     * 
     * @param parent
     * @param userIds
     * @param callback
     */
    public AddNotifierDlg(Shell parent, String[] userIds,
            ICloseCallback callback) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Add Notifier");
        this.userIds = userIds;
        this.callback = callback;
    }

    public AddNotifierDlg(Shell parent, String[] userIds) {
        this(parent, userIds, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 4;
        mainLayout.marginWidth = 4;
        return mainLayout;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeComponents(Shell shell) {
        createUserComp(shell);
        createActionComp(shell);
        createBtnComp(shell);
    }

    /**
     * Create the user composite.
     * 
     * @param shell
     */
    private void createUserComp(Shell shell) {
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.LEFT, SWT.DEFAULT, false, false);
        Composite userComp = new Composite(shell, SWT.NONE);
        userComp.setLayout(gl);
        userComp.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        Label userLbl = new Label(userComp, SWT.NONE);
        userLbl.setText("Add Notifier to: ");
        userLbl.setLayoutData(gd);

        gd = new GridData(165, SWT.DEFAULT);
        userCbo = new Combo(userComp, SWT.NONE);
        userCbo.setLayoutData(gd);
        userCbo.setItems(getUserIds());
        userCbo.select(0);
        userCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                NotifierTask task = NotifierTools.getNotifierTask(userCbo
                        .getText());
                if (task != null) {
                    populate(task);
                } else {
                    reset();
                }
            }
        });
    }

    /**
     * Create the action composite
     * 
     * @param shell
     */
    private void createActionComp(Shell shell) {
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.LEFT, SWT.DEFAULT, false, false);
        Composite lblComp = new Composite(shell, SWT.NONE);
        lblComp.setLayout(gl);
        lblComp.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        Label actionLbl = new Label(lblComp, SWT.NONE);
        actionLbl.setText("Notify when Contact...");
        actionLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        gd.horizontalIndent = 20;
        gl = new GridLayout(2, false);
        Composite btnComp = new Composite(shell, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        for (Notifier notifier : Notifier.values()) {
            Button chk = new Button(btnComp, SWT.CHECK);
            chk.setText(notifier.getDescription());
            chk.setData(notifier);
            buttonMap.put(chk, notifier);
        }

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        gl = new GridLayout(1, false);
        Composite lblComp2 = new Composite(shell, SWT.NONE);
        lblComp2.setLayout(gl);
        lblComp2.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        Label soundLbl = new Label(lblComp2, SWT.NONE);
        soundLbl.setText("Play this sound: ");
        soundLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        gd.horizontalIndent = 20;
        gl = new GridLayout(3, false);
        Composite soundComp = new Composite(shell, SWT.NONE);
        soundComp.setLayout(gl);
        soundComp.setLayoutData(gd);

        gd = new GridData(175, SWT.DEFAULT);
        soundTxt = new Text(soundComp, SWT.BORDER);
        soundTxt.setLayoutData(gd);

        gd = new GridData(75, SWT.DEFAULT);
        Button browseBtn = new Button(soundComp, SWT.PUSH);
        browseBtn.setText("Browse...");
        browseBtn.setLayoutData(gd);
        browseBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PathManager manager = (PathManager) PathManagerFactory
                        .getPathManager();
                LocalizationContext context = manager.getContext(
                        LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
                LocalizationFile file = manager.getLocalizationFile(context,
                        "collaboration" + File.separator + "sounds"
                                + File.separator);
                if (!file.exists()) {
                    file.getFile().mkdirs();
                }

                FileDialog fileDlg = new FileDialog(Display.getCurrent()
                        .getActiveShell(), SWT.OPEN);
                fileDlg.setFilterPath(file.getFile().getAbsolutePath());
                String filePath = fileDlg.open();
                if (!StringUtil.isEmptyString(filePath)) {
                    soundTxt.setText(filePath);
                }
            }
        });

        gd = new GridData(75, SWT.DEFAULT);
        Button previewBtn = new Button(soundComp, SWT.PUSH);
        previewBtn.setText("Preview");
        previewBtn.setLayoutData(gd);
        previewBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String soundFile = soundTxt.getText().trim();
                if (soundFile.length() > 0) {
                    SoundUtil.playSound(soundFile);
                }
            }
        });

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        Label freqLabel = new Label(shell, SWT.NONE);
        freqLabel.setText("Notifier Frequency: ");
        freqLabel.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        gd.horizontalIndent = 20;
        gl = new GridLayout(2, false);
        Composite freqComp = new Composite(shell, SWT.NONE);
        freqComp.setLayout(gl);
        freqComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        singleRdo = new Button(freqComp, SWT.RADIO);
        singleRdo.setLayoutData(gd);
        singleRdo.setText("Single Instance");
        singleRdo.setSelection(true);

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        recurringRdo = new Button(freqComp, SWT.RADIO);
        recurringRdo.setLayoutData(gd);
        recurringRdo.setText("Recurring");
    }

    private void createBtnComp(Shell shell) {
        Label sl = new Label(shell, SWT.HORIZONTAL | SWT.SEPARATOR);
        sl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite comp = new Composite(shell, SWT.NONE);
        comp.setLayout(gl);
        comp.setLayoutData(gd);

        GridData btnData = new GridData(75, SWT.DEFAULT);
        Button addBtn = new Button(comp, SWT.PUSH);
        addBtn.setText("Add");
        addBtn.setLayoutData(btnData);
        addBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                addNotifierTask();
                returnValue = true;
            }
        });

        btnData = new GridData(75, SWT.DEFAULT);
        Button cancelBtn = new Button(comp, SWT.PUSH);
        cancelBtn.setText("Close");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                returnValue = false;
                close();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#opened()
     */
    @Override
    protected void opened() {
        List<NotifierTask> taskList = NotifierTools.getNotifierTasks();
        for (NotifierTask task : taskList) {
            this.taskMap.put(task.getUserName(), task);
            if (task.getUserName().equals(userCbo.getText())) {
                this.populate(task);
            }
        }
    }

    /**
     * Populate the controls based on the provided task.
     * 
     * @param task
     *            The task to populate the controls from
     */
    private void populate(NotifierTask task) {
        for (Button b : buttonMap.keySet()) {
            if (task.getNotifierList().contains(b.getData())) {
                b.setSelection(true);
            }
        }

        soundTxt.setText(task.getSoundFilePath());

        if (task.isRecurring()) {
            recurringRdo.setSelection(true);
            singleRdo.setSelection(false);
        } else {
            recurringRdo.setSelection(false);
            singleRdo.setSelection(true);
        }
    }

    /**
     * Reset the dialog controls.
     */
    protected void reset() {
        for (Button b : buttonMap.keySet()) {
            b.setSelection(false);
        }

        soundTxt.setText("");
    }

    /**
     * Get the user ids
     * 
     * @return Array of user ids
     */
    private String[] getUserIds() {
        String[] usernames = new String[this.userIds.length];
        for (int i = 0; i < userIds.length; i++) {
            usernames[i] = userIds[i];
        }

        Arrays.sort(usernames);
        return usernames;
    }

    /**
     * Add a notifier task based on the setting in the dialog.
     */
    private void addNotifierTask() {
        NotifierTask task = new NotifierTask();
        task.setUserName(userCbo.getText());
        task.setSoundFilePath(soundTxt.getText());
        for (Button b : buttonMap.keySet()) {
            if (b.getSelection()) {
                task.addNotifier((Notifier) b.getData());
            }
        }

        task.setRecurring(recurringRdo.getSelection());
        this.taskMap.put(task.getUserName(), task);

        updatePreferences();

        MessageBox messageDialog = new MessageBox(this.getShell(), SWT.OK);
        messageDialog.setText("Notifier Saved");
        messageDialog
                .setMessage("The contact notifier was successfully saved.");
        messageDialog.open();
    }

    /**
     * Update the preferences.
     */
    private void updatePreferences() {
        if (this.callback == null) {
            NotifierTools.saveNotifiers(Lists.newArrayList(taskMap.values()));
        } else {
            callback.dialogClosed(taskMap);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        if (this.callback != null) {
            callback.dialogClosed(taskMap);
        }
    }
}
