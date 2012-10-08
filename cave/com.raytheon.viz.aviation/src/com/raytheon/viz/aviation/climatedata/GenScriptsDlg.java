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
package com.raytheon.viz.aviation.climatedata;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXB;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.aviation.xml.ClimateDataFTPArgs;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * This class displays the generate scripts dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2009 #3438      lvenable     Initial creation
 * Jan 19, 2011 #4864      rferrel      FTP site information is
 *                                      now configurable.
 * May 10, 2011 #9059      rferrel      Fixed script instructions
 *                                      to the correct directory.
 * May 10, 2011 #8844      rferrel      Display error message when unable
 *                                      to save a script.
 * Oct 08, 2012 #1229      rferrel      Made non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class GenScriptsDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GenScriptsDlg.class);

    /**
     * Text font.
     */
    private Font textFont;

    /**
     * Dark blue background color for styled text controls.
     */
    private Color darkBlueBgColor;

    /**
     * Script styled text.
     */
    private StyledText scriptST;

    /**
     * Linux FTP button.
     */
    private Button linuxFtpBtn;

    /**
     * Windows FTP button.
     */
    private Button windowsFtpBtn;

    /**
     * Save button.
     */
    private Button saveBtn;

    /**
     * Bypass button.
     */
    private Button bypassBtn;

    private String ftpSite;

    private String ftpDataDir;

    private String ftpIshDir;

    private String ftpUser;

    private String ftpPassword;

    private String style;

    private boolean isLinuxScript;

    /**
     * Constructor.
     * 
     * @param style
     * @param parentShell
     *            Parent shell.
     */
    public GenScriptsDlg(Shell parentShell, String style) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("Generate Scripts");
        this.style = style;
        initFtpArgs();
    }

    public void preOpened() {
        setReturnValue(Boolean.TRUE);
        super.preOpened();
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void disposed() {
        textFont.dispose();
        darkBlueBgColor.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        textFont = new Font(getDisplay(), "Sans", 10, SWT.BOLD);
        darkBlueBgColor = new Color(getDisplay(), 82, 107, 129);

        createScriptTextControl();

        createBottomButtons();
    }

    /**
     * Create the script text control.
     */
    private void createScriptTextControl() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 600;
        gd.heightHint = 350;
        scriptST = new StyledText(shell, SWT.BORDER | SWT.MULTI);
        scriptST.setLayoutData(gd);
        scriptST.setBackground(darkBlueBgColor);
        scriptST.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        scriptST.setFont(textFont);
        scriptST.setEditable(false);
    }

    /**
     * Create the bottom control buttons.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(5, false));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(150, SWT.DEFAULT);
        linuxFtpBtn = new Button(buttonComp, SWT.NONE);
        linuxFtpBtn.setText("Linux FTP Script");
        linuxFtpBtn.setLayoutData(gd);
        linuxFtpBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                linuxScript();
                saveBtn.setEnabled(true);
            }
        });

        gd = new GridData(150, SWT.DEFAULT);
        windowsFtpBtn = new Button(buttonComp, SWT.NONE);
        windowsFtpBtn.setText("Windows FTP Script");
        windowsFtpBtn.setLayoutData(gd);
        windowsFtpBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                windowsScript();
                saveBtn.setEnabled(true);
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        saveBtn = new Button(buttonComp, SWT.NONE);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveScript();
            }
        });
        saveBtn.setEnabled(false);

        gd = new GridData(80, SWT.DEFAULT);
        bypassBtn = new Button(buttonComp, SWT.NONE);
        bypassBtn.setText("Bypass");
        bypassBtn.setLayoutData(gd);
        bypassBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(false);
                shell.dispose();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.NONE);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(true);
                shell.dispose();
            }
        });
    }

    private String fileYear(String f) {
        return f.split("-")[2].substring(0, 4);
    }

    @SuppressWarnings("unchecked")
    private void linuxScript() {
        StringBuilder scriptMsg = new StringBuilder("#!/bin/bash\n\n");

        String wgetStart = "wget ftp://" + ftpSite + "/";

        if (style.equals("inv") || style.equals("his")) {
            String fname = null;

            if (style.equals("inv")) {
                fname = "ish-inventory.txt";
            } else {
                fname = "ish-history.txt";
            }

            scriptMsg.append(wgetStart).append(ftpIshDir).append("/")
                    .append(fname).append("\n");
        } else {
            Map<String, Object> stationsMap = ClimateDataManager.getInstance()
                    .getStationsMap();
            Set<String> keys = stationsMap.keySet();

            for (String key : keys) {
                List<String> fileList = (List<String>) stationsMap.get(key);

                for (String file : fileList) {
                    scriptMsg.append(wgetStart).append(ftpDataDir).append("/")
                            .append(fileYear(file)).append("/").append(file)
                            .append("\n");
                }
            }
        }

        scriptST.setText(scriptMsg.toString());
        isLinuxScript = true;
    }

    @SuppressWarnings("unchecked")
    private void windowsScript() {
        StringBuilder scriptMsg = new StringBuilder();

        scriptMsg.append("@ECHO OFF\r\n>  script.ftp ECHO open ")
                .append(ftpSite).append("\r\n>> script.ftp ECHO USER ")
                .append(ftpUser).append("\r\n>> script.ftp ECHO PASS ")
                .append(ftpPassword).append("\r\n>> script.ftp ECHO cd ");

        String scriptMsgB = "\r\n>> script.ftp ECHO bin\r\n";

        if (style.equals("inv") || style.equals("his")) {
            scriptMsg.append(ftpIshDir).append(scriptMsgB);

            String fname = null;

            if (style.equals("inv")) {
                fname = "ish-inventory.txt";
            } else {
                fname = "ish-history.txt";
            }

            scriptMsg.append(">> script.ftp ECHO get ").append(fname)
                    .append("\r\n");
        } else {
            scriptMsg.append(ftpDataDir).append(scriptMsgB);

            Map<String, Object> stationsMap = ClimateDataManager.getInstance()
                    .getStationsMap();
            Set<String> keys = stationsMap.keySet();

            for (String key : keys) {
                List<String> fileList = (List<String>) stationsMap.get(key);

                for (String file : fileList) {
                    scriptMsg.append(">> script.ftp ECHO get ")
                            .append(fileYear(file)).append("/").append(file)
                            .append("\r\n");
                }
            }
        }

        scriptMsg
                .append(">> script.ftp ECHO BYE\r\nFTP -v -n -s:script.ftp\r\nDEL script.ftp\r\n");
        scriptST.setText(scriptMsg.toString());
        isLinuxScript = false;
    }

    private void saveScript() {
        try {
            StringBuilder scriptMsg = new StringBuilder(scriptST.getText());
            String scriptFile;
            String ext;
            String targetPath = ClimateDataPython.getIshFilePath();
            String instructions;

            if (style.equals("data")) {
                instructions = "==================================================\n";
                instructions += "Please move these just-downloaded NCDC files to an\n";
                instructions += "AWIPS machine into the " + targetPath
                        + "/tmp\n";
                instructions += "directory. When finished, press the \'Process Data\'\n";
                instructions += "button on the Climate Data Update GUI to resume\n";
                instructions += "further processing of the data into a HDF5 climate\n";
                instructions += "file.\n";
                instructions += "==================================================\n";
            } else {
                instructions = "==================================================\n";
                instructions += "Please move this just-downloaded NCDC file to an\n";
                instructions += "AWIPS machine into the " + targetPath
                        + "/etc\n";
                instructions += "directory.\n";
                instructions += "==================================================\n";
            }

            String[] inst = instructions.split("\n");

            if (isLinuxScript) {
                ext = ".sh";

                for (String s : inst) {
                    scriptMsg.append("\necho ").append(s);
                }
                scriptMsg.append("\n");
            } else {
                ext = ".bat";

                for (String s : inst) {
                    scriptMsg.append("\r\nECHO ").append(s);
                }

                scriptMsg.append("\r\nPAUSE");
            }

            if (style.equals("data")) {
                StringBuilder sb = new StringBuilder();
                sb.append("getNCDCData");
                Map<String, Object> stationsMap = ClimateDataManager
                        .getInstance().getStationsMap();
                Set<String> ks = stationsMap.keySet();
                String[] keys = new String[ks.size()];
                ks.toArray(keys);
                Arrays.sort(keys);
                for (String ident : keys) {
                    sb.append("-").append(ident);

                }
                sb.append(ext);
                scriptFile = sb.toString();
            } else {
                scriptFile = "getNCDC-ISHFile" + ext;
            }

            FileDialog dlg = new FileDialog(shell, SWT.SAVE);
            dlg.setFileName(scriptFile);
            String filename = dlg.open();
            if (filename == null) {
                return;
            }

            File file = new File(filename);
            FileWriter writer;
            try {
                writer = new FileWriter(file);
                BufferedWriter buf = new BufferedWriter(writer);

                buf.write(scriptMsg.toString());

                buf.close();
                writer.close();

                file.setReadable(true);
                file.setWritable(true);
                file.setExecutable(true);
            } catch (IOException e) {
                MessageBox msgBox = new MessageBox(shell, SWT.ERROR);
                msgBox.setText("Error Saving File");
                msgBox.setMessage(e.getMessage());
                msgBox.open();
            }
        } catch (VizException e) {
            e.printStackTrace();
        } finally {
            shell.dispose();
        }
    }

    private void initFtpArgs() {
        StringBuilder path = new StringBuilder();
        path.append("aviation").append(File.separator).append("config")
                .append(File.separator).append("scripts")
                .append(File.separator).append("ClimateDataFTPArgs.xml");
        IPathManager pm = PathManagerFactory.getPathManager();
        File fname = pm.getStaticFile(path.toString());
        try {
            ClimateDataFTPArgs cdArgs = JAXB.unmarshal(fname,
                    ClimateDataFTPArgs.class);
            ftpSite = cdArgs.getSite();

            ftpDataDir = normalizeDir(cdArgs.getDataDir());

            ftpIshDir = normalizeDir(cdArgs.getIshDir());

            ftpUser = cdArgs.getUser();

            ftpPassword = cdArgs.getPassword();
        } catch (RuntimeException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        }
    }

    /**
     * Normalize directory path to have no leading or trailing slashes.
     * 
     * @param path
     *            - directory path to normalize
     * @return nPath
     */
    private String normalizeDir(String path) {
        if (path == null || path.length() == 0) {
            return "";
        }

        StringBuilder nPath = new StringBuilder();

        String[] dirs = path.split("[\\\\,/]+"); // Win32
        int index = 0;
        if (dirs[index].length() == 0) {
            index++;
        }

        int end = dirs.length - 1;
        while (index < end) {
            nPath.append(dirs[index]).append("/");
            index++;
        }
        nPath.append(dirs[index]);
        return nPath.toString();
    }
}
