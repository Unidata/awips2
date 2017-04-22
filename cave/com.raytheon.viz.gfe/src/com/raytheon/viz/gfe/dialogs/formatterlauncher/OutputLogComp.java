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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.gfe.tasks.AbstractGfeTask;
import com.raytheon.viz.gfe.tasks.AbstractGfeTask.TaskStatus;
import com.raytheon.viz.gfe.tasks.TaskManager;

/**
 * Composite containing the output log controls..
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 18, 2008  ###      lvenable  Initial creation
 * May 29, 2014  2841     randerso  Fix NPE when formatter fails to queue
 * Nov 12, 2015  5106     dgilling  Disable word-wrap to improve performance for
 *                                  large log files.
 * Dec 08, 2016  6026     randerso  Fixed issues saving log files  
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class OutputLogComp extends Composite {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Output log styled text control.
     */
    private StyledText outputLogST;

    /**
     * Text font.
     */
    private Font textFont;

    /**
     * Log file name.
     */
    private String fileName = null;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public OutputLogComp(Composite parent) {
        super(parent, SWT.BORDER);

        this.parent = parent;

        init();
    }

    /**
     * Initialize the composite.
     */
    private void init() {
        textFont = new Font(parent.getDisplay(), "Monospace", 12, SWT.NORMAL);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        setLayout(gl);
        setLayoutData(gd);

        initializeComponents();

        this.pack();

        addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent arg0) {
                textFont.dispose();
            }
        });
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        outputLogST = new StyledText(this, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        outputLogST.setFont(textFont);
        outputLogST.setEditable(false);
        outputLogST.setLayoutData(gd);
        outputLogST.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_BLACK));
        outputLogST.setForeground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WHITE));

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Button saveAsBtn = new Button(this, SWT.PUSH);
        saveAsBtn.setText("Save As...");
        saveAsBtn.setLayoutData(gd);
        saveAsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveOutputLog();
            }
        });
    }

    /**
     * Save the output log.
     */
    private void saveOutputLog() {
        FileDialog dialog = new FileDialog(parent.getShell(), SWT.SAVE);
        String[] filterNames = new String[] { "Log Files (*.log)" };
        String[] filterExtensions = new String[] { "*.log" };
        String filterPath = System.getProperty("user.home");
        dialog.setFilterNames(filterNames);
        dialog.setFilterExtensions(filterExtensions);
        dialog.setFilterPath(filterPath);
        dialog.setFileName(fileName);
        String newName = dialog.open();

        if (newName != null) {
            try (BufferedWriter out = new BufferedWriter(
                    new FileWriter(newName))) {
                out.write(outputLogST.getText());
            } catch (IOException e) {
                statusHandler.error("Error saving log file " + newName + ": "
                        + e.getLocalizedMessage(), e);
            }
        }
    }

    /**
     * Show task log
     * 
     * @param taskName name of task
     */
    public void showLog(String taskName) {
        StringBuilder buffer = new StringBuilder();
        AbstractGfeTask task = TaskManager.getInstance().getTask(taskName);
        File file = null;
        if ((task != null) && ((file = task.getLogFile()) != null)) {
            this.fileName = taskName;
            if ((file != null) && file.exists()) {
                try (BufferedReader in = new BufferedReader(
                        new FileReader(file))) {
                    String str;
                    while ((str = in.readLine()) != null) {
                        buffer.append(str + "\n");
                    }
                } catch (IOException e) {
                    statusHandler.error("Error loading log file " + file + ": "
                            + e.getLocalizedMessage(), e);
                }
            } else {
                buffer.append("Log file " + file + " not found");
            }
        } else {
            if ((task != null)
                    && task.getStatus().equals(TaskStatus.CANCELED)) {
                buffer.append(taskName + " was canceled\n");
            }
            buffer.append("No log file found for " + taskName);
        }

        outputLogST.setText(buffer.toString());
    }
}
