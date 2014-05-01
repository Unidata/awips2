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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.AlertVizPython;
import com.raytheon.uf.viz.alertviz.INeedsSaveListener;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;

/**
 * This class the controls for a specific priority.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 30 Dec 2010  5133       cjeanbap    Add functionality for restoring/displaying the
 *                                     selected sound file.
 * 14 Dec 2010  5149       cjeanbap    Added Python Action functionality.
 * 03 Mar 2011  8059       rferrel     Fix tooltip for audioChk
 * 24 Mar 2011	5853	   cjeanbap    Check AlertMetadata for Null.
 * 08 Sep 2012  13528     Xiaochuan    Run setNewConfig to update priorities
 * 									   setting.
 * 14 Dec 2012  4827	  Xiaochuan	   Set toolTip on changeActionBtn.
 * 	
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class PriorityControls {
    /**
     * Parent composite.
     */
    private Composite parentComp;

    /**
     * Canvas displaying the priority.
     */
    private Canvas priorityCanvas;

    /**
     * Color of the priority.
     */
    private Color priorityColor;

    /**
     * Foreground color.
     */
    private Color fgColor;

    /**
     * Background color.
     */
    private Color bgColor;

    /**
     * Priority number.
     */
    private int priority = 0;;

    /**
     * Text message check box.
     */
    private Button textChk;

    /**
     * Blink check box.
     */
    private Button blinkChk;

    /**
     * Pop-up check box.
     */
    private Button popupChk;

    /**
     * Audio check box.
     */
    private Button audioChk;

    /**
     * Button used to change the audio file.
     */
    private Button changeAudioBtn;

    /**
     * Log check box.
     */
    private Button logChk;

    /**
     * Label showing the foreground and background text colors.
     */
    private Label textColorLbl;

    /**
     * Button used to display the color dialog.
     */
    private Button changeTextColorBtn;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 60;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 40;

    /**
     * Indent value.
     */
    private final int indent = 30;

    /**
     * Alert metadata.
     */
    private AlertMetadata alertMetadata;

    /**
     * Parent shell.
     */
    private Shell parentShell;

    private Button actionChk;
    
    private AlertVisConfigDlg configDialog;

    /**
     * Button used to change the audio file.
     */
    private Button changeActionBtn;

    private FileSelectDlg actionDlg;

    private FileSelectDlg audioDlg;

    private INeedsSaveListener needsSaveListener;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param parentComp
     *            parent composite.
     * @param priority
     *            Priority number.
     */
    public PriorityControls(Shell parentShell, Composite parentComp,
            int priority, INeedsSaveListener needsSaveListener, AlertVisConfigDlg configDialog ) {
        this.parentComp = parentComp;
        this.priority = priority;
        this.parentShell = parentShell;
        this.configDialog = configDialog;
        
        createPriorityColor();

        this.controlFont = new Font(parentComp.getDisplay(), "Monospace", 10,
                SWT.NORMAL);
        this.needsSaveListener = needsSaveListener;
    }

    /**
     * Create the priority canvas.
     */
    public void createPriorityCanvas() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = CANVAS_WIDTH;
        gd.heightHint = CANVAS_HEIGHT;

        priorityCanvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED);
        priorityCanvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);
        priorityCanvas.setLayoutData(gd);
        priorityCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });

        priorityCanvas.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                priorityColor.dispose();
                controlFont.dispose();
                fgColor.dispose();
                bgColor.dispose();
            }
        });
    }

    /**
     * Create the text check box.
     */
    public void createTextCheckbox() {
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        gd.widthHint = 18;
        gd.horizontalIndent = indent;
        textChk = new Button(parentComp, SWT.CHECK);
        textChk.setLayoutData(gd);
        textChk.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateBlinkButton();
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });
    }

    /**
     * Create the blink text box.
     */
    public void createBlinkCheckbox() {
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        gd.widthHint = 18;
        gd.horizontalIndent = indent;
        blinkChk = new Button(parentComp, SWT.CHECK);
        blinkChk.setEnabled(false);
        blinkChk.setLayoutData(gd);
        blinkChk.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	alertMetadata.setBlink(blinkChk.getSelection());
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });
    }

    /**
     * Create the pop-up check box.
     */
    public void createPopupCheckbox() {
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        gd.widthHint = 18;
        gd.horizontalIndent = indent;
        popupChk = new Button(parentComp, SWT.CHECK);
        popupChk.setLayoutData(gd);
        popupChk.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                alertMetadata.setPopup(popupChk.getSelection());
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });
    }

    /**
     * Create the audio controls.
     */
    public void createAudioControls() {
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        Composite audioComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        audioComp.setLayout(gl);
        audioComp.setLayoutData(gd);

        gd = new GridData(18, SWT.DEFAULT);
        gd.horizontalIndent = indent;
        audioChk = new Button(audioComp, SWT.CHECK);
        audioChk.setLayoutData(gd);
        audioChk.setToolTipText("");
        audioChk.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	updateAudioChangeButton();
                alertMetadata.setAudioEnabled(audioChk.getSelection());
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });

        changeAudioBtn = new Button(audioComp, SWT.PUSH);
        changeAudioBtn.setText("...");
        changeAudioBtn.setToolTipText("Change audio file");
        changeAudioBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                selectAudioFile();
                configDialog.setNewConfig();
            }
        });

        updateAudioChangeButton();
    }

    public void createActionControls() {
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        Composite actionComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        actionComp.setLayout(gl);
        actionComp.setLayoutData(gd);

        gd = new GridData(18, SWT.DEFAULT);
        gd.horizontalIndent = indent;
        actionChk = new Button(actionComp, SWT.CHECK);
        actionChk.setLayoutData(gd);
        actionChk.setToolTipText("Action File not available...");
        actionChk.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateActionChangeButton();
                alertMetadata.setPythonEnabled(actionChk.getSelection());
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });

        changeActionBtn = new Button(actionComp, SWT.PUSH);
        changeActionBtn.setText("...");
        changeActionBtn.setToolTipText("Change action file");
        changeActionBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                selectActionFile();
                configDialog.setNewConfig();
            }
        });

        updateActionChangeButton();
    }

    /**
     * Create the log check box.
     */
    public void createLogCheckbox() {
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        gd.widthHint = 18;
        gd.horizontalIndent = indent;
        logChk = new Button(parentComp, SWT.CHECK);
        logChk.setLayoutData(gd);
        logChk.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                alertMetadata.setLog(logChk.getSelection());
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });

        if (this.priority < Priority.SIGNIFICANT.ordinal()) {
            logChk.setSelection(true);
            logChk.setEnabled(false);
        }
    }

    /**
     * Create the foreground/background color controls.
     */
    public void createFgBgControls() {
        fgColor = new Color(parentComp.getDisplay(), 0, 0, 0);
        bgColor = new Color(parentComp.getDisplay(), 255, 255, 255);

        GridData gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        final Composite fgbgComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        fgbgComp.setLayout(gl);
        fgbgComp.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        textColorLbl = new Label(fgbgComp, SWT.BORDER | SWT.CENTER);
        textColorLbl.setFont(controlFont);
        textColorLbl.setText(" MSG ");
        textColorLbl.setForeground(fgColor);
        textColorLbl.setBackground(bgColor);
        textColorLbl.setLayoutData(gd);

        changeTextColorBtn = new Button(fgbgComp, SWT.PUSH);
        changeTextColorBtn.setText("...");
        changeTextColorBtn.setToolTipText("Change foreground/background color");
        changeTextColorBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	showColorDialog();
                configDialog.setNewConfig();

            }
        });
    }

    /**
     * Draw the canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawCanvas(GC gc) {
        gc.setLineWidth(4);

        gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.setForeground(priorityColor);
        gc.drawRectangle(5, 5, CANVAS_WIDTH - 10, CANVAS_HEIGHT - 10);

        gc.setForeground(parentComp.getDisplay()
                .getSystemColor(SWT.COLOR_BLACK));

        gc.drawString(String.valueOf(priority), CANVAS_WIDTH / 2 - 3, 10, true);
    }

    /**
     * Create the priority color.
     */
    private void createPriorityColor() {
        switch (priority) {
        case 0:
            priorityColor = new Color(parentComp.getDisplay(), 255, 0, 247);
            break;
        case 1:
            priorityColor = new Color(parentComp.getDisplay(), 255, 0, 0);
            break;
        case 2:
            priorityColor = new Color(parentComp.getDisplay(), 255, 165, 0);
            break;
        case 3:
            priorityColor = new Color(parentComp.getDisplay(), 255, 255, 0);
            break;
        case 4:
            priorityColor = new Color(parentComp.getDisplay(), 0, 255, 0);
            break;
        case 5:
            priorityColor = new Color(parentComp.getDisplay(), 0, 0, 255);
            break;
        }
    }

    /**
     * Show the color dialog.
     */
    private void showColorDialog() {
        AlertVisColorDlg colorDlg = new AlertVisColorDlg(parentShell,
                fgColor.getRGB(), bgColor.getRGB());

        Boolean applyColors = (Boolean) colorDlg.open();

        if (applyColors == null) {
            return;
        }

        if (applyColors == true) {
            fgColor.dispose();
            bgColor.dispose();

            fgColor = new Color(parentComp.getDisplay(),
                    colorDlg.getForegroungRGB());
            bgColor = new Color(parentComp.getDisplay(),
                    colorDlg.getBackgroungRGB());

            textColorLbl.setForeground(fgColor);
            textColorLbl.setBackground(bgColor);

            alertMetadata.setForeground(fgColor.getRGB());
            alertMetadata.setBackground(bgColor.getRGB());
            needsSaveListener.saveNeeded(true);
        }
    }

    /**
     * Select the audio file.
     */
    private void selectAudioFile() {
        boolean saveNeeded = false;
        String audioFile = alertMetadata.getAudioFile();

        audioDlg = new FileSelectDlg(parentComp.getShell(),
                SWT.APPLICATION_MODAL, "alertVizAudio",
                new String[] { ".wav" }, audioFile);
        Boolean retVal = (Boolean) audioDlg.open("Audio Selection File",
                audioFile);

        if (retVal == null && audioFile != null) {
            audioChk.setSelection(false);
            changeAudioBtn.setEnabled(false);
            audioChk.setToolTipText("");
        } else if (retVal != null && retVal == true) {
            File selectedFile = audioDlg.getSelectedFile();

            if (selectedFile == null && audioFile == null) {
                // do nothing
            } else if (selectedFile != null) {
                String selectedFileName = selectedFile.getName();
                saveNeeded = !selectedFileName.equals(audioFile);
                alertMetadata.setAudioFile(selectedFileName);
                audioChk.setToolTipText(selectedFileName);
            } else {
                alertMetadata.setAudioFile(null);
                alertMetadata.setAudioEnabled(false);
                audioChk.setToolTipText("System Beep");
                audioChk.setSelection(false);
                changeAudioBtn.setEnabled(false);
            }
        }
        if (saveNeeded) {
            needsSaveListener.saveNeeded(saveNeeded);
        }
    }

    /**
     * Update the change audio button based on the selection.
     */
    private void updateAudioChangeButton() {
        if (audioChk.getSelection() == true) {
            changeAudioBtn.setEnabled(true);
            audioChk.setToolTipText("System Beep");
        } else {
            changeAudioBtn.setEnabled(false);
            audioChk.setToolTipText("");
            if (alertMetadata != null) {
                alertMetadata.setAudioFile(null);
            }
        }
    }

    /**
     * Update the blink check box.
     */
    private void updateBlinkButton() {
        if (textChk.getSelection() == true) {
            blinkChk.setEnabled(true);
        } else {
            blinkChk.setSelection(false);
            blinkChk.setEnabled(false);
        }
        alertMetadata.setText(textChk.getSelection());
        alertMetadata.setBlink(blinkChk.getSelection());
    }

    private void initBlinkButton() {
        if (textChk.getSelection() == true) {
            blinkChk.setEnabled(true);
        } else {
            blinkChk.setSelection(false);
            blinkChk.setEnabled(false);
        }
    }

    /**
     * Set initial text box check state.
     * 
     * @param flag
     *            Selected flag.
     */
    private void setInitialTextChecked(boolean flag) {
        textChk.setSelection(flag);
    }

    /**
     * Set the blink check box state.
     * 
     * @param flag
     *            Selected flag.
     */
    private void setBlinkChecked(boolean flag) {
        if (blinkChk.isEnabled() == true) {
            blinkChk.setSelection(flag);
        } else {
            blinkChk.setSelection(false);
        }
    }

    /**
     * Set the pop-up check box state.
     * 
     * @param flag
     *            Selected flag.
     */
    private void setPopupChecked(boolean flag) {
        popupChk.setSelection(flag);
    }

    /**
     * Set the audio check box state.
     * 
     * @param flag
     *            Selected flag.
     */
    private void setAudioChecked(boolean flag) {
        audioChk.setSelection(flag);
        audioChk.setToolTipText("");
        changeAudioBtn.setEnabled(flag);
    }

    /**
     * Set the audio file.
     * 
     * @param file
     *            The audio file.
     */
    private void setAudioFile(String file) {
        if (file != null) {
            audioChk.setToolTipText(file);
        } else {
            audioChk.setToolTipText("System Beep");
        }
    }

    /**
     * Set the log check box state.
     * 
     * @param flag
     *            Selected flag.
     */
    private void setLogChecked(boolean flag) {
        // TODO : need to use enumeration ordinal...
        if (priority <= Priority.SIGNIFICANT.ordinal()) {
            logChk.setSelection(true);
            logChk.setEnabled(false);
        } else {
            logChk.setSelection(flag);
        }
    }

    /**
     * Set the foreground and background colors.
     * 
     * @param fg
     *            Foreground color.
     * @param bg
     *            Background color.
     */
    private void setFgBgColors(RGB fg, RGB bg) {
        if (fgColor != null) {
            fgColor.dispose();
        }

        if (bgColor != null) {
            bgColor.dispose();
        }

        fgColor = new Color(parentComp.getDisplay(), fg);
        bgColor = new Color(parentComp.getDisplay(), bg);

        textColorLbl.setForeground(fgColor);
        textColorLbl.setBackground(bgColor);
    }

    /**
     * Set the alert metadata.
     * 
     * @param amd
     *            Alert metadata.
     */
    public void setAlertMetadata(AlertMetadata amd) {
        alertMetadata = amd;

        updateControls();
    }

    /**
     * Update the control states.
     */
    private void updateControls() {
        validateAlertMetadata();

        setInitialTextChecked(alertMetadata.isText());
        initBlinkButton();
        setBlinkChecked(alertMetadata.isBlink());
        setPopupChecked(alertMetadata.isPopup());
        setAudioChecked(alertMetadata.isAudioEnabled());
        if (alertMetadata.isAudioEnabled()) {
            String name = alertMetadata.getAudioFile();
            if (name != null) {
                name = (new File(name)).getName();
            }
            setAudioFile(name);
        }
        setLogChecked(alertMetadata.isLog());
        setFgBgColors(alertMetadata.getForeground(),
                alertMetadata.getBackground());
        setActionChecked(alertMetadata.isPythonEnabled());
        setActionFile(alertMetadata.getPythonScript());
        if (actionDlg != null && !actionDlg.isDisposed()) {
            actionDlg.setSelectedFile(alertMetadata.getPythonScript());
        }
        if (audioDlg != null && !audioDlg.isDisposed()) {
            audioDlg.setSelectedFile(alertMetadata.getAudioFile());
        }
    }

    /**
     * Validate the alert metadata.
     */
    private void validateAlertMetadata() {

        if (alertMetadata != null) {
            if (alertMetadata.getForeground() == null) {
                alertMetadata.setForeground(new RGB(0, 0, 0));
            }

            if (alertMetadata.getBackground() == null) {
                alertMetadata.setBackground(new RGB(255, 255, 255));
            }
        }
    }

    /**
     * Set the text field based on selection of the checkbox and script value.
     * 
     * @param script
     *            a String value.
     */
    private void setActionChecked(boolean flag) {
        actionChk.setSelection(flag);
        changeActionBtn.setEnabled(flag);
    }

    /**
     * Select the audio file.
     */
    private void selectActionFile() {
        boolean saveNeeded = false;
        String actionFile = alertMetadata.getPythonScript();

        Map<String, String> filteredExtensions = new HashMap<String, String>();
        filteredExtensions.put(".pyo", ".pyo");
        filteredExtensions.put(".py~", ".py~");

        actionDlg = new FileSelectDlg(parentComp.getShell(),
                SWT.APPLICATION_MODAL, AlertVizPython.ALERTVIZ_PYTHON_PATH,
                new String[] { ".py", "*", "*.*" }, false, filteredExtensions);

        actionDlg.setSelectedFile(actionFile);
        Boolean retVal = (Boolean) actionDlg.open("Action Selection File",
                actionFile);

        if (retVal == null && actionFile != null) {
            actionChk.setSelection(false);
            changeActionBtn.setEnabled(false);
        } else if (retVal != null && retVal == true) {
            File selectedFile = actionDlg.getSelectedFile();

            if (selectedFile == null && actionFile == null) {
                // do nothing
            } else if (selectedFile != null) {
                String selectedFileName = selectedFile.getName();
                saveNeeded = !selectedFileName.equals(actionFile);
                alertMetadata.setPythonScript(selectedFileName);
                changeActionBtn.setToolTipText(selectedFileName);
                
            } else {
                alertMetadata.setAudioFile(null);
                alertMetadata.setAudioEnabled(false);
                actionChk.setToolTipText("Action file not available...");
                actionChk.setSelection(false);
                changeActionBtn.setEnabled(false);
            }
        }
        if (saveNeeded) {
            needsSaveListener.saveNeeded(saveNeeded);
        }
    }

    /**
     * Set the action file.
     * 
     * @param file
     *            The audio file.
     */
    private void setActionFile(String file) {
        if (file != null) {
            actionChk.setToolTipText(file);
        } else {
            actionChk.setToolTipText("Action file not available...");
        }
    }

    /**
     * Update the change audio button based on the selection.
     */
    private void updateActionChangeButton() {
        if (actionChk.getSelection() == true) {
            changeActionBtn.setEnabled(true);
        } else {
            changeActionBtn.setEnabled(false);
            actionChk.setToolTipText("Action file not available...");
            if (alertMetadata != null) {
                alertMetadata.setPythonScript(null);
                alertMetadata.setPythonEnabled(false);
            }
        }
    }
}
