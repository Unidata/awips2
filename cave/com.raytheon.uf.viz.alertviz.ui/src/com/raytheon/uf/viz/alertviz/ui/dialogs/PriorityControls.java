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

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.python.PyCacheUtil;
import com.raytheon.uf.viz.alertviz.INeedsSaveListener;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.core.RGBColors;

/**
 * This class the controls for a specific priority.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Oct 05, 2008           lvenable   Initial creation.
 * Dec 30, 2010  5133     cjeanbap   Add functionality for restoring/displaying
 *                                   the selected sound file.
 * Dec 14, 2010  5149     cjeanbap   Added Python Action functionality.
 * Mar 03, 2011  8059     rferrel    Fix tooltip for audioChk
 * Mar 24, 2011  5853     cjeanbap   Check AlertMetadata for Null.
 * Sep 08, 2012  13528    Xiaochuan  Run setNewConfig to update priorities
 *                                   setting.
 * Dec 14, 2012  4827     Xiaochuan  Set toolTip on changeActionBtn.
 * Mar 18, 2015  4234     njensen    Remove reference to dead code
 * Sep 24, 2018  7481     randerso   Code and GUI cleanup.
 * Sep 25, 2018  7458     randerso   Ensure action cannot be enabled with
 *                                   setting a script file
 * Oct 10, 2018  7511     randerso   Changed tool tips for consistency.
 * Nov 02, 2018  7600     randerso   Changes to support standard script files
 *                                   for AlertViz actions.
 * Nov 05, 2018  7509     randerso   Remove Log check boxes
 * Nov 13, 2018  7512     randerso   Moved AlertViz audio files
 * Sep 12, 2019  7917     tgurney    Update handling of pyc files for Python 3
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class PriorityControls {

    private static final RGB[] PRIORITY_COLORS = new RGB[] {
            RGBColors.getRGBColor("magenta"), RGBColors.getRGBColor("red"),
            RGBColors.getRGBColor("orange"), RGBColors.getRGBColor("yellow"),
            RGBColors.getRGBColor("green"), RGBColors.getRGBColor("blue"), };

    /**
     * Canvas width.
     */
    private static final int CANVAS_WIDTH = 60;

    /**
     * Canvas height.
     */
    private static final int CANVAS_HEIGHT = 30;

    /**
     * Indent value.
     */
    private static final int indent = 30;

    /**
     * Parent composite.
     */
    private final Composite parentComp;

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
    private int priority = 0;

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
     * Button used to change the image file.
     */
    private Button changeImageBtn;

    /**
     * Audio check box.
     */
    private Button audioChk;

    /**
     * Button used to change the audio file.
     */
    private Button changeAudioBtn;

    /**
     * Label showing the foreground and background text colors.
     */
    private Text textColorLbl;

    /**
     * Alert metadata.
     */
    private AlertMetadata alertMetadata;

    /**
     * Parent shell.
     */
    private final Shell parentShell;

    private Button actionChk;

    private final AlertVisConfigDlg configDialog;

    /**
     * Button used to change the audio file.
     */
    private Button changeActionBtn;

    private FileSelectDlg actionDlg;

    private FileSelectDlg audioDlg;

    private FileSelectDlg imageDlg;

    private final INeedsSaveListener needsSaveListener;

    /**
     * Constructor.
     *
     * @param parentShell
     *            Parent shell.
     * @param parentComp
     *            parent composite.
     * @param priority
     *            Priority number.
     * @param needsSaveListener
     * @param configDialog
     */
    public PriorityControls(Shell parentShell, Composite parentComp,
            int priority, INeedsSaveListener needsSaveListener,
            AlertVisConfigDlg configDialog) {
        this.parentComp = parentComp;
        this.priority = priority;
        this.parentShell = parentShell;
        this.configDialog = configDialog;

        this.needsSaveListener = needsSaveListener;
    }

    /**
     * Create the priority canvas.
     */
    public void createPriorityCanvas() {
        priorityColor = new Color(parentComp.getDisplay(),
                PRIORITY_COLORS[this.priority]);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = CANVAS_WIDTH;
        gd.heightHint = CANVAS_HEIGHT;

        Canvas priorityCanvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED);
        priorityCanvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);
        priorityCanvas.setLayoutData(gd);
        priorityCanvas.addPaintListener(e -> drawCanvas(e.gc));

        priorityCanvas.addDisposeListener(e -> priorityColor.dispose());
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
            @Override
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
            @Override
            public void widgetSelected(SelectionEvent event) {
                alertMetadata.setBlink(blinkChk.getSelection());
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });
    }

    /**
     * Create the pop-up controls.
     */
    public void createPopupControls() {
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        Composite popupComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        popupComp.setLayout(gl);
        popupComp.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        gd.widthHint = 18;
        gd.horizontalIndent = indent;
        popupChk = new Button(popupComp, SWT.CHECK);
        popupChk.setLayoutData(gd);
        popupChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateImageChangeButton();
                alertMetadata.setPopup(popupChk.getSelection());
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.heightHint = popupChk.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
        changeImageBtn = new Button(popupComp, SWT.PUSH);
        changeImageBtn.setLayoutData(gd);
        changeImageBtn.setText("...");
        changeImageBtn.setToolTipText("Change image file");
        changeImageBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectImageFile();
                configDialog.setNewConfig();
            }
        });

        updateImageChangeButton();
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
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateAudioChangeButton();
                alertMetadata.setAudioEnabled(audioChk.getSelection());
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.heightHint = audioChk.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
        changeAudioBtn = new Button(audioComp, SWT.PUSH);
        changeAudioBtn.setLayoutData(gd);
        changeAudioBtn.setText("...");
        changeAudioBtn.setToolTipText("Change audio file");
        changeAudioBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectAudioFile();
                configDialog.setNewConfig();
            }
        });

        updateAudioChangeButton();
    }

    /**
     * Create the action controls
     */
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
        actionChk.setToolTipText("");
        actionChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateActionChangeButton();
                if (actionChk.getSelection()) {
                    selectActionFile();
                } else {
                    alertMetadata.setAction(null);
                    setActionFile(null);
                    changeActionBtn.setEnabled(false);
                }
                configDialog.setNewConfig();
                needsSaveListener.saveNeeded(true);
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.heightHint = actionChk.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
        changeActionBtn = new Button(actionComp, SWT.PUSH);
        changeActionBtn.setLayoutData(gd);
        changeActionBtn.setText("...");
        changeActionBtn.setToolTipText("Change action file");
        changeActionBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectActionFile();
                configDialog.setNewConfig();
            }
        });

        updateActionChangeButton();
    }

    /**
     * Create the foreground/background color controls.
     */
    public void createFgBgControls() {
        GridData gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        final Composite fgbgComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        fgbgComp.setLayout(gl);
        fgbgComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        textColorLbl = new Text(fgbgComp,
                SWT.READ_ONLY | SWT.BORDER | SWT.CENTER);
        textColorLbl.setLayoutData(gd);

        Font controlFont = new Font(parentComp.getDisplay(), "Monospace", 10,
                SWT.NORMAL);
        textColorLbl.setFont(controlFont);
        textColorLbl.setText("MSG");

        fgColor = new Color(parentComp.getDisplay(), 0, 0, 0);
        bgColor = new Color(parentComp.getDisplay(), 255, 255, 255);
        textColorLbl.setForeground(fgColor);
        textColorLbl.setBackground(bgColor);
        textColorLbl.addDisposeListener(e -> {
            controlFont.dispose();
            fgColor.dispose();
            bgColor.dispose();
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.heightHint = textColorLbl.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
        Button changeTextColorBtn = new Button(fgbgComp, SWT.PUSH);
        changeTextColorBtn.setLayoutData(gd);
        changeTextColorBtn.setText("...");
        changeTextColorBtn.setToolTipText("Change foreground/background color");
        changeTextColorBtn.addSelectionListener(new SelectionAdapter() {
            @Override
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

        gc.setBackground(parentComp.getDisplay()
                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        gc.setForeground(priorityColor);
        gc.drawRectangle(5, 5, CANVAS_WIDTH - 10, CANVAS_HEIGHT - 10);

        gc.setForeground(
                parentComp.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        String text = String.valueOf(priority);
        Point extent = gc.textExtent(text);
        gc.drawString(text, (CANVAS_WIDTH - extent.x) / 2,
                (CANVAS_HEIGHT - extent.y) / 2, true);
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

        if (applyColors) {
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

        audioDlg = new FileSelectDlg(parentComp.getShell(), "Audio",
                LocalizationUtil.join("alertViz", "audio"),
                new String[] { ".wav" }, null);
        audioDlg.setSelectedFile(audioFile);
        boolean retVal = audioDlg.open();

        if (retVal) {
            String selectedFile = audioDlg.getSelectedFile();
            saveNeeded = !StringUtils.equals(selectedFile, audioFile);

            alertMetadata.setAudioFile(selectedFile);

            if (selectedFile != null) {
                audioChk.setToolTipText(selectedFile);
            } else {
                if (alertMetadata.isAudioEnabled()) {
                    audioChk.setToolTipText("System Beep");
                } else {
                    audioChk.setToolTipText("");
                }
            }
        }
        if (saveNeeded) {
            needsSaveListener.saveNeeded(saveNeeded);
        }
    }

    /**
     * Select the audio file.
     */
    private void selectImageFile() {
        boolean saveNeeded = false;
        String imageFile = alertMetadata.getImage();

        imageDlg = new FileSelectDlg(parentComp.getShell(), "Image",
                LocalizationUtil.join("alertViz", "images"),
                new String[] { ".png", ".gif", ".bmp" }, null);

        imageDlg.setSelectedFile(imageFile);
        boolean retVal = imageDlg.open();

        if (retVal) {
            String selectedFile = imageDlg.getSelectedFile();
            saveNeeded = !StringUtils.equals(selectedFile, imageFile);

            alertMetadata.setImage(selectedFile);

            if (selectedFile != null) {
                popupChk.setToolTipText(selectedFile);
            } else {
                popupChk.setToolTipText("");
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
        if (audioChk.getSelection()) {
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
     * Update the change image button based on the selection.
     */
    private void updateImageChangeButton() {
        if (popupChk.getSelection()) {
            changeImageBtn.setEnabled(true);
            popupChk.setToolTipText("");
        } else {
            changeImageBtn.setEnabled(false);
            popupChk.setToolTipText("");
            if (alertMetadata != null) {
                alertMetadata.setImage(null);
            }
        }
    }

    /**
     * Update the blink check box.
     */
    private void updateBlinkButton() {
        if (textChk.getSelection()) {
            blinkChk.setEnabled(true);
        } else {
            blinkChk.setSelection(false);
            blinkChk.setEnabled(false);
        }
        alertMetadata.setText(textChk.getSelection());
        alertMetadata.setBlink(blinkChk.getSelection());
    }

    private void initBlinkButton() {
        if (textChk.getSelection()) {
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
        if (blinkChk.isEnabled()) {
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
        popupChk.setToolTipText("");
        changeImageBtn.setEnabled(flag);
    }

    /**
     * Set the image file.
     *
     * @param file
     *            The image file.
     */
    private void setImageFile(String file) {
        if (file != null) {
            popupChk.setToolTipText(file);
        } else {
            popupChk.setToolTipText("");
        }
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

        setImageFile(alertMetadata.getImage());

        if (alertMetadata.isAudioEnabled()) {
            String name = alertMetadata.getAudioFile();
            if (name != null) {
                name = new File(name).getName();
            }
            setAudioFile(name);
        }

        setFgBgColors(alertMetadata.getForeground(),
                alertMetadata.getBackground());
        setActionFile(alertMetadata.getAction());

        if (actionDlg != null && !actionDlg.isDisposed()) {
            actionDlg.setSelectedFile(alertMetadata.getAction());
        }
        if (audioDlg != null && !audioDlg.isDisposed()) {
            audioDlg.setSelectedFile(alertMetadata.getAudioFile());
        }
        if (imageDlg != null && !imageDlg.isDisposed()) {
            imageDlg.setSelectedFile(alertMetadata.getImage());
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
     * Select the audio file.
     */
    private void selectActionFile() {
        boolean saveNeeded = false;
        String actionFile = alertMetadata.getAction();

        String[] filteredExtensions = { ".py~",
                PyCacheUtil.COMPILED_FILE_EXTENSION };

        actionDlg = new FileSelectDlg(parentComp.getShell(), "Action",
                LocalizationUtil.join("alertViz", "actions"), null,
                filteredExtensions);

        actionDlg.setSelectedFile(actionFile);
        boolean retVal = actionDlg.open();

        if (retVal) {
            String selectedFile = actionDlg.getSelectedFile();
            saveNeeded = !StringUtils.equals(selectedFile, actionFile);

            if (selectedFile != null) {
                alertMetadata.setAction(selectedFile);
                actionChk.setToolTipText(selectedFile);

            } else {
                alertMetadata.setAction(null);
                actionChk.setToolTipText("");
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
        actionChk.setSelection(file != null);
        changeActionBtn.setEnabled(file != null);

        if (file != null) {
            actionChk.setToolTipText(file);
        } else {
            actionChk.setToolTipText("");
        }
    }

    /**
     * Update the change action button based on the selection.
     */
    private void updateActionChangeButton() {
        if (actionChk.getSelection()) {
            changeActionBtn.setEnabled(true);
        } else {
            changeActionBtn.setEnabled(false);
            actionChk.setToolTipText("");
            if (alertMetadata != null) {
                alertMetadata.setAction(null);
            }
        }
    }
}
