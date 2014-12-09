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
package com.raytheon.uf.viz.collaboration.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.colordialog.ColorWheelComp;
import com.raytheon.viz.ui.dialogs.colordialog.IColorWheelChange;

/**
 * A dialog that displays a label with settable foreground and background colors
 * using a color control.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2014  3709       lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ForegroundBackgroundColorDlg extends CaveSWTDialog implements
        IColorWheelChange {

    /** Color wheel composite. */
    private ColorWheelComp colorWheelComp;

    /** Foreground color. */
    private Color foregroundClr = null;

    /** Background color. */
    private Color backgroundClr = null;

    /** Foreground/Background label control. */
    private Label fgbgLabel = null;

    /** Fond for the foreground/background label. */
    private Font labelFont = null;

    private Button foregroundRdo;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public ForegroundBackgroundColorDlg(Shell parentShell) {
        this(parentShell, null, null);
    }

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param fgRGB
     *            Foreground RGB.
     * @param bgRGB
     *            Background RGB.
     */
    public ForegroundBackgroundColorDlg(Shell parentShell, RGB fgRGB, RGB bgRGB) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN, CAVE.DO_NOT_BLOCK
                | CAVE.PERSPECTIVE_INDEPENDENT);
        setText("Foreground/Background Color Chooser");

        /*
         * If the foreground RGB is null then set it to a blue color.
         */
        if (fgRGB == null) {
            foregroundClr = new Color(parentShell.getDisplay(), new RGB(0, 0,
                    255));
        } else {
            foregroundClr = new Color(parentShell.getDisplay(), fgRGB);
        }

        /*
         * If the background RGB is null then set it to a white color.
         */
        if (bgRGB == null) {
            backgroundClr = new Color(parentShell.getDisplay(), new RGB(255,
                    255, 255));
        } else {
            backgroundClr = new Color(parentShell.getDisplay(), bgRGB);
        }
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.verticalSpacing = 3;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        return gd;
    }

    @Override
    protected void disposed() {
        if (foregroundClr != null) {
            foregroundClr.dispose();
        }

        if (backgroundClr != null) {
            backgroundClr.dispose();
        }

        if (labelFont != null) {
            labelFont.dispose();
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createColorWheelControl();
        createColorControls();
        addSeparator();
        createBottomButtons();

        colorWheelComp.setColor(foregroundClr.getRGB());
    }

    /**
     * Create the color wheel controls.
     */
    private void createColorWheelControl() {
        colorWheelComp = new ColorWheelComp(shell, this, " Color Chooser: ",
                true);
    }

    /**
     * Create the color controls.
     */
    private void createColorControls() {
        Composite colorControlComp = new Composite(shell, SWT.NONE);
        colorControlComp.setLayout(new GridLayout(3, false));
        colorControlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                true, false));

        /*
         * Foreground/background radio buttons.
         */
        foregroundRdo = new Button(colorControlComp, SWT.RADIO);
        foregroundRdo.setText("Foreground Color");
        foregroundRdo.setSelection(true);
        foregroundRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                colorWheelComp.setColor(foregroundClr.getRGB());
            }
        });

        GridData gd = new GridData();
        gd.horizontalIndent = 13;
        Button backgroundRdo = new Button(colorControlComp, SWT.RADIO);
        backgroundRdo.setText("Background Color");
        backgroundRdo.setLayoutData(gd);
        backgroundRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                colorWheelComp.setColor(backgroundClr.getRGB());
            }
        });

        /*
         * Label displaying the foreground/background colors.
         */
        gd = new GridData();
        gd.horizontalIndent = 13;
        fgbgLabel = new Label(colorControlComp, SWT.BORDER);
        FontData fd = fgbgLabel.getFont().getFontData()[0];
        fd.setHeight(16);
        fd.setStyle(SWT.BOLD);
        labelFont = new Font(getDisplay(), fd);
        fgbgLabel.setFont(labelFont);
        fgbgLabel.setText("    Sample Text    ");
        fgbgLabel.setLayoutData(gd);

        fgbgLabel.setForeground(foregroundClr);
        fgbgLabel.setBackground(backgroundClr);
    }

    /**
     * Create the bottom OK/Cancel buttons.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        int buttonWidth = 70;

        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText(" OK ");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                RGB[] rgbArray = new RGB[] { foregroundClr.getRGB(),
                        backgroundClr.getRGB() };
                setReturnValue(rgbArray);
                close();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText(" Cancel ");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(null);
                close();
            }
        });
    }

    /**
     * Add a separator line to the dialog.
     */
    private void addSeparator() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.colordialog.IColorWheelChange#colorChange
     * (org.eclipse.swt.graphics.RGB, java.lang.String)
     */
    @Override
    public void colorChange(RGB rgb, String colorWheelTitle) {
        if (foregroundRdo.getSelection()) {
            foregroundClr.dispose();
            foregroundClr = new Color(getDisplay(), rgb);
            fgbgLabel.setForeground(foregroundClr);
        } else {
            backgroundClr.dispose();
            backgroundClr = new Color(getDisplay(), rgb);
            fgbgLabel.setBackground(backgroundClr);
        }

    }
}
