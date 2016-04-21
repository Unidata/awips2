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
package com.raytheon.uf.viz.collaboration.ui.colors;

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

import com.raytheon.uf.viz.collaboration.display.data.UserColorInfo;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.colordialog.ColorWheelComp;
import com.raytheon.viz.ui.dialogs.colordialog.IColorWheelChange;

/**
 * A dialog that displays a label with settable foreground color using a color
 * control.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 09, 2015 3709       bclement     Initial creation, logic from ForegroundBackgroundColorDlg
 * Jan 13, 2015 3709       bclement     return UserColorInfo instead of RGB
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ForegroundColorDlg extends CaveSWTDialog implements
        IColorWheelChange {

    /** Color wheel composite. */
    protected ColorWheelComp colorWheelComp;

    /** Foreground color. */
    protected Color foregroundClr = null;

    /** preview label control. */
    protected Label previewLabel = null;

    /** Font for the preview label. */
    protected Font labelFont = null;

    protected final String description;

    protected Label descriptionLabel = null;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public ForegroundColorDlg(Shell parentShell, String description) {
        this(parentShell, description, null);
    }

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param fgRGB
     *            Foreground RGB.
     */
    public ForegroundColorDlg(Shell parentShell, String description, RGB fgRGB) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN, CAVE.DO_NOT_BLOCK
                | CAVE.PERSPECTIVE_INDEPENDENT);
        setText("Foreground Color Chooser");
        this.description = description;
        /*
         * If the foreground RGB is null then set it to a blue color.
         */
        if (fgRGB == null) {
            foregroundClr = new Color(parentShell.getDisplay(), new RGB(0, 0,
                    255));
        } else {
            foregroundClr = new Color(parentShell.getDisplay(), fgRGB);
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

        if (labelFont != null) {
            labelFont.dispose();
        }
        if (descriptionLabel != null) {
            descriptionLabel.dispose();
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createColorWheelControl();
        createColorControls();
        if (description != null && !description.isEmpty()) {
            createDescriptionLabel();
        }
        addSeparator();
        createBottomButtons();

        colorWheelComp.setColor(foregroundClr.getRGB());
    }

    /**
     * Create the color wheel controls.
     */
    protected void createColorWheelControl() {
        colorWheelComp = new ColorWheelComp(shell, this, " Color Chooser: ",
                true);
    }

    /**
     * Create the color controls.
     */
    protected void createColorControls() {
        previewLabel = new Label(shell, SWT.BORDER);
        FontData fd = previewLabel.getFont().getFontData()[0];
        fd.setHeight(16);
        fd.setStyle(SWT.BOLD);
        labelFont = new Font(getDisplay(), fd);
        previewLabel.setFont(labelFont);
        previewLabel.setText("    Sample Text    ");
        previewLabel.setLayoutData(new GridData(SWT.CENTER, SWT.FILL, true,
                true));
        previewLabel.setForeground(foregroundClr);
    }

    /**
     * Create a label that describes the scope of the color change.
     */
    protected void createDescriptionLabel() {
        descriptionLabel = new Label(shell, SWT.CENTER);
        descriptionLabel.setText(description);
        descriptionLabel.setLayoutData(new GridData(SWT.CENTER, SWT.FILL, true,
                true));
    }

    /**
     * Create the bottom OK/Cancel buttons.
     */
    protected void createBottomButtons() {
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
                collectReturnValue();
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
     * Collect the return value from fields. Called when user clicks the ok
     * button.
     */
    protected void collectReturnValue() {
        setReturnValue(new UserColorInfo(foregroundClr.getRGB()));
    }

    /**
     * Add a separator line to the dialog.
     */
    protected void addSeparator() {
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
        foregroundClr.dispose();
        foregroundClr = new Color(getDisplay(), rgb);
        previewLabel.setForeground(foregroundClr);
    }

}
