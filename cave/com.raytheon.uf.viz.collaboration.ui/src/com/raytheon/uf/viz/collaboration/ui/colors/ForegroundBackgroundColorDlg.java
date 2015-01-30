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
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.collaboration.display.data.UserColorInfo;

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
 * Dec 04, 2014 3709       lvenable    Initial creation
 * Jan 09, 2015 3709       bclement    moved primary logic to new super class
 * Jan 13, 2015 3709       bclement    return UserColorInfo instead of RGB[]
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ForegroundBackgroundColorDlg extends ForegroundColorDlg {

    /** Background color. */
    private Color backgroundClr = null;

    private Button foregroundRdo;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public ForegroundBackgroundColorDlg(Shell parentShell, String description) {
        this(parentShell, description, null, null);
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
    public ForegroundBackgroundColorDlg(Shell parentShell, String description,
            RGB fgRGB, RGB bgRGB) {
        super(parentShell, description, fgRGB);
        setText("Foreground/Background Color Chooser");

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
    protected void disposed() {
        super.disposed();

        if (backgroundClr != null) {
            backgroundClr.dispose();
        }
    }

    @Override
    protected void createColorControls() {
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
        previewLabel = new Label(colorControlComp, SWT.BORDER);
        FontData fd = previewLabel.getFont().getFontData()[0];
        fd.setHeight(16);
        fd.setStyle(SWT.BOLD);
        labelFont = new Font(getDisplay(), fd);
        previewLabel.setFont(labelFont);
        previewLabel.setText("    Sample Text    ");
        previewLabel.setLayoutData(gd);

        previewLabel.setForeground(foregroundClr);
        previewLabel.setBackground(backgroundClr);
    }

    @Override
    protected void collectReturnValue() {
        UserColorInfo colors = new UserColorInfo(foregroundClr.getRGB(),
                backgroundClr.getRGB());
        setReturnValue(colors);
    }

    @Override
    public void colorChange(RGB rgb, String colorWheelTitle) {
        if (foregroundRdo.getSelection()) {
            foregroundClr.dispose();
            foregroundClr = new Color(getDisplay(), rgb);
            previewLabel.setForeground(foregroundClr);
        } else {
            backgroundClr.dispose();
            backgroundClr = new Color(getDisplay(), rgb);
            previewLabel.setBackground(backgroundClr);
        }
    }

}
