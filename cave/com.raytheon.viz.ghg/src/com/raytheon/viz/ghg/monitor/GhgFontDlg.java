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
package com.raytheon.viz.ghg.monitor;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the GHG Font Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation 
 * 17Jun2008    1157       MW Fegan    Added interaction with GHG Configuration.
 * 28 NOV 2012  1353       rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GhgFontDlg extends CaveSWTDialog {

    /**
     * Return object when the shell is disposed.
     */
    private FontData returnFontData = null;

    /**
     * Extra small font.
     */
    private Font xSmallFont;

    /**
     * Small font.
     */
    private Font smallFont;

    /**
     * Medium font.
     */
    private Font mediumFont;

    /**
     * Large font.
     */
    private Font largeFont;

    /**
     * Extra large font.
     */
    private Font xLargeFont;

    /**
     * Extra small font radio button.
     */
    private Button xSmallRdo;

    /**
     * Small font radio button.
     */
    private Button smallRdo;

    /**
     * Medium font radio button.
     */
    private Button mediumRdo;

    /**
     * Large font radio button.
     */
    private Button largeRdo;

    /**
     * Extra large font radio button.
     */
    private Button xLargeRdo;

    /**
     * The GHG Hazards Monitor configuration.
     */
    private GhgConfigData config = null;

    /**
     * Currently selected font.
     */
    private GhgConfigData.GhgFontSizeEnum font;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public GhgFontDlg(Shell parent, GhgConfigData config) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("GHG Font Dialog");

        this.config = config;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        xSmallFont.dispose();
        smallFont.dispose();
        mediumFont.dispose();
        largeFont.dispose();
        xLargeFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        xSmallFont = new Font(getDisplay(), "Monospace", 8, SWT.NORMAL);
        smallFont = new Font(getDisplay(), "Monospace", 10, SWT.NORMAL);
        mediumFont = new Font(getDisplay(), "Monospace", 12, SWT.NORMAL);
        largeFont = new Font(getDisplay(), "Monospace", 14, SWT.NORMAL);
        xLargeFont = new Font(getDisplay(), "Monospace", 16, SWT.NORMAL);

        createLabelAndFontControls();

        createBottomButtons();
    }

    /**
     * Create the description label and the font controls.
     */
    private void createLabelAndFontControls() {
        Label topLabel = new Label(shell, SWT.NONE);
        topLabel.setText("Choose font size for GHG Monitor");

        Group fontGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        fontGroup.setLayout(gl);
        fontGroup.setText(" Font Sizes: ");

        xSmallRdo = new Button(fontGroup, SWT.RADIO);
        xSmallRdo.setText("Monitor Font");
        xSmallRdo.setFont(xSmallFont);
        xSmallRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateFontSelection(GhgConfigData.GhgFontSizeEnum.X_SMALL_FONT);
            }
        });

        smallRdo = new Button(fontGroup, SWT.RADIO);
        smallRdo.setText("Monitor Font");
        smallRdo.setFont(smallFont);
        smallRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateFontSelection(GhgConfigData.GhgFontSizeEnum.SMALL_FONT);
            }
        });

        mediumRdo = new Button(fontGroup, SWT.RADIO);
        mediumRdo.setText("Monitor Font");
        mediumRdo.setFont(mediumFont);
        mediumRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateFontSelection(GhgConfigData.GhgFontSizeEnum.MEDIUM_FONT);
            }
        });

        largeRdo = new Button(fontGroup, SWT.RADIO);
        largeRdo.setText("Monitor Font");
        largeRdo.setFont(largeFont);
        largeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateFontSelection(GhgConfigData.GhgFontSizeEnum.LARGE_FONT);
            }
        });

        xLargeRdo = new Button(fontGroup, SWT.RADIO);
        xLargeRdo.setText("Monitor Font");
        xLargeRdo.setFont(xLargeFont);
        xLargeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateFontSelection(GhgConfigData.GhgFontSizeEnum.X_LARGE_FONT);
            }
        });
        /*
         * set the font button from the configuration.
         */
        setCurrentFontRdo(config.getCurrentFont());
    }

    /**
     * Set the selected radio button based on the current font size.
     * 
     * @param font
     */
    private void setCurrentFontRdo(GhgConfigData.GhgFontSizeEnum font) {
        switch (font) {
        case X_SMALL_FONT:
            xSmallRdo.setSelection(true);
            break;
        case SMALL_FONT:
            smallRdo.setSelection(true);
            break;
        case MEDIUM_FONT:
            mediumRdo.setSelection(true);
            break;
        case LARGE_FONT:
            largeRdo.setSelection(true);
            break;
        case X_LARGE_FONT:
            xLargeRdo.setSelection(true);
            break;
        }
    }

    /**
     * Event handler called when the user clicks on a radio button. Sets the
     * return value for the {@link #open()} method and updates the the
     * configuration object.
     * 
     * @param font
     *            the size of the newly selected font
     */
    private void updateFontSelection(GhgConfigData.GhgFontSizeEnum font) {
        this.font = font;
        switch (font) {
        case X_SMALL_FONT:
            returnFontData = xSmallFont.getFontData()[0];
            break;
        case SMALL_FONT:
            returnFontData = smallFont.getFontData()[0];
            break;
        case MEDIUM_FONT:
            returnFontData = mediumFont.getFontData()[0];
            break;
        case LARGE_FONT:
            returnFontData = largeFont.getFontData()[0];
            break;
        case X_LARGE_FONT:
            returnFontData = xLargeFont.getFontData()[0];
            break;
        }
    }

    /**
     * Create the bottom buttons.
     */
    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(2, true));

        gd = new GridData(120, SWT.DEFAULT);
        Button applyFontBtn = new Button(buttons, SWT.PUSH);
        applyFontBtn.setText("Apply Font");
        applyFontBtn.setLayoutData(gd);
        applyFontBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (returnFontData != null) {
                    config.setCurrentFont(font);
                }

                setReturnValue(returnFontData);
                close();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                returnFontData = null;
                close();
            }
        });
    }
}