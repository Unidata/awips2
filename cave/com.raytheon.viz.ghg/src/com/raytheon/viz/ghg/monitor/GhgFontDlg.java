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
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ghg.monitor.data.GhgConfigData.GhgFontSizeEnum;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the GHG Font Dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 25, 2008  N/A      lvenable  Initial creation
 * Jun 17, 2008  1157     MW Fegan  Added interaction with GHG Configuration.
 * Nov 28, 2012  1353     rferrel   Changes for non-blocking dialog.
 * Feb 05, 2016  5316     randerso  Code cleanup
 * Nov 14, 2019  7919     randerso  Changed to return GhgFontSizeEnum instead of
 *                                  Font. Code cleanup.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class GhgFontDlg extends CaveSWTDialog {

    /**
     * Currently selected font.
     */
    private GhgFontSizeEnum initialFont;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent Shell.
     * @param initialFont
     *            the font that is initially selected by default
     */
    public GhgFontDlg(Shell parent, GhgFontSizeEnum initialFont) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("GHG Font Dialog");

        this.initialFont = initialFont;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createLabelAndFontControls();
        createBottomButtons();
    }

    /**
     * Create the description label and the font controls.
     */
    private void createLabelAndFontControls() {
        Label topLabel = new Label(shell, SWT.NONE);
        topLabel.setText("Choose font size for GHG Monitor");

        Group fontGroup = new Group(shell, SWT.SHADOW_ETCHED_IN);
        GridLayout gl = new GridLayout(1, false);
        fontGroup.setLayout(gl);
        fontGroup.setText("Font Sizes:");

        for (GhgFontSizeEnum fontEnum : GhgFontSizeEnum.values()) {
            Button button = new Button(fontGroup, SWT.RADIO);
            button.setText("Monitor Font");
            Font font = new Font(getDisplay(), fontEnum.getFontName(),
                    fontEnum.getFontHeight(), SWT.NORMAL);
            button.setFont(font);
            button.setData(fontEnum);
            button.setSelection(fontEnum == initialFont);

            button.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    Button button = (Button) event.widget;
                    if (button.getSelection()) {
                        setReturnValue(button.getData());
                    }
                }
            });

            button.addDisposeListener(new DisposeListener() {

                @Override
                public void widgetDisposed(DisposeEvent e) {
                    button.getFont().dispose();
                }
            });
        }

    }

    /**
     * Create the bottom buttons.
     */
    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonArea.setLayoutData(gd);
        buttonArea.setLayout(new GridLayout(2, true));

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button applyFontBtn = new Button(buttonArea, SWT.PUSH);
        applyFontBtn.setText("Apply Font");
        applyFontBtn.setLayoutData(gd);
        applyFontBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button cancelBtn = new Button(buttonArea, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
                close();
            }
        });
    }
}