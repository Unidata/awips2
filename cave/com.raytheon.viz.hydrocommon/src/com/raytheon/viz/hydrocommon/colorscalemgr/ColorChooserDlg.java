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
package com.raytheon.viz.hydrocommon.colorscalemgr;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.viz.hydrocommon.data.ColorNameData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to select a color by name.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2008            mschenke     Initial creation
 * Jul 1, 2013  2088       rferrel     Now extends CaveSWTDialog and made non-blocking.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class ColorChooserDlg extends CaveSWTDialog {
    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Color table.
     */
    private Table colorTable;

    private int selectedIndex = 0;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public ColorChooserDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL,
                CAVE.DO_NOT_BLOCK);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(null);
        shell.setText("Color Chooser");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createColorTable();
        createBottomButtons();
    }

    /**
     * Setup Color Table.
     */
    private void createColorTable() {
        Color c = null;

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite colorComp = new Composite(shell, SWT.NONE);
        colorComp.setLayout(new GridLayout(1, false));
        colorComp.setLayoutData(gd);

        Label colorLabel = new Label(colorComp, SWT.NONE);
        colorLabel.setText("Select New Color:");

        gd = new GridData(SWT.FILL, SWT.FILL, false, true);
        gd.heightHint = 200;
        gd.widthHint = 380;
        colorTable = new Table(colorComp, SWT.BORDER | SWT.MULTI);
        colorTable.setLayoutData(gd);
        TableColumn column1 = new TableColumn(colorTable, SWT.NONE);
        TableColumn column2 = new TableColumn(colorTable, SWT.NONE);

        List<ColorNameData> colorNames = DbRGBColors.getAllColors();
        Display display = getDisplay();

        for (ColorNameData colorName : colorNames) {
            if (colorName.getColorValue() != null) {
                c = new Color(display, colorName.getColorValue());
                TableItem ti = new TableItem(colorTable, SWT.NONE);
                ti.setText(new String[] { "    ", colorName.getColorName() });
                ti.setBackground(0, c);

                // Need to dispose of the color...
                if (c != null) {
                    c.dispose();
                }
            }
        }
        colorTable.setSelection(selectedIndex);

        column1.pack();
        column2.pack();
    }

    /**
     * Create control buttons in the bottom composite.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(getSelectedColorName());
                close();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
                close();
            }
        });
    }

    /**
     * Get selected color name or null if none selected.
     * 
     * @return
     */
    private String getSelectedColorName() {
        int index = colorTable.getSelectionIndex();
        if (index < 0)
            return null;

        TableItem ti = colorTable.getItem(index);

        return ti.getText(1);
    }

    /**
     * Set the initial color color table index; must be done prior to opening
     * the dialog.
     * 
     * @param index
     */
    public void setSelected(int index) {
        selectedIndex = index;
    }
}
