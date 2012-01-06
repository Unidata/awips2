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

package com.raytheon.viz.awipstools.ui.dialog;

import java.text.DecimalFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.awipstools.common.UnitsCalculator;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * UnitsCalculatorDialog.
 * 
 * Tool for viewing conversion information for various values.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ----------	----------	-----------	--------------------------
 * 08/28/07     433         Eric Babin    Initial Creation.
 * </pre>
 * 
 * @author Eric Babin
 * @version 1
 */
public class UnitsCalculatorDialog extends CaveJFACEDialog implements
        SelectionListener {

    private Composite top = null;

    private String title = "";

    private String[][] units = {
            { "Temperature", UnitsCalculator.CEL, UnitsCalculator.FAH,
                    UnitsCalculator.KEL },
            { "Speed", UnitsCalculator.KNT, UnitsCalculator.MPS,
                    UnitsCalculator.MPH, UnitsCalculator.FPS,
                    UnitsCalculator.KPH },
            { "Distance", UnitsCalculator.METER, UnitsCalculator.KM,
                    UnitsCalculator.SM, UnitsCalculator.CM, UnitsCalculator.FT,
                    UnitsCalculator.IN, UnitsCalculator.NMI,
                    UnitsCalculator.MILE },
            { "Time", UnitsCalculator.DAY, UnitsCalculator.HR,
                    UnitsCalculator.MIN, UnitsCalculator.SEC },
            { "Pressure", UnitsCalculator.INHG, UnitsCalculator.MB,
                    UnitsCalculator.HPA, UnitsCalculator.PA } };

    private Text convertFromField, convertToField = null;

    private Label convertFromLabel, convertToLabel = null;

    private Group leftSideGroup, rightSideGroup = null;

    private DecimalFormat decimalFormat = new DecimalFormat("0.00000");

    public UnitsCalculatorDialog(Shell parShell, String dialogTitle)
            throws VizException {
        super(parShell);
        this.title = dialogTitle;
        setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    public Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        GridLayout gridLayout = new GridLayout(2, true);
        top.setLayout(gridLayout);

        createToolControls();

        return top;
    }

    /**
     * Method for creating the various labels, checkboxes...
     */
    private void createToolControls() {

        Composite lSideComposite = new Composite(top, SWT.NONE);
        Composite rSideComposite = new Composite(top, SWT.NONE);

        Composite lSideEntryComposite = new Composite(lSideComposite, SWT.NONE);
        Composite rSideEntryComposite = new Composite(rSideComposite, SWT.NONE);

        lSideComposite.setLayout(new GridLayout(1, true));
        rSideComposite.setLayout(new GridLayout(1, true));

        lSideEntryComposite.setLayout(new FormLayout());
        lSideEntryComposite.setLayoutData(new GridData(GridData.FILL_BOTH));

        rSideEntryComposite.setLayout(new FormLayout());
        rSideEntryComposite.setLayoutData(new GridData(GridData.FILL_BOTH));

        FormData data = new FormData();
        data.left = new FormAttachment(0, 0);
        data.width = 90;

        Font smallFont = new Font(top.getDisplay(), "Sans", 9, 0);
        convertFromField = new Text(lSideEntryComposite, SWT.SINGLE | SWT.RIGHT
                | SWT.BORDER);
        convertFromField.setLayoutData(data);
        convertFromField.setFont(smallFont);
        convertFromField.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                if (!convertFromField.getText().equalsIgnoreCase("")) {
                    String s = convertFromField.getText();

                    try {
                        Double.parseDouble(s.trim());
                    } catch (NumberFormatException nfe) {
                    }
                }
            }
        });
        convertFromField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR) {
                    updateTheConversion();
                }
            }
        });

        data = (FormData) convertFromField.getLayoutData();

        FormData labelLoc = new FormData();
        labelLoc.left = new FormAttachment(0, data.width + 12);
        labelLoc.top = new FormAttachment(0, 10);
        labelLoc.width = 40;

        convertFromLabel = new Label(lSideEntryComposite, SWT.BOTTOM);
        convertFromLabel.setText("C");
        convertFromLabel.setLayoutData(labelLoc);
        convertFromLabel.setFont(smallFont);

        FormData data2 = new FormData();
        data2.left = new FormAttachment(0, 0);
        data2.width = 90;

        convertToField = new Text(rSideEntryComposite, SWT.SINGLE | SWT.RIGHT
                | SWT.BORDER);
        convertToField.setFont(smallFont);
        convertToField.setLayoutData(data2);
        data2 = (FormData) convertToField.getLayoutData();

        FormData rightlabelLoc = new FormData();
        rightlabelLoc.left = new FormAttachment(0, data2.width + 12);
        rightlabelLoc.top = new FormAttachment(0, 10);
        rightlabelLoc.width = 40;

        convertToLabel = new Label(rSideEntryComposite, SWT.BOTTOM);
        convertToLabel.setText("C");
        convertToLabel.setLayoutData(rightlabelLoc);
        convertToLabel.setFont(smallFont);

        leftSideGroup = new Group(lSideComposite, SWT.SHADOW_NONE);
        rightSideGroup = new Group(rSideComposite, SWT.SHADOW_NONE);

        leftSideGroup.setLayoutData(new GridData(GridData.FILL_BOTH));
        leftSideGroup.setLayout(new RowLayout(SWT.VERTICAL));

        rightSideGroup.setLayoutData(new GridData(GridData.FILL_BOTH));
        rightSideGroup.setLayout(new RowLayout(SWT.VERTICAL));

        for (int i = 0; i < units.length; i++) {

            for (int j = 0; j < units[i].length; j++) {
                if (j == 0) {
                    Label llabel = new Label(leftSideGroup, SWT.NONE);
                    llabel.setText(units[i][0]);
                    Label rlabel = new Label(rightSideGroup, SWT.NONE);
                    rlabel.setText(units[i][0]);

                } else {
                    Button lSideCheckBox = new Button(leftSideGroup, SWT.RADIO);
                    lSideCheckBox.addSelectionListener(this);
                    lSideCheckBox.setText(units[i][j]);
                    lSideCheckBox.setData(i);
                    Button rSideCheckBox = new Button(rightSideGroup, SWT.RADIO);
                    rSideCheckBox.setText(units[i][j]);
                    rSideCheckBox.setData(i);
                    rSideCheckBox.addSelectionListener(this);
                }
            }
            new Label(leftSideGroup, SWT.SEPARATOR | SWT.HORIZONTAL);
            new Label(rightSideGroup, SWT.SEPARATOR | SWT.HORIZONTAL);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        if (title != null) {
            shell.setText(title);
        }
    }

    @Override
    public void widgetDefaultSelected(SelectionEvent e) {

    }

    @Override
    public void widgetSelected(SelectionEvent e) {

        Button b = (Button) e.getSource();

        if (b.getSelection()) {
            Group g = (Group) b.getParent();
            if (g.equals(leftSideGroup)) {
                disableRightSideOtherGroups(((Integer) b.getData()).intValue());
                updateEntryLabel(b.getText(), true);
            } else {
                updateEntryLabel(b.getText(), false);
            }
            updateTheConversion();
        }
    }

    /*
     * Updates the conversion field.
     */

    private void updateTheConversion() {
        double origValue = 0;
        try {
            origValue = Double.valueOf(convertFromField.getText());
        } catch (NumberFormatException e) {
            convertFromField.setText("0");
        }
        double conversion = 0.0;

        conversion = UnitsCalculator.convertUnits(
                getSelectionInGroup(leftSideGroup),
                getSelectionInGroup(rightSideGroup), origValue);

        convertToField.setText(decimalFormat.format(conversion));
    }

    /**
     * Update the entry labels with the appropriate scale. Note for all but few
     * exceptions, the label should match the button text
     * 
     * @param text
     * @param left
     */
    private void updateEntryLabel(String leftSelection, boolean left) {
        String s = leftSelection;

        if (leftSelection.equalsIgnoreCase(UnitsCalculator.CEL)) {
            s = "C";
        } else if (leftSelection.equalsIgnoreCase(UnitsCalculator.FAH)) {
            s = "F";
        } else if (leftSelection.equalsIgnoreCase(UnitsCalculator.KEL)) {
            s = "K";
        } else if (leftSelection.equalsIgnoreCase(UnitsCalculator.KNT)) {
            s = "kt";
        } else if (leftSelection.equalsIgnoreCase(UnitsCalculator.KPH)) {
            s = "km/h";
        } else if (leftSelection.equalsIgnoreCase(UnitsCalculator.DAY)) {
            s = "d";
        } else if (leftSelection.equalsIgnoreCase(UnitsCalculator.SEC)) {
            s = "s";
        }

        if (left) {
            convertFromLabel.setText(s);
        } else {
            convertToLabel.setText(s);
        }
    }

    /**
     * When the left side group is selected, it disables all others on right
     * side... (ex. clicking time, enables "time" on right side, and disables
     * all others on right side. Also, should select the first radio in the
     * group.
     * 
     * @param idToKeep
     */
    private void disableRightSideOtherGroups(int groupNumber) {

        if (convertFromField.getText().equalsIgnoreCase("")) {
            convertFromField.setText("0");
        }

        boolean firstButton = false;
        boolean buttonSelected = false;
        Button firstButtonInGroup = null;

        Control[] children = rightSideGroup.getChildren();

        for (int i = 0; i < children.length; i++) {
            if (children[i] instanceof Button) {
                Button b = (Button) children[i];
                int childKey = ((Integer) b.getData()).intValue();
                if (childKey == groupNumber) {
                    if (b.getSelection()) {
                        buttonSelected = true;
                    }
                    if (!firstButton) {
                        firstButtonInGroup = b;
                        firstButton = true;
                    }
                    b.setEnabled(true);
                } else {
                    b.setEnabled(false);
                    b.setSelection(false);
                }
            }
        }

        if (!buttonSelected) {
            firstButtonInGroup.setSelection(true);
            updateEntryLabel(firstButtonInGroup.getText(), false);
        }
    }

    /**
     * Returns the currently selected button in the group
     * 
     * @param g
     *            Group to check
     * @return text of button selected.
     */
    private String getSelectionInGroup(Group g) {
        Control[] children = g.getChildren();

        for (int i = 0; i < children.length; i++) {
            if (children[i] instanceof Button) {
                Button b = (Button) children[i];
                if (b.getSelection())
                    return b.getText();
            }
        }
        return "";
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {

    }

}
