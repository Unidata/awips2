/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.FrontAttrDlg
 * 
 * 14 July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Singleton attribute dialog for PGEN fronts.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/10			?		B. Yin   	Initial Creation.
 * 11/13        1065        J. Wu       Adjust for changes in LineAttrDLg.
 * 
 * </pre>
 * 
 * @author B. Yin
 */

public class FrontAttrDlg extends LineAttrDlg {

    static private FrontAttrDlg INSTANCE = null;

    private Button labelChkBox;

    private Button labelColorChkBox;

    private boolean lastLabelStatus;

    private boolean lastUseColorStatus;

    /**
     * constructor
     * 
     * @param parShell
     * @throws VizException
     */
    protected FrontAttrDlg(Shell parShell) throws VizException {

        super(parShell);

    }

    /**
     * Creates a symbol attribute dialog if the dialog does not exist and
     * returns the instance. If the dialog exists, return the instance.
     * 
     * @param parShell
     * @return
     */
    public static FrontAttrDlg getInstance(Shell parShell) {

        if (INSTANCE == null) {

            try {

                INSTANCE = new FrontAttrDlg(parShell);

            } catch (VizException e) {

                e.printStackTrace();

            }
        }

        return INSTANCE;

    }

    @Override
    /**
     * Creates buttons, menus, and other controls in the dialog area
     */
    protected void initializeComponents() {
        super.initializeComponents();

        Composite inCmp = new Composite(top, SWT.NONE);
        inCmp.setLayout(getGridLayout(3, false, 0, 0, 0, 0));

        chkBox[ChkBox.LABEL.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.LABEL.ordinal()].setLayoutData(new GridData(CHK_WIDTH,
                CHK_HEIGHT));
        chkBox[ChkBox.LABEL.ordinal()]
                .addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button btn = (Button) e.widget;
                        if (btn.getSelection()) {
                            labelChkBox.setEnabled(true);
                        } else {
                            labelChkBox.setEnabled(false);
                        }
                    }

                });

        chkBox[ChkBox.LABEL.ordinal()].setVisible(false);

        labelChkBox = new Button(inCmp, SWT.CHECK);
        labelChkBox.setText("Label");

        labelColorChkBox = new Button(inCmp, SWT.CHECK);
        labelColorChkBox.setText("Use Front Color");

        labelChkBox.addListener(SWT.MouseUp, new Listener() {

            @Override
            public void handleEvent(Event event) {

                if (((Button) event.widget).getSelection()) {
                    labelColorChkBox.setEnabled(true);
                } else {
                    labelColorChkBox.setEnabled(false);

                }

                lastLabelStatus = ((Button) event.widget).getSelection();
            }
        });

        labelColorChkBox.addListener(SWT.MouseUp, new Listener() {

            @Override
            public void handleEvent(Event event) {
                lastUseColorStatus = ((Button) event.widget).getSelection();
            }
        });

        if (de != null && de.getPgenType().equalsIgnoreCase(this.pgenType)) {
            labelChkBox.setSelection(lastLabelStatus);
            labelColorChkBox.setSelection(lastUseColorStatus);
        } else {
            labelChkBox.setSelection(false);
            labelColorChkBox.setSelection(false);
            lastLabelStatus = false;
            lastUseColorStatus = false;
        }

    }

    /**
     * Check if the label box is checked
     */
    public boolean labelEnabled() {
        return labelChkBox.getSelection();
    }

    /**
     * Check if the 'Use Symbol Color' box is checked.
     * 
     * @return
     */
    public boolean useFrontColor() {
        return labelColorChkBox.getSelection();
    }

    /**
     * Set the 'Label' check box
     * 
     * @param enabled
     */
    public void setLabelChkBox(boolean enabled) {
        labelChkBox.setEnabled(enabled);

        if (!labelChkBox.isEnabled() || !labelChkBox.getSelection()) {
            labelColorChkBox.setEnabled(false);
        }
    }

    @Override
    public int open() {

        this.create();
        int rt = super.open();
        // if current action is MultiSelect, make the check boxes visible
        if (PgenSession.getInstance().getPgenPalette().getCurrentAction()
                .equalsIgnoreCase("MultiSelect")) {
            enableChkBoxes(true);
            enableAllWidgets(false);
        } else {
            enableAllWidgets(true);
            enableChkBoxes(false);
        }

        this.getShell().setText("Front Attributes");

        return rt;
    }

    /**
     * Make the check boxes visible/invisible
     * 
     * @param flag
     */
    private void enableChkBoxes(boolean flag) {

        if (!flag) {
            setAllChkBoxes();
        }
        for (ChkBox chk : ChkBox.values()) {
            if (chkBox[chk.ordinal()] != null) {
                chkBox[chk.ordinal()].setSelection(true);
            }
        }

        chkBox[ChkBox.LABEL.ordinal()].setVisible(false);

    }

    /**
     * Enable/disable all widgets in the dialog
     * 
     * @param flag
     */
    private void enableAllWidgets(boolean flag) {

        colorLbl.setEnabled(flag);

        patternSizeLbl.setEnabled(flag);

        widthLbl.setEnabled(flag);
        widthSpinnerSlider.setEnabled(flag);

        smoothLbl.setEnabled(flag);
        smoothLvlCbo.setEnabled(flag);

        closedBtn.setEnabled(false);
        filledBtn.setEnabled(false);

        fillPatternLbl.setEnabled(false);
        fillPatternCbo.setEnabled(false);

        labelChkBox.setEnabled(flag);

    }

    /**
     * Set all multi-selection check boxes to true.
     */
    private void setAllChkBoxes() {

        for (ChkBox chk : ChkBox.values()) {
            if (chkBox[chk.ordinal()] != null) {
                chkBox[chk.ordinal()].setSelection(true);
            }
        }

    }
}
