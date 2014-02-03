/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.KinkLineAttrDlg
 * 
 * Nov. 17, 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.display.ArrowHead.ArrowHeadType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IKink;

import java.awt.Color;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Singleton attribute dialog for PGEN kink lines.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/13        TTR 850     J. Wu      Initial Creation.
 * 01/14        TTR 850     J. Wu      Update kink position.
 * 
 * </pre>
 * 
 * @author J. Wu
 */

public class KinkLineAttrDlg extends LineAttrDlg implements IKink {

    static private KinkLineAttrDlg INSTANCE = null;

    protected static double MIN_KINK_LINE_POS = 0.25;

    protected static double MAX_KINK_LINE_POS = 0.75;

    protected Label kinkPosLbl;

    protected gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider kinkPosSpinnerSlider = null;

    /**
     * constructor
     * 
     * @param parShell
     * @throws VizException
     */
    protected KinkLineAttrDlg(Shell parShell) throws VizException {

        super(parShell);

    }

    /**
     * Creates a symbol attribute dialog if the dialog does not exist and
     * returns the instance. If the dialog exists, return the instance.
     * 
     * @param parShell
     * @return
     */
    public static KinkLineAttrDlg getInstance(Shell parShell) {

        if (INSTANCE == null) {

            try {

                INSTANCE = new KinkLineAttrDlg(parShell);

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

        super.createUpperComponents();

        createKinkPosAttr();

        super.createLowerComponents();

        super.setSmoothLvl(0);
        super.setLineWidth(2.0F);
        super.setSizeScale(1.0);

        setKinkPosition(0.5);
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

        this.getShell().setText("Kink Line Attributes");

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

        chkBox[ChkBox.KINK_POSITION.ordinal()].setVisible(false);

    }

    /**
     * Enable/disable all widgets in the dialog
     * 
     * @param flag
     */
    private void enableAllWidgets(boolean flag) {

        colorLbl.setEnabled(flag);

        widthLbl.setEnabled(flag);
        widthSpinnerSlider.setEnabled(flag);

        patternSizeLbl.setEnabled(flag);
        patternSizeSpinnerSlider.setEnabled(flag);

        smoothLbl.setEnabled(false);
        smoothLvlCbo.setEnabled(false);
        closedBtn.setEnabled(false);
        filledBtn.setEnabled(false);
        fillPatternLbl.setEnabled(false);
        fillPatternCbo.setEnabled(false);

        kinkPosLbl.setEnabled(flag);
        kinkPosSpinnerSlider.setEnabled(flag);
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

    /**
     * Create widgets for the kink position attribute
     */
    private void createKinkPosAttr() {
        Composite inCmp = new Composite(top, SWT.NONE);
        inCmp.setLayout(getGridLayout(3, false, 0, 0, 0, 0));

        chkBox[ChkBox.KINK_POSITION.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.KINK_POSITION.ordinal()].setLayoutData(new GridData(
                CHK_WIDTH, CHK_HEIGHT));
        chkBox[ChkBox.KINK_POSITION.ordinal()]
                .addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button btn = (Button) e.widget;
                        if (btn.getSelection()) {
                            kinkPosLbl.setEnabled(true);
                            kinkPosSpinnerSlider.setEnabled(true);
                        } else {
                            kinkPosLbl.setEnabled(false);
                            kinkPosSpinnerSlider.setEnabled(false);

                        }
                    }

                });

        chkBox[ChkBox.KINK_POSITION.ordinal()].setVisible(false);

        kinkPosLbl = new Label(inCmp, SWT.LEFT);
        kinkPosLbl.setText("Kink Position ");

        kinkPosSpinnerSlider = new gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider(
                inCmp, SWT.HORIZONTAL, 1);
        kinkPosSpinnerSlider.setLayoutData(new GridData(140, 25));
        kinkPosSpinnerSlider.setMinimum(25);
        kinkPosSpinnerSlider.setMaximum(75);
        kinkPosSpinnerSlider.setIncrement(5);
        kinkPosSpinnerSlider.setPageIncrement(5);
        kinkPosSpinnerSlider.setDigits(2);

    }

    protected boolean validateKinkPosition(VerifyEvent ve) {
        boolean stat = false;

        if (ve.widget instanceof Text) {
            Text wText = (Text) ve.widget;
            StringBuffer str = new StringBuffer(wText.getText());
            str.replace(ve.start, ve.end, ve.text);

            if (str.toString().isEmpty())
                return true;
            else {
                try {
                    double value = Double.parseDouble(str.toString());
                    if (value >= MIN_KINK_LINE_POS
                            && value <= MAX_KINK_LINE_POS) {
                        stat = true;
                    } else
                        stat = false;
                } catch (NumberFormatException e1) {
                    stat = false;
                }
            }
        }

        return stat;
    }

    /**
     * Gets the starting coordinate of the line segment
     * 
     * @return starting Coordinate of the segment
     */
    public Coordinate getStartPoint() {
        return null;
    }

    /**
     * Gets the ending coordinate of the line segment
     * 
     * @return Ending Coordinate of the segment
     */
    public Coordinate getEndPoint() {
        return null;
    }

    /**
     * Gets the color of this line
     * 
     * @return the color
     */
    public Color getColor() {
        if (getColors() != null) {
            return getColors()[0];
        } else {
            return null;
        }
    }

    /**
     * Gets the location of the kink along the line. Should be in the range 0.25
     * to 0.75
     * 
     * @return location as fraction of the way along the line.
     */
    public double getKinkPosition() {
        if (chkBox[ChkBox.KINK_POSITION.ordinal()].getSelection()) {
            return kinkPosSpinnerSlider.getSelection() / 100.0;
        } else {
            return java.lang.Double.NaN;
        }

    }

    /**
     * Sets the the location of the kink along the line. Should be in the range
     * 0.25 to 0.75.
     * 
     * @param ps
     *            .
     */
    private void setKinkPosition(double ps) {

        kinkPosSpinnerSlider.setSelection((int) (ps * 100));

    }

    /**
     * Gets the arrow head type. open or closed.
     * 
     * @return Arrow Head type.
     */
    public ArrowHeadType getArrowHeadType() {
        if (pgenType != null && pgenType.equals("KINK_LINE_1")) {
            return ArrowHeadType.OPEN;
        } else {
            return ArrowHeadType.FILLED;
        }
    }

    /**
     * Sets values of all attributes of the dialog.
     */
    public void setAttrForDlg(IAttribute iattr) {

        super.setAttrForDlg(iattr);

        if (iattr instanceof IKink) {

            IKink attr = (IKink) iattr;
            double kinkPos = attr.getKinkPosition();
            if (kinkPos >= 0.25 && kinkPos <= 0.75) {
                this.setKinkPosition(kinkPos);
            }
        }
    }

}
