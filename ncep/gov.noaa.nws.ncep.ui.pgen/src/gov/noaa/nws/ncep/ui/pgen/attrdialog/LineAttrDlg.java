/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.LineAttrDlg
 * 
 * 20 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.Activator;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Singleton attribute dialog for lines.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/09					B. Yin   	Initial Creation.
 * 04/09        90          B. Hebbard  Replace ColorSelector with ColorMatrixSelector.
 * 09/09		149			B. Yin		added check boxes for multi-selection
 * 03/10        231         Archana     Altered the Line attribute dialog
 *                                      to display only a button showing the 
 *                                      selected color instead of displaying 
 *                                      the complete color matrix .
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 07/12	   #610(TTR419)	B. Yin		Multi-Select GFA does not need check boxes for 
 * 									    every attributes
 * 03/13		#928	    B. Yin		Added a separator above the button bar.
 * 									    Added cancelPressed() to go back to selecting mode.
 * 04/13		TTR399		J. Wu		make the dialog smaller.
 * 04/13        1065        J. Wu       Added KINK_POSITION.
 * </pre>
 * 
 * @author B. Yin
 */

public class LineAttrDlg extends AttrDlg implements ILine {

    protected static enum ChkBox {
        COLOR, WIDTH, SMOOTH, PATTERN_SIZE, CLOSE, FILL, FILL_PATTERN, LABEL, KINK_POSITION
    };

    protected static int MIN_LINE_WIDTH = 1;

    protected static int MAX_LINE_WIDTH = 20;

    protected static int DEFAULT_LINE_WIDTH = 2;

    protected static double MIN_PATTERN_SIZE = 0.1;

    protected static double MAX_PATTERN_SIZE = 10.0;

    protected static double DEFAULT_PATTERN_SIZE = 2.0;

    private static String FILL_PATTERNS[];

    private static HashMap<String, Image> FILL_PATTERN_MENU_ITEMS;

    private static LineAttrDlg INSTANCE;

    protected Composite top;

    protected Composite colorGroup;

    protected Label colorLbl;

    protected ColorButtonSelector cs;

    protected List<ColorButtonSelector> csList;

    protected Label widthLbl;

    protected gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider widthSpinnerSlider = null;

    protected Label patternSizeLbl;

    protected gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider patternSizeSpinnerSlider = null;

    protected Label smoothLbl;

    protected Combo smoothLvlCbo = null;

    protected Label fillPatternLbl;

    protected SymbolCombo fillPatternCbo;

    protected Button closedBtn = null;

    protected Button filledBtn = null;

    protected Button chkBox[];

    /**
     * Private constructor
     * 
     * @param parShell
     * @throws VizException
     */
    protected LineAttrDlg(Shell parShell) throws VizException {

        super(parShell);

    }

    /**
     * Creates a line attribute dialog if the dialog does not exist and returns
     * the instance. If the dialog exists, return the instance.
     * 
     * @param parShell
     * @returnGrid
     */
    public static LineAttrDlg getInstance(Shell parShell) {

        if (INSTANCE == null) {

            try {
                INSTANCE = new LineAttrDlg(parShell);
            } catch (VizException e) {
                e.printStackTrace();
            }

        }

        return INSTANCE;

    }

    /**
     * Creates the dialog area
     */
    @Override
    public Control createDialogArea(Composite parent) {

        // Create the main composite with a dense grid layout.
        top = (Composite) super.createDialogArea(parent);
        top.setLayout(getGridLayout(1, false, 0, 0, 0, 0));

        // Initialize all of the menus, controls, and layouts
        initializeComponents();

        return top;
    }

    /**
     * Creates buttons, menus, and other controls in the dialog area
     */
    protected void initializeComponents() {

        this.getShell().setText("Line Attributes");

        chkBox = new Button[ChkBox.values().length];

        createColorCloseFillAttr();
        createWidthAttr();
        createPatternSizeAttr();
        createSmoothAttr();
        createFillPatternAttr();
        addSeparator(top.getParent());
    }

    /**
     * Creates color,, close, fill, width, pattern size in the dialog area
     */
    protected void createUpperComponents() {

        chkBox = new Button[ChkBox.values().length];

        createColorCloseFillAttr();
        createWidthAttr();
        createPatternSizeAttr();
    }

    /**
     * Creates smooth factor, fill pattern in the dialog area
     */
    protected void createLowerComponents() {
        createSmoothAttr();
        createFillPatternAttr();
        addSeparator(top.getParent());
    }

    /**
     * Return color from the color picker of the dialog
     */
    public Color[] getColors() {
        if (chkBox[ChkBox.COLOR.ordinal()].getSelection()) {
            // IAttribute requires to return an array of colors
            // Only the first color is used at this time.
            Color[] colors = new Color[csList.size()];

            for (int j = 0; j < csList.size(); j++) {
                RGB rgb = csList.get(j).getColorValue();
                colors[j] = new java.awt.Color(rgb.red, rgb.green, rgb.blue);
            }

            return colors;
        } else {
            return null;
        }
    }

    /**
     * Sets the color of the color picker of the dialog.
     * 
     * @param clr
     */
    public void setColor(Color[] colors) {

        manageColorBoxes(colors.length);

        for (int j = 0; j < colors.length; j++) {
            Color clr = colors[j];
            csList.get(j).setColorValue(
                    new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));
        }

    }

    /*
     * Make sure there are num widgets available
     */
    private void manageColorBoxes(int num) {

        for (ColorButtonSelector cbs : csList) {
            cbs.dispose();
        }
        csList.clear();

        for (int n = 0; n < num; n++) {
            ColorButtonSelector cbs = new ColorButtonSelector(colorGroup);
            csList.add(cbs);
        }
        colorGroup.pack();
        colorGroup.layout();
    }

    /**
     * Returns the line width from the dialog.
     */
    public float getLineWidth() {
        if (chkBox[ChkBox.WIDTH.ordinal()].getSelection()) {

            return widthSpinnerSlider.getSelection();
        } else {
            return java.lang.Float.NaN;
        }

    }

    /**
     * Sets the line width value of the dialog.
     * 
     * @param lw
     */
    protected void setLineWidth(float lw) {

        widthSpinnerSlider.setSelection((int) lw);

    }

    public double getSizeScale() {
        if (chkBox[ChkBox.PATTERN_SIZE.ordinal()].getSelection()) {
            return patternSizeSpinnerSlider.getSelection() / 10.0;
        } else {
            return java.lang.Double.NaN;
        }

    }

    protected void setSizeScale(double ps) {

        patternSizeSpinnerSlider.setSelection((int) (ps * 10));

    }

    /**
     * Returns the Close flag of the dialog.
     */
    public Boolean isClosedLine() {
        if (chkBox[ChkBox.CLOSE.ordinal()].getSelection()) {

            return closedBtn.getSelection();
        } else {
            return null;
        }

    }

    /**
     * Returns the fill pattern from the dialog.
     */
    public FillPattern getFillPattern() {

        if (chkBox[ChkBox.FILL_PATTERN.ordinal()].getSelection()) {
            return FillPattern.valueOf(fillPatternCbo.getSelectedText());
        } else {
            return null;
        }

    }

    /**
     * Sets the Close flag of the dialog.
     * 
     * @param cls
     */
    private void setClosed(Boolean cls) {

        if (closedBtn != null) {
            closedBtn.setSelection(cls);
        }
    }

    /**
     * Returns the Filled flag of the dialog.
     */
    public Boolean isFilled() {
        if (chkBox[ChkBox.FILL.ordinal()].getSelection()) {

            return filledBtn.getSelection();
        } else {
            return null;
        }

    }

    /**
     * Sets the Filled flag of the dialog.
     * 
     * @param filled
     */
    private void setFilled(Boolean filled) {

        if (filledBtn != null) {
            filledBtn.setSelection(filled);
        }
    }

    /**
     * Returns the smooth level of the dialog.
     */
    public int getSmoothFactor() {
        if (chkBox[ChkBox.SMOOTH.ordinal()].getSelection()) {
            return smoothLvlCbo.getSelectionIndex();
        } else {
            return -1;
        }

    }

    /**
     * Sets the smooth level of the dialog.
     * 
     * @param sl
     */
    public void setSmoothLvl(int sl) {

        smoothLvlCbo.select(sl);
    }

    /**
     * Sets values of all attributes of the dialog.
     */
    public void setAttrForDlg(IAttribute iattr) {

        if (iattr instanceof ILine) {
            ILine attr = (ILine) iattr;
            Color[] clr = attr.getColors();
            if (clr != null)
                this.setColor(clr);

            float lw = attr.getLineWidth();
            if (lw > 0)
                this.setLineWidth(lw);

            double ps = attr.getSizeScale();
            if (ps > 0)
                this.setSizeScale(ps);

            this.setClosed(attr.isClosedLine());
            this.setFilled(attr.isFilled());

            if (attr.isFilled()) {
                fillPatternCbo.setEnabled(true);
            }

            this.setFillPattern(attr.getFillPattern());

            int sl = attr.getSmoothFactor();
            if (sl >= 0)
                this.setSmoothLvl(sl);
        }
    }

    private void setFillPattern(FillPattern fp) {
        if (fp != null && fillPatternCbo != null)
            fillPatternCbo.select(fp.ordinal());
    }

    /**
     * Create widgets for the Color attribute
     */
    private void createColorAttr(Composite comp) {

        Composite inCmp = new Composite(comp, SWT.NONE);
        inCmp.setLayout(getGridLayout(3, false, 0, 0, 0, 0));

        chkBox[ChkBox.COLOR.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.COLOR.ordinal()].setLayoutData(new GridData(CHK_WIDTH,
                CHK_HEIGHT));
        chkBox[ChkBox.COLOR.ordinal()]
                .addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button btn = (Button) e.widget;
                        if (btn.getSelection()) {
                            colorLbl.setEnabled(true);
                        } else {
                            colorLbl.setEnabled(false);
                        }
                    }

                });

        colorLbl = new Label(inCmp, SWT.LEFT);
        colorLbl.setText("Color ");

        colorGroup = new Composite(inCmp, SWT.NONE);
        colorGroup.setLayout(getGridLayout(1, false, 0, 0, 0, 0));
        csList = new ArrayList<ColorButtonSelector>();

        ColorButtonSelector dflt = new ColorButtonSelector(colorGroup);
        dflt.setColorValue(new RGB(0, 255, 0));
        csList.add(dflt);
    }

    /*
     * Create widgets for the Line Width attribute
     */
    private void createWidthAttr() {

        Composite inCmp = new Composite(top, SWT.NONE);
        inCmp.setLayout(getGridLayout(3, false, 0, 0, 0, 0));

        chkBox[ChkBox.WIDTH.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.WIDTH.ordinal()].setLayoutData(new GridData(CHK_WIDTH,
                CHK_HEIGHT));
        chkBox[ChkBox.WIDTH.ordinal()]
                .addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button btn = (Button) e.widget;
                        if (btn.getSelection()) {
                            widthLbl.setEnabled(true);
                            widthSpinnerSlider.setEnabled(true);
                        } else {
                            widthLbl.setEnabled(false);
                            widthSpinnerSlider.setEnabled(false);

                        }
                    }

                });

        widthLbl = new Label(inCmp, SWT.LEFT);
        widthLbl.setText("Line Width ");

        widthSpinnerSlider = new gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider(
                inCmp, SWT.HORIZONTAL, 1);
        widthSpinnerSlider.setLayoutData(new GridData(148, 25));
        widthSpinnerSlider.setMinimum(1);
        widthSpinnerSlider.setMaximum(20);
        widthSpinnerSlider.setIncrement(1);
        widthSpinnerSlider.setPageIncrement(3);
        widthSpinnerSlider.setDigits(0);

    }

    /**
     * Validate the line width text field.
     */
    protected boolean validateLineWidth(VerifyEvent ve) {
        boolean stat = false;

        if (ve.widget instanceof Text) {
            Text wText = (Text) ve.widget;
            StringBuffer str = new StringBuffer(wText.getText());
            str.replace(ve.start, ve.end, ve.text);

            if (str.toString().isEmpty())
                return true;
            else {
                try {
                    int value = Integer.parseInt(str.toString());
                    if (value >= MIN_LINE_WIDTH && value <= MAX_LINE_WIDTH) {
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
     * Create widgets for the smooth Level attribute
     */
    private void createSmoothAttr() {

        Composite inCmp = new Composite(top, SWT.NONE);
        inCmp.setLayout(getGridLayout(3, false, 0, 0, 0, 0));

        chkBox[ChkBox.SMOOTH.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.SMOOTH.ordinal()].setLayoutData(new GridData(CHK_WIDTH,
                CHK_HEIGHT));
        chkBox[ChkBox.SMOOTH.ordinal()]
                .addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button btn = (Button) e.widget;
                        if (btn.getSelection()) {
                            smoothLbl.setEnabled(true);
                            smoothLvlCbo.setEnabled(true);
                        } else {
                            smoothLbl.setEnabled(false);
                            smoothLvlCbo.setEnabled(false);

                        }
                    }

                });

        smoothLbl = new Label(inCmp, SWT.LEFT);
        smoothLbl.setText("Smooth Level ");

        smoothLvlCbo = new Combo(inCmp, SWT.DROP_DOWN | SWT.READ_ONLY);

        smoothLvlCbo.add("0");
        smoothLvlCbo.add("1");
        smoothLvlCbo.add("2");

        smoothLvlCbo.select(2);
    }

    /**
     * Create widgets for the pattern size attribute
     */
    private void createPatternSizeAttr() {
        Composite inCmp = new Composite(top, SWT.NONE);
        inCmp.setLayout(getGridLayout(3, false, 0, 0, 0, 0));

        chkBox[ChkBox.PATTERN_SIZE.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.PATTERN_SIZE.ordinal()].setLayoutData(new GridData(
                CHK_WIDTH, CHK_HEIGHT));
        chkBox[ChkBox.PATTERN_SIZE.ordinal()]
                .addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button btn = (Button) e.widget;
                        if (btn.getSelection()) {
                            patternSizeLbl.setEnabled(true);
                            patternSizeSpinnerSlider.setEnabled(true);
                        } else {
                            patternSizeLbl.setEnabled(false);
                            patternSizeSpinnerSlider.setEnabled(false);

                        }
                    }

                });

        patternSizeLbl = new Label(inCmp, SWT.LEFT);
        patternSizeLbl.setText("Pattern Size ");

        patternSizeSpinnerSlider = new gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.SpinnerSlider(
                inCmp, SWT.HORIZONTAL, 1);
        patternSizeSpinnerSlider.setLayoutData(new GridData(140, 25));
        patternSizeSpinnerSlider.setMinimum(1);
        patternSizeSpinnerSlider.setMaximum(100);
        patternSizeSpinnerSlider.setIncrement(1);
        patternSizeSpinnerSlider.setPageIncrement(10);
        patternSizeSpinnerSlider.setDigits(1);

    }

    protected boolean validatePatternSize(VerifyEvent ve) {
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
                    if (value >= MIN_PATTERN_SIZE && value <= MAX_PATTERN_SIZE) {
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
     * Create widgets for the Closed attribute
     */
    private void createCloseAttr(Composite comp) {
        Composite inCmp = new Composite(comp, SWT.NONE);
        inCmp.setLayout(getGridLayout(2, false, 0, 0, 0, 0));

        chkBox[ChkBox.CLOSE.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.CLOSE.ordinal()].setLayoutData(new GridData(CHK_WIDTH,
                CHK_HEIGHT));
        chkBox[ChkBox.CLOSE.ordinal()]
                .addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button btn = (Button) e.widget;
                        if (btn.getSelection()) {
                            closedBtn.setEnabled(true);
                        } else {
                            closedBtn.setEnabled(false);

                        }
                    }

                });
        closedBtn = new Button(inCmp, SWT.CHECK);
        closedBtn.setText("Closed");
    }

    /**
     * Create widgets for the fill patterns attribute
     */
    private void createFillPatternAttr() {

        Composite inCmp = new Composite(top, SWT.NONE);
        inCmp.setLayout(getGridLayout(3, false, 0, 0, 0, 0));

        chkBox[ChkBox.FILL_PATTERN.ordinal()] = new Button(inCmp, SWT.CHECK);
        chkBox[ChkBox.FILL_PATTERN.ordinal()].setLayoutData(new GridData(
                CHK_WIDTH, CHK_HEIGHT));
        chkBox[ChkBox.FILL_PATTERN.ordinal()]
                .addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button btn = (Button) e.widget;
                        if (btn.getSelection()) {
                            fillPatternLbl.setEnabled(true);
                            fillPatternCbo.setEnabled(true);
                        } else {
                            fillPatternLbl.setEnabled(false);
                            fillPatternCbo.setEnabled(false);

                        }
                    }

                });

        fillPatternLbl = new Label(inCmp, SWT.LEFT);
        fillPatternLbl.setText("Fill Pattern ");

        fillPatternCbo = new SymbolCombo(inCmp);
        fillPatternCbo.setLayoutData(new GridData(10, 1));

        if (FILL_PATTERNS == null) {
            FILL_PATTERNS = new String[FillPatternList.FillPattern.values().length];
            FILL_PATTERN_MENU_ITEMS = new HashMap<String, Image>();

            int ii = 0;
            for (FillPattern fp : FillPatternList.FillPattern.values()) {
                FILL_PATTERNS[ii++] = fp.name();
            }
        }

        Image icons[] = new Image[FILL_PATTERNS.length];

        for (int ii = 0; ii < FILL_PATTERNS.length; ii++) {

            Image img = null;
            try {
                switch (ii) {
                case 7:
                    img = getFillPatternIcon("icons/patt_solid.gif");
                    break;
                case 8:
                    img = getFillPatternIcon("icons/patt_trans.gif");
                    break;
                case 0:
                    // shrink the first image. All other images in the list will
                    // follow.
                    Image orgImg = getFillPatternIcon("icons/patt00.gif");
                    img = new Image(this.getShell().getDisplay(), orgImg
                            .getImageData().scaledTo(40, 20));
                    break;
                case 1:
                    img = getFillPatternIcon("icons/patt01.gif");
                    break;
                case 2:
                    img = getFillPatternIcon("icons/patt02.gif");
                    break;
                case 3:
                    img = getFillPatternIcon("icons/patt03.gif");
                    break;
                case 4:
                    img = getFillPatternIcon("icons/patt04.gif");
                    break;
                case 5:
                    img = getFillPatternIcon("icons/patt05.gif");
                    break;
                case 6:
                    img = getFillPatternIcon("icons/patt06.gif");
                    break;
                }
            } catch (Exception e) {
                // no icon file
            }

            FILL_PATTERN_MENU_ITEMS.put(FILL_PATTERNS[ii], img);
            icons[ii] = img;
        }

        fillPatternCbo.setItems(FILL_PATTERNS, icons);

        fillPatternCbo.select(0);

        fillPatternCbo.setEnabled(false);

    }

    /*
     * Create widgets for the Filled attribute
     */
    private void createFillAttr(Composite comp) {

        Composite fillGrp = new Composite(comp, SWT.NONE);
        fillGrp.setLayout(getGridLayout(2, false, 0, 0, 0, 0));

        chkBox[ChkBox.FILL.ordinal()] = new Button(fillGrp, SWT.CHECK);
        chkBox[ChkBox.FILL.ordinal()].setLayoutData(new GridData(CHK_WIDTH,
                CHK_HEIGHT + 5));
        chkBox[ChkBox.FILL.ordinal()]
                .addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button btn = (Button) e.widget;
                        if (btn.getSelection()) {
                            filledBtn.setEnabled(true);
                        } else {
                            filledBtn.setEnabled(false);

                        }
                    }

                });

        filledBtn = new Button(fillGrp, SWT.CHECK);
        filledBtn.setText("Filled");
        filledBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                Button btn = (Button) e.widget;
                if (btn.getSelection()) {
                    fillPatternCbo.setEnabled(true);
                    fillPatternCbo.select(0);
                } else {
                    fillPatternCbo.setEnabled(false);

                }
            }

        });

    }

    @Override
    public int open() {

        if (this.getShell() == null || this.getShell().isDisposed()) {
            this.create();
        }

        if (PgenSession.getInstance().getPgenPalette().getCurrentAction()
                .equalsIgnoreCase("MultiSelect")
                && !(this instanceof GfaAttrDlg)) {
            enableChkBoxes(true);
            enableAllWidgets(false);
        } else {
            if (chkBox != null) {
                enableChkBoxes(false);
            }
        }

        return super.open();
    }

    /**
     * Set check boxes visible/invisible
     * 
     * @param flag
     */
    private void enableChkBoxes(boolean flag) {

        if (!flag) {
            setAllChkBoxes();
        }
        for (ChkBox chk : ChkBox.values()) {
            if (chkBox[chk.ordinal()] != null)
                chkBox[chk.ordinal()].setVisible(flag);
        }

    }

    /**
     * enable/disable all widgets
     * 
     * @param flag
     */
    private void enableAllWidgets(boolean flag) {

        colorLbl.setEnabled(flag);

        widthLbl.setEnabled(flag);
        widthSpinnerSlider.setEnabled(flag);

        smoothLbl.setEnabled(flag);
        smoothLvlCbo.setEnabled(flag);

        filledBtn.setEnabled(flag);
        closedBtn.setEnabled(flag);

        patternSizeLbl.setEnabled(flag);
        patternSizeSpinnerSlider.setEnabled(flag);

        fillPatternLbl.setEnabled(flag);

    }

    /**
     * Set all multi-selection check boxes to true.
     */
    private void setAllChkBoxes() {

        for (ChkBox chk : ChkBox.values()) {
            if (chkBox[chk.ordinal()] != null)
                chkBox[chk.ordinal()].setSelection(true);
        }
    }

    /**
     * Read fill pattern icons from PGEN icon directory
     */
    private Image getFillPatternIcon(String iconLocation) {

        ImageDescriptor id = Activator.imageDescriptorFromPlugin(
                Activator.PLUGIN_ID, iconLocation);
        Image icon = null;
        if (id != null) {
            icon = id.createImage();
        }

        return icon;
    }

    @Override
    public String getPatternName() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Coordinate[] getLinePoints() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Removes ghost line, handle bars, and closes the dialog
     */
    public void cancelPressed() {
        PgenUtil.setSelectingMode();
        super.cancelPressed();

    }

    /*
     * Create color, closed, fill attributes in one line.
     */
    private void createColorCloseFillAttr() {

        Composite inCmp = new Composite(top, SWT.NONE);
        inCmp.setLayout(getGridLayout(3, false, 0, 0, 0, 0));

        createColorAttr(inCmp);
        createCloseAttr(inCmp);
        createFillAttr(inCmp);

    }

}
