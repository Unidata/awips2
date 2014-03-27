/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.ContoursAttrDlg
 * 
 * October 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourCircle;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourMinmax;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.contours.IContours;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.graphtogrid.G2GCommon;
import gov.noaa.nws.ncep.ui.pgen.graphtogrid.GraphToGridParamDialog;
import gov.noaa.nws.ncep.ui.pgen.palette.PgenPaletteWindow;
import gov.noaa.nws.ncep.ui.pgen.tools.AbstractPgenTool;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenContoursTool;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenContoursTool.PgenContoursHandler;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenSelectHandler;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;

import org.dom4j.Document;
import org.dom4j.Node;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Singleton attribute dialog for Contours.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/09		#167		J. Wu   	Initial Creation.
 * 12/09		#167		J. Wu   	Allow editing line and label attributes.
 * 03/10		#215		J. Wu   	Make grid from contours.
 * 06/10		#215		J. Wu   	Added capability to draw mix/max symbols.
 * 11/10		#345		J. Wu   	Added capability to draw circle.
 * 12/10		#321		J. Wu   	Added capability to move labels.
 * 12/10		#167		J. Wu   	Added a second symbol.
 * 12/10		#312		J. Wu   	track the last-status of "Closed" button.
 * 01/11		#203		J. Wu   	Allow user to define more quick access symbols.
 * 02/11					J. Wu   	Implemented default "settings".
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 07/11		#?			J. Wu		Updated symbol selection panel and allowed closed
 * 08/11		#?			J. Wu   	TTR78: keep line/symbol/label attr window open.
 * 01/12		#?			J. Wu   	Fixed exceptions when closing the dialog.
 * 05/12		756			B. Yin		Fixed the place symbol exception
 * 12/13        1084        J. Wu       Add table-control for Cint in contoursInfo.xml
 * </pre>
 * 
 * @author J. Wu
 */

public class ContoursAttrDlg extends AttrDlg implements IContours,
        SelectionListener, ILine {

    private static ContoursAttrDlg INSTANCE = null;

    private static ContoursInfoDlg contoursInfoDlg = null;

    private static GraphToGridParamDialog g2gDlg = null;

    /**
     * Defines which type of DE to be drawn.
     */
    public static enum ContourDrawingStatus {
        DRAW_LINE, DRAW_SYMBOL, DRAW_CIRCLE, SELECT
    }

    private ContourDrawingStatus drawingStatus = ContourDrawingStatus.DRAW_LINE;

    private String contourParm = "HGMT";

    private String contourLevel = "1000";

    private String contourFcstHr = "f000";

    private Calendar contourTime1 = (Calendar) Calendar.getInstance();

    private Calendar contourTime2 = (Calendar) Calendar.getInstance();

    private String defCint = ContoursInfoDlg.getCints().get(
            contourParm + "-" + contourLevel);

    private String contourCint = (defCint != null && defCint.trim().length() > 0) ? defCint
            : "10/0/100";

    private final String SELECT_CONTOURLINE = "Select";

    private final String ADD_CONTOURLINE = "Add";

    private final String DELETE_CONTOURLINE = "Delete";

    private final int MAX_QUICK_SYMBOLS = 15;

    private int numOfQuickSymbols = 2;

    private Composite top;

    private Group attrComp;

    private Group textGrp;

    private Composite infoComp = null;

    private Button infoBtn = null;

    private Text labelTxt = null;

    private Button lineClosedBtn = null;

    private Button lineTypeBtn = null;

    private Composite labelGrp = null;

    private ArrayList<Button> labelBtns = null;

    private Spinner labelNumSpinner = null;

    private List<Button> quickSymbolBtns = null;

    private Button activeQuickSymbolBtn = null;

    private Button applyAllLineBtn = null;

    private Button applyAllLabelBtn = null;

    private Button applyAllSymbolBtn = null;

    private Button applyAllCircleBtn = null;

    private Button circleTypeBtn = null;

    private Button hideCircleLabelBtn = null;

    private Button selectLineBtn = null;

    private Button deleteLineBtn = null;

    private Contours currentContours = null;

    private LinkedHashMap<String, String> lineIconType = null;

    private LinkedHashMap<String, IConfigurationElement> symbolItemMap = null;

    private LinkedHashMap<String, Boolean> quickSymbolType = null;

    private LineTypeSelectionDlg lineTypePanel = null;

    private SymbolTypeSelectionDlg symbolTypePanel = null;

    private final int SYMBOLCOL = 16;

    private boolean drawClosedLine = false;

    /**
     * Default colors for the default and active product of layer name button.
     */
    private final Color defaultButtonColor = Color.lightGray;

    private final Color activeButtonColor = Color.green;

    /**
     * Line attribute dialog
     */
    private ContourLineAttrDlg lineAttrDlg;

    /**
     * stored line attribute
     */
    private Line lineTemplate = null;

    /**
     * Label text attribute dialog
     */
    private LabelAttrDlg labelAttrDlg;

    /**
     * stored label text attributeSlider
     */
    private gov.noaa.nws.ncep.ui.pgen.elements.Text labelTemplate = null;

    /**
     * Contours symbol attribute dialog
     */
    private ContourMinmaxAttrDlg minmaxAttrDlg;

    /**
     * stored symbol attribute
     */
    private Symbol minmaxTemplate = null;

    /**
     * Line attribute dialog
     */
    private ContourCircleAttrDlg circleAttrDlg;

    /**
     * stored line attribute
     */
    private Arc circleTemplate = null;

    /**
     * stored symbol attribute
     */
    private HashMap<String, AbstractDrawableComponent> contoursAttrSettings = null;

    private PgenContoursTool tool;

    /**
     * Private constructor
     * 
     * @param parShell
     * @throws VizException
     */
    private ContoursAttrDlg(Shell parShell) throws VizException {

        super(parShell);

        retrieveIconType();

        if (quickSymbolType == null) {
            quickSymbolType = getQuickSymbols();
        }

        if (quickSymbolBtns == null) {
            quickSymbolBtns = new ArrayList<Button>();
        }

        retrieveContoursSettings();

    }

    /**
     * Creates a Contours attribute dialog if the dialog does not exist and
     * returns the instance. If the dialog exists, return the instance.
     * 
     * @param parShell
     * @return
     */
    public static ContoursAttrDlg getInstance(Shell parShell) {

        if (INSTANCE == null) {

            try {

                INSTANCE = new ContoursAttrDlg(parShell);

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

        top = (Composite) super.createDialogArea(parent);
        this.getShell().setText("Contours Attributes");

        // Create the main layout for the dialog.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.verticalSpacing = 0;
        top.setLayout(mainLayout);

        // Button to pop up the dialog to editing the contour's info.
        infoComp = new Composite(top, SWT.NONE);
        infoComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));

        GridLayout layout = new GridLayout(2, false);
        layout.horizontalSpacing = 15;
        layout.marginHeight = 1;
        layout.marginWidth = 1;
        layout.horizontalSpacing = 1;
        infoComp.setLayout(layout);

        infoBtn = new Button(infoComp, SWT.PUSH);
        infoBtn.setToolTipText("Bring up the contours info attribute dialog");
        setInfoBtnText();

        infoBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                openContourInfoDlg();
            }
        });

        // Button to activate the graph-to-grid processing.
        Button makeGridBtn = new Button(infoComp, SWT.PUSH);
        makeGridBtn.setText("Make Grid");
        makeGridBtn.setToolTipText("Generate grid from this Contours");
        makeGridBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                openG2GDlg();
            }
        });

        addSeparator(top);

        /*
         * Create a composite to editing other attributes.
         */
        attrComp = new Group(top, SWT.SHADOW_ETCHED_OUT);
        GridLayout layout1 = new GridLayout(1, false);
        layout1.marginHeight = 1;
        layout1.marginWidth = 1;
        layout1.verticalSpacing = 0;
        attrComp.setLayout(layout1);

        /*
         * / Create a composite to editing the line attributes.
         */
        Composite lineComp = new Composite(attrComp, SWT.NONE);
        GridLayout layout2 = new GridLayout(2, false);
        layout2.horizontalSpacing = 1;
        layout2.marginWidth = 1;
        layout2.marginHeight = 1;
        layout2.verticalSpacing = 1;
        lineComp.setLayout(layout2);

        Composite closelineComp = new Composite(lineComp, SWT.NONE);
        GridLayout closelineCompGl = new GridLayout(3, false);
        closelineCompGl.verticalSpacing = 0;
        closelineCompGl.marginHeight = 0;
        closelineCompGl.marginWidth = 1;
        closelineComp.setLayout(closelineCompGl);

        lineTypeBtn = new Button(closelineComp, SWT.PUSH);
        ;
        lineTypeBtn.setToolTipText("Click to select a line type");
        String ltype = retrieveDefaultLineType();

        if (lineTemplate == null) {
            lineTemplate = (Line) contoursAttrSettings.get(ltype);
            lineTemplate.setPgenType(ltype);
        }

        setButtonColor(lineTypeBtn, defaultButtonColor,
                lineTemplate.getColors()[0]);
        lineTypeBtn.setImage(getIcon(ltype));
        lineTypeBtn.setData(ltype);

        lineTypeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (!ContoursAttrDlg.this.drawContourLine()) {
                    if (lineTemplate == null) {
                        lineTemplate = (Line) contoursAttrSettings
                                .get(retrieveDefaultLineType());
                        lineTemplate.setPgenType(retrieveDefaultLineType());
                    }
                    setButtonColor(lineTypeBtn, defaultButtonColor,
                            lineTemplate.getColors()[0]);

                    setDrawingStatus(ContourDrawingStatus.DRAW_LINE);
                } else {
                    openLineTypePanel();
                }
            }
        });

        lineClosedBtn = new Button(closelineComp, SWT.CHECK);
        ;
        lineClosedBtn.setText("Closed");
        lineClosedBtn.setToolTipText("Click to draw a closed line");
        lineClosedBtn.setSelection(drawClosedLine);
        lineClosedBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                drawClosedLine = lineClosedBtn.getSelection();
            }
        });

        Composite editlineComp = new Composite(lineComp, SWT.NONE);
        GridLayout editLineGl = new GridLayout(2, false);
        editLineGl.horizontalSpacing = 1;
        editLineGl.marginWidth = 1;
        editLineGl.verticalSpacing = 0;
        editLineGl.marginHeight = 0;
        editlineComp.setLayout(editLineGl);

        // Create a composite to editing the line attributes.
        Button lineAttrBtn = new Button(editlineComp, SWT.PUSH);
        ;
        lineAttrBtn.setText("Edit");
        lineAttrBtn.setToolTipText("Edit contour's line attributes");
        lineAttrBtn.addListener(SWT.MouseDown, new Listener() {

            @Override
            public void handleEvent(Event event) {
                try {
                    if (lineAttrDlg == null) {
                        lineAttrDlg = new ContourLineAttrDlg(PlatformUI
                                .getWorkbench().getActiveWorkbenchWindow()
                                .getShell());
                    }

                    openAttrDlg(lineAttrDlg);
                    lineAttrDlg.initDlg();

                    // get stored attributes
                    DrawableElement de = drawingLayer.getSelectedDE();

                    if (de != null && de.getParent() != null
                            && de.getParent() instanceof ContourLine) {
                        ContourLine pde = (ContourLine) de.getParent();
                        lineAttrDlg.setAttrForDlg((IAttribute) pde.getLine());
                    } else {

                        if (lineTemplate == null) {
                            lineTemplate = (Line) contoursAttrSettings
                                    .get(retrieveDefaultLineType());
                            lineTemplate.setPgenType(lineTypeBtn.getData()
                                    .toString());
                        }

                        lineAttrDlg.setAttrForDlg((IAttribute) lineTemplate);

                    }

                    // disable un-used attributes
                    lineAttrDlg.disableWidgets();

                } catch (VizException e) {
                    e.printStackTrace();
                }
            }
        });

        applyAllLineBtn = new Button(editlineComp, SWT.CHECK);
        ;
        applyAllLineBtn.setText("All");

        /*
         * Create buttons for adding Min/Max
         */
        Composite quickSymbolComp = new Composite(attrComp, SWT.NONE);
        GridLayout layoutm = new GridLayout(2, false);
        layoutm.marginWidth = 1;
        layoutm.marginHeight = 1;
        layoutm.verticalSpacing = 1;

        if (numOfQuickSymbols == 1) {
            layoutm.horizontalSpacing = 72;
        } else if (numOfQuickSymbols == 2) {
            layoutm.horizontalSpacing = 38;
        } else {
            layoutm.horizontalSpacing = 8;
        }

        quickSymbolComp.setLayout(layoutm);

        Composite quickSymbolTypeComp = new Composite(quickSymbolComp, SWT.NONE);
        GridLayout quickSymbolTypeCompGl = new GridLayout(2, false);
        quickSymbolTypeCompGl.marginHeight = 1;
        quickSymbolTypeCompGl.marginWidth = 1;
        quickSymbolTypeCompGl.verticalSpacing = 0;
        quickSymbolTypeComp.setLayout(quickSymbolTypeCompGl);

        Composite quickComp = new Composite(quickSymbolTypeComp, SWT.NONE);
        GridLayout quickCompGl = new GridLayout(3, false);
        quickCompGl.marginHeight = 1;
        quickCompGl.marginWidth = 1;
        quickCompGl.verticalSpacing = 1;
        quickCompGl.horizontalSpacing = 1;
        quickComp.setLayout(quickCompGl);

        int ii = 0;
        for (String str : quickSymbolType.keySet()) {

            if (quickSymbolType.get(str) && ii < MAX_QUICK_SYMBOLS) {

                ii++;

                Button btn = new Button(quickComp, SWT.PUSH);
                btn.setToolTipText(symbolItemMap.get(str).getAttribute("label"));
                btn.setImage(getIcon(str));
                btn.setData(str);

                quickSymbolBtns.add(btn);

                btn.addListener(SWT.MouseDoubleClick, new Listener() {
                    public void handleEvent(Event event) {
                        openSymbolPanel();
                    }
                });

                btn.addListener(SWT.MouseDown, new Listener() {
                    @Override
                    public void handleEvent(Event event) {

                        Button btnClicked = (Button) event.widget;
                        if (ContoursAttrDlg.this.drawSymbol()
                                && btnClicked == activeQuickSymbolBtn) {
                            openSymbolPanel();
                        } else {
                            activeQuickSymbolBtn = btnClicked;

                            for (Button b : quickSymbolBtns) {
                                if (b == activeQuickSymbolBtn) {
                                    DrawableElement de = (SinglePointElement) contoursAttrSettings
                                            .get(b.getData().toString());
                                    setButtonColor(b, defaultButtonColor,
                                            de.getColors()[0]);
                                } else {
                                    setButtonColor(b, activeButtonColor,
                                            defaultButtonColor);
                                }
                            }

                            if (!ContoursAttrDlg.this.drawSymbol()) {
                                setDrawingStatus(ContourDrawingStatus.DRAW_SYMBOL);
                            }
                        }
                    }
                });
            }
        }

        activeQuickSymbolBtn = quickSymbolBtns.get(0);
        DrawableElement de = (SinglePointElement) contoursAttrSettings
                .get(activeQuickSymbolBtn.getData().toString());
        setButtonColor(activeQuickSymbolBtn, defaultButtonColor,
                de.getColors()[0]);

        Composite editSymbolAttrComp = new Composite(quickSymbolComp, SWT.NONE);
        GridLayout editSymbolAttrCompGl = new GridLayout(2, false);
        editSymbolAttrCompGl.marginWidth = 1;
        editSymbolAttrCompGl.horizontalSpacing = 1;
        editSymbolAttrComp.setLayout(editSymbolAttrCompGl);

        Button editSymbolAttrBtn = new Button(editSymbolAttrComp, SWT.PUSH);
        ;
        editSymbolAttrBtn.setText("Edit");
        editSymbolAttrBtn.setToolTipText("Edit contour's symbol attributes");
        editSymbolAttrBtn.addListener(SWT.MouseDown, new Listener() {

            @Override
            public void handleEvent(Event event) {
                try {
                    if (minmaxAttrDlg == null) {
                        minmaxAttrDlg = new ContourMinmaxAttrDlg(PlatformUI
                                .getWorkbench().getActiveWorkbenchWindow()
                                .getShell());
                        minmaxAttrDlg.setPgenCategory("SYMBOL");
                        minmaxAttrDlg.setPgenType(getActiveSymbolObjType());
                    }

                    openAttrDlg(minmaxAttrDlg);

                    minmaxAttrDlg.initDlg();

                    // get stored attributes
                    DrawableElement de = drawingLayer.getSelectedDE();
                    if (de != null && de instanceof Symbol
                            && de.getParent() instanceof ContourMinmax) {
                        minmaxAttrDlg.setAttrForDlg((IAttribute) de);
                    } else {
                        minmaxTemplate = (Symbol) contoursAttrSettings
                                .get(activeQuickSymbolBtn.getData().toString());
                        if (minmaxTemplate == null) {
                            minmaxTemplate = new Symbol(null,
                                    new Color[] { Color.green }, 2.0F, 2.0,
                                    true, null, "Symbol",
                                    getActiveSymbolObjType());

                        }

                        minmaxAttrDlg
                                .setAttrForDlg((IAttribute) minmaxTemplate);

                    }

                } catch (VizException e) {
                    e.printStackTrace();
                }
            }
        });

        applyAllSymbolBtn = new Button(editSymbolAttrComp, SWT.CHECK);
        ;
        applyAllSymbolBtn.setText("All");

        /*
         * Create buttons for adding circle
         */
        Composite circleComp = new Composite(attrComp, SWT.NONE);
        GridLayout layoutc = new GridLayout(2, false);
        layoutc.horizontalSpacing = 12;
        layoutc.marginHeight = 1;
        layoutc.marginWidth = 1;
        layoutc.verticalSpacing = 1;
        circleComp.setLayout(layoutc);

        Composite circleTypeComp = new Composite(circleComp, SWT.NONE);
        GridLayout circleTypeCompGl = new GridLayout(3, false);
        circleTypeCompGl.marginHeight = 1;
        circleTypeCompGl.marginWidth = 1;
        circleTypeCompGl.verticalSpacing = 0;
        circleTypeComp.setLayout(circleTypeCompGl);

        circleTypeBtn = new Button(circleTypeComp, SWT.PUSH);
        circleTypeBtn.setToolTipText("Click to select a line type for circle");
        circleTypeBtn.setImage(getIcon("Circle"));
        circleTypeBtn.setData("Circle");
        if (circleTemplate == null) {
            circleTemplate = (Arc) contoursAttrSettings.get("Circle");
            circleTemplate.setPgenType("Circle");
        }
        setButtonColor(circleTypeBtn, circleTemplate.getColors()[0]);

        circleTypeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (!ContoursAttrDlg.this.drawCircle()) {
                    if (circleTemplate == null) {
                        circleTemplate = (Arc) contoursAttrSettings
                                .get("Circle");
                        circleTemplate.setPgenType("Circle");
                    }
                    setButtonColor(circleTypeBtn, circleTemplate.getColors()[0]);
                    setDrawingStatus(ContourDrawingStatus.DRAW_CIRCLE);
                }
                // openSymbolPanel();
            }
        });

        hideCircleLabelBtn = new Button(circleTypeComp, SWT.CHECK);
        ;
        hideCircleLabelBtn.setText("Hide\nLabel");
        hideCircleLabelBtn
                .setToolTipText("Check to hide the label for this circle");

        Composite editCircleComp = new Composite(circleComp, SWT.NONE);
        GridLayout editCircleCompGl = new GridLayout(2, false);
        editCircleCompGl.marginHeight = 0;
        editCircleCompGl.verticalSpacing = 0;
        editCircleCompGl.marginWidth = 1;
        editCircleCompGl.horizontalSpacing = 1;
        editCircleComp.setLayout(editCircleCompGl);

        Button circleAttrBtn = new Button(editCircleComp, SWT.PUSH);
        ;
        circleAttrBtn.setText("Edit");
        circleAttrBtn.setEnabled(true);
        circleAttrBtn.setToolTipText("Edit contour's circle attributes");

        circleAttrBtn.addListener(SWT.MouseDown, new Listener() {

            @Override
            public void handleEvent(Event event) {
                try {
                    if (circleAttrDlg == null) {
                        circleAttrDlg = new ContourCircleAttrDlg(PlatformUI
                                .getWorkbench().getActiveWorkbenchWindow()
                                .getShell());
                    }

                    openAttrDlg(circleAttrDlg);
                    circleAttrDlg.initDlg();

                    // get stored attributes
                    DrawableElement de = drawingLayer.getSelectedDE();

                    if (de != null && de.getParent() != null
                            && de.getParent() instanceof ContourCircle) {
                        ContourCircle pde = (ContourCircle) de.getParent();
                        circleAttrDlg.setAttrForDlg((IAttribute) pde
                                .getCircle());
                    } else {

                        if (circleTemplate == null) {
                            circleTemplate = (Arc) contoursAttrSettings
                                    .get("Circle");
                            circleTemplate.setPgenType("Circle");
                        }

                        circleAttrDlg
                                .setAttrForDlg((IAttribute) circleTemplate);

                    }

                    // disable un-used attributes
                    circleAttrDlg.disableWidgets();

                } catch (VizException e) {
                    e.printStackTrace();
                }
            }
        });

        applyAllCircleBtn = new Button(editCircleComp, SWT.CHECK);
        ;
        applyAllCircleBtn.setText("All");
        applyAllCircleBtn.setEnabled(true);

        // Create a composite to editing the label attributes.
        textGrp = new Group(attrComp, SWT.SHADOW_ETCHED_IN);
        textGrp.setText("Label");
        GridLayout labelGl = new GridLayout(1, false);
        labelGl.marginHeight = 1;
        labelGl.marginWidth = 1;
        labelGl.verticalSpacing = 1;
        labelGl.horizontalSpacing = 1;

        textGrp.setLayout(labelGl);

        Composite textComp = new Composite(textGrp, SWT.NONE);
        GridLayout layout3 = new GridLayout(2, false);
        layout3.horizontalSpacing = 1;
        layout3.marginWidth = 1;
        layout3.verticalSpacing = 0;
        layout3.marginHeight = 0;

        textComp.setLayout(layout3);

        Composite textValueComp = new Composite(textComp, SWT.NONE);
        GridLayout layout4 = new GridLayout(4, false);
        layout4.horizontalSpacing = 1;
        layout4.marginWidth = 1;

        textValueComp.setLayout(layout4);

        labelTxt = new Text(textValueComp, SWT.SINGLE);
        labelTxt.setLayoutData(new GridData(45, 15));
        labelTxt.setEditable(true);
        labelTxt.setText("0");
        labelTxt.addFocusListener(new FocusListener() {

            public void focusLost(FocusEvent e) {
                float value = 0;
                try {
                    value = Float.parseFloat(getLabel());
                    if (value == G2GCommon.RMISSD || value == -G2GCommon.RMISSD) {
                        lineClosedBtn.setSelection(true);
                        drawClosedLine = true;
                    }
                } catch (NumberFormatException el) {
                }

            }

            public void focusGained(FocusEvent e) {
            }
        });

        Button valueUpArrow = new Button(textValueComp, SWT.ARROW | SWT.UP);
        valueUpArrow.setLayoutData(new GridData(20, 22));
        valueUpArrow.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                changeLabel(true);
            }

        });

        Button valueDownArrow = new Button(textValueComp, SWT.ARROW | SWT.DOWN);
        valueDownArrow.setLayoutData(new GridData(20, 22));
        valueDownArrow.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                changeLabel(false);
            }

        });

        labelNumSpinner = new Spinner(textValueComp, SWT.BORDER);
        labelNumSpinner.setMinimum(1);
        labelNumSpinner.setMaximum(10);
        labelNumSpinner.setSelection(1);
        labelNumSpinner.setIncrement(1);
        labelNumSpinner.setPageIncrement(1);

        Composite editTextComp = new Composite(textComp, SWT.NONE);
        GridLayout editTextCompGl = new GridLayout(2, false);
        editTextCompGl.verticalSpacing = 0;
        editTextCompGl.marginHeight = 1;
        editTextCompGl.marginWidth = 1;
        editTextCompGl.horizontalSpacing = 1;
        editTextComp.setLayout(editTextCompGl);

        Button textAttrBtn = new Button(editTextComp, SWT.PUSH);
        ;
        textAttrBtn.setText("Edit");
        textAttrBtn.setToolTipText("Edit contour's label attributes");
        textAttrBtn.addListener(SWT.MouseDown, new Listener() {

            @Override
            public void handleEvent(Event event) {
                try {
                    if (labelAttrDlg == null) {
                        labelAttrDlg = new LabelAttrDlg(PlatformUI
                                .getWorkbench().getActiveWorkbenchWindow()
                                .getShell());
                    }

                    openAttrDlg(labelAttrDlg);

                    labelAttrDlg.initDlg();

                    // get stored attributes
                    DrawableElement de = drawingLayer.getSelectedDE();

                    if (de != null) {
                        if (de.getParent() instanceof ContourLine
                                && ((ContourLine) (de.getParent())).getLabels()
                                        .size() > 0) {
                            labelAttrDlg
                                    .setAttrForDlg((IAttribute) ((ContourLine) (de
                                            .getParent())).getLabels().get(0));
                        } else if (de.getParent() instanceof ContourMinmax
                                && ((ContourMinmax) (de.getParent()))
                                        .getLabel() != null) {
                            labelAttrDlg
                                    .setAttrForDlg((IAttribute) ((ContourMinmax) (de
                                            .getParent())).getLabel());
                        } else if (de.getParent() instanceof ContourCircle
                                && ((ContourCircle) (de.getParent()))
                                        .getLabel() != null) {
                            labelAttrDlg
                                    .setAttrForDlg((IAttribute) ((ContourCircle) (de
                                            .getParent())).getLabel());
                        }
                    } else {

                        if (labelTemplate == null) {
                            labelTemplate = (gov.noaa.nws.ncep.ui.pgen.elements.Text) contoursAttrSettings
                                    .get("General Text");
                        }

                        labelAttrDlg.setAttrForDlg((IAttribute) labelTemplate);
                    }

                    labelAttrDlg.setText(new String[] { getLabel() });

                    // disable un-used attributes
                    labelAttrDlg.disableWidgets();

                } catch (VizException e) {
                    e.printStackTrace();
                }
            }
        });

        applyAllLabelBtn = new Button(editTextComp, SWT.CHECK);
        ;
        applyAllLabelBtn.setText("All");

        // Create a composite to display contour levels created on the value of
        // CINT.
        createLabelBtns(textGrp, true);

        addSeparator(top);

        // Add buttons to add/delete a contour line.
        Composite editContourLineComp = new Composite(top, SWT.NONE);
        GridLayout editContourLineCompGl = new GridLayout(3, false);
        editContourLineCompGl.marginHeight = 1;
        editContourLineCompGl.verticalSpacing = 0;
        editContourLineComp.setLayout(editContourLineCompGl);

        editContourLineComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT,
                false, false));

        selectLineBtn = new Button(editContourLineComp, SWT.PUSH);
        ;
        selectLineBtn.setText(SELECT_CONTOURLINE);
        selectLineBtn.setToolTipText("Select a contour component");
        selectLineBtn.addSelectionListener(this);
        setButtonColor(selectLineBtn, defaultButtonColor);

        deleteLineBtn = new Button(editContourLineComp, SWT.PUSH);
        ;
        deleteLineBtn.setText(DELETE_CONTOURLINE);
        deleteLineBtn.setToolTipText("Delete a contour component");
        deleteLineBtn.addSelectionListener(this);
        setButtonColor(deleteLineBtn, defaultButtonColor);

        addSeparator(top);

        return top;

    }

    @Override
    public void setAttrForDlg(IAttribute ia) {
        // TODO Auto-generated method stub
    }

    /*
     * set attributes to the given attributes in an IContours
     */
    public void setAttrForDlg(IContours ic) {
        setAttributes(ic);
    }

    @Override
    public String getCint() {
        return contourCint;
    }

    @Override
    public String getLevel() {
        return contourLevel;
    }

    @Override
    public String getForecastHour() {
        return contourFcstHr;
    }

    @Override
    public String getParm() {
        return contourParm;
    }

    @Override
    public Calendar getTime1() {
        return contourTime1;
    }

    @Override
    public Calendar getTime2() {
        return contourTime2;
    }

    @Override
    public Boolean isClosedLine() {
        return lineClosedBtn.getSelection();
    }

    public String getLabel() {

        String lbl = labelTxt.getText();
        if (lbl == null || lbl.trim().length() == 0) {
            lbl = new String("0.0");
            setLabel(lbl);
        }

        return lbl;
    }

    /*
     * Get the number of labels.
     */
    public int getNumOfLabels() {

        int nlabels;

        // if ( isClosedLine() ) {
        // nlabels = 1;
        // }
        // else {
        nlabels = Integer.parseInt(labelNumSpinner.getText());
        // }

        return nlabels;
    }

    /*
     * Pops up the Contours information dialog
     */
    public void setNumOfLabels(int nlabels) {

        labelNumSpinner.setSelection(nlabels);

    }

    /*
     * Open the dialog to edit the contours info.
     */
    private void openContourInfoDlg() {

        if (contoursInfoDlg == null) {
            contoursInfoDlg = new ContoursInfoDlg(this.getShell());
            contoursInfoDlg.setContoursAttrDlg(this);
        }

        contoursInfoDlg.open();

    }

    /*
     * Set the dialog attributes.
     */
    public void setAttributes(IContours attr) {

        setParm(attr.getParm());
        setLevel(attr.getLevel());
        setFcstHr(attr.getForecastHour());
        setTime1(attr.getTime1());
        setTime2(attr.getTime2());
        setCint(attr.getCint());

        setInfoBtnText();
    }

    /*
     * Set the label for the contour line.
     */
    public void setLabel(String lbl) {
        labelTxt.setText(lbl);
        updateLabelBtnsSelection(lbl);
    }

    /*
     * Set parm
     */
    public void setParm(String parm) {
        contourParm = parm;
    }

    /*
     * Set level
     */
    public void setLevel(String level) {
        contourLevel = level;
    }

    /*
     * Set forecast hour
     */
    public void setFcstHr(String fcstHr) {
        contourFcstHr = fcstHr;
    }

    /*
     * Set time
     */
    public void setTime1(Calendar time) {
        contourTime1 = time;
    }

    /*
     * Set time
     */
    public void setTime2(Calendar time) {
        contourTime2 = time;
    }

    /*
     * set cint
     */
    public void setCint(String cint) {
        contourCint = cint;
        createLabelBtns(textGrp, false);
    }

    /*
     * set closed button
     */
    public void setClosed(boolean closed) {
        lineClosedBtn.setSelection(closed);
        drawClosedLine = closed;
    }

    /*
     * Change the label text to the text of the next selected button.
     */
    private void changeLabel(boolean upArrowClicked) {

        int ii = 0;
        for (Button btn : labelBtns) {

            if (btn.getSelection()) {

                int next;
                if (upArrowClicked) {
                    next = (ii + 1) % labelBtns.size();
                } else {
                    next = ((ii - 1) + labelBtns.size()) % labelBtns.size();
                }

                btn.setSelection(false);
                labelBtns.get(next).setSelection(true);
                labelTxt.setText(labelBtns.get(next).getText());
                break;
            }

            ii++;
        }

        if (labelTemplate != null) {
            labelTemplate.setText(new String[] { getLabel() });
        }

    }

    /*
     * Select the label button that has the same value as the given text.
     */
    private void updateLabelBtnsSelection(String lbl) {

        for (Button btn : labelBtns) {
            btn.setSelection(false);
        }

        for (Button btn : labelBtns) {
            if (btn.getText().equals(lbl)) {
                btn.setSelection(true);
                break;
            }
        }

    }

    /*
     * create label buttons based on the contours' Cint value.
     */
    private void createLabelBtns(Composite comp, boolean firstTime) {

        if (firstTime) {
            labelGrp = new Composite(comp, SWT.SHADOW_ETCHED_OUT);
            GridLayout gl = new GridLayout(3, true);
            gl.marginHeight = 1;
            gl.marginWidth = 1;
            gl.horizontalSpacing = 1;
            gl.verticalSpacing = 1;
            labelGrp.setLayout(gl);
        } else {
            Control[] wids = labelGrp.getChildren();
            if (wids != null) {
                for (int jj = 0; jj < wids.length; jj++) {
                    wids[jj].dispose();
                }
            }

        }

        // pack it to force the change.
        if (!firstTime)
            this.getShell().pack();

        // recreate.
        if (labelBtns != null) {
            labelBtns.clear();
        } else {
            labelBtns = new ArrayList<Button>();
        }

        /*
         * Note: zoom level starts from 1, not 0.
         */
        // CINT cd = new CINT(contourCint);
        // if (cd.isCINTStringParsed()) {
        List<String> cints = GraphToGridParamDialog.parseCints(contourCint);
        if (cints != null && cints.size() > 0) {
            // First figure out how many decimal digits are specified.
            /*
             * String cint = cd.getCINTString(CINT.FIRST_ZOOM_LEVEL); String[]
             * values = cint.split("/"); int ndecimals = 0; for (String str :
             * values) { int pos = str.indexOf("."); if (pos >= 0) { int nd =
             * str.length() - pos - 1; if (ndecimals < nd) ndecimals = nd; } }
             * 
             * for (String dbl : cd
             * .getContourLabelsForZoomLevel(CINT.FIRST_ZOOM_LEVEL)) {
             * 
             * // Format the label to the specified number of decimal digits.
             * String lblstr = new String(dbl); if (ndecimals == 0) { lblstr =
             * lblstr.substring(0, lblstr.indexOf(".")); } else { int len =
             * lblstr.length() - lblstr.indexOf(".") - 1; if (len < ndecimals) {
             * for (int ii = 0; ii < (ndecimals - len); ii++) { lblstr += "0"; }
             * } }
             */
            for (String lblstr : cints) {
                Button btn = new Button(labelGrp, SWT.RADIO);
                btn.setText(lblstr);
                btn.setData(lblstr);
                btn.addSelectionListener(new SelectionAdapter() {
                    public void widgetSelected(SelectionEvent event) {
                        labelTxt.setText(event.widget.getData().toString());
                        if (labelTemplate != null) {
                            labelTemplate.setText(new String[] { getLabel() });
                        }
                    }

                });

                labelBtns.add(btn);
            }

            if (labelBtns.size() > 0) {
                labelBtns.get(0).setSelection(true);
                labelTxt.setText(labelBtns.get(0).getText());
            }
        }

        /*
         * Repack
         */
        if (!firstTime) {
            this.getShell().pack();
            this.getShell().layout();
        }

    }

    @Override
    public void widgetDefaultSelected(SelectionEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void widgetSelected(SelectionEvent e) {

        if (e.widget instanceof Button) {

            Button b = (Button) e.widget;
            DrawableElement de = drawingLayer.getSelectedDE();

            if (de != null) {
                currentContours = (Contours) de.getParent().getParent();
            }

            /*
             * Which button?
             */
            String btnName = b.getText();

            /*
             * Close the attribute editing dialogs.
             */
            if (btnName.equals(SELECT_CONTOURLINE)
                    || btnName.equals(ADD_CONTOURLINE)
                    || btnName.equals(DELETE_CONTOURLINE)) {

                closeAttrEditDialogs();
            }

            /*
             * Notify the Drawing Tool to allow new contour line to be selected
             */
            if (btnName.equals(SELECT_CONTOURLINE)) {

                setButtonColor(selectLineBtn, activeButtonColor);
                setButtonColor(deleteLineBtn, defaultButtonColor);

                drawingLayer.removeSelected();

                this.setDrawingStatus(ContourDrawingStatus.SELECT);
                tool.setPgenSelectHandler();
            }

            /*
             * Draw and add a new contour line to the current contours and the
             * notify the drawing tool.
             */
            else if (btnName.equals(ADD_CONTOURLINE)) {

                tool.setPgenContoursHandler();

                this.getButton(IDialogConstants.CANCEL_ID).setEnabled(false);
                this.getButton(IDialogConstants.OK_ID).setEnabled(false);

                setButtonColor(selectLineBtn, defaultButtonColor);
                setButtonColor(deleteLineBtn, defaultButtonColor);

            }

            /*
             * Remove the selected contour line or min/max from the contours and
             * the notify the drawing tool.
             */
            else if (btnName.equals(DELETE_CONTOURLINE)) {

                setButtonColor(selectLineBtn, activeButtonColor);
                setButtonColor(deleteLineBtn, defaultButtonColor);

                if (de != null
                        && (de instanceof Line && de.getParent() instanceof ContourLine)
                        || (de instanceof Symbol && de.getParent() instanceof ContourMinmax)
                        || (de instanceof Arc && de.getParent() instanceof ContourCircle)) {

                    Contours oldContours = (Contours) de.getParent()
                            .getParent();
                    Contours newContours = new Contours();

                    if (((DECollection) oldContours).size() <= 1) {
                        // If no ContourLine left, start a new Contours.
                        drawingLayer.removeElement(oldContours);
                        newContours = null;
                    } else {

                        Iterator<AbstractDrawableComponent> iterator = oldContours
                                .getComponentIterator();

                        /*
                         * Remove the selected contour line and copy over other
                         * non-selected contour lines.
                         */
                        while (iterator.hasNext()) {

                            AbstractDrawableComponent oldAdc = iterator.next();
                            AbstractDrawableComponent newAdc = oldAdc.copy();

                            if (!(oldAdc.equals(de.getParent()))) {
                                newAdc.setParent(newContours);
                                newContours.add(newAdc);
                            }

                        }

                        /*
                         * Update the contours attributes and replace the old
                         * one with the new Contours.
                         */
                        newContours.update(this);
                        drawingLayer.replaceElement(oldContours, newContours);

                    }

                    /*
                     * Reset the current contours
                     */
                    currentContours = newContours;
                    drawingLayer.removeSelected();

                    if (tool != null)
                        tool.setCurrentContour(newContours);

                }

                if (mapEditor != null) {
                    mapEditor.refresh();
                }

            }
        }
    }

    /*
     * Set the text on the info button based on parm, level, and time.
     */
    private void setInfoBtnText() {

        String str = contourParm + ", " + contourLevel + ", " + contourFcstHr
                + "\n" + contourTime1.get(Calendar.YEAR) + "-"
                + (contourTime1.get(Calendar.MONTH) + 1) + "-"
                + contourTime1.get(Calendar.DAY_OF_MONTH) + "  "
                + contourTime1.get(Calendar.HOUR_OF_DAY) + ":"
                + contourTime1.get(Calendar.MINUTE) + "Z";

        infoBtn.setText(str);
        infoComp.pack();

    }

    /**
     * Check if the conotur line drawing button is selected.
     */
    public boolean drawContourLine() {

        return drawingStatus == ContourDrawingStatus.DRAW_LINE;

    }

    /**
     * Updates the selected contours and contour line, then redraws the PGEN
     * layer.
     */
    public void okPressed() {

        /*
         * Create a new Contours as well as a new ContourLine with a new line
         * and a label.
         */
        DrawableElement de = drawingLayer.getSelectedDE();
        if (de != null && de.getParent() != null
                && de.getParent().getParent() instanceof Contours) {

            DrawableElement newEl = (DrawableElement) de.copy();

            Contours oldContours = (Contours) de.getParent().getParent();

            Iterator<AbstractDrawableComponent> iterator = oldContours
                    .getComponentIterator();

            Contours newContours = new Contours();

            /*
             * Replace the selected contour line with a new contour line and
             * copy over other non-selected contour lines.
             */
            while (iterator.hasNext()) {

                AbstractDrawableComponent oldAdc = iterator.next();
                AbstractDrawableComponent newAdc = oldAdc.copy();

                if (oldAdc.equals(de.getParent())) {

                    newEl.setParent(newAdc);

                    if (oldAdc instanceof ContourCircle) {

                        if (newEl instanceof Arc) {
                            ((DECollection) newAdc)
                                    .replace(((ContourCircle) newAdc)
                                            .getCircle(), newEl);
                        }

                        ((ContourCircle) newAdc)
                                .updateLabelString(new String[] { getLabel() });
                        ((ContourCircle) newAdc).getLabel().setHide(
                                hideCircleLabel());
                    } else if (oldAdc instanceof ContourLine) {

                        if (newEl instanceof Line) {
                            ((DECollection) newAdc).replace(
                                    ((ContourLine) newAdc).getLine(), newEl);
                        }

                        ((ContourLine) newAdc).getLine().setPgenType(
                                getContourLineType());
                        ((ContourLine) newAdc).getLine().setClosed(
                                lineClosedBtn.getSelection());

                        int nlabels = ((ContourLine) newAdc).getNumOfLabels();
                        if (nlabels != getNumOfLabels()) {

                            ((ContourLine) newAdc)
                                    .updateNumOfLabels(getNumOfLabels());

                            if (newEl instanceof gov.noaa.nws.ncep.ui.pgen.elements.Text) {
                                if (((ContourLine) newAdc).getLabels().size() > 0) {
                                    newEl = ((ContourLine) newAdc).getLabels()
                                            .get(0);
                                } else {
                                    drawingLayer.removeSelected();
                                    newEl = null;
                                }
                            }
                        }

                        ((ContourLine) newAdc)
                                .updateLabelString(new String[] { getLabel() });

                    } else if (oldAdc instanceof ContourMinmax) {

                        if (newEl instanceof Symbol) {
                            ((DECollection) newAdc)
                                    .replace(((ContourMinmax) newAdc)
                                            .getSymbol(), newEl);
                        }

                        ((ContourMinmax) newAdc).getSymbol().setPgenCategory(
                                getActiveSymbolClass());
                        ((ContourMinmax) newAdc).getSymbol().setPgenType(
                                getActiveSymbolObjType());

                        ((ContourMinmax) newAdc)
                                .updateLabelString(new String[] { getLabel() });
                    }

                }

                newAdc.setParent(newContours);
                newContours.add(newAdc);
            }

            /*
             * Update the contours attributes and replace the old one with the
             * new Contours.
             */
            newContours.update(this);
            drawingLayer.replaceElement(oldContours, newContours);
            if (tool != null)
                tool.setCurrentContour(newContours);

            /*
             * Reset the selected contours and DE to the updated ones.
             */
            currentContours = newContours;
            drawingLayer.removeSelected();
            if (newEl != null) {
                drawingLayer.setSelected(newEl);
            }

        } else if (currentContours != null) {

            Contours oldContours = currentContours;

            if (!contourParm.equals(oldContours.getParm())
                    || !contourLevel.equals(oldContours.getLevel())
                    || !contourFcstHr.equals(oldContours.getForecastHour())
                    || !contourCint.equals(oldContours.getCint())) {

                Contours newContours = oldContours.copy();

                newContours.update(this);
                drawingLayer.replaceElement(oldContours, newContours);
                if (tool != null)
                    tool.setCurrentContour(newContours);

                currentContours = newContours;
            }

        }

        if (mapEditor != null) {
            mapEditor.refresh();
        }

    }

    /**
     * Disable Select/Add/Delete action buttons on dialog.
     */
    public void disableActionButtons() {

        selectLineBtn.setEnabled(false);
        deleteLineBtn.setEnabled(false);

    }

    /**
     * Close all contours related dialogs.
     */
    @Override
    public boolean close() {

        closeAttrEditDialogs();

        if (g2gDlg != null)
            g2gDlg.close();

        if (quickSymbolBtns != null && quickSymbolBtns.size() > 0) {
            quickSymbolBtns.clear();
        }

        currentContours = null;

        return super.close();

    }

    /**
     * Close line/label attribute editing dialogs.
     */
    private void closeAttrEditDialogs() {

        if (labelAttrDlg != null)
            labelAttrDlg.close();
        if (lineAttrDlg != null)
            lineAttrDlg.close();
        if (minmaxAttrDlg != null)
            minmaxAttrDlg.close();
        if (circleAttrDlg != null)
            circleAttrDlg.close();

    }

    /**
     * Returns the current contours.
     */
    public Contours getCurrentContours() {
        return currentContours;
    }

    /**
     * Sets the current contours.
     */
    public void setCurrentContours(Contours currentContours) {
        this.currentContours = currentContours;
    }

    /**
     * See if we need to update all line's attributes.
     */
    private Boolean updateAllLineAttr() {
        return applyAllLineBtn.getSelection();
    }

    /**
     * See if we need to update all label's attributes.
     */
    private Boolean updateAllLabelAttr() {
        return applyAllLabelBtn.getSelection();
    }

    /**
     * See if we need to update all circle's attributes.
     */
    private Boolean updateAllCircleAttr() {
        return applyAllCircleBtn.getSelection();
    }

    /**
     * @return the labelTemplate
     */
    public IAttribute getLabelTemplate() {
        if (labelAttrDlg != null && labelAttrDlg.getShell() != null) {
            return labelAttrDlg;
        } else if (labelTemplate != null) {
            return labelTemplate;
        } else {
            return (IAttribute) contoursAttrSettings.get("General Text");
        }
    }

    /**
     * @param labelTemplate
     *            the labelTemplate to set
     */
    public void setLabelTemplate(
            gov.noaa.nws.ncep.ui.pgen.elements.Text labelTemplate) {
        this.labelTemplate = labelTemplate;
    }

    /**
     * @return the lineTemplate
     */
    public IAttribute getLineTemplate() {
        if (lineAttrDlg != null && lineAttrDlg.getShell() != null) {
            return lineAttrDlg;
        } else if (lineTemplate != null) {
            return lineTemplate;
        } else {
            lineTemplate = (Line) contoursAttrSettings
                    .get(retrieveDefaultLineType());
            lineTemplate.setPgenType(lineTypeBtn.getData().toString());
            return lineTemplate;
        }
    }

    /**
     * @param lineTemplate
     *            the lineTemplate to set
     */
    public void setLineTemplate(Line lineTemplate) {
        this.lineTemplate = lineTemplate;
    }

    /**
     * @return the minmaxTemplate
     */
    public IAttribute getMinmaxTemplate() {
        if (minmaxAttrDlg != null && minmaxAttrDlg.getShell() != null) {
            return minmaxAttrDlg;
        } else {
            return (IAttribute) contoursAttrSettings.get(activeQuickSymbolBtn
                    .getData().toString());
        }
    }

    /**
     * @param minmaxTemplate
     *            the minmaxTemplate to set
     */
    public void setMinmaxTemplate(Symbol minmaxTemplate) {
        this.minmaxTemplate = minmaxTemplate;
    }

    /**
     * @return the circleTemplate
     */
    public IAttribute getCircleTemplate() {
        if (circleAttrDlg != null && circleAttrDlg.getShell() != null) {
            return circleAttrDlg;
        } else if (circleTemplate != null) {
            return circleTemplate;
        } else {
            circleTemplate = (Arc) contoursAttrSettings.get("Circle");
            return circleTemplate;
        }
    }

    /**
     * @param circleTemplate
     *            the circleTemplate to set
     */
    public void setCircleTemplate(Arc circleTemplate) {
        this.circleTemplate = circleTemplate;
    }

    /**
     * Initialize and open the line and label attribute editing dialog
     * 
     * @param dlg
     */
    private void openAttrDlg(AttrDlg dlg) {
        dlg.setBlockOnOpen(false);
        dlg.setDrawingLayer(drawingLayer);
        dlg.setMapEditor(mapEditor);
        dlg.open();
        dlg.enableButtons();
    }

    /**
     * See if we need to update all min/max symbol's attributes.
     */
    private Boolean updateAllMinmaxAttr() {
        return applyAllSymbolBtn.getSelection();
    }

    /**
     * Private Label Text dialog class
     * 
     * @author jwu
     * 
     */
    private class LabelAttrDlg extends TextAttrDlg {

        private LabelAttrDlg(Shell parShell) throws VizException {

            super(parShell);

        }

        /**
         * Update the label attributes
         */
        @Override
        public void okPressed() {

            /*
             * Update the label template first.
             */
            labelTemplate = (gov.noaa.nws.ncep.ui.pgen.elements.Text) new DrawableElementFactory()
                    .create(DrawableType.TEXT, this, "Text", "General Text",
                            (Coordinate) null, null);

            labelTemplate.setText(new String[] { getLabel() });

            /*
             * Update the contours.
             */
            updateLabelAttributes();

            this.close();

        }

        /**
         * closes the text attribute dialog only
         */
        @Override
        public void cancelPressed() {
            if (tool != null) {
                tool.setPgenContoursHandler();
            }
            PgenUtil.setSelectingMode();
            this.close();
        }

        /**
         * disable un-used widgets
         */
        private void disableWidgets() {
            text.setEnabled(false);
            textLabel.setEnabled(false);
        }

        /**
         * initialize dialog
         */
        private void initDlg() {
            this.getShell().setText("Contour Label Attributes");
            setBoxText(true, DisplayType.NORMAL);
        }

    }

    /**
     * Updates the selected contours' label attributes, then redraws the PGEN
     * layer.
     */
    private void updateLabelAttributes() {
        /*
         * If no element is selected and "All" is not checked, do nothing.
         */
        DrawableElement de = drawingLayer.getSelectedDE();

        if (de == null && !updateAllLabelAttr()) {
            return;
        }

        /*
         * Create a new Contours with all components in the old Contours and
         * update the label attributes if required.
         */
        DrawableElement newEl = null;
        Contours oldContours = null;

        if (de != null && de.getParent() != null
                && de.getParent().getParent() instanceof Contours) {
            newEl = (DrawableElement) de.copy();
            oldContours = (Contours) de.getParent().getParent();
        } else {
            oldContours = currentContours;
        }

        if (oldContours != null) {

            Iterator<AbstractDrawableComponent> iterator = oldContours
                    .getComponentIterator();

            Contours newContours = new Contours();

            /*
             * Copy all contour collections and update the label attributes.
             */
            while (iterator.hasNext()) {

                AbstractDrawableComponent oldAdc = iterator.next();
                AbstractDrawableComponent newAdc = oldAdc.copy();

                if (newAdc instanceof ContourLine) {

                    if (updateAllLabelAttr()) {
                        for (gov.noaa.nws.ncep.ui.pgen.elements.Text lbl : ((ContourLine) newAdc)
                                .getLabels()) {
                            String[] str = lbl.getText();
                            boolean hide = lbl.getHide();
                            boolean auto = lbl.getAuto();
                            lbl.update(labelTemplate);
                            lbl.setHide(hide);
                            lbl.setAuto(auto);
                            lbl.setText(str);
                        }
                    }

                    if (newEl != null && oldAdc.equals(de.getParent())) {

                        newEl.setParent(newAdc);

                        if (newEl instanceof Line) {
                            ((DECollection) newAdc).replace(
                                    ((ContourLine) newAdc).getLine(), newEl);
                        }

                        for (gov.noaa.nws.ncep.ui.pgen.elements.Text lbl : ((ContourLine) newAdc)
                                .getLabels()) {

                            if (lbl.equals(de)) {
                                ((DECollection) newAdc).replace(lbl, newEl);
                            }

                            boolean hide = lbl.getHide();
                            boolean auto = lbl.getAuto();
                            lbl.update(labelTemplate);
                            lbl.setHide(hide);
                            lbl.setAuto(auto);
                            lbl.setText(new String[] { getLabel() });
                        }

                    }
                } else if (newAdc instanceof ContourMinmax) {

                    if (updateAllLabelAttr()) {
                        gov.noaa.nws.ncep.ui.pgen.elements.Text lbl = ((ContourMinmax) newAdc)
                                .getLabel();
                        String[] str = lbl.getText();
                        boolean hide = lbl.getHide();
                        boolean auto = lbl.getAuto();
                        lbl.update(labelTemplate);
                        lbl.setHide(hide);
                        lbl.setAuto(auto);
                        lbl.setText(str);
                    }

                    if (newEl != null && oldAdc.equals(de.getParent())) {
                        newEl.setParent(newAdc);
                        if (newEl instanceof Symbol) {
                            ((DECollection) newAdc)
                                    .replace(((ContourMinmax) newAdc)
                                            .getSymbol(), newEl);
                        } else {
                            ((DECollection) newAdc).replace(
                                    ((ContourMinmax) newAdc).getLabel(), newEl);
                        }

                        gov.noaa.nws.ncep.ui.pgen.elements.Text lbl = ((ContourMinmax) newAdc)
                                .getLabel();
                        boolean hide = lbl.getHide();
                        boolean auto = lbl.getAuto();
                        lbl.update(labelTemplate);
                        lbl.setHide(hide);
                        lbl.setAuto(auto);
                        lbl.setText(new String[] { getLabel() });

                    }
                } else if (newAdc instanceof ContourCircle) {

                    if (updateAllLabelAttr()) {
                        gov.noaa.nws.ncep.ui.pgen.elements.Text lbl = ((ContourCircle) newAdc)
                                .getLabel();
                        String[] str = lbl.getText();
                        boolean hide = lbl.getHide();
                        boolean auto = lbl.getAuto();
                        lbl.update(labelTemplate);
                        lbl.setHide(hide);
                        lbl.setAuto(auto);
                        lbl.setText(str);
                    }

                    if (newEl != null && oldAdc.equals(de.getParent())) {
                        newEl.setParent(newAdc);
                        if (newEl instanceof Arc) {
                            ((DECollection) newAdc)
                                    .replace(((ContourCircle) newAdc)
                                            .getCircle(), newEl);
                        } else {
                            ((DECollection) newAdc).replace(
                                    ((ContourCircle) newAdc).getLabel(), newEl);
                        }

                        gov.noaa.nws.ncep.ui.pgen.elements.Text lbl = ((ContourCircle) newAdc)
                                .getLabel();
                        boolean hide = lbl.getHide();
                        boolean auto = lbl.getAuto();
                        lbl.update(labelTemplate);
                        lbl.setHide(hide);
                        lbl.setAuto(auto);
                        lbl.setText(new String[] { getLabel() });
                    }
                }

                newAdc.setParent(newContours);
                newContours.add(newAdc);
            }

            /*
             * Update the contours attributes and replace the old one with the
             * new Contours.
             */
            newContours.update(oldContours);
            drawingLayer.replaceElement(oldContours, newContours);

            /*
             * Reset the selected Contours and DE to the updated ones.
             */
            currentContours = newContours;
            if (tool != null)
                tool.setCurrentContour(newContours);

            if (newEl != null) {
                drawingLayer.removeSelected();
                drawingLayer.setSelected(newEl);
            }

        }

        if (mapEditor != null) {
            mapEditor.refresh();
        }

    }

    /**
     * Private Contour Line attribute dialog class
     * 
     * @author jwu
     * 
     */
    private class ContourLineAttrDlg extends LineAttrDlg {

        private ContourLineAttrDlg(Shell parShell) throws VizException {

            super(parShell);

        }

        /**
         * Update the line attributes
         */
        @Override
        public void okPressed() {
            /*
             * Update the line template first.
             */
            lineTemplate = (gov.noaa.nws.ncep.ui.pgen.elements.Line) new DrawableElementFactory()
                    .create(DrawableType.LINE, this, "Line", "LINE_SOLID",
                            (Coordinate) null, null);

            lineTemplate.setClosed(isClosedLine());

            if (ContoursAttrDlg.this.drawContourLine()) {
                setButtonColor(lineTypeBtn, defaultButtonColor,
                        lineTemplate.getColors()[0]);
            }

            /*
             * Update the Contours.
             */
            updateLineAttributes();
            this.close();
        }

        /**
         * closes the line attribute dialog only
         */
        @Override
        public void cancelPressed() {
            this.close();
        }

        /**
         * disable un-used widgets
         */
        private void disableWidgets() {
            closedBtn.setEnabled(false);
        }

        /**
         * initialize dialog
         */
        private void initDlg() {
            this.getShell().setText("Contour Line Attributes");
        }

    }

    /**
     * Updates the selected contours' line attributes, then redraws the PGEN
     * layer. Note: if
     */
    private void updateLineAttributes() {

        /*
         * If no ContourLine is selected and "All" is not checked, do nothing.
         */
        DrawableElement de = drawingLayer.getSelectedDE();

        // if ( de == null || de.getParent() == null || !(de.getParent()
        // instanceof ContourLine ) ) {
        // return;
        // }

        /*
         * Create a new Contours with all components in the old Contours and
         * update the line attributes if required.
         */
        DrawableElement newEl = null;

        Contours oldContours = null;

        if (de != null && de.getParent() instanceof ContourLine) {
            newEl = (DrawableElement) de.copy();
            oldContours = (Contours) de.getParent().getParent();
        } else {
            oldContours = currentContours;
        }

        if (oldContours != null) {

            Iterator<AbstractDrawableComponent> iterator = oldContours
                    .getComponentIterator();

            Contours newContours = new Contours();

            /*
             * Copy all contour lines and update the line attributes.
             */
            while (iterator.hasNext()) {

                AbstractDrawableComponent oldAdc = iterator.next();
                AbstractDrawableComponent newAdc = oldAdc.copy();

                if (newAdc instanceof ContourLine) {
                    if (updateAllLineAttr()) {
                        Line oneLine = ((ContourLine) newAdc).getLine();
                        Boolean isClosed = oneLine.isClosedLine();
                        oneLine.update(lineTemplate);
                        oneLine.setClosed(isClosed);
                    }

                    if (newEl != null && oldAdc.equals(de.getParent())) {
                        newEl.setParent(newAdc);
                        if (newEl instanceof Line) {
                            ((DECollection) newAdc).replace(
                                    ((ContourLine) newAdc).getLine(), newEl);
                        }

                        ((ContourLine) newAdc).getLine().update(lineTemplate);
                        ((ContourLine) newAdc).getLine().setClosed(
                                isClosedLine());

                    }

                }

                newAdc.setParent(newContours);
                newContours.add(newAdc);
            }

            /*
             * Update the contours attributes and replace the old one with the
             * new Contours.
             */
            newContours.update(oldContours);
            drawingLayer.replaceElement(oldContours, newContours);

            /*
             * Reset the selected contours and DE to the updated ones.
             */
            currentContours = newContours;
            if (tool != null)
                tool.setCurrentContour(newContours);

            if (newEl != null) {
                drawingLayer.removeSelected();
                drawingLayer.setSelected(newEl);
            }

        }

        if (mapEditor != null) {
            mapEditor.refresh();
        }

    }

    /**
     * Private Contour Circle attribute dialog class
     * 
     * @author jwu
     * 
     */
    private class ContourCircleAttrDlg extends ArcAttrDlg {

        private ContourCircleAttrDlg(Shell parShell) throws VizException {

            super(parShell);

        }

        /**
         * Update the circle attributes
         */
        @Override
        public void okPressed() {
            /*
             * Update the circle template first.
             */
            circleTemplate = (gov.noaa.nws.ncep.ui.pgen.elements.Arc) new DrawableElementFactory()
                    .create(DrawableType.ARC, this, "Arc", "Circle",
                            (Coordinate) null, null);

            if (ContoursAttrDlg.this.drawCircle()) {
                setButtonColor(circleTypeBtn, defaultButtonColor,
                        circleTemplate.getColors()[0]);
            }

            /*
             * Update the Contours.
             */
            updateCircleAttributes();

            this.close();
        }

        /**
         * closes the circle attribute dialog only
         */
        @Override
        public void cancelPressed() {
            this.close();
        }

        /**
         * disable un-used widgets
         */
        private void disableWidgets() {
            axisRatioLbl.setEnabled(false);
            axisRatioSlider.setEnabled(false);
            axisRatioText.setEnabled(false);

            startAngleLbl.setEnabled(false);
            startAngleSlider.setEnabled(false);
            startAngleText.setEnabled(false);

            endAngleLbl.setEnabled(false);
            endAngleSlider.setEnabled(false);
            endAngleText.setEnabled(false);
        }

        /**
         * initialize dialog
         */
        private void initDlg() {
            this.getShell().setText("Contour Circle Attributes");
        }

    }

    /**
     * Updates the selected contours' circle attributes, then redraws the PGEN
     * layer.
     */
    private void updateCircleAttributes() {

        /*
         * If no Contourcircle is selected and "All" is not checked, do nothing.
         */
        DrawableElement de = drawingLayer.getSelectedDE();

        // if ( de == null || de.getParent() == null || !(de.getParent()
        // instanceof ContourCircle ) ) {
        // return;
        // }

        /*
         * Create a new Contours with all components in the old Contours and
         * update the line attributes if required.
         */
        DrawableElement newEl = null;

        Contours oldContours = null;

        if (de != null && de.getParent() instanceof ContourCircle) {
            newEl = (DrawableElement) de.copy();
            oldContours = (Contours) de.getParent().getParent();
        } else {
            oldContours = currentContours;
        }

        if (oldContours != null) {

            Iterator<AbstractDrawableComponent> iterator = oldContours
                    .getComponentIterator();

            Contours newContours = new Contours();

            /*
             * Copy all contour lines and update the line attributes.
             */
            while (iterator.hasNext()) {

                AbstractDrawableComponent oldAdc = iterator.next();
                AbstractDrawableComponent newAdc = oldAdc.copy();

                if (newAdc instanceof ContourCircle) {
                    if (updateAllCircleAttr()) {
                        Arc oneCircle = (Arc) ((ContourCircle) newAdc)
                                .getCircle();
                        oneCircle.update(circleTemplate);
                    }

                    if (newEl != null && oldAdc.equals(de.getParent())) {
                        newEl.setParent(newAdc);
                        if (newEl instanceof Arc) {
                            ((DECollection) newAdc)
                                    .replace(((ContourCircle) newAdc)
                                            .getCircle(), newEl);
                        }

                        ((ContourCircle) newAdc).getCircle().update(
                                circleTemplate);

                    }

                }

                newAdc.setParent(newContours);
                newContours.add(newAdc);
            }

            /*
             * Update the contours attributes and replace the old one with the
             * new Contours.
             */
            newContours.update(oldContours);
            drawingLayer.replaceElement(oldContours, newContours);

            /*
             * Reset the selected contours and DE to the updated ones.
             */
            currentContours = newContours;
            if (tool != null)
                tool.setCurrentContour(newContours);

            if (newEl != null) {
                drawingLayer.removeSelected();
                drawingLayer.setSelected(newEl);
            }

        }

        if (mapEditor != null) {
            mapEditor.refresh();
        }

    }

    /*
     * Open the dialog to do graph-to-grid processing.
     */
    private void openG2GDlg() {

        if (g2gDlg == null) {
            try {

                g2gDlg = new GraphToGridParamDialog(PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell());
                g2gDlg.setCntAttrDlg(this);

            } catch (VizException e) {

                e.printStackTrace();
            }

        }

        if (g2gDlg != null) {
            g2gDlg.open();
        }

    }

    /**
     * Retrieve all line types, symbol and marker types defined in PGEN palette.
     */
    private void retrieveIconType() {

        PgenPaletteWindow plt = PgenSession.getInstance().getPgenPalette();

        // Get all Line types
        if (lineIconType == null) {
            lineIconType = new LinkedHashMap<String, String>();
        }

        List<String> lineObjNames = plt.getObjectNames("Lines");

        for (String str : lineObjNames) {
            lineIconType.put(str, "Lines");
        }

        // Get all Symbols
        symbolItemMap = new LinkedHashMap<String, IConfigurationElement>();

        HashMap<String, IConfigurationElement> itemMap = plt.getItemMap();
        for (String str : itemMap.keySet()) {
            IConfigurationElement ifg = itemMap.get(str);
            String type = ifg.getName();
            if (type.equalsIgnoreCase("object")) {
                String cls = ifg.getAttribute("className");
                if (cls.equals("Symbol") && !str.contains("TURBULENCE")) {
                    symbolItemMap.put(str, ifg);
                }
            }
        }

        // Get all Markers
        for (String str : itemMap.keySet()) {
            IConfigurationElement ifg = itemMap.get(str);
            String type = ifg.getName();
            if (type.equalsIgnoreCase("object")) {
                String cls = ifg.getAttribute("className");
                if (cls.equals("Marker")) {
                    symbolItemMap.put(str, ifg);
                }
            }
        }

        /*
         * Bet all Combo Symbols Combo symbol's class "ComboSymbol" and
         * symbol/marker's class is "Symbol", switching between them requires
         * more detailed handling - so we do not use combos for now.
         */
    }

    /**
     * Retrieve image for an icon defined in PGEN palette.
     */
    protected Image getIcon(String iconName) {

        PgenPaletteWindow plt = PgenSession.getInstance().getPgenPalette();

        return plt.getButtonImage(iconName);

    }

    /**
     * Check if the Symbol check button is selected.
     */
    public boolean drawSymbol() {

        return drawingStatus == ContourDrawingStatus.DRAW_SYMBOL;

    }

    /**
     * Set the symbol drawing button.
     */
    public void setDrawingSymbol() {
        setDrawingStatus(ContourDrawingStatus.DRAW_SYMBOL);
    }

    /**
     * Set the active symbol's type and image to a selected one. If not found in
     * the list, force to be the first one.
     */
    public void setActiveSymbol(DrawableElement elem) {

        boolean found = false;
        String symboltype = elem.getPgenType();
        Color clr = elem.getColors()[0];
        for (Button btn : quickSymbolBtns) {

            if (symboltype.equals(btn.getData().toString())) {
                btn.setToolTipText(symbolItemMap.get(symboltype).getAttribute(
                        "label"));
                btn.setImage(getIcon(symboltype));
                activeQuickSymbolBtn = btn;
                setButtonColor(btn, defaultButtonColor, clr);
                found = true;
            } else {
                setButtonColor(btn, activeButtonColor, defaultButtonColor);
            }
        }

        if (!found) {
            activeQuickSymbolBtn = quickSymbolBtns.get(0);
            activeQuickSymbolBtn.setData(symboltype);
            activeQuickSymbolBtn.setImage(getIcon(symboltype));
            setButtonColor(activeQuickSymbolBtn, defaultButtonColor, clr);
        }

        contoursAttrSettings.put(symboltype, elem);

    }

    /**
     * Get the active symbol's type (pgenType string).
     */
    public String getActiveSymbolObjType() {
        return activeQuickSymbolBtn.getData().toString();
    }

    /**
     * Get the active symbol's class (pgenCategory string).
     */
    public String getActiveSymbolClass() {
        return symbolItemMap.get(getActiveSymbolObjType()).getAttribute(
                "className");

    }

    /**
     * Set the Line Drawing button.
     */
    public void setDrawingLine() {
        setDrawingStatus(ContourDrawingStatus.DRAW_LINE);
    }

    /**
     * Set the line type and image.
     */
    public void setContourLineType(String str) {
        lineTypeBtn.setData(str);
        lineTypeBtn.setImage(getIcon(str));
    }

    /**
     * Get the line type (pgenType string).
     */
    public String getContourLineType() {
        return lineTypeBtn.getData().toString();
    }

    /**
     * Check if the circle drawing button is selected.
     */
    public boolean drawCircle() {
        return drawingStatus == ContourDrawingStatus.DRAW_CIRCLE;
    }

    /**
     * Set the circle drawing button.
     */
    public void setDrawingCircle() {
        setDrawingStatus(ContourDrawingStatus.DRAW_CIRCLE);
    }

    /**
     * Check if the circle label hiding button is selected.
     */
    public boolean hideCircleLabel() {
        return hideCircleLabelBtn.getSelection();
    }

    /**
     * Set the circle label hiding button.
     */
    public void setHideCircleLabel(boolean hide) {
        hideCircleLabelBtn.setSelection(hide);
    }

    /**
     * Class for a a list of Symbols to be selected for drawing min/max.
     * 
     * @author jun
     * 
     */
    public class LineTypeSelectionDlg extends Dialog {

        private List<String> objNames;

        private String selectedType;

        private Button activator;

        private String title;

        private Composite top;

        /**
         * constructor
         */
        public LineTypeSelectionDlg(Shell parent, List<String> objNames,
                Button activator, String title) {

            super(parent);

            this.objNames = objNames;
            this.activator = activator;
            this.selectedType = activator.getData().toString();
            this.title = title;

        }

        /**
         * Add Accept and Cancel buttons on the dialog's button bar.
         */
        @Override
        public void createButtonsForButtonBar(Composite parent) {

            super.createButtonsForButtonBar(parent);
            this.getButton(IDialogConstants.OK_ID).setText("Accept");

        }

        /**
         * @param activator
         *            the activator to set
         */
        public void setActivator(Button activator) {
            this.activator = activator;
        }

        /**
         * Creates the dialog area
         */
        @Override
        public Control createDialogArea(Composite parent) {

            top = (Composite) super.createDialogArea(parent);
            this.getShell().setText(title);

            GridLayout mainLayout = new GridLayout(SYMBOLCOL, false);
            mainLayout.marginHeight = 3;
            mainLayout.marginWidth = 3;
            mainLayout.horizontalSpacing = 0;
            mainLayout.verticalSpacing = 0;
            top.setLayout(mainLayout);

            final Color clr;
            if (lineTemplate != null) {
                clr = lineTemplate.getColors()[0];
            } else {
                clr = activeButtonColor;
            }

            for (String str : objNames) {
                Button btn = new Button(top, SWT.PUSH);
                btn.setData(str);
                btn.setImage(getIcon(str));

                if (str.equals(activator.getData().toString())) {
                    setButtonColor(btn, clr);
                } else {
                    setButtonColor(btn, defaultButtonColor);
                }

                btn.addListener(SWT.MouseDown, new Listener() {

                    @Override
                    public void handleEvent(Event event) {

                        Control[] wids = top.getChildren();

                        if (wids != null) {
                            for (int kk = 0; kk < wids.length; kk++) {
                                setButtonColor((Button) wids[kk],
                                        defaultButtonColor);
                            }
                        }

                        String objstr = event.widget.getData().toString();
                        selectedType = objstr;
                        setButtonColor((Button) event.widget, clr);

                    }
                });

            }

            return top;
        }

        /**
         * Updates the selected type's data string and image icon.
         */
        public void okPressed() {
            activator.setData(selectedType);
            activator.setImage(getIcon(selectedType));
            close();
        }

    }

    /**
     * Class for a a list of Symbols to be selected for drawing min/max.
     * 
     * @author jun
     * 
     */
    public class SymbolTypeSelectionDlg extends Dialog {

        private Composite top;

        private List<String> objNames;

        private String selectedType;

        private Button activator;

        private String title;

        /**
         * constructor
         */
        public SymbolTypeSelectionDlg(Shell parent, List<String> objNames,
                Button activator, String title) {

            super(parent);

            this.objNames = objNames;
            this.activator = activator;
            this.selectedType = activator.getData().toString();
            this.title = title;

        }

        /**
         * Add Accept and Cancel buttons on the dialog's button bar.
         */
        @Override
        public void createButtonsForButtonBar(Composite parent) {

            super.createButtonsForButtonBar(parent);
            this.getButton(IDialogConstants.OK_ID).setText("Accept");

        }

        /**
         * @param activator
         *            the activator to set
         */
        public void setActivator(Button activator) {
            this.activator = activator;
        }

        /**
         * Creates the dialog area
         */
        @Override
        public Control createDialogArea(Composite parent) {

            top = (Composite) super.createDialogArea(parent);
            this.getShell().setText(title);

            GridLayout mainLayout = new GridLayout(SYMBOLCOL, false);
            mainLayout.marginHeight = 3;
            mainLayout.marginWidth = 3;
            mainLayout.horizontalSpacing = 0;
            mainLayout.verticalSpacing = 0;
            top.setLayout(mainLayout);

            for (String str : objNames) {
                Button btn = new Button(top, SWT.PUSH);
                btn.setData(str);
                btn.setImage(getIcon(str));
                btn.setToolTipText(symbolItemMap.get(str).getAttribute("label"));

                if (str.equals(activator.getData().toString())) {
                    Color clr = defaultButtonColor;
                    SinglePointElement ade = (SinglePointElement) retrieveDefaultSettings(str);
                    if (ade != null) {
                        clr = ade.getColors()[0];
                    }
                    setButtonColor(btn, clr);
                } else {
                    setButtonColor(btn, defaultButtonColor);
                }

                btn.addListener(SWT.MouseDown, new Listener() {

                    @Override
                    public void handleEvent(Event event) {

                        Control[] wids = top.getChildren();

                        if (wids != null) {
                            for (int kk = 0; kk < wids.length; kk++) {
                                setButtonColor((Button) wids[kk],
                                        defaultButtonColor);
                            }
                        }

                        String objstr = event.widget.getData().toString();
                        selectedType = objstr;
                        Color clr = defaultButtonColor;
                        // if ( objstr.equals( activator.getData().toString() )
                        // && minmaxTemplate != null ) {
                        // clr = minmaxTemplate.getColors()[0];
                        // }
                        // else {
                        SinglePointElement ade = (SinglePointElement) retrieveDefaultSettings(objstr);
                        if (ade != null) {
                            clr = ade.getColors()[0];
                        }
                        // }

                        setButtonColor((Button) event.widget, clr);

                    }
                });

            }

            return top;
        }

        /**
         * Updates the selected type's data string and image icon.
         */
        public void okPressed() {

            /*
             * if ( !selectedType.equals(activator.getData().toString() ) &&
             * minmaxTemplate != null ) { Color clr =
             * minmaxTemplate.getColors()[0]; SinglePointElement ade =
             * (SinglePointElement)retrieveDefaultSettings( selectedType ); if (
             * ade != null ){ clr = ade.getColors()[0]; }
             * minmaxTemplate.setColors( new Color[]{clr, clr});
             * 
             * setButtonColor( activator, clr ); }
             */
            Color clr = activeButtonColor;
            ;
            SinglePointElement ade = (SinglePointElement) retrieveDefaultSettings(selectedType);
            if (ade != null) {
                clr = ade.getColors()[0];
            }
            setButtonColor(activator, clr);

            activator.setData(selectedType);
            activator.setImage(getIcon(selectedType));

            close();
        }

    }

    /*
     * Open the dialog to choose a symbol for drawing min/max.
     */
    private void openSymbolPanel() {

        if (symbolTypePanel == null) {
            List<String> objs = new ArrayList<String>(symbolItemMap.keySet());
            Shell sh = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            symbolTypePanel = new SymbolTypeSelectionDlg(sh, objs,
                    activeQuickSymbolBtn, "Select Contours Quick Access Symbol");
        }

        if (symbolTypePanel != null) {
            symbolTypePanel.setActivator(activeQuickSymbolBtn);
            symbolTypePanel.open();
        }

    }

    /*
     * Open the dialog to choose a line type for drawing.
     */
    private void openLineTypePanel() {

        if (lineTypePanel == null) {
            List<String> objs = new ArrayList<String>(lineIconType.keySet());
            Shell sh = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            lineTypePanel = new LineTypeSelectionDlg(sh, objs, lineTypeBtn,
                    "Contours Line Types");
        }

        if (lineTypePanel != null) {
            lineTypePanel.setActivator(lineTypeBtn);
            lineTypePanel.open();
        }

    }

    /**
     * Private Contour Min/Max attribute dialog class
     * 
     * @author jwu
     * 
     */
    private class ContourMinmaxAttrDlg extends LabeledSymbolAttrDlg {

        private PgenContoursTool tool = null;

        private ContourMinmaxAttrDlg(Shell parShell) throws VizException {

            super(parShell);

        }

        /**
         * Update the min/max attributes
         */
        @Override
        public void okPressed() {
            /*
             * Update the symbol template first.
             */
            minmaxTemplate = (gov.noaa.nws.ncep.ui.pgen.elements.Symbol) new DrawableElementFactory()
                    .create(DrawableType.SYMBOL, (IAttribute) this, "Symbol",
                            getActiveSymbolObjType(), (Coordinate) null, null);
            contoursAttrSettings.put(getActiveSymbolObjType(), minmaxTemplate);

            if (ContoursAttrDlg.this.drawSymbol()) {
                setButtonColor(activeQuickSymbolBtn, defaultButtonColor,
                        this.getColors()[0]);
            }

            /*
             * Update the Contours.
             */
            updateMinmaxAttributes();
            this.close();

        }

        /**
         * closes the line attribute dialog only
         */
        @Override
        public void cancelPressed() {
            this.close();
        }

        /**
         * initialize dialog
         */
        private void initDlg() {
            this.getShell().setText("Contour Min/Max Attributes");
            super.setLabelChkBox(false);
            AbstractPgenTool apt = (AbstractPgenTool) VizPerspectiveListener
                    .getCurrentPerspectiveManager().getToolManager()
                    .getSelectedModalTool("gov.noaa.nws.ncep.viz.ui.modalTool");
            if (apt instanceof PgenContoursTool)
                tool = (PgenContoursTool) apt;
            if (tool != null) {
                tool.resetUndoRedoCount();
                PgenSession.getInstance().getCommandManager()
                        .addStackListener(tool);
            }
        }

        @Override
        public boolean close() {
            if (tool != null) {
                tool.resetUndoRedoCount();
                PgenSession.getInstance().getCommandManager()
                        .removeStackListener(tool);
            }
            return super.close();
        }

        /**
         * Place the symbol at location from the lat/lon text fields
         */
        @Override
        protected void placeSymbol() {

            if (tool != null) {

                ((PgenContoursHandler) tool.getMouseHandler())
                        .drawContourMinmax(new Coordinate(Double
                                .parseDouble(longitudeText.getText()), Double
                                .parseDouble(latitudeText.getText())));
                placeBtn.setEnabled(false);
                undoBtn.setEnabled(true);
                undoBtn.setText("Undo Symbol");

            }
        }
    }

    /**
     * Updates the selected contours' line attributes, then redraws the PGEN
     * layer.
     */
    private void updateMinmaxAttributes() {

        /*
         * If no ContourLine is selected and "All" is not checked, do nothing.
         */
        DrawableElement de = drawingLayer.getSelectedDE();

        // if ( de == null || de.getParent() == null || !(de.getParent()
        // instanceof ContourMinmax ) ) {
        // return;
        // }

        /*
         * Create a new Contours with all components in the old Contours and
         * update the line attributes if required.
         */
        DrawableElement newEl = null;

        Contours oldContours = null;

        if (de != null && de.getParent() instanceof ContourMinmax) {
            newEl = (DrawableElement) de.copy();
            oldContours = (Contours) de.getParent().getParent();
        } else {
            oldContours = currentContours;
        }

        if (oldContours != null) {

            Iterator<AbstractDrawableComponent> iterator = oldContours
                    .getComponentIterator();

            Contours newContours = new Contours();

            /*
             * Copy all contour Minmax symbols and update the minmax attributes.
             */
            while (iterator.hasNext()) {

                AbstractDrawableComponent oldAdc = iterator.next();
                AbstractDrawableComponent newAdc = oldAdc.copy();

                if (newAdc instanceof ContourMinmax) {
                    if (updateAllMinmaxAttr()) {
                        Symbol oneSymb = (Symbol) ((ContourMinmax) newAdc)
                                .getSymbol();
                        oneSymb.update(minmaxTemplate);
                    }

                    if (newEl != null && oldAdc.equals(de.getParent())) {
                        newEl.setParent(newAdc);
                        if (newEl instanceof Symbol) {
                            ((DECollection) newAdc)
                                    .replace(((ContourMinmax) newAdc)
                                            .getSymbol(), newEl);
                        }

                        ((ContourMinmax) newAdc).getSymbol().update(
                                minmaxTemplate);
                    }
                }

                newAdc.setParent(newContours);
                newContours.add(newAdc);
            }

            /*
             * Update the contours attributes and replace the old one with the
             * new Contours.
             */
            newContours.update(oldContours);
            drawingLayer.replaceElement(oldContours, newContours);

            /*
             * Reset the selected contours and DE to the updated ones.
             */
            currentContours = newContours;
            if (tool != null)
                tool.setCurrentContour(newContours);

            if (newEl != null) {
                drawingLayer.removeSelected();
                drawingLayer.setSelected(newEl);
            }

        }

        if (mapEditor != null) {
            mapEditor.refresh();
        }

    }

    /**
     * Set a button's color
     */
    private void setButtonColor(Button btn, Color clr) {

        btn.setBackground(new org.eclipse.swt.graphics.Color(this.getShell()
                .getDisplay(), clr.getRed(), clr.getGreen(), clr.getBlue()));

    }

    /**
     * set a button's background/foreground color
     */
    private void setButtonColor(Button btn, Color fclr, Color bclr) {

        if (btn != null) {
            btn.setBackground(new org.eclipse.swt.graphics.Color(this
                    .getShell().getDisplay(), bclr.getRed(), bclr.getGreen(),
                    bclr.getBlue()));

            btn.setForeground(new org.eclipse.swt.graphics.Color(this
                    .getShell().getDisplay(), fclr.getRed(), fclr.getGreen(),
                    fclr.getBlue()));
        }
    }

    /**
     * Set the drawing status (LINE/MINMAX/CIRCLE)
     */
    public void setDrawingStatus(ContourDrawingStatus st) {

        if (tool != null) {
            tool.clearSelected();
        }

        switch (st) {

        case DRAW_SYMBOL:

            setButtonColor(lineTypeBtn, activeButtonColor, defaultButtonColor);
            setButtonColor(circleTypeBtn, activeButtonColor, defaultButtonColor);
            setButtonColor(selectLineBtn, defaultButtonColor);

            drawingStatus = ContourDrawingStatus.DRAW_SYMBOL;
            if (!(tool.getMouseHandler() instanceof PgenContoursHandler)) {
                tool.setPgenContoursHandler();
            }
            break;

        case DRAW_CIRCLE:

            setButtonColor(lineTypeBtn, activeButtonColor, defaultButtonColor);
            setButtonColor(activeQuickSymbolBtn, activeButtonColor,
                    defaultButtonColor);
            setButtonColor(selectLineBtn, defaultButtonColor);

            drawingStatus = ContourDrawingStatus.DRAW_CIRCLE;
            if (!(tool.getMouseHandler() instanceof PgenContoursHandler)) {
                tool.setPgenContoursHandler();
            }

            break;

        case DRAW_LINE:

            setButtonColor(activeQuickSymbolBtn, activeButtonColor,
                    defaultButtonColor);
            setButtonColor(circleTypeBtn, activeButtonColor, defaultButtonColor);
            setButtonColor(selectLineBtn, defaultButtonColor);

            lineClosedBtn.setSelection(drawClosedLine);
            drawingStatus = ContourDrawingStatus.DRAW_LINE;
            if (tool != null
                    && !(tool.getMouseHandler() instanceof PgenContoursHandler)) {
                tool.setPgenContoursHandler();
            }
            break;

        case SELECT:
            setSelectMode();
            break;

        default:

            setButtonColor(activeQuickSymbolBtn, activeButtonColor,
                    defaultButtonColor);
            setButtonColor(circleTypeBtn, activeButtonColor, defaultButtonColor);

            lineClosedBtn.setSelection(drawClosedLine);
            drawingStatus = ContourDrawingStatus.DRAW_LINE;

            break;
        }

    }

    /**
     * Set to selecting mode.
     */
    public void setSelectMode() {
        setButtonColor(activeQuickSymbolBtn, activeButtonColor,
                defaultButtonColor);
        setButtonColor(circleTypeBtn, activeButtonColor, defaultButtonColor);
        setButtonColor(lineTypeBtn, activeButtonColor, defaultButtonColor);
        setButtonColor(selectLineBtn, activeButtonColor);

        drawingStatus = ContourDrawingStatus.SELECT;

        if (!(tool.getMouseHandler() instanceof PgenSelectHandler)) {
            tool.setPgenSelectHandler();
        }
    }

    /**
     * Get a list of fast access symbols defined in for ContoursInfo.xml If none
     * of them is selected, the "H" and "L" will be selected by default.
     * 
     * @param
     * @return
     */
    private LinkedHashMap<String, Boolean> getQuickSymbols() {

        LinkedHashMap<String, Boolean> lbls = new LinkedHashMap<String, Boolean>();
        String xpath = ContoursInfoDlg.CNTRINFO_XPATH
                + "[@name='QuickSymbols']";

        Document dm = ContoursInfoDlg.readInfoTbl();

        int selected = 0;
        if (dm != null) {
            Node cntrInfo = dm.selectSingleNode(xpath);
            List<Node> nodes = cntrInfo.selectNodes("object");
            for (Node node : nodes) {
                String quick = node.valueOf("@quickAccess");
                if (quick != null && quick.trim().equalsIgnoreCase("true")) {
                    lbls.put(node.valueOf("@name"), true);
                    selected++;
                } else {
                    lbls.put(node.valueOf("@name"), false);
                }
            }
        }

        numOfQuickSymbols = selected;

        if (selected == 0) {
            lbls.put("FILLED_HIGH_PRESSURE_H", true);
            lbls.put("FILLED_LOW_PRESSURE_L", true);
            numOfQuickSymbols = 2;
        }

        return lbls;
    }

    /**
     * Retrieve default settings from "settings.tbl" for contour line, symbol,
     * label, and circle.
     * 
     * @param
     * @return
     */
    private void retrieveContoursSettings() {

        if (contoursAttrSettings == null) {
            contoursAttrSettings = new HashMap<String, AbstractDrawableComponent>();

            // Get all symbols/markers from "settings.tbl"
            for (String str : quickSymbolType.keySet()) {
                contoursAttrSettings.put(str, retrieveDefaultSettings(str));
            }

            // Get Default for Circle.
            // contoursAttrSettings.put( "Circle", retrieveDefaultSettings(
            // "Circle" ) );

            /*
             * Get line, text, symbols/markers, circles found in the Contours in
             * "settings.tbl"
             */
            AbstractDrawableComponent adc = retrieveDefaultSettings("Contours");
            boolean lineFound = false;
            boolean labelFound = false;
            boolean circleFound = false;
            if (adc != null && adc instanceof Contours) {
                List<ContourLine> cline = ((Contours) adc).getContourLines();
                if (cline != null && cline.size() > 0) {
                    Line ln = cline.get(0).getLine();
                    if (ln != null) {
                        contoursAttrSettings.put(ln.getPgenType(), ln.copy());
                        lineFound = true;
                    }

                    if (cline.get(0).getLabels() != null
                            && cline.get(0).getLabels().size() > 0) {
                        labelFound = true;
                        contoursAttrSettings.put(cline.get(0).getLabels()
                                .get(0).getPgenType(), cline.get(0).getLabels()
                                .get(0).copy());
                    }
                }

                List<ContourMinmax> csymbols = ((Contours) adc)
                        .getContourMinmaxs();
                if (csymbols != null && csymbols.size() > 0) {
                    for (ContourMinmax cmx : csymbols) {
                        contoursAttrSettings.put(cmx.getSymbol().getPgenType(),
                                cmx.getSymbol().copy());
                        if (!labelFound) {
                            contoursAttrSettings.put(cmx.getLabel()
                                    .getPgenType(), cmx.getLabel());
                            labelFound = true;
                        }
                    }
                }

                List<ContourCircle> ccircles = ((Contours) adc)
                        .getContourCircles();
                if (ccircles != null && ccircles.size() > 0) {
                    Arc cc = (Arc) ccircles.get(0).getCircle();
                    if (cc != null) {
                        contoursAttrSettings.put(cc.getPgenType(), cc.copy());
                        circleFound = true;
                    }

                    if (!labelFound) {
                        if (ccircles.get(0).getLabel() != null) {
                            labelFound = true;
                            contoursAttrSettings.put(ccircles.get(0).getLabel()
                                    .getPgenType(), ccircles.get(0).getLabel()
                                    .copy());
                        }
                    }
                }
            }

            // Build a default line, text, circle as template
            if (!lineFound) {
                Line dln = new Line(null, new Color[] { Color.red }, 2.0f, 2.0,
                        false, false, null, 2, FillPattern.SOLID, "Lines",
                        "LINE_SOLID");

                contoursAttrSettings.put("LINE_SOLID", dln);
            }

            if (!labelFound) {
                gov.noaa.nws.ncep.ui.pgen.elements.Text txt = new gov.noaa.nws.ncep.ui.pgen.elements.Text(
                        null, "Courier", 14.0f, TextJustification.CENTER, null,
                        0.0, TextRotation.SCREEN_RELATIVE,
                        new String[] { "text" }, FontStyle.REGULAR,
                        Color.GREEN, 0, 0, true, DisplayType.NORMAL, "Text",
                        "General Text");

                contoursAttrSettings.put("General Text", txt);
            }

            if (!circleFound) {

                Arc ccr = new Arc(null, Color.red, 2.0f, 2.0, false, false, 2,
                        FillPattern.SOLID, "Circle", null, null, "Arc", 1.0,
                        0.0, 360.0);

                contoursAttrSettings.put("Circle", ccr);
            }
        }
    }

    /**
     * Retrieve default settings for a given type of DE.
     * 
     * @param pgenType
     * @return AbstractDrawableComponent
     */
    private AbstractDrawableComponent retrieveDefaultSettings(String pgenType) {
        return AttrSettings.getInstance().getSettings().get(pgenType);
    }

    /**
     * Retrieve default line type for Contour Line.
     * 
     * @return String
     */
    private String retrieveDefaultLineType() {
        retrieveContoursSettings();
        String type = new String("LINE_SOLID");
        for (AbstractDrawableComponent adc : contoursAttrSettings.values()) {
            if (adc instanceof Line && !(adc instanceof Arc)) {
                type = new String(adc.getPgenType());
                break;
            }
        }

        return type;
    }

    @Override
    public Coordinate[] getLinePoints() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getPatternName() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public int getSmoothFactor() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public Boolean isFilled() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public FillPattern getFillPattern() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * set the drawing tool(can be PgenSeletingTool or PgenJetDrawingTool)
     */
    public void setDrawingTool(PgenContoursTool tool) {
        this.tool = tool;
    }

    /**
     * Removes ghost line, handle bars, and closes the dialog
     */
    public void cancelPressed() {

        PgenUtil.setSelectingMode();
        super.cancelPressed();

    }
}
