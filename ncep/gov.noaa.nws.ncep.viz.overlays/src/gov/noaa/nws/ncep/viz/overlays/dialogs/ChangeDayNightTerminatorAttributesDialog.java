package gov.noaa.nws.ncep.viz.overlays.dialogs;

import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerType;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.viz.overlays.Activator;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.EnumMap;
import java.util.Map;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

/**
 * Provides an interface to modify the day/night terminator overlay parameters
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/29/2014   1130        S. Gurung     Initial creation
 * 
 * @author sgurung
 * @version 1.0
 */

public class ChangeDayNightTerminatorAttributesDialog extends
        AbstractEditResourceAttrsDialog {

    private final static org.apache.log4j.Logger log = org.apache.log4j.Logger
            .getLogger(ChangeDayNightTerminatorAttributesDialog.class);

    protected ResourceAttrSet prevRscAttrSet = null;

    private RscAttrValue termLineColor = null;

    private RscAttrValue termLineWidth = null;

    private RscAttrValue termLineStyle = null;

    private RscAttrValue sunMarkerColor = null;

    private RscAttrValue sunMarkerType = null;

    private RscAttrValue sunMarkerSize = null;

    private RscAttrValue sunMarkerWidth = null;

    private RscAttrValue displaySun = null;

    private MarkerType selSunMarkerType = null; // working copy

    private RscAttrValue midnightMeridianLineStyle = null;

    private RscAttrValue midnightMeridianLineColor = null;

    private RscAttrValue midnightMeridianLineWidth = null;

    private RscAttrValue displayMidnightMeridian = null;

    private RscAttrValue dayShadeColor = null;

    private RscAttrValue nightShadeColor = null;

    private RscAttrValue shadeAlpha = null;

    private RscAttrValue shadePattern = null;

    private RscAttrValue applyShading = null;

    private FillPattern selFillPatternType = null; // working copy

    private float lineLengthFactor = 1;

    private Button[] selectLineWidthButtons;

    private Map<LineStyle, Button> termLineStyleButtonMap;

    private Button[] selectMmLineWidthButtons;

    private Map<LineStyle, Button> mmLineStyleButtonMap;

    private final LineStyle[] lineStyleButtonSequence = { // ...for 2-column
            // grid layout
            LineStyle.DOTS, LineStyle.LONG_DASHED, LineStyle.SOLID,
            LineStyle.LONG_DASH_THREE_SHORT_DASHES, LineStyle.SHORT_DASHED,
            LineStyle.LONG_DASH_DOT, LineStyle.MEDIUM_DASHED,
            LineStyle.LONG_DASH_THREE_DOTS, LineStyle.LONG_DASH_SHORT_DASH,
            LineStyle.MEDIUM_DASH_DOT, };

    public ChangeDayNightTerminatorAttributesDialog(Shell parentShell,
            INatlCntrsResourceData rd, Boolean apply) {
        super(parentShell, rd, apply);
    }

    @Override
    public Composite createDialog(Composite composite) {
        final Display display = composite.getDisplay();

        FormLayout layout0 = new FormLayout();
        composite.setLayout(layout0);

        prevRscAttrSet = new ResourceAttrSet(editedRscAttrSet);

        termLineStyle = editedRscAttrSet.getRscAttr("termLineStyle");
        termLineColor = editedRscAttrSet.getRscAttr("termLineColor");
        termLineWidth = editedRscAttrSet.getRscAttr("termLineWidth");

        sunMarkerColor = editedRscAttrSet.getRscAttr("sunMarkerColor");
        sunMarkerType = editedRscAttrSet.getRscAttr("sunMarkerType");
        sunMarkerSize = editedRscAttrSet.getRscAttr("sunMarkerSize");
        sunMarkerWidth = editedRscAttrSet.getRscAttr("sunMarkerWidth");

        midnightMeridianLineColor = editedRscAttrSet
                .getRscAttr("midnightMeridianLineColor");
        midnightMeridianLineWidth = editedRscAttrSet
                .getRscAttr("midnightMeridianLineWidth");
        midnightMeridianLineStyle = editedRscAttrSet
                .getRscAttr("midnightMeridianLineStyle");

        dayShadeColor = editedRscAttrSet.getRscAttr("dayShadeColor");
        nightShadeColor = editedRscAttrSet.getRscAttr("nightShadeColor");
        shadeAlpha = editedRscAttrSet.getRscAttr("shadeAlpha");
        shadePattern = editedRscAttrSet.getRscAttr("shadePattern");

        displaySun = editedRscAttrSet.getRscAttr("displaySun");
        applyShading = editedRscAttrSet.getRscAttr("applyShading");
        displayMidnightMeridian = editedRscAttrSet
                .getRscAttr("displayMidnightMeridian");

        selSunMarkerType = (MarkerType) sunMarkerType.getAttrValue();
        selFillPatternType = (FillPattern) getFillPattern(shadePattern
                .getAttrValue().toString());

        // confirm the classes of the attributes..
        if (termLineStyle.getAttrClass() != LineStyle.class) {
            System.out.println("termLineStyle is not of expected class? "
                    + termLineStyle.getAttrClass().toString());
        } else if (termLineColor.getAttrClass() != RGB.class) {
            System.out.println("termLineColor is not of expected class? "
                    + termLineColor.getAttrClass().toString());
        } else if (termLineWidth.getAttrClass() != Integer.class) {
            System.out.println("termLineWidth is not of expected class? "
                    + termLineWidth.getAttrClass().toString());
        } else if (sunMarkerColor.getAttrClass() != RGB.class) {
            System.out.println("sunMarkerColor is not of expected class? "
                    + sunMarkerColor.getAttrClass().toString());
        } else if (sunMarkerType.getAttrClass() != MarkerType.class) {
            System.out.println("sunMarkerType is not of expected class? "
                    + sunMarkerType.getAttrClass().toString());
        } else if (sunMarkerSize.getAttrClass() != Float.class) {
            System.out.println("sunMarkerSize is not of expected class? "
                    + sunMarkerSize.getAttrClass().toString());
        } else if (sunMarkerWidth.getAttrClass() != Integer.class) {
            System.out.println("sunMarkerWidth is not of expected class? "
                    + sunMarkerWidth.getAttrClass().toString());
        } else if (midnightMeridianLineWidth.getAttrClass() != Integer.class) {
            System.out
                    .println("midnightMeridianLineWidth is not of expected class? "
                            + midnightMeridianLineWidth.getAttrClass()
                                    .toString());
        } else if (midnightMeridianLineWidth.getAttrClass() != Integer.class) {
            System.out
                    .println("midnightMeridianLineWidth is not of expected class? "
                            + midnightMeridianLineWidth.getAttrClass()
                                    .toString());
        } else if (midnightMeridianLineColor.getAttrClass() != RGB.class) {
            System.out
                    .println("midnightMeridianLineColor is not of expected class? "
                            + midnightMeridianLineColor.getAttrClass()
                                    .toString());
        } else if (midnightMeridianLineStyle.getAttrClass() != LineStyle.class) {
            System.out
                    .println("midnightMeridianLineStyle is not of expected class? "
                            + midnightMeridianLineStyle.getAttrClass()
                                    .toString());
        } else if (dayShadeColor.getAttrClass() != RGB.class) {
            System.out.println("dayShadeColor is not of expected class? "
                    + dayShadeColor.getAttrClass().toString());
        } else if (nightShadeColor.getAttrClass() != RGB.class) {
            System.out.println("nightShadeColor is not of expected class? "
                    + nightShadeColor.getAttrClass().toString());

        } else if (shadeAlpha.getAttrClass() != Float.class) {
            System.out.println("shadeAlpha is not of expected class? "
                    + shadeAlpha.getAttrClass().toString());
        } else if (shadePattern.getAttrClass() != String.class) {
            System.out.println("shadePattern is not of expected class? "
                    + shadePattern.getAttrClass().toString());
        } else if (displaySun.getAttrClass() != Boolean.class) {
            System.out.println("displaySun is not of expected class? "
                    + displaySun.getAttrClass().toString());
        } else if (applyShading.getAttrClass() != Boolean.class) {
            System.out.println("applyShading is not of expected class? "
                    + applyShading.getAttrClass().toString());
        } else if (displayMidnightMeridian.getAttrClass() != Boolean.class) {
            System.out
                    .println("displayMidnightMeridian is not of expected class? "
                            + displayMidnightMeridian.getAttrClass().toString());
        }

        // Lay out the various groups within the dialog

        composite.setLayout(new GridLayout(2, true));
        GridData gd = new GridData();

        // Sub-solar Point

        Group sunMarkerGroup = new Group(composite, SWT.SHADOW_NONE);
        sunMarkerGroup.setText("Sub-solar Point");
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        sunMarkerGroup.setLayoutData(gd);
        sunMarkerGroup.setLayout(new FormLayout());

        createSunMarkerControls(sunMarkerGroup, display);

        // Day/Night Shading

        Group shadingGroup = new Group(composite, SWT.SHADOW_NONE);
        shadingGroup.setText("Day/Night Shading");

        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        shadingGroup.setLayoutData(gd);
        shadingGroup.setLayout(new FormLayout());

        createShadingControls(shadingGroup, composite);

        // Terminator Line

        Group termGroup = new Group(composite, SWT.SHADOW_NONE);
        termGroup.setText("Terminator Line");
        // GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;

        termGroup.setLayoutData(gd);
        termGroup.setLayout(new FormLayout());

        createTerminatorControls(termGroup, display);

        // Midnight Meridian Line

        Group midnightMeriLineGroup = new Group(composite, SWT.SHADOW_NONE);
        midnightMeriLineGroup.setText("Midnight Meridian Line");

        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        midnightMeriLineGroup.setLayoutData(gd);
        midnightMeriLineGroup.setLayout(new FormLayout());

        createMidnightMeridianControls(midnightMeriLineGroup, display);

        return composite;
    }

    public void createTerminatorControls(Group termGroup, final Display display) {

        Group linePreviewAreaGroup = new Group(termGroup, SWT.SHADOW_NONE);
        linePreviewAreaGroup.setLayout(new FillLayout());

        FormData formData0 = new FormData();
        formData0.top = new FormAttachment(5, 0);
        formData0.left = new FormAttachment(2, 0);
        formData0.width = 196;
        formData0.height = 30;
        linePreviewAreaGroup.setLayoutData(formData0);

        Group selectLineWidthGroup = new Group(termGroup, SWT.SHADOW_NONE);
        selectLineWidthGroup.setText("Width");
        GridLayout lineWidthGridLayout = new GridLayout();
        lineWidthGridLayout.numColumns = 2;
        lineWidthGridLayout.marginHeight = 18;
        lineWidthGridLayout.marginWidth = 18;
        lineWidthGridLayout.horizontalSpacing = 8;
        lineWidthGridLayout.verticalSpacing = 8;
        selectLineWidthGroup.setLayout(lineWidthGridLayout);

        FormData formData1 = new FormData();
        formData1.top = new FormAttachment(linePreviewAreaGroup, 7);
        formData1.left = new FormAttachment(2, 0);
        selectLineWidthGroup.setLayoutData(formData1);

        Group selectLineStyleGroup = new Group(termGroup, SWT.SHADOW_NONE);
        selectLineStyleGroup.setText("Style");
        GridLayout lineStyleGridLayout = new GridLayout();
        lineStyleGridLayout.numColumns = 2;
        lineStyleGridLayout.marginHeight = 18;
        lineStyleGridLayout.marginWidth = 18;
        lineStyleGridLayout.horizontalSpacing = 8;
        lineStyleGridLayout.verticalSpacing = 8;
        selectLineStyleGroup.setLayout(lineStyleGridLayout);

        FormData formData2 = new FormData();
        formData2.left = new FormAttachment(linePreviewAreaGroup, 16);
        formData2.top = new FormAttachment(2, 7);
        selectLineStyleGroup.setLayoutData(formData2);

        Group selectLineColorGroup = new Group(termGroup, SWT.SHADOW_NONE);
        selectLineColorGroup.setText("Color");
        GridLayout lineColorGridLayout = new GridLayout();
        lineColorGridLayout.numColumns = 1;
        lineColorGridLayout.marginHeight = 7;
        lineColorGridLayout.marginWidth = 55;
        lineColorGridLayout.horizontalSpacing = 0;
        lineColorGridLayout.verticalSpacing = 10;
        selectLineColorGroup.setLayout(lineColorGridLayout);

        FormData formData3 = new FormData();
        formData3.top = new FormAttachment(selectLineWidthGroup, 7);
        formData3.left = new FormAttachment(2, 0);
        formData3.width = 196;
        formData3.height = 40;
        selectLineColorGroup.setLayoutData(formData3);

        final Color black = display.getSystemColor(SWT.COLOR_BLACK);
        final Color white = display.getSystemColor(SWT.COLOR_WHITE);

        // Associate with each line style a list of segment lengths (in pixels)
        // of the repeating pattern. Numbers are pixels on, pixels off, on, off,
        // ...
        // (Derived from similar structure in NMAP NxmLineA.c)
        // CAUTION: Duplication (of a sort). This governs only local display of
        // line patterns in this dialog (preview and line style selector
        // buttons).
        // Actual drawing of lines with these styles is up to the implementation
        // of IGraphicsTarget being used.

        final Map<LineStyle, int[]> styleMap = new EnumMap<LineStyle, int[]>(
                LineStyle.class);
        styleMap.put(LineStyle.SOLID, new int[] { 4 }); // GEMPAK line type 1
        styleMap.put(LineStyle.SHORT_DASHED, new int[] { 4, 4 }); // GEMPAK line
                                                                  // type 2
        styleMap.put(LineStyle.MEDIUM_DASHED, new int[] { 8, 8 }); // GEMPAK
                                                                   // line type
                                                                   // 3
        styleMap.put(LineStyle.LONG_DASH_SHORT_DASH, new int[] { 16, 8, 4, 8 }); // GEMPAK
                                                                                 // line
                                                                                 // type
                                                                                 // 4
        styleMap.put(LineStyle.LONG_DASHED, new int[] { 16, 8 }); // GEMPAK line
                                                                  // type 5
        styleMap.put(LineStyle.LONG_DASH_THREE_SHORT_DASHES, new int[] { 16, 8,
                4, 8, 4, 8, 4, 8 }); // GEMPAK line type 6
        styleMap.put(LineStyle.LONG_DASH_DOT, new int[] { 16, 8, 2, 8 }); // GEMPAK
                                                                          // line
                                                                          // type
                                                                          // 7
        styleMap.put(LineStyle.LONG_DASH_THREE_DOTS, new int[] { 16, 8, 2, 8,
                2, 8, 2, 8 }); // GEMPAK line type 8
        styleMap.put(LineStyle.MEDIUM_DASH_DOT, new int[] { 8, 8, 2, 8 }); // GEMPAK
                                                                           // line
                                                                           // type
                                                                           // 9
        styleMap.put(LineStyle.DOTS, new int[] { 2, 4 }); // GEMPAK line type 10

        // Line Preview Area

        final Canvas linePreviewAreaCanvas = new Canvas(linePreviewAreaGroup,
                SWT.NONE);

        final int previewLineXmin = 16;
        final int previewLineXmax = 180;
        final int previewLineYctr = 16;

        linePreviewAreaCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent event) {
                GC gc = event.gc;
                gc.setLineWidth((Integer) termLineWidth.getAttrValue());
                gc.setForeground(new Color(display, (RGB) termLineColor
                        .getAttrValue()));
                linePreviewAreaCanvas.setBackground(((RGB) termLineColor
                        .getAttrValue()).getHSB()[2] > 0.2 ? black : white);
                int x1 = previewLineXmin;
                int x2 = previewLineXmin;
                int[] segLengths = styleMap.get((LineStyle) termLineStyle
                        .getAttrValue());
                if (segLengths == null) {
                    return;
                }
                while (x2 < previewLineXmax) {
                    boolean draw = true;
                    for (int eachLineLength : segLengths) {
                        int calculatedLineLength = (int) (eachLineLength * lineLengthFactor);
                        x2 = Math.min(x1 + calculatedLineLength,
                                previewLineXmax);
                        if (draw) {
                            gc.drawLine(x1, previewLineYctr, x2,
                                    previewLineYctr);
                        }
                        if (x2 >= previewLineXmax) {
                            break;
                        }
                        draw = !draw;
                        x1 = x2;
                    }
                }
            }
        });

        // Parameters to give a uniform look to all line width/style buttons

        final int lineButtonHeight = 75;
        final int lineButtonWidth = 15;
        final int buttonLineXmin = 8;
        final int buttonLineXmax = 68;
        final int buttonLineYctr = 7;

        // Line Width

        selectLineWidthButtons = new Button[4];
        final int[] lineWidthButtonSequence = { 0, 2, // ...for 2-column grid
                                                      // layout
                1, 3 };
        for (int i : lineWidthButtonSequence) {
            selectLineWidthButtons[i] = new Button(selectLineWidthGroup,
                    SWT.TOGGLE);
            GridData gridData = new GridData();
            gridData.heightHint = lineButtonWidth;
            gridData.widthHint = lineButtonHeight;
            selectLineWidthButtons[i].setLayoutData(gridData);
            selectLineWidthButtons[i].setData(i + 1);
            selectLineWidthButtons[i].setToolTipText("Width " + (i + 1));
            selectLineWidthButtons[i].addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent event) {
                    GC gc = event.gc;
                    int width = (Integer) event.widget.getData();
                    gc.setLineWidth(width);
                    gc.setForeground(black);
                    gc.drawLine(buttonLineXmin, buttonLineYctr, buttonLineXmax,
                            buttonLineYctr);
                }
            });
            selectLineWidthButtons[i]
                    .addSelectionListener(new SelectionAdapter() {
                        public void widgetSelected(SelectionEvent event) {
                            selectLineWidthButtons[(Integer) termLineWidth
                                    .getAttrValue() - 1].setSelection(false);
                            termLineWidth.setAttrValue((Integer) event.widget
                                    .getData());
                            linePreviewAreaCanvas.redraw();
                            linePreviewAreaCanvas.update();
                        }
                    });
        }
        selectLineWidthButtons[(Integer) termLineWidth.getAttrValue() - 1]
                .setSelection(true); // set initial state

        // Line Style
        termLineStyleButtonMap = new EnumMap<LineStyle, Button>(LineStyle.class);

        for (LineStyle ls : lineStyleButtonSequence) {
            Button lineStyleButton = new Button(selectLineStyleGroup,
                    SWT.TOGGLE);
            termLineStyleButtonMap.put(ls, lineStyleButton);
            GridData gridData = new GridData();
            gridData.heightHint = lineButtonWidth;
            gridData.widthHint = lineButtonHeight;
            lineStyleButton.setLayoutData(gridData);
            lineStyleButton.setData(ls);
            lineStyleButton.setToolTipText(ls.name());
            lineStyleButton.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent event) {
                    GC gc = event.gc;
                    gc.setLineWidth(1);
                    gc.setForeground(black);
                    LineStyle ls = (LineStyle) event.widget.getData();
                    int[] segLengths = styleMap.get(ls);
                    if (segLengths == null)
                        return;
                    int x1 = buttonLineXmin;
                    int x2 = buttonLineXmin;
                    while (x2 < buttonLineXmax) {
                        boolean draw = true;
                        for (int i : segLengths) {
                            x2 = Math.min(x1 + i, buttonLineXmax);
                            if (draw) {
                                gc.drawLine(x1, buttonLineYctr, x2,
                                        buttonLineYctr);
                            }
                            if (x2 >= buttonLineXmax) {
                                break;
                            }
                            draw = !draw;
                            x1 = x2;
                        }
                    }
                }
            });
            lineStyleButton.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    termLineStyleButtonMap.get(
                            (LineStyle) termLineStyle.getAttrValue())
                            .setSelection(false);
                    termLineStyle.setAttrValue((LineStyle) event.widget
                            .getData());
                    linePreviewAreaCanvas.redraw();
                    linePreviewAreaCanvas.update();
                }
            });
        }
        termLineStyleButtonMap.get((LineStyle) termLineStyle.getAttrValue())
                .setSelection(true);

        // Line Color

        final ColorButtonSelector cms = new ColorButtonSelector(
                selectLineColorGroup, 85, 25);
        cms.setColorValue((RGB) termLineColor.getAttrValue());
        cms.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                termLineColor.setAttrValue(cms.getColorValue());
                linePreviewAreaCanvas.redraw();
                linePreviewAreaCanvas.update();
            }
        });

    }

    public void createSunMarkerControls(Group sunMarkerGroup,
            final Display display) {

        Group displaySunGroup = new Group(sunMarkerGroup, SWT.SHADOW_NONE);
        displaySunGroup.setLayout(new FillLayout());

        FormData formData0 = new FormData();
        formData0.top = new FormAttachment(5, 0);
        formData0.left = new FormAttachment(2, 0);
        formData0.width = 190;
        formData0.height = 30;
        displaySunGroup.setLayoutData(formData0);

        Group selectMarkerTypeGroup = new Group(sunMarkerGroup, SWT.SHADOW_NONE);
        selectMarkerTypeGroup.setText("Marker Type");
        GridLayout markerTypeGridLayout = new GridLayout();
        markerTypeGridLayout.numColumns = 1;
        markerTypeGridLayout.marginLeft = 30;
        selectMarkerTypeGroup.setLayout(markerTypeGridLayout);

        FormData formData2 = new FormData();
        formData2.top = new FormAttachment(displaySunGroup, 10);
        formData2.left = new FormAttachment(2, 0);
        formData2.right = new FormAttachment(47, 0);
        formData2.height = 60;
        selectMarkerTypeGroup.setLayoutData(formData2);

        Group selectMarkerSizeGroup = new Group(sunMarkerGroup, SWT.SHADOW_NONE);
        selectMarkerSizeGroup.setText("Marker Size");
        GridLayout markerSizeGridLayout = new GridLayout();
        markerSizeGridLayout.numColumns = 1;
        markerSizeGridLayout.marginLeft = 35;
        selectMarkerSizeGroup.setLayout(markerSizeGridLayout);

        FormData formData3 = new FormData();
        formData3.top = new FormAttachment(2, 0);
        formData3.left = new FormAttachment(selectMarkerTypeGroup, 16);
        formData3.right = new FormAttachment(98, 0);
        formData3.height = 50;
        selectMarkerSizeGroup.setLayoutData(formData3);

        Group selectMarkerWidthGroup = new Group(sunMarkerGroup,
                SWT.SHADOW_NONE);
        selectMarkerWidthGroup.setText("Marker Width");
        GridLayout markerWidthGridLayout = new GridLayout();
        markerWidthGridLayout.numColumns = 1;
        markerWidthGridLayout.marginLeft = 35;
        selectMarkerWidthGroup.setLayout(markerWidthGridLayout);

        FormData formData4 = new FormData();
        formData4.top = new FormAttachment(selectMarkerSizeGroup, 7);
        formData4.left = new FormAttachment(selectMarkerTypeGroup, 16);
        formData4.right = new FormAttachment(98, 0);
        formData3.height = 50;
        selectMarkerWidthGroup.setLayoutData(formData4);

        Group selectMarkerColorGroup = new Group(sunMarkerGroup,
                SWT.SHADOW_NONE);
        selectMarkerColorGroup.setText("Color");
        GridLayout markerColorGridLayout = new GridLayout();
        markerColorGridLayout.numColumns = 1;
        markerColorGridLayout.marginHeight = 7;
        markerColorGridLayout.marginWidth = 55;
        markerColorGridLayout.horizontalSpacing = 0;
        markerColorGridLayout.verticalSpacing = 10;
        selectMarkerColorGroup.setLayout(markerColorGridLayout);

        FormData formData7 = new FormData();
        formData7.top = new FormAttachment(selectMarkerTypeGroup, 4);
        formData7.left = new FormAttachment(2, 0);
        formData7.right = new FormAttachment(48, 0);
        formData7.height = 40;
        formData7.width = 196;
        selectMarkerColorGroup.setLayoutData(formData7);

        // Display Marker or not

        final Button displaySunTog = new Button(displaySunGroup, SWT.CHECK);
        displaySunTog.setText("Display Marker");
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.CENTER;
        gd.verticalAlignment = SWT.FILL;
        displaySunTog.setLayoutData(gd);
        displaySunTog.setSelection((Boolean) displaySun.getAttrValue());

        displaySunTog.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent ev) {
                displaySun.setAttrValue(displaySunTog.getSelection());
            }
        });

        // Marker Type

        /*
         * Use a toolbar with a single drop-down button item that pops up a
         * menu, to simulate a combo that works for images.
         */
        final ToolBar tb = new ToolBar(selectMarkerTypeGroup, SWT.HORIZONTAL);
        final ToolItem ti = new ToolItem(tb, SWT.DROP_DOWN);
        final Menu mu = new Menu(shell, SWT.POP_UP);
        for (MarkerType mt : MarkerType.values()) {
            MenuItem mi = new MenuItem(mu, SWT.PUSH);
            mi.setData(mt);
            mi.setText(mt.getDesignator());
            // TODO: Use PGEN icons via extension points; avoid ordinal
            Integer ord1 = mt.ordinal() + 1;
            String iconString = "icons/marker" + ord1.toString() + ".gif";
            ImageDescriptor id = Activator.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, iconString);
            if (id != null) {
                Image icon = id.createImage();
                mi.setImage(icon);
            }
            mi.addListener(SWT.Selection, new Listener() {
                /*
                 * A new marker type has been chosen off the pop-up menu:
                 * Remember it AND set its icon back on the main button.
                 */
                public void handleEvent(Event event) {
                    selSunMarkerType = (MarkerType) event.widget.getData();
                    sunMarkerType.setAttrValue(selSunMarkerType);
                    Image icon = mu.getItem(selSunMarkerType.ordinal())
                            .getImage();
                    ti.setImage(icon);
                    ti.setToolTipText(selSunMarkerType.getDesignator());
                }
            });

        }
        ti.addListener(SWT.Selection, new Listener() {
            /* Main button clicked: Pop up the menu showing all the symbols. */
            public void handleEvent(Event event) {
                Rectangle bounds = ti.getBounds();
                Point point = tb.toDisplay(bounds.x, bounds.y + bounds.height);
                mu.setLocation(point);
                mu.setVisible(true);
            }
        });

        // Set initial state

        Image icon = mu.getItem(selSunMarkerType.ordinal()).getImage();
        ti.setImage(icon);
        ti.setToolTipText(selSunMarkerType.getDesignator());

        // Marker Size

        final Label selectMarkerSizeSliderText = new Label(
                selectMarkerSizeGroup, SWT.NONE);
        GridData gridData1 = new GridData();
        gridData1.horizontalIndent = 50;
        selectMarkerSizeSliderText.setLayoutData(gridData1);
        final Slider selectMarkerSizeSlider = new Slider(selectMarkerSizeGroup,
                SWT.HORIZONTAL);
        selectMarkerSizeSlider.setMinimum(5);
        selectMarkerSizeSlider.setMaximum(31);
        selectMarkerSizeSlider.setIncrement(1);
        selectMarkerSizeSlider.setThumb(1);
        selectMarkerSizeSlider.setSelection(10);
        float mSize = ((Float) sunMarkerSize.getAttrValue()).floatValue();
        selectMarkerSizeSlider.setSelection((int) (mSize * 10f + 0.5));
        selectMarkerSizeSlider.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                sunMarkerSize
                        .setAttrValue((Float) ((float) selectMarkerSizeSlider
                                .getSelection() / 10));
                selectMarkerSizeSliderText.setText(sunMarkerSize.getAttrValue()
                        .toString());
                selectMarkerSizeSliderText.redraw();
                selectMarkerSizeSliderText.update();
            }
        });
        selectMarkerSizeSliderText.setText(sunMarkerSize.getAttrValue()
                .toString());

        // Marker Width

        final Label selectMarkerWidthSliderText = new Label(
                selectMarkerWidthGroup, SWT.NONE);
        GridData gridData2 = new GridData();
        gridData2.horizontalIndent = 55;
        selectMarkerWidthSliderText.setLayoutData(gridData2);
        final Slider selectMarkerWidthSlider = new Slider(
                selectMarkerWidthGroup, SWT.HORIZONTAL);
        selectMarkerWidthSlider.setMinimum(1);
        selectMarkerWidthSlider.setMaximum(6);
        selectMarkerWidthSlider.setIncrement(1);
        selectMarkerWidthSlider.setThumb(1);
        selectMarkerWidthSlider.setSelection(1);
        selectMarkerWidthSlider.setSelection((Integer) sunMarkerWidth
                .getAttrValue());
        selectMarkerWidthSlider.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                selectMarkerWidthSliderText.setText(new Integer(
                        selectMarkerWidthSlider.getSelection()).toString());
                sunMarkerWidth.setAttrValue((Integer) selectMarkerWidthSlider
                        .getSelection());
            }
        });
        selectMarkerWidthSliderText.setText(new Integer(selectMarkerWidthSlider
                .getSelection()).toString());

        // Marker Color

        final ColorButtonSelector cms = new ColorButtonSelector(
                selectMarkerColorGroup, 85, 25);
        cms.setColorValue((RGB) sunMarkerColor.getAttrValue());
        cms.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                sunMarkerColor.setAttrValue(cms.getColorValue());
            }
        });

    }

    public void createMidnightMeridianControls(Group mmGroup,
            final Display display) {

        Group linePreviewAreaGroup = new Group(mmGroup, SWT.SHADOW_NONE);
        linePreviewAreaGroup.setLayout(new FillLayout());

        FormData formData0 = new FormData();
        formData0.top = new FormAttachment(2, 0);
        formData0.left = new FormAttachment(2, 0);
        formData0.width = 196;
        formData0.height = 30;
        linePreviewAreaGroup.setLayoutData(formData0);

        Group displayMMLineGroup = new Group(mmGroup, SWT.SHADOW_NONE);
        displayMMLineGroup.setLayout(new FillLayout());

        FormData formData00 = new FormData();
        formData00.top = new FormAttachment(2, 0);
        formData00.left = new FormAttachment(linePreviewAreaGroup, 15);
        formData00.width = 190;
        formData00.height = 30;
        displayMMLineGroup.setLayoutData(formData00);

        Group selectLineWidthGroup = new Group(mmGroup, SWT.SHADOW_NONE);
        selectLineWidthGroup.setText("Width");
        GridLayout lineWidthGridLayout = new GridLayout();
        lineWidthGridLayout.numColumns = 2;
        lineWidthGridLayout.marginHeight = 18;
        lineWidthGridLayout.marginWidth = 18;
        lineWidthGridLayout.horizontalSpacing = 8;
        lineWidthGridLayout.verticalSpacing = 8;
        selectLineWidthGroup.setLayout(lineWidthGridLayout);

        FormData formData1 = new FormData();
        formData1.top = new FormAttachment(linePreviewAreaGroup, 7);
        formData1.left = new FormAttachment(2, 0);
        selectLineWidthGroup.setLayoutData(formData1);

        Group selectLineStyleGroup = new Group(mmGroup, SWT.SHADOW_NONE);
        selectLineStyleGroup.setText("Style");
        GridLayout lineStyleGridLayout = new GridLayout();
        lineStyleGridLayout.numColumns = 2;
        lineStyleGridLayout.marginHeight = 18;
        lineStyleGridLayout.marginWidth = 18;
        lineStyleGridLayout.horizontalSpacing = 8;
        lineStyleGridLayout.verticalSpacing = 8;
        selectLineStyleGroup.setLayout(lineStyleGridLayout);

        FormData formData2 = new FormData();
        formData2.left = new FormAttachment(selectLineWidthGroup, 16);
        formData2.top = new FormAttachment(displayMMLineGroup, 7);
        selectLineStyleGroup.setLayoutData(formData2);

        Group selectLineColorGroup = new Group(mmGroup, SWT.SHADOW_NONE);
        selectLineColorGroup.setText("Color");
        GridLayout lineColorGridLayout = new GridLayout();
        lineColorGridLayout.numColumns = 1;
        lineColorGridLayout.marginHeight = 7;
        lineColorGridLayout.marginWidth = 55;
        lineColorGridLayout.horizontalSpacing = 0;
        lineColorGridLayout.verticalSpacing = 10;
        selectLineColorGroup.setLayout(lineColorGridLayout);

        FormData formData3 = new FormData();
        formData3.top = new FormAttachment(selectLineWidthGroup, 7);
        formData3.left = new FormAttachment(2, 0);
        formData3.width = 196;
        formData3.height = 40;
        selectLineColorGroup.setLayoutData(formData3);

        // Display Midnight Meridian Line or not

        final Button displayMMLineTog = new Button(displayMMLineGroup,
                SWT.CHECK);
        displayMMLineTog.setText("Display Meridian Line");
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.CENTER;
        gd.verticalAlignment = SWT.FILL;
        displayMMLineTog.setLayoutData(gd);
        displayMMLineTog.setSelection((Boolean) displayMidnightMeridian
                .getAttrValue());

        displayMMLineTog.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent ev) {
                displayMidnightMeridian.setAttrValue(displayMMLineTog
                        .getSelection());
            }
        });

        final Color black = display.getSystemColor(SWT.COLOR_BLACK);
        final Color white = display.getSystemColor(SWT.COLOR_WHITE);

        // Associate with each line style a list of segment lengths (in pixels)
        // of the repeating pattern. Numbers are pixels on, pixels off, on, off,
        // ...
        // (Derived from similar structure in NMAP NxmLineA.c)
        // CAUTION: Duplication (of a sort). This governs only local display of
        // line patterns in this dialog (preview and line style selector
        // buttons).
        // Actual drawing of lines with these styles is up to the implementation
        // of IGraphicsTarget being used.

        final Map<LineStyle, int[]> styleMap = new EnumMap<LineStyle, int[]>(
                LineStyle.class);
        styleMap.put(LineStyle.SOLID, new int[] { 4 }); // GEMPAK line type 1
        styleMap.put(LineStyle.SHORT_DASHED, new int[] { 4, 4 }); // GEMPAK line
                                                                  // type 2
        styleMap.put(LineStyle.MEDIUM_DASHED, new int[] { 8, 8 }); // GEMPAK
                                                                   // line type
                                                                   // 3
        styleMap.put(LineStyle.LONG_DASH_SHORT_DASH, new int[] { 16, 8, 4, 8 }); // GEMPAK
                                                                                 // line
                                                                                 // type
                                                                                 // 4
        styleMap.put(LineStyle.LONG_DASHED, new int[] { 16, 8 }); // GEMPAK line
                                                                  // type 5
        styleMap.put(LineStyle.LONG_DASH_THREE_SHORT_DASHES, new int[] { 16, 8,
                4, 8, 4, 8, 4, 8 }); // GEMPAK line type 6
        styleMap.put(LineStyle.LONG_DASH_DOT, new int[] { 16, 8, 2, 8 }); // GEMPAK
                                                                          // line
                                                                          // type
                                                                          // 7
        styleMap.put(LineStyle.LONG_DASH_THREE_DOTS, new int[] { 16, 8, 2, 8,
                2, 8, 2, 8 }); // GEMPAK line type 8
        styleMap.put(LineStyle.MEDIUM_DASH_DOT, new int[] { 8, 8, 2, 8 }); // GEMPAK
                                                                           // line
                                                                           // type
                                                                           // 9
        styleMap.put(LineStyle.DOTS, new int[] { 2, 4 }); // GEMPAK line type 10

        // Line Preview Area

        final Canvas linePreviewAreaCanvas = new Canvas(linePreviewAreaGroup,
                SWT.NONE);

        final int previewLineXmin = 16;
        final int previewLineXmax = 180;
        final int previewLineYctr = 16;

        linePreviewAreaCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent event) {
                GC gc = event.gc;
                gc.setLineWidth((Integer) midnightMeridianLineWidth
                        .getAttrValue());
                gc.setForeground(new Color(display,
                        (RGB) midnightMeridianLineColor.getAttrValue()));
                linePreviewAreaCanvas
                        .setBackground(((RGB) midnightMeridianLineColor
                                .getAttrValue()).getHSB()[2] > 0.2 ? black
                                : white);
                int x1 = previewLineXmin;
                int x2 = previewLineXmin;
                int[] segLengths = styleMap
                        .get((LineStyle) midnightMeridianLineStyle
                                .getAttrValue());
                if (segLengths == null) {
                    return;
                }
                while (x2 < previewLineXmax) {
                    boolean draw = true;
                    for (int eachLineLength : segLengths) {
                        int calculatedLineLength = (int) (eachLineLength * lineLengthFactor);
                        x2 = Math.min(x1 + calculatedLineLength,
                                previewLineXmax);
                        if (draw) {
                            gc.drawLine(x1, previewLineYctr, x2,
                                    previewLineYctr);
                        }
                        if (x2 >= previewLineXmax) {
                            break;
                        }
                        draw = !draw;
                        x1 = x2;
                    }
                }
            }
        });

        // Parameters to give a uniform look to all line width/style buttons

        final int lineButtonHeight = 75;
        final int lineButtonWidth = 15;
        final int buttonLineXmin = 8;
        final int buttonLineXmax = 68;
        final int buttonLineYctr = 7;

        // Line Width

        selectMmLineWidthButtons = new Button[4];
        final int[] lineWidthButtonSequence = { 0, 2, // ...for 2-column grid
                                                      // layout
                1, 3 };
        for (int i : lineWidthButtonSequence) {
            selectMmLineWidthButtons[i] = new Button(selectLineWidthGroup,
                    SWT.TOGGLE);
            GridData gridData = new GridData();
            gridData.heightHint = lineButtonWidth;
            gridData.widthHint = lineButtonHeight;
            selectMmLineWidthButtons[i].setLayoutData(gridData);
            selectMmLineWidthButtons[i].setData(i + 1);
            selectMmLineWidthButtons[i].setToolTipText("Width " + (i + 1));
            selectMmLineWidthButtons[i].addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent event) {
                    GC gc = event.gc;
                    int width = (Integer) event.widget.getData();
                    gc.setLineWidth(width);
                    gc.setForeground(black);
                    gc.drawLine(buttonLineXmin, buttonLineYctr, buttonLineXmax,
                            buttonLineYctr);
                }
            });
            selectMmLineWidthButtons[i]
                    .addSelectionListener(new SelectionAdapter() {
                        public void widgetSelected(SelectionEvent event) {
                            selectMmLineWidthButtons[(Integer) midnightMeridianLineWidth
                                    .getAttrValue() - 1].setSelection(false);
                            midnightMeridianLineWidth
                                    .setAttrValue((Integer) event.widget
                                            .getData());
                            linePreviewAreaCanvas.redraw();
                            linePreviewAreaCanvas.update();
                        }
                    });
        }
        selectMmLineWidthButtons[(Integer) midnightMeridianLineWidth
                .getAttrValue() - 1].setSelection(true); // set initial state

        // Line Style
        mmLineStyleButtonMap = new EnumMap<LineStyle, Button>(LineStyle.class);

        for (LineStyle ls : lineStyleButtonSequence) {
            Button lineStyleButton = new Button(selectLineStyleGroup,
                    SWT.TOGGLE);
            mmLineStyleButtonMap.put(ls, lineStyleButton);
            GridData gridData = new GridData();
            gridData.heightHint = lineButtonWidth;
            gridData.widthHint = lineButtonHeight;
            lineStyleButton.setLayoutData(gridData);
            lineStyleButton.setData(ls);
            lineStyleButton.setToolTipText(ls.name());
            lineStyleButton.addPaintListener(new PaintListener() {
                public void paintControl(PaintEvent event) {
                    GC gc = event.gc;
                    gc.setLineWidth(1);
                    gc.setForeground(black);
                    LineStyle ls = (LineStyle) event.widget.getData();
                    int[] segLengths = styleMap.get(ls);
                    if (segLengths == null)
                        return;
                    int x1 = buttonLineXmin;
                    int x2 = buttonLineXmin;
                    while (x2 < buttonLineXmax) {
                        boolean draw = true;
                        for (int i : segLengths) {
                            x2 = Math.min(x1 + i, buttonLineXmax);
                            if (draw) {
                                gc.drawLine(x1, buttonLineYctr, x2,
                                        buttonLineYctr);
                            }
                            if (x2 >= buttonLineXmax) {
                                break;
                            }
                            draw = !draw;
                            x1 = x2;
                        }
                    }
                }
            });
            lineStyleButton.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    mmLineStyleButtonMap.get(
                            (LineStyle) midnightMeridianLineStyle
                                    .getAttrValue()).setSelection(false);
                    midnightMeridianLineStyle
                            .setAttrValue((LineStyle) event.widget.getData());
                    linePreviewAreaCanvas.redraw();
                    linePreviewAreaCanvas.update();
                }
            });
        }

        mmLineStyleButtonMap.get(
                (LineStyle) midnightMeridianLineStyle.getAttrValue())
                .setSelection(true);

        // Line Color

        final ColorButtonSelector cms = new ColorButtonSelector(
                selectLineColorGroup, 85, 25);
        cms.setColorValue((RGB) midnightMeridianLineColor.getAttrValue());
        cms.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                midnightMeridianLineColor.setAttrValue(cms.getColorValue());
                linePreviewAreaCanvas.redraw();
                linePreviewAreaCanvas.update();
            }
        });

    }

    public void createShadingControls(Group shadeGroup,
            final Composite composite) {

        Group applyShadingGroup = new Group(shadeGroup, SWT.SHADOW_NONE);
        applyShadingGroup.setLayout(new FillLayout());

        FormData formData0 = new FormData();
        formData0.top = new FormAttachment(5, 0);
        formData0.left = new FormAttachment(2, 0);
        formData0.width = 190;
        formData0.height = 30;
        applyShadingGroup.setLayoutData(formData0);

        Group selectDayShadeColorGroup = new Group(shadeGroup, SWT.SHADOW_NONE);
        selectDayShadeColorGroup.setText("Day Color");
        GridLayout dayShadeColorGridLayout = new GridLayout();
        dayShadeColorGridLayout.numColumns = 1;
        dayShadeColorGridLayout.marginHeight = 7;
        dayShadeColorGridLayout.marginWidth = 45;
        dayShadeColorGridLayout.horizontalSpacing = 0;
        dayShadeColorGridLayout.verticalSpacing = 10;
        selectDayShadeColorGroup.setLayout(dayShadeColorGridLayout);

        FormData formData2 = new FormData();
        formData2.top = new FormAttachment(applyShadingGroup, 7);
        formData2.left = new FormAttachment(2, 0);
        formData2.right = new FormAttachment(48, 0);
        formData2.height = 45;
        selectDayShadeColorGroup.setLayoutData(formData2);

        Group selectNightShadeColorGroup = new Group(shadeGroup,
                SWT.SHADOW_NONE);
        selectNightShadeColorGroup.setText("Night Color");
        GridLayout nightShadeColorGridLayout = new GridLayout();
        nightShadeColorGridLayout.numColumns = 1;
        nightShadeColorGridLayout.marginHeight = 7;
        nightShadeColorGridLayout.marginWidth = 45;
        nightShadeColorGridLayout.horizontalSpacing = 0;
        nightShadeColorGridLayout.verticalSpacing = 10;
        selectNightShadeColorGroup.setLayout(nightShadeColorGridLayout);

        FormData formData4 = new FormData();
        formData4.top = new FormAttachment(selectDayShadeColorGroup, 7);
        formData4.left = new FormAttachment(2, 0);
        formData4.right = new FormAttachment(48, 0);
        formData4.height = 45;
        selectNightShadeColorGroup.setLayoutData(formData4);

        Group selectFillPatternGroup = new Group(shadeGroup, SWT.SHADOW_NONE);
        selectFillPatternGroup.setText("Fill Pattern");
        GridLayout fillPatternGridLayout = new GridLayout();
        fillPatternGridLayout.numColumns = 1;
        fillPatternGridLayout.marginLeft = 30;
        selectFillPatternGroup.setLayout(fillPatternGridLayout);

        FormData formData1 = new FormData();
        formData1.top = new FormAttachment(5, 0);
        formData1.left = new FormAttachment(selectDayShadeColorGroup, 16);
        formData1.right = new FormAttachment(98, 0);
        formData1.height = 55;
        selectFillPatternGroup.setLayoutData(formData1);

        Group selectAlphaGroup = new Group(shadeGroup, SWT.SHADOW_NONE);
        selectAlphaGroup.setText("Alpha");
        GridLayout selectAlphaGridLayout = new GridLayout();
        selectAlphaGridLayout.numColumns = 1;
        selectAlphaGridLayout.marginLeft = 25;
        selectAlphaGroup.setLayout(selectAlphaGridLayout);

        FormData formData3 = new FormData();
        formData3.top = new FormAttachment(selectFillPatternGroup, 7);
        formData3.left = new FormAttachment(selectDayShadeColorGroup, 16);
        formData3.right = new FormAttachment(98, 0);
        formData3.height = 50;
        selectAlphaGroup.setLayoutData(formData3);

        // Apply Shading

        final Button applyShadingTog = new Button(applyShadingGroup, SWT.CHECK);
        applyShadingTog.setText("Apply Shading");
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.CENTER;
        gd.verticalAlignment = SWT.FILL;
        applyShadingTog.setLayoutData(gd);
        applyShadingTog.setSelection((Boolean) applyShading.getAttrValue());

        applyShadingTog.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent ev) {
                applyShading.setAttrValue(applyShadingTog.getSelection());
            }
        });

        // Fill Pattern

        createFillPatternAttr(selectFillPatternGroup, composite);

        // Shade Alpha

        final Label selectAlphaSliderText = new Label(selectAlphaGroup,
                SWT.NONE);
        GridData gridData1 = new GridData();
        gridData1.horizontalIndent = 50;
        selectAlphaSliderText.setLayoutData(gridData1);
        final Slider selectAlphaSizeSlider = new Slider(selectAlphaGroup,
                SWT.HORIZONTAL);
        selectAlphaSizeSlider.setMinimum(0);
        selectAlphaSizeSlider.setMaximum(11);
        selectAlphaSizeSlider.setIncrement(1);
        selectAlphaSizeSlider.setThumb(1);
        selectAlphaSizeSlider.setSelection(2);
        float alpha = ((Float) shadeAlpha.getAttrValue()).floatValue();
        selectAlphaSizeSlider.setSelection((int) (alpha * 10f));
        selectAlphaSizeSlider.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shadeAlpha.setAttrValue((Float) ((float) selectAlphaSizeSlider
                        .getSelection()) / 10.0f);
                selectAlphaSliderText.setText(shadeAlpha.getAttrValue()
                        .toString());
                selectAlphaSliderText.redraw();
                selectAlphaSliderText.update();
            }
        });
        selectAlphaSliderText.setText(shadeAlpha.getAttrValue().toString());

        // Day Shading Color

        final ColorButtonSelector dayCms = new ColorButtonSelector(
                selectDayShadeColorGroup, 85, 25);
        dayCms.setColorValue((RGB) dayShadeColor.getAttrValue());
        dayCms.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                dayShadeColor.setAttrValue(dayCms.getColorValue());
            }
        });

        // Night Shading Color

        final ColorButtonSelector nightCms = new ColorButtonSelector(
                selectNightShadeColorGroup, 85, 25);
        nightCms.setColorValue((RGB) nightShadeColor.getAttrValue());
        nightCms.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                nightShadeColor.setAttrValue(nightCms.getColorValue());
            }
        });
    }

    /*
     * Create widgets for the fill patterns attribute
     */
    private void createFillPatternAttr(Group group, Composite composite) {

        final ToolBar fpTb = new ToolBar(group, SWT.HORIZONTAL);
        final ToolItem fpTi = new ToolItem(fpTb, SWT.DROP_DOWN);
        final Menu fpMu = new Menu(shell, SWT.POP_UP);
        for (FillPattern fp : FillPatternList.FillPattern.values()) {

            if (fp.equals(FillPattern.TRANSPARENCY))
                continue;

            MenuItem mi = new MenuItem(fpMu, SWT.PUSH);
            mi.setData(fp);
            mi.setText(fp.name());
            Integer ord1 = fp.ordinal();
            String iconString = "icons/patt" + ord1.toString() + ".gif";
            ImageDescriptor id = Activator.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, iconString);
            if (id != null) {
                Image icon = id.createImage();
                mi.setImage(icon);
            }
            mi.addListener(SWT.Selection, new Listener() {
                /*
                 * A new fill pattern has been chosen off the pop-up menu:
                 * Remember it AND set its icon back on the main button.
                 */
                public void handleEvent(Event event) {
                    selFillPatternType = (FillPattern) event.widget.getData();
                    shadePattern.setAttrValue(selFillPatternType.name());
                    Image icon = fpMu.getItem(selFillPatternType.ordinal())
                            .getImage();
                    fpTi.setImage(icon);
                    fpTi.setToolTipText(selFillPatternType.name());
                }
            });
        }
        fpTi.addListener(SWT.Selection, new Listener() {
            /* Main button clicked: Pop up the menu showing all the symbols. */
            public void handleEvent(Event event) {
                Rectangle bounds = fpTi.getBounds();
                Point point = fpTb
                        .toDisplay(bounds.x, bounds.y + bounds.height);
                fpMu.setLocation(point);
                fpMu.setVisible(true);
            }
        });

        // Set initial state

        Image icon = fpMu.getItem(selFillPatternType.ordinal()).getImage();
        fpTi.setImage(icon);
        fpTi.setToolTipText(selFillPatternType.name());

    }

    public FillPattern getFillPattern(String fillPattern) {
        return FillPattern.valueOf(fillPattern);
    }

    @Override
    public void initWidgets() {

    }

    @Override
    public void createShell() {
        int style = SWT.DIALOG_TRIM | SWT.RESIZE;
        if (!hasApplyBtn) {
            style |= SWT.APPLICATION_MODAL;
        }

        shell = new Shell(getParent(), style);
        shell.setText(dlgTitle);
        // shell.setSize( 600, 800 ); // pack later

        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        Composite topComp = new Composite(shell, SWT.NONE);
        topComp.setLayout(new GridLayout());
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        topComp.setLayoutData(gd);

        Composite editComp = createDialog(topComp);
        Label sep = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        sep.setLayoutData(gd);

        Composite okCanComp = new Composite(shell, SWT.NONE);
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalAlignment = SWT.CENTER;
        okCanComp.setLayoutData(gd);

        okCanComp.setLayout(new GridLayout((hasApplyBtn ? 3 : 2), true));

        Button canBtn = new Button(okCanComp, SWT.PUSH);
        canBtn.setText(" Cancel ");

        canBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                ok = false;
                rscData.setRscAttrSet(new ResourceAttrSet(prevRscAttrSet));
                shell.dispose();
            }
        });

        if (hasApplyBtn) {
            Button applyBtn = new Button(okCanComp, SWT.PUSH);
            applyBtn.setText(" Apply ");

            applyBtn.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent e) {
                    rscData.setRscAttrSet(editedRscAttrSet);
                    rscData.setIsEdited(true);
                    NcDisplayMngr.getActiveNatlCntrsEditor().refresh();
                }
            });
        }

        Button okBtn = new Button(okCanComp, SWT.PUSH);
        okBtn.setText("    OK    ");

        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                ok = true;
                shell.dispose();
            }
        });
    }
}
