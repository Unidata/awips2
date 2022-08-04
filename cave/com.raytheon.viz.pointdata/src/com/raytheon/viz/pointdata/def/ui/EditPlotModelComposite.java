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
package com.raytheon.viz.pointdata.def.ui;

import java.awt.image.BufferedImage;
import java.awt.image.DirectColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.WritableRaster;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.function.Function;

import org.eclipse.jface.layout.TableColumnLayout;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.viz.pointdata.IPlotModelElement;
import com.raytheon.viz.pointdata.IPlotModelFactory;
import com.raytheon.viz.pointdata.PlotModelElement;
import com.raytheon.viz.pointdata.PlotModelFactory;
import com.raytheon.viz.pointdata.PlotModelFactory.DisplayMode;
import com.raytheon.viz.pointdata.PlotModelFactory.DisplayType;
import com.raytheon.viz.pointdata.PlotModelFactoryDefault;
import com.raytheon.viz.pointdata.def.ConditionalColor;
import com.raytheon.viz.pointdata.def.PlotParameterDefinition;
import com.raytheon.viz.pointdata.def.PlotParameterDefinitions;
import com.raytheon.viz.pointdata.def.PlotParameterDefinitionsManager;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * EditPlotModelComposite. Opens the new dialog that contains widgets to support
 * new plot customization requirement.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 10/10/2019   71272      Mark Peters   Initial Creation
 * 10/13/2020   73084      ksunil        addButton.widgetSelected now adds the element selected from the available params
 *                                        instead of defaulting to the first one.
 * 02/05/2020   74587      ksunil        code to prevent exception in sample plot when all parameters are unchecked.
 * 03/02/2020    75528     ksunil        Changed enum PLOT to VISIBLE. Use svg's built in visibility attribute.
 *
 *
 * </pre>
 *
 * @author mpeters
 */

public class EditPlotModelComposite extends Composite {

    public static final String NORMAL = "Normal";

    public static final String ITALIC = "Italic";

    public static final String BOLD = "Bold";

    public static final String BOLD_ITALIC = BOLD + "-" + ITALIC;

    private static final String[] textSizeOptions = { "8", "10", "12", "14",
            "16", "18", "20", "22", "28" };

    private static final String[] textFontOptions = { "Courier", "Helvetica",
            "Times", "Monospaced", // same as "DialogInput"
            "SansSerif", // same as "Dialog"
            "Liberation Serif" };

    private static final String[] textStyleOptions = { NORMAL, ITALIC, BOLD,
            BOLD_ITALIC };

    private static final int CANVAS_BUFFER = 10;

    private final String plotModelFile;

    private IPlotModelFactory factory;

    private Canvas canvas;

    private Table table;

    private Composite paramEditComp, symbolSizeComp;

    private Group textGroup, posGroup, colorGroup, markerGroup, paramsGroup;

    private Combo sizeCombo, fontCombo, styleCombo, anchorCombo, markerCombo;

    private Spinner xSpinner, ySpinner;

    private ColorFieldEditor cfe;

    private org.eclipse.swt.widgets.List plotParamsList;

    private Label sizeLabel2, widthLabel;

    private Scale sizeScale, widthScale;

    private Image samplePlot;

    public enum TextAnchor {
        START("Start"), MIDDLE("Middle"), END("End");

        private final String displayName;

        private TextAnchor(String displayName) {
            this.displayName = displayName;
        }

        public String getName() {
            return displayName;
        }

        public static TextAnchor fromName(String name) {
            for (TextAnchor anchor : values()) {
                if (anchor.getName().equals(name)) {
                    return anchor;
                }
            }

            throw new IllegalArgumentException(
                    "Invalid text anchor name: " + name);
        }
    }

    public EditPlotModelComposite(Composite parent, int style,
            String plotModelFile, RGB defaultColor) throws VizException {
        super(parent, style);

        this.plotModelFile = plotModelFile;
        factory = getFactoryFromFile();
        factory.setColor(defaultColor);

        setLayout(new GridLayout());
        initializeComponents();
        populateValues();

        addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (samplePlot != null) {
                    samplePlot.dispose();
                }
            }
        });
    }

    private static ImageData convertToSWT(BufferedImage bufferedImage) {
        if (bufferedImage.getColorModel() instanceof DirectColorModel) {
            DirectColorModel colorModel = (DirectColorModel) bufferedImage
                    .getColorModel();
            PaletteData palette = new PaletteData(colorModel.getRedMask(),
                    colorModel.getGreenMask(), colorModel.getBlueMask());
            ImageData data = new ImageData(bufferedImage.getWidth(),
                    bufferedImage.getHeight(), colorModel.getPixelSize(),
                    palette);
            for (int y = 0; y < data.height; y++) {
                for (int x = 0; x < data.width; x++) {
                    int rgb = bufferedImage.getRGB(x, y);
                    int pixel = palette.getPixel(new RGB((rgb >> 16) & 0xFF,
                            (rgb >> 8) & 0xFF, rgb & 0xFF));
                    data.setPixel(x, y, pixel);
                    if (colorModel.hasAlpha()) {
                        data.setAlpha(x, y, (rgb >> 24) & 0xFF);
                    }
                }
            }
            return data;
        } else if (bufferedImage.getColorModel() instanceof IndexColorModel) {
            IndexColorModel colorModel = (IndexColorModel) bufferedImage
                    .getColorModel();
            int size = colorModel.getMapSize();
            byte[] reds = new byte[size];
            byte[] greens = new byte[size];
            byte[] blues = new byte[size];
            colorModel.getReds(reds);
            colorModel.getGreens(greens);
            colorModel.getBlues(blues);
            RGB[] rgbs = new RGB[size];
            for (int i = 0; i < rgbs.length; i++) {
                rgbs[i] = new RGB(reds[i] & 0xFF, greens[i] & 0xFF,
                        blues[i] & 0xFF);
            }
            PaletteData palette = new PaletteData(rgbs);
            ImageData data = new ImageData(bufferedImage.getWidth(),
                    bufferedImage.getHeight(), colorModel.getPixelSize(),
                    palette);
            data.transparentPixel = colorModel.getTransparentPixel();
            WritableRaster raster = bufferedImage.getRaster();
            int[] pixelArray = new int[1];
            for (int y = 0; y < data.height; y++) {
                for (int x = 0; x < data.width; x++) {
                    raster.getPixel(x, y, pixelArray);
                    data.setPixel(x, y, pixelArray[0]);
                }
            }
            return data;
        }
        return null;
    }

    private void initializeComponents() {
        Composite canvasComp = new Composite(this, SWT.NONE);
        canvasComp.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        canvas = new Canvas(canvasComp, SWT.NONE);
        canvas.setSize(factory.getDefinedPlotModelWidth() + CANVAS_BUFFER,
                factory.getDefinedPlotModelHeight() + CANVAS_BUFFER);
        canvas.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        canvas.addPaintListener(new PaintListener() {

            @Override
            public void paintControl(PaintEvent e) {
                /*
                 * if user doesn't select any of the check boxes,the following
                 * will throw exception deep inside image factory.
                 */
                if (atleastOneChecked()) {
                    ImageData imageData = convertToSWT(factory.getSamplePlot());
                    if (samplePlot != null) {
                        samplePlot.dispose();
                    }
                    samplePlot = new Image(Display.getCurrent(), imageData);
                    e.gc.drawImage(samplePlot, CANVAS_BUFFER / 2,
                            CANVAS_BUFFER / 2);
                }
            }
        });

        Composite pmTableComp = new Composite(this, SWT.NONE);
        pmTableComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        pmTableComp.setLayout(new GridLayout(2, false));

        Composite pmTableButtonComp = new Composite(pmTableComp, SWT.NONE);
        pmTableButtonComp.setLayoutData(
                new GridData(SWT.DEFAULT, SWT.CENTER, false, true));
        pmTableButtonComp.setLayout(new GridLayout());

        Button addButton = new Button(pmTableButtonComp, SWT.PUSH);
        addButton.setText("Add");
        addButton.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        addButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                if (((String[]) plotParamsList.getSelection()).length > 0) {
                    PlotParameterDefinitions paramDefs = getParamDefs();
                    PlotParameterDefinition paramDef = paramDefs
                            .getParamDef(plotParamsList.getSelection()[0]);

                    IPlotModelElement element = factory.addElement(paramDef);
                    element.setParamDef(paramDef);
                    TableItem newItem = addTableItem(element);
                    table.setSelection(newItem);

                    populateFieldsForSelectedParams();

                    canvas.redraw();
                }
            }
        });
        Button removeButton = new Button(pmTableButtonComp, SWT.PUSH);
        removeButton.setText("Remove");
        removeButton.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        removeButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (IPlotModelElement element : getSelectedElements()) {
                    element.dispose();
                }
                table.remove(table.getSelectionIndices());
                populateFieldsForSelectedParams();
                canvas.redraw();
            }
        });

        Composite pmTableComp2 = new Composite(pmTableComp, SWT.NONE);
        pmTableComp2
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        table = new Table(pmTableComp2, SWT.CHECK | SWT.MULTI | SWT.BORDER
                | SWT.V_SCROLL | SWT.H_SCROLL);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);
        table.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (e.detail == SWT.CHECK) {
                    TableItem ti = (TableItem) e.item;
                    DisplayMode mode = ti.getChecked() ? DisplayMode.VISIBLE
                            : DisplayMode.HIDDEN;
                    ((IPlotModelElement) ti.getData()).setMode(mode);
                    canvas.redraw();
                }
                populateFieldsForSelectedParams();
            }
        });

        TableColumn paramColumn = new TableColumn(table, SWT.NONE);
        paramColumn.setText("Parameter");
        TableColumn xColumn = new TableColumn(table, SWT.NONE);
        xColumn.setText("x");
        TableColumn yColumn = new TableColumn(table, SWT.NONE);
        yColumn.setText("y");

        TableColumnLayout tcl = new TableColumnLayout(true);
        tcl.setColumnData(paramColumn, new ColumnWeightData(80));
        tcl.setColumnData(xColumn, new ColumnWeightData(10));
        tcl.setColumnData(yColumn, new ColumnWeightData(10));
        pmTableComp2.setLayout(tcl);

        paramEditComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout();
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        paramEditComp.setLayout(gl);

        textGroup = new Group(paramEditComp, SWT.NONE);
        textGroup.setText("Text");
        textGroup.setLayout(new GridLayout(8, false));

        Label sizeLabel = new Label(textGroup, SWT.NONE);
        sizeLabel.setText("Size");
        sizeCombo = new Combo(textGroup, SWT.READ_ONLY);
        sizeCombo.setItems(textSizeOptions);
        sizeCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int size = Integer.valueOf(sizeCombo.getText());
                for (IPlotModelElement element : getSelectedElements()) {
                    element.setFontSize(size);
                }
                canvas.redraw();
            }
        });

        Label fontLabel = new Label(textGroup, SWT.NONE);
        fontLabel.setText(" Font");
        fontLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));
        fontCombo = new Combo(textGroup, SWT.READ_ONLY);
        fontCombo.setItems(textFontOptions);
        fontCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (IPlotModelElement element : getSelectedElements()) {
                    element.setFontFamily(fontCombo.getText());
                }
                canvas.redraw();
            }
        });

        Label styleLabel = new Label(textGroup, SWT.NONE);
        styleLabel.setText(" Style");
        styleCombo = new Combo(textGroup, SWT.READ_ONLY);
        styleCombo.setItems(textStyleOptions);
        styleCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (IPlotModelElement element : getSelectedElements()) {
                    element.setFontStyle(styleCombo.getText());
                }
                canvas.redraw();
            }
        });

        Label anchorLabel = new Label(textGroup, SWT.NONE);
        anchorLabel.setText(" Anchor");
        anchorCombo = new Combo(textGroup, SWT.READ_ONLY);
        String[] anchors = Arrays.stream(TextAnchor.values())
                .map(TextAnchor::getName).toArray(String[]::new);
        anchorCombo.setItems(anchors);
        anchorCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TextAnchor anchor = TextAnchor.fromName(anchorCombo.getText());
                for (IPlotModelElement element : getSelectedElements()) {
                    element.setTextAnchor(anchor);
                }
                canvas.redraw();
            }
        });

        Composite attrsComp = new Composite(paramEditComp, SWT.NONE);
        attrsComp.setLayout(new GridLayout(3, false));
        attrsComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        posGroup = new Group(attrsComp, SWT.NONE);
        posGroup.setText("Position");
        posGroup.setLayout(new GridLayout(4, false));

        Label xLabel = new Label(posGroup, SWT.NONE);
        xLabel.setText("x");
        xSpinner = new Spinner(posGroup, SWT.BORDER);
        xSpinner.setValues(0, -50, 50, 0, 1, 10);
        xSpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (IPlotModelElement element : getSelectedElements()) {
                    element.setX(xSpinner.getSelection());
                }
                canvas.redraw();
            }
        });

        Label yLabel = new Label(posGroup, SWT.NONE);
        yLabel.setText(" y");
        ySpinner = new Spinner(posGroup, SWT.BORDER);
        ySpinner.setValues(0, -50, 50, 0, 1, 10);
        ySpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (IPlotModelElement element : getSelectedElements()) {
                    element.setY(ySpinner.getSelection());
                }
                canvas.redraw();
            }
        });

        colorGroup = new Group(attrsComp, SWT.NONE);
        colorGroup.setText("Color");
        colorGroup.setLayout(new GridLayout());
        Composite cfeComp = new Composite(colorGroup, SWT.NONE);
        cfe = new ColorFieldEditor("noProperty", "Color", cfeComp);
        cfe.getColorSelector().addListener(new IPropertyChangeListener() {

            @Override
            public void propertyChange(PropertyChangeEvent event) {
                RGB rgb = (RGB) event.getNewValue();
                for (IPlotModelElement element : getSelectedElements()) {
                    element.setColor(rgb);
                }
                canvas.redraw();
            }
        });
        Button advColorButton = new Button(colorGroup, SWT.PUSH);
        advColorButton.setText("Advanced...");
        advColorButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                EditConditionalColorDialog dialog;
                ConditionalColor color = null;
                if (allSame(getSelectedElements(),
                        EnumSet.noneOf(DisplayType.class),
                        PlotModelElement::getConditionalColor)) {
                    color = getSelectedElements()[0].getConditionalColor();
                }

                if (color == null) {
                    color = new ConditionalColor(new ArrayList<>(),
                            cfe.getColorSelector().getColorValue());
                }

                dialog = new EditConditionalColorDialog(
                        EditPlotModelComposite.this.getShell(),
                        factory.getPlugin(), color);

                dialog.addCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof ConditionalColor) {
                            ConditionalColor color = (ConditionalColor) returnValue;
                            for (IPlotModelElement element : getSelectedElements()) {
                                element.setConditionalColor(color);
                            }
                            canvas.redraw();
                        }
                    }
                });

                dialog.open();
            }
        });

        markerGroup = new Group(attrsComp, SWT.NONE);
        markerGroup.setText("Marker Type");
        markerGroup.setLayout(new GridLayout());
        markerCombo = new Combo(markerGroup, SWT.READ_ONLY);
        try {
            markerCombo.setItems(PlotModelFactory.findMarkerSymbolNames());
        } catch (VizException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
        markerCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (IPlotModelElement element : getSelectedElements()) {
                    if (element.getParamDef()
                            .getDisplayType() == DisplayType.MARKER) {
                        element.setValue(markerCombo.getText());
                    }
                }
                canvas.redraw();
            }
        });

        Composite attrsComp2 = new Composite(paramEditComp, SWT.NONE);
        attrsComp2.setLayout(new GridLayout(2, false));

        paramsGroup = new Group(attrsComp2, SWT.NONE);
        paramsGroup.setText("Plot Parameters");
        paramsGroup.setLayout(new GridLayout());
        plotParamsList = new org.eclipse.swt.widgets.List(paramsGroup,
                SWT.V_SCROLL);
        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        gd.heightHint = plotParamsList.getItemHeight() * 5;
        plotParamsList.setLayoutData(gd);

        symbolSizeComp = new Composite(attrsComp2, SWT.NONE);
        symbolSizeComp.setLayoutData(
                new GridData(SWT.DEFAULT, SWT.FILL, false, true));
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        symbolSizeComp.setLayout(gl);

        Group sizeComp = new Group(symbolSizeComp, SWT.NONE);
        sizeComp.setText("Size");
        sizeComp.setLayoutData(
                new GridData(SWT.DEFAULT, SWT.FILL, false, true));
        sizeComp.setLayout(new GridLayout());
        sizeLabel2 = new Label(sizeComp, SWT.NONE);
        sizeLabel2.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        sizeScale = new Scale(sizeComp, SWT.VERTICAL);
        sizeScale.setLayoutData(new GridData(SWT.CENTER, SWT.FILL, true, true));
        sizeScale.setMinimum(5);
        sizeScale.setMaximum(30);
        sizeScale.setIncrement(1);
        sizeScale.setPageIncrement(1);
        sizeScale.setSelection(5);
        sizeScale.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                double size = sizeScale.getSelection() / 10d;
                sizeLabel2.setText(String.valueOf(size));
                for (IPlotModelElement element : getSelectedElements()) {
                    element.setSymbolSize(size);
                }
                canvas.redraw();
            }
        });

        Group widthComp = new Group(symbolSizeComp, SWT.NONE);
        widthComp.setText("Width");
        widthComp.setLayoutData(
                new GridData(SWT.DEFAULT, SWT.FILL, false, true));
        widthComp.setLayout(new GridLayout());
        widthLabel = new Label(widthComp, SWT.NONE);
        widthLabel.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        widthScale = new Scale(widthComp, SWT.VERTICAL);
        widthScale
                .setLayoutData(new GridData(SWT.CENTER, SWT.FILL, true, true));
        widthScale.setMinimum(5);
        widthScale.setMaximum(50);
        widthScale.setIncrement(1);
        widthScale.setPageIncrement(1);
        widthScale.setSelection(5);
        widthScale.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                // NCP divides by 10 but then has lineWidthScaleFactor of 0.5 in
                // DisplayElementFactory...line width still gets too big though
                double width = widthScale.getSelection() / 20d;
                widthLabel.setText(String.valueOf(width));
                for (IPlotModelElement element : getSelectedElements()) {
                    element.setSymbolWidth(width);
                }
                canvas.redraw();
            }
        });
    }

    private PlotModelElement[] getSelectedElements() {
        PlotModelElement[] elements = Arrays.stream(table.getSelection())
                .map(item -> (PlotModelElement) item.getData())
                .toArray(PlotModelElement[]::new);
        return elements;
    }

    private boolean atleastOneChecked() {
        for (TableItem item : table.getItems()) {
            if (item.getChecked()) {
                return true;
            }
        }
        return false;
    }

    private void populateValues() {
        for (TableItem item : table.getItems()) {
            item.dispose();
        }

        for (IPlotModelElement plotElement : factory.getPlotFields()) {
            addTableItem(plotElement);
        }

        plotParamsList.removeAll();
        Collection<String> availableParams = PlotParameterDefinitionsManager
                .getInstance().getDefinitions(factory.getPlugin())
                .getParamDisplayNames();
        plotParamsList.setItems(availableParams.toArray(new String[0]));

        populateFieldsForSelectedParams();

        canvas.redraw();
    }

    private TableItem addTableItem(IPlotModelElement plotElement) {
        TableItem paramEntry = null;
        DisplayMode mode = plotElement.getMode();
        if (mode == DisplayMode.VISIBLE || mode == DisplayMode.HIDDEN) {
            paramEntry = new TableItem(table, SWT.NONE);

            String[] text = { plotElement.getParamDef().getDisplayName(),
                    String.valueOf(plotElement.getX()),
                    String.valueOf(plotElement.getY()) };

            paramEntry.setText(text);
            paramEntry.setChecked(mode == DisplayMode.VISIBLE);
            paramEntry.setData(plotElement);
        }
        return paramEntry;
    }

    private void populateFieldsForSelectedParams() {
        PlotModelElement[] elements = getSelectedElements();

        populateTextFieldsForSelectedParams(elements);
        populatePositionFieldsForSelectedParams(elements);
        populateColorFieldsForSelectedParams(elements);
        populateMarkerFieldForSelectedParams(elements);
        populateAvailableParamsForSelectedParams(elements);
        populateSymbolSizeFieldsForSelectedParams(elements);
    }

    private void populateTextFieldsForSelectedParams(
            IPlotModelElement... elements) {
        if (elements.length == 0) {
            setEnabledRecursive(textGroup, false);
        } else {
            boolean isText = true;
            for (IPlotModelElement element : elements) {
                if (element.getParamDef()
                        .getDisplayType() != DisplayType.TEXT) {
                    isText = false;
                    break;
                }
            }

            if (isText && elements.length == 1) {
                IPlotModelElement element = elements[0];
                sizeCombo.setText(String.valueOf(element.getFontSize()));
                fontCombo.setText(element.getFontFamily());
                styleCombo.setText(element.getFontStyle());
                anchorCombo.setText(element.getTextAnchor().getName());
            } else {

                sizeCombo.setText("");
                fontCombo.setText("");
                styleCombo.setText("");
                anchorCombo.setText("");
            }

            setEnabledRecursive(textGroup, isText);
        }
    }

    private void populatePositionFieldsForSelectedParams(
            IPlotModelElement... elements) {
        if (elements.length == 1) {
            setEnabledRecursive(posGroup, true);
            IPlotModelElement element = elements[0];
            xSpinner.setSelection(element.getX());
            ySpinner.setSelection(element.getY());
        } else {
            setEnabledRecursive(posGroup, false);
            xSpinner.setSelection(0);
            ySpinner.setSelection(0);
        }
    }

    private void populateColorFieldsForSelectedParams(
            IPlotModelElement... elements) {
        setEnabledRecursive(colorGroup, elements.length > 0);

        if (elements.length == 1) {
            // TODO need to set some special color image on button if no color
            // or advanced (e.g. grey/white checkered for no color, rainbow
            // striped for advanced (like NCP), or could stripes of colors in
            // the conditional color
            RGB color = elements[0].getColor();
            if (color == null) {
                color = new RGB(255, 255, 255);
            }
            cfe.getColorSelector().setColorValue(color);
        } else {
            // TODO set some default, or shared color if multiple
            // Look at ColorSelector.updateColorImage()
        }
    }

    private void populateMarkerFieldForSelectedParams(
            IPlotModelElement... elements) {
        if (elements.length == 0) {
            setEnabledRecursive(markerGroup, false);
        } else {
            boolean isMarker = true;
            for (IPlotModelElement element : elements) {
                if (element.getParamDef()
                        .getDisplayType() != DisplayType.MARKER) {
                    isMarker = false;
                    break;
                }
            }

            setEnabledRecursive(markerGroup, isMarker);

        }
    }

    private void populateAvailableParamsForSelectedParams(
            PlotModelElement... elements) {
        setEnabledRecursive(paramsGroup, elements.length > 0);
        String[] displayNames = Arrays.stream(elements)
                .map(element -> element.getParamDef().getDisplayName())
                .toArray(String[]::new);
        plotParamsList.setSelection(displayNames);
    }

    private void populateSymbolSizeFieldsForSelectedParams(
            IPlotModelElement... elements) {
        if (elements.length == 0) {
            setEnabledRecursive(symbolSizeComp, false);
        } else {
            boolean isSymbol = true;
            for (IPlotModelElement element : elements) {
                DisplayType type = element.getParamDef().getDisplayType();
                if (type != DisplayType.MARKER && type != DisplayType.TABLE
                        && type != DisplayType.RANGE && type != DisplayType.BARB
                        && type != DisplayType.ARROW
                        && type != DisplayType.ARROWUV) {
                    isSymbol = false;
                    break;
                }
            }

            setEnabledRecursive(symbolSizeComp, isSymbol);

            if (isSymbol && elements.length == 1) {
                IPlotModelElement element = elements[0];
                double size = element.getSymbolSize();
                double width = element.getSymbolWidth();
                sizeScale.setSelection((int) (element.getSymbolSize() * 10));
                widthScale.setSelection((int) (element.getSymbolWidth()));
                sizeLabel2.setText(String.valueOf(size));
                widthLabel.setText(String.valueOf(width));
            } else {
                sizeScale.setSelection(sizeScale.getMinimum());
                widthScale.setSelection(widthScale.getMinimum());
                sizeLabel2.setText("");
                widthLabel.setText("");
            }

            // TODO I feel like this shouldn't be needed...
            layout(new Control[] { sizeLabel2, widthLabel });
        }
    }

    public void reset() {
        try {
            this.factory = getFactoryFromFile();
        } catch (VizException e) {
            // TODO Decide what to do if svg file is missing
            throw new RuntimeException(e);
        }
        populateValues();
    }

    public void apply() {
        // TODO validate (e.g. added parameter that doesn't actually have
        // parameter selected yet)
        factory.savePlotModel();
    }

    private IPlotModelFactory getFactoryFromFile() throws VizException {
        MapDescriptor fakeDescriptor = new MapDescriptor();
        IPlotModelFactory factory = null;
        if (PlotModelFactory.isNewSVGFormat(plotModelFile)) {
            factory = new PlotModelFactory(fakeDescriptor, plotModelFile);
        } else {
            factory = new PlotModelFactoryDefault(fakeDescriptor,
                    plotModelFile);
        }
        factory.setLowerLimit(-Double.MAX_VALUE);
        factory.setUpperLimit(Double.MAX_VALUE);
        return factory;
    }

    private static void setEnabledRecursive(Composite composite,
            boolean enabled) {
        for (Control control : composite.getChildren()) {
            if (control instanceof Composite) {
                setEnabledRecursive((Composite) control, enabled);
            } else {
                control.setEnabled(enabled);
            }
        }

        composite.setEnabled(enabled);
    }

    // TODO potential generic way of checking if we should, for example, show
    // font when multiple are selected (only if they all have the same value)
    private static boolean allSame(PlotModelElement[] elements,
            EnumSet<DisplayType> types,
            Function<PlotModelElement, Object> getter) {
        if (elements.length == 0) {
            return false;
        }

        DisplayType type = null;
        Object value = null;
        for (PlotModelElement element : elements) {
            if (!types.isEmpty()) {
                DisplayType currType = element.getParamDef().getDisplayType();
                if (!types.contains(currType)
                        || type != null && currType != type) {
                    return false;
                }
                type = currType;
            }

            Object currValue = getter.apply(element);
            if (value != null && !value.equals(currValue)) {
                return false;
            }
            value = currValue;
        }

        return true;
    }

    private PlotParameterDefinitions getParamDefs() {
        return PlotParameterDefinitionsManager.getInstance()
                .getDefinitions(factory.getPlugin());
    }
}
