package gov.noaa.gsd.viz.ensemble.display.distribution;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;

import gov.noaa.gsd.viz.ensemble.display.chart.BasicChartView;
import gov.noaa.gsd.viz.ensemble.display.chart.ChartConfig;
import gov.noaa.gsd.viz.ensemble.display.chart.PDFCDFChartView;
import gov.noaa.gsd.viz.ensemble.display.chart.SingleSampleInfo;
import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.EnsembleToolViewer;

/**
 * 
 * The GUI of the distribution viewer. It's contains the chart title,
 * configuration dialog ("Settings") button and the chart drawing area.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 26, 2015  12301        jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class DistributionViewUI extends Composite {

    /** The distribution display object to control the chart drawing */
    private DistributionDisplay disp;

    /** The single sample data to display the chart */
    private SingleSampleInfo singleSampleInfo = null;

    /**
     * The configuration object to selected chart style and display features.
     */
    private ChartConfig config;

    /** The chart drawing area */
    private Canvas chartCanvas;

    /** The composite to hold drawing chart area */
    private Composite canvasComposite;

    /**
     * Title to show the current chart, level, unit and location of the sampling
     * data.
     */
    private Label titleLabel;

    /**
     * The button that allows the user the settings/preferences dialog.
     */
    private Button configButton = null;

    /**
     * If true, the mouse-drag can be used to sample CDF probabilities.
     */
    private boolean mouseDownMove = false;

    /**
     * The constructor
     * 
     * @param parent
     *            - The composite which created this GUI
     * @param style
     *            - SWT style
     */
    public DistributionViewUI(Composite parent, int style) {
        super(parent, style);

        this.config = new ChartConfig();

        initializeUI();

        this.disp = new DistributionDisplay(this);

    }

    /**
     * Returns the chart configuration
     */
    public ChartConfig getConfig() {
        return config;
    }

    /**
     * Sets the chart configuration.
     * 
     * @param config
     *            - The configuration.
     */
    public void setConfig(ChartConfig config) {
        this.config = config;
    }

    /**
     * Gets the current canvas.
     * 
     * @return the current canvas
     */
    public Canvas getChartCanvas() {
        return chartCanvas;
    }

    /**
     * Initializes the Distribution UI in the Ensemble Tool view
     */
    private void initializeUI() {
        createBaseUI();

    }

    /**
     * Builds the top-level items in the GUI, such as the chart information,
     * title, and a "Settings" button to pop up the chart configuration dialog,
     * then builds the chart drawing area.
     */
    private void createBaseUI() {
        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
        GridLayout this_gl = new GridLayout(1, false);
        this_gl.horizontalSpacing = 0;
        this_gl.verticalSpacing = 2;
        this_gl.marginHeight = 2;
        this_gl.marginWidth = 2;
        this.setLayout(this_gl);

        /* The title and configuration button bar */
        Composite barComposite = new Composite(this, SWT.BORDER);
        GridData barComposite_gd = new GridData(SWT.FILL, SWT.TOP, true, false,
                1, 1);
        barComposite.setLayoutData(barComposite_gd);
        this_gl = new GridLayout(20, false);
        barComposite.setLayout(this_gl);

        /* Left-most horizontal cosmetic spacer */
        Label spacer = new Label(barComposite, SWT.NONE);
        GridData spacer_gd = new GridData(SWT.LEFT, SWT.CENTER, false, false, 2,
                1);
        spacer.setLayoutData(spacer_gd);

        /* The label of the chart title */
        titleLabel = new Label(barComposite, SWT.NONE);
        GridData titleLabel_gd = new GridData(SWT.LEFT, SWT.CENTER, true, false,
                14, 1);
        titleLabel.setLayoutData(titleLabel_gd);
        titleLabel.setFont(EnsembleToolViewer.getViewFontSmall());
        titleLabel.setText("PDF-CDF: ");

        /** The button for popping up the configuration dialog */
        configButton = new Button(barComposite, SWT.PUSH);
        GridData configButton_gd = new GridData(SWT.FILL, SWT.CENTER, true,
                false, 4, 1);
        configButton.setLayoutData(configButton_gd);
        configButton.setFont(EnsembleToolViewer.getViewFontSmall());
        configButton.setText("Settings...");

        configButton.setToolTipText("Configuration Dialog");

        configButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                /* Pops up the chart configuration dialog */
                DistributionConfigDialog configDialog = new DistributionConfigDialog(
                        canvasComposite.getShell(), config);

                configDialog.setBlockOnOpen(true);

                // If the "Change" button (which is the same as OK) is clicked
                // on the configuration dialog, then save the current
                // selections, update the chart display and close the dialog.

                if (configDialog.open() == Window.OK) {
                    configDialog.updateConfig();
                    disp.changeChartStyle(config.getChartStyle());
                    updateDisplay();
                    configDialog.close();
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO:do nothing currently
            }
        });

        GridData gridData = new GridData();

        /* Distribution drawing area itself, the canvas */
        canvasComposite = new Composite(this, SWT.BORDER);
        gridData = new GridData();
        gridData.horizontalSpan = 2;
        gridData.verticalSpan = 2;
        gridData.horizontalAlignment = SWT.FILL;
        gridData.verticalAlignment = SWT.FILL;
        gridData.heightHint = 0;
        gridData.widthHint = 0;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        canvasComposite.setLayoutData(gridData);
        canvasComposite.setLayout(new FillLayout());
        chartCanvas = new Canvas(canvasComposite, SWT.DOUBLE_BUFFERED);
        chartCanvas.setBounds(canvasComposite.getBounds());

        chartCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                updateDisplay();

            }
        });

        /* Mouse interaction within a chart */
        chartCanvas.addListener(SWT.MouseMove, new Listener() {

            @Override
            public void handleEvent(Event event) {

                // When mouse is moving over the CDF, and under the mouse
                // "down-move reading" or "continue reading" probability state,
                // gets the cursor location, passes it into the PDFCDFChartView
                // object and redraws the chart.

                BasicChartView chartView = disp.getChart().getChartView();
                ChartConfig.ChartStyle style = config.getChartStyle();
                if ((style == ChartConfig.ChartStyle.CHART_PDF_CDF
                        || style == ChartConfig.ChartStyle.CHART_CDF_ONLY)
                        && (config.isMouseReadCDFMove()
                                || (config.isMouseDownReadCDF()
                                        && mouseDownMove))
                        && disp.getChart().getChartView().isReadyForPaint()) {
                    ((PDFCDFChartView) chartView).setmX(event.x);
                    ((PDFCDFChartView) chartView).setmY(event.y);
                    chartView.setPainting(true);
                    disp.getChart().paint(new GC(chartCanvas));
                    disp.getChart().getChartView().setPainting(false);

                }

            }
        });

        chartCanvas.addListener(SWT.MouseUp, new Listener() {

            @Override
            public void handleEvent(Event event) {

                // When the mouse is up, stop the "mouse-drag reading" state by
                // setting the mouseDownMove flag to false.
                mouseDownMove = false;

            }
        });

        chartCanvas.addListener(SWT.MouseDown, new Listener() {

            @Override
            public void handleEvent(Event event) {

                // Starts the CDF "mouse-drag reading" probability state. When
                // the mouse is down, gets the cursor location, passes it into
                // the PDFCDFChartView object and redraws the chart.

                BasicChartView chartView = disp.getChart().getChartView();
                ChartConfig.ChartStyle style = config.getChartStyle();
                if ((style == ChartConfig.ChartStyle.CHART_PDF_CDF
                        || style == ChartConfig.ChartStyle.CHART_CDF_ONLY)
                        && config.isMouseDownReadCDF()
                        && chartView.isReadyForPaint()) {
                    ((PDFCDFChartView) chartView).setmX(event.x);
                    ((PDFCDFChartView) chartView).setmY(event.y);
                    chartView.setPainting(true);
                    disp.getChart().paint(new GC(chartCanvas));
                    disp.getChart().getChartView().setPainting(false);

                    mouseDownMove = true;

                }

            }
        });

    }

    /**
     * Sets the chart title.
     * 
     * @param title
     *            - The chart title
     */
    public void setDistributionTitle(String title) {
        this.titleLabel.setText(title);
    }

    /**
     * Notifies the UI to refresh or create the distribution graphics with the
     * new data.
     * 
     * @param data
     *            - the single sample data set.
     */
    public void updateDisplay(SingleSampleInfo data) {

        /* Saves the data for reusing */
        singleSampleInfo = data;

        updateDisplay();
    }

    /**
     * Notify the UI to refresh or create the distribution graphics with current
     * data. Ignores updating if there is no data or is still painting.
     * 
     * TODO: This may need to be wrapped in a Job in the future.
     */
    public void updateDisplay() {
        if (singleSampleInfo == null
                || disp.getChart().getChartView().isPainting() == true) {
            return;
        }

        /* Draws chart in the distribution view */
        disp.getChart().getChartView().setPainting(true);
        disp.displayChart(singleSampleInfo);
        disp.getChart().getChartView().setPainting(false);

    }

    /**
     * Returns the distribution display.
     * 
     * @return
     */
    public DistributionDisplay getDisp() {
        return disp;
    }

    /**
     * Disposes resources and unregisters listeners.
     */
    public void dispose() {
        super.dispose();
        disp.dispose();
        chartCanvas.dispose();
        canvasComposite.dispose();

    }

    synchronized public void setEditable(boolean enabled) {
        titleLabel.setEnabled(enabled);
        configButton.setEnabled(enabled);
        disp.setEditable(enabled);
    }

}
