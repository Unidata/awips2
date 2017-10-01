package gov.noaa.gsd.viz.ensemble.display.distribution;

import gov.noaa.gsd.viz.ensemble.display.chart.ChartConfig;
import gov.noaa.gsd.viz.ensemble.display.chart.ChartConfig.BinChooser;
import gov.noaa.gsd.viz.ensemble.display.chart.ChartConfig.ChartStyle;
import gov.noaa.gsd.viz.ensemble.display.chart.ChartConfig.HistFrequencyType;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * 
 * The pop up dialog to interactively configure chart display features. There
 * are three components on the dialog. They are the chart selector, chart
 * options and chart configuration. Each chart is with specified options and
 * configuration. Click the "Change" button, the current configuration will be
 * effected.
 * 
 * The first time this dialog is opened it is initialized using the default
 * chart configuration object, and will save specified changes, storing
 * configuration, options, and selector changes into the ChartConfig object when
 * closing it. Opening the dialog for subsequent changes will read the
 * previously saved changes from the chart configuration object.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 21, 2015  12301         jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class DistributionConfigDialog extends Dialog {
    /* Current selected chart Style */
    private ChartConfig.ChartStyle chartStyle = ChartStyle.CHART_PDF_CDF;

    /* The flag if the mean line available */
    private boolean isMeanLine = true;

    /* The flag if the median line available */
    private boolean isMedianLine = true;

    /* The flag if the grid lines available */
    private boolean isGridLines = true;

    /* The flag if the histogram bars available */
    private boolean isHistogramBar = true;

    /* The flag if drawing the distribution lines */
    private boolean isPlotLines = true;

    /*
     * Is CDF hover sampling available? If yes, then calculate the probabilities
     * of less and great than this x value. This responds continuously with the
     * mouse movement.
     */
    private boolean isMouseReadCDFHover = false;

    /*
     * Is CDF sampling by mouse-click or mouse-drag available? If yes, then
     * calculate the probabilities of less and great than this x value. This
     * responds continuously with the mouse movement.
     */
    private boolean isMouseDownReadCDF = true;

    /*
     * If true, the actual data distribution values will be shown on the x-axis
     * as plot member.
     */
    private boolean isPlotMembers = true;

    /* The current configured frequency type */
    private HistFrequencyType frequencyType = HistFrequencyType.FREQUENCY_PERCENT;

    /* The current configured bin number generate method */
    private BinChooser binNum = BinChooser.BIN_7;

    /* The chart configuration object from outside */
    private ChartConfig config;

    /* The composite which contains selection and configuration controls. */
    private Composite selectConfigComposite;

    /* The base composite */
    private Composite baseComposite;

    /* The selection composite */
    private Composite selectComposite;

    /* The configuration composite */
    private Composite configComposite;

    /* The shell of this dialog, is used to auto resize the window */
    Shell shell;

    /**
     * The constructor
     * 
     * @param parentShell
     *            - The parent shell
     * @param config
     *            - The chart configuration
     */
    protected DistributionConfigDialog(Shell parentShell, ChartConfig config) {
        super(parentShell);
        this.config = config;
        initialConfig();

    }

    /**
     * Builds the dialog area.
     */
    @Override
    protected Control createDialogArea(Composite parent) {

        /*
         * TODO is storing this really necessary? Should be able to use
         * getShell() instead.
         */
        shell = this.getShell();

        baseComposite = new Composite(parent, SWT.NONE);
        baseComposite.setLayout(new GridLayout(1, true));
        createAppsSelectArea(baseComposite);

        selectConfigComposite = new Composite(baseComposite, SWT.BORDER);
        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        selectConfigComposite.setLayout(gridLayout);

        Label optionLabel = new Label(selectConfigComposite, SWT.NONE);
        optionLabel.setText("Options:");

        Label configLabel = new Label(selectConfigComposite, SWT.NONE);
        configLabel.setText("Configuration:");

        createSelectionArea(selectConfigComposite);
        createSelectButtons();

        createConfigArea(selectConfigComposite);
        createConfigButtons();

        return baseComposite;

    }

    /**
     * Builds the application area which contains chart style selected list.
     * 
     * @param parent
     *            - top composite is the baseComposite.
     * @return appComposit
     */
    private Composite createAppsSelectArea(Composite parent) {
        Composite appComposite = new Composite(parent, SWT.BORDER);
        GridData gd_appComposite = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 1, 1);
        gd_appComposite.widthHint = 419;
        appComposite.setLayoutData(gd_appComposite);
        appComposite.setLayout(new GridLayout(2, true));

        Label appMenuName = new Label(appComposite, SWT.LEFT);
        appMenuName.setText("Chart Style:");

        final Combo appCombo = new Combo(appComposite, SWT.NONE);
        appCombo.setText("PDF-CDF");
        appCombo.add("PDF-CDF");
        appCombo.add("PDF");
        appCombo.add("CDF");

        /* TODO:Implement later */
        // appCombo.add("Multiple PDFs");
        // appCombo.add("Whisker");appCombo.add("Slope");
        // appCombo.add("Time Series");

        /** initial selection */
        switch (chartStyle) {
        case CHART_PDF_CDF:
            appCombo.select(0);
            break;
        case CHART_PDF_ONLY:
            appCombo.select(1);
            break;
        case CHART_CDF_ONLY:
            appCombo.select(2);
            break;

        default:
            appCombo.select(0);
            break;
        }

        /* listener for the chart style selection */
        appCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {

                ChartConfig.ChartStyle oldChartStyle = chartStyle;
                String app = appCombo.getText();

                switch (app) {
                case "PDF-CDF":
                    chartStyle = ChartStyle.CHART_PDF_CDF;
                    break;
                case "PDF":
                    chartStyle = ChartStyle.CHART_PDF_ONLY;
                    break;
                case "CDF":
                    chartStyle = ChartStyle.CHART_CDF_ONLY;
                    break;

                /* TODO: More charts will be implemented later */

                // case "Multiple Distribution":
                // chartStyle = ChartStyle.CHART_MULTI_DISRTIBUTIONS;
                // break;
                // case "Slope":
                // chartStyle = ChartStyle.CHART_SLOPE;
                // break;
                // case "Whisker":
                // chartStyle = ChartStyle.CHART_WHISKER;
                // break;
                // case "Time Series":
                // chartStyle = ChartStyle.CHART_TIMESERIES;
                // break;

                default:
                    chartStyle = ChartStyle.CHART_PDF_CDF;
                    break;
                }

                /*
                 * Updates the distribution display to the selected chart style.
                 * Same style or select items, need not change GUI.
                 */

                if (chartStyle != oldChartStyle
                        && (chartStyle == ChartStyle.CHART_PDF_ONLY || oldChartStyle == ChartStyle.CHART_PDF_ONLY)) {
                    createSelectButtons();

                    /*
                     * TODO: Why aren't we just calling getShell()?
                     */
                    final Point newSize = shell.computeSize(SWT.DEFAULT,
                            SWT.DEFAULT, true);
                    shell.setSize(newSize.x, newSize.y);

                }

                /*
                 * TODO: For some charts, need to update the Configuration Area
                 * by calling the createConfigButtons() which is not the case
                 * now, the PDF, CDF and PDF_CDF charts are with same
                 * configuration items.
                 */

            }

        });

        return appComposite;
    }

    /**
     * Builds the selection area composite.
     * 
     * @param parent
     *            - the selectConfigComposite
     * @return select composite
     */
    private Composite createSelectionArea(Composite parent) {
        if (selectComposite != null) {
            selectComposite.dispose();
            selectComposite = null;
        }
        selectComposite = new Composite(parent, SWT.BORDER);

        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 1;
        selectComposite.setLayout(gridLayout);

        return selectComposite;
    }

    /**
     * Builds the option buttons to be selected for any chart.
     */
    private void createSelectButtons() {

        /* Removes old buttons if there are any */
        Control[] children = selectComposite.getChildren();
        if (children != null && children.length > 0) {
            for (Control kid : children) {
                kid.dispose();
            }
        }

        /* Creates buttons depending on chart style */
        if (chartStyle == ChartConfig.ChartStyle.CHART_PDF_ONLY) {
            createPDFSelectButtons();
        } else {
            createCDFSelectButtons();
        }

        selectComposite.redraw();
        selectComposite.pack(true);
        selectComposite.layout(true);

        shell.layout(true, true);

    }

    /**
     * Builds the related option buttons to be selected for PDF chart.
     */
    private void createPDFSelectButtons() {
        final Button distLineCheckButton = new Button(selectComposite,
                SWT.CHECK | SWT.MULTI);
        distLineCheckButton.setText("Distribution Line(s)");
        distLineCheckButton.setSelection(isPlotLines);
        distLineCheckButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                isPlotLines = distLineCheckButton.getSelection();

            }
        });

        final Button hisBarCheckButton = new Button(selectComposite, SWT.CHECK
                | SWT.MULTI);
        hisBarCheckButton.setText("Histogram Bars");
        hisBarCheckButton.setSelection(isHistogramBar);
        hisBarCheckButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                isHistogramBar = hisBarCheckButton.getSelection();

            }
        });

        final Button meanLineCheckButton = new Button(selectComposite,
                SWT.CHECK | SWT.MULTI);
        meanLineCheckButton.setText("Mean Line");
        meanLineCheckButton.setSelection(isMeanLine);
        meanLineCheckButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                isMeanLine = meanLineCheckButton.getSelection();

            }
        });

        final Button medianLineCheckButton = new Button(selectComposite,
                SWT.CHECK | SWT.MULTI);
        medianLineCheckButton.setText("Median Line");
        medianLineCheckButton.setSelection(isMedianLine);
        medianLineCheckButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                isMedianLine = medianLineCheckButton.getSelection();

            }
        });

        final Button gridLineCheckButton = new Button(selectComposite,
                SWT.CHECK | SWT.MULTI);
        gridLineCheckButton.setText("Grid Lines");
        gridLineCheckButton.setSelection(isGridLines);
        gridLineCheckButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                isGridLines = gridLineCheckButton.getSelection();

            }
        });
        final Button membersCheckButton = new Button(selectComposite, SWT.CHECK
                | SWT.MULTI);
        membersCheckButton.setText("Plot Members");
        membersCheckButton.setSelection(isPlotMembers);
        membersCheckButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                isPlotMembers = membersCheckButton.getSelection();

            }
        });

    }

    /**
     * Builds the related option buttons to be selected for CDF chart.
     */
    private void createCDFSelectButtons() {
        createPDFSelectButtons();

        final Button readMoveCheckButton = new Button(selectComposite,
                SWT.RADIO | SWT.MULTI);
        readMoveCheckButton.setText("CDF Read Hover");
        readMoveCheckButton.setSelection(isMouseReadCDFHover);
        readMoveCheckButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                isMouseReadCDFHover = readMoveCheckButton.getSelection();

            }
        });

        final Button readDownCheckButton = new Button(selectComposite,
                SWT.RADIO | SWT.MULTI);
        readDownCheckButton.setText("CDF Read Drop/Click");
        readDownCheckButton.setSelection(isMouseDownReadCDF);
        readDownCheckButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                isMouseDownReadCDF = readDownCheckButton.getSelection();

            }
        });
    }

    /**
     * Builds the configuration area composite.
     * 
     * @param parent
     *            - Is the selection and configuration composite
     * @return configComposite
     */
    private Composite createConfigArea(Composite parent) {
        if (configComposite != null) {
            configComposite.dispose();
            configComposite = null;
        }
        configComposite = new Composite(parent, SWT.BORDER);
        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        configComposite.setLayout(gridLayout);

        return configComposite;
    }

    /**
     * Builds related configuration controls
     */
    private void createConfigButtons() {

        /* Removes old configuration controls if there are any */
        Control[] children = configComposite.getChildren();
        if (children != null && children.length > 0) {
            for (Control kid : children) {
                kid.dispose();
            }
        }

        /* Creates items depending on chart style */
        createPDFConfigButtons();
        if (chartStyle != ChartConfig.ChartStyle.CHART_PDF_ONLY) {

            /*
             * TODO: Keeps same configuration items at this release. More chart
             * configuration items to related chart will added at here.
             */
            // createCDFConfigButtons();

        }

        configComposite.redraw();

        configComposite.pack(true);
    }

    /**
     * Builds PDF configuration items
     */
    private void createPDFConfigButtons() {
        /* The frequency options */
        Label frequencyLabel = new Label(configComposite, SWT.NONE);
        frequencyLabel.setText("Frequency Type");
        final Combo frequencyCombo = new Combo(configComposite, SWT.NONE);
        frequencyCombo.add("Relative Frequency");

        /* TODO: Implement later */
        // frequencyCombo.add("Frequency");
        // frequencyCombo.add("Rate");

        frequencyCombo.select(0);

        frequencyCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                String frequencyTypeStr = frequencyCombo.getText();
                switch (frequencyTypeStr) {
                case "Relative Frequency":
                    frequencyType = HistFrequencyType.FREQUENCY_PERCENT;
                    break;

                /* TODO:Implement later */
                // case "Frequency":
                // frequencyType = HisFrequencyType.FREQUENCY_NUMBER;
                // break;
                // case "Rate":
                // frequencyType = HisFrequencyType.FREQUENCY_RATE;
                // break;

                default:
                    frequencyType = HistFrequencyType.FREQUENCY_PERCENT;
                    break;
                }
            }
        });

        /*
         * Bin number options: User can specify bin number or a method to auto
         * generate a bin number according a source data set. The "ALPS" method
         * is from the Text Histogram of ALPS (Advanced Linux Prototype System
         * of AWIPS I), created by our AWIPS expert James Ramer at NOAA/GSD. I
         * feel the 7 and 5 bin number is optimal for the 50 members or less
         * ensemble case, but need more feedback from forecasters to improve it
         * later.
         */
        Label binLabel = new Label(configComposite, SWT.NONE);
        binLabel.setText("Bin Numbers");
        final Combo binCombo = new Combo(configComposite, SWT.NONE);
        binCombo.add("5");
        binCombo.add("7");
        binCombo.add("9");
        binCombo.add("11");
        binCombo.add("ALPS");
        binCombo.add("Square Root");
        binCombo.add("Sturges Formula");
        binCombo.add("Rise Rule");
        binCombo.select(binNum.ordinal());
        binCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {

                String binStr = binCombo.getText();

                switch (binStr) {
                case "5":
                    binNum = BinChooser.BIN_5;
                    break;
                case "7":
                    binNum = BinChooser.BIN_7;
                    break;
                case "9":
                    binNum = BinChooser.BIN_9;
                    break;
                case "11":
                    binNum = BinChooser.BIN_11;
                    break;
                case "ALPS":
                    binNum = BinChooser.BIN_ALPS;
                    break;
                case "Square Root":
                    binNum = BinChooser.SQUARE_ROOT;
                    break;
                case "Sturges Formula":
                    binNum = BinChooser.STURGES_FORMULA;
                    break;
                case "Rise Rule":
                    binNum = BinChooser.RISE_RULE;
                    break;

                default:
                    binNum = BinChooser.BIN_7;
                    break;
                }
            }
        });
    }

    /**
     * Initializes the chart configuration dialog with a ChartConfig object
     */
    private void initialConfig() {
        chartStyle = config.getChartStyle();
        binNum = config.getBinChoicer();
        isGridLines = config.isGridLines();
        isHistogramBar = config.isHistogramBar();
        frequencyType = config.getHisYType();
        isMeanLine = config.isMeanLine();
        isMedianLine = config.isMedianLine();
        isPlotLines = config.isPlotLines();

        isMouseReadCDFHover = config.isMouseReadCDFMove();
        isMouseDownReadCDF = config.isMouseDownReadCDF();
        isPlotMembers = config.isMembers();

    }

    /**
     * The override method to use "Change" as label for the OK
     * button(non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "Change", true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    /** Sets the window title */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Distribution Viewer Settings");
    }

    /**
     * A interface to save the current configuration on the GUI to a ChartConfig
     * object.
     */
    public synchronized void updateConfig() {
        config.setChartStyle(chartStyle);
        config.setBinChoicer(binNum);
        config.setGridLines(isGridLines);
        config.setHistogramBar(isHistogramBar);
        config.setHisYType(frequencyType);
        config.setMeanLine(isMeanLine);
        config.setMedianLine(isMedianLine);
        config.setPlotLines(isPlotLines);
        config.setMouseReadCDFMove(isMouseReadCDFHover);
        config.setMouseDownReadCDF(isMouseDownReadCDF);
        config.setMembers(isPlotMembers);

    }

}
