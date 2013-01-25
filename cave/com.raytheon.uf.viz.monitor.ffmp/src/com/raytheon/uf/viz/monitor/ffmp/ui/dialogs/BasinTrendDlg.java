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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.BasinTrendCommon.PlotItems;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.BasinTrendCommon.TimeDuration;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.BasinTrendCommon.Underlays;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.TableCellColor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.ThreshColNames;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPGraphData;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResource;
import com.raytheon.uf.viz.monitor.ffmp.xml.FFMPConfigBasinXML;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Display FFMP Basin Trend Graph.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Dec 6, 2012  1353       rferrel     Code clean up for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class BasinTrendDlg extends CaveSWTDialog {
    /**
     * Main composite.
     */
    private Composite mainComp;

    /**
     * Basin trend graph
     */
    private BasinTrendGraph basinTrendGraph;

    /**
     * Label fonts - large, medium, and small.
     */
    private Font largeLabelFont;

    private Font medLabelFont;

    private Font smallLabelFont;

    /**
     * Aggregate name label.
     */
    private Label aggregateNameLbl;

    /**
     * Time frame label.
     */
    private Label timeFrameLbl;

    /**
     * Data time label.
     */
    private Label dataTimeLbl;

    /**
     * Date string.
     */
    private String dateStr = "";

    /**
     * Under-lay radio buttons.
     */
    private Button rateUlRdo;

    /**
     * QPE under-lay radio button.
     */
    private Button qpeUlRdo;

    private Button ratioUlRdo;

    private Button diffUlRdo;

    /**
     * Plot check buttons.
     */
    private Button ratePlotChk;

    private Button qpePlotChk;

    private Button qpfPlotChk;

    private Button guidPlotChk;

    private Button vgbPlotChk;

    /**
     * QPFSCAN radio button.
     */
    private List<Button> qpfRdos = new ArrayList<Button>();

    /**
     * RFCFFG radio button.
     */
    private List<Button> ffgRdos = new ArrayList<Button>();

    /**
     * Background color for controls and graph.
     */
    private Color bgColor;

    /**
     * Labels to describe the upper, mid, lower, and missing colors.
     */
    private Label colorForLbl;

    private Label lowerColorLbl;

    private Label midColorLbl;

    private Label upperColorLbl;

    private Label missingColorLbl;

    /**
     * String prefix for the color for label.
     */
    private final String colorForStr = "Color for: ";

    /**
     * Button to reverse the X axis of the graph (all hours only).
     */
    private Button reverseXAxisBtn;

    /**
     * Colors for Rate, QPE, QPF, Guidance
     */
    private Color rateColor;

    private Color qpeColor;

    private Color qpfColor;

    private Color guidColor;

    private Color vgbColor;

    /**
     * Arrays of buttons for time durations, under-lay, and plots.
     */
    private List<Button> timeDurationButtons;

    private List<Button> underlayButtons;

    private List<Button> plotButtons;

    /**
     * Ratio item label.
     */
    private Label ratioItemLbl;

    /**
     * Diff item label.
     */
    private Label diffItemLbl;

    /**
     * Graph data to be displayed.
     */
    private FFMPGraphData graphData;

    /**
     * Reverse X axis flag.
     */
    private boolean reverseXAxis = false;

    /**
     * Date/Time format.
     */
    private SimpleDateFormat sdf = new SimpleDateFormat("MMM dd yy HH:mm z");

    private boolean vgb = false;

    private String pfaf;

    private Date currentDate;

    private FFMPResource resource;

    private final List<ISourceUpdate> sourceListeners = new ArrayList<ISourceUpdate>();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param graphData
     *            Graph data.
     */
    public BasinTrendDlg(Shell parent, FFMPResource resource, Date date,
            String pfaf, boolean vgb, FFMPGraphData graphData) {
        super(parent, SWT.DIALOG_TRIM | SWT.MIN, CAVE.DO_NOT_BLOCK
                | CAVE.MODE_INDEPENDENT | CAVE.INDEPENDENT_SHELL);

        // Set the text in the dialog title bar
        setText("FFMP Basin Trend Graph");

        this.resource = resource;
        this.graphData = graphData;

        this.pfaf = pfaf;
        this.vgb = vgb;
        this.currentDate = date;

        this.graphData.setDisplayDate(date);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;

        return mainLayout;
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
        mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        mainComp.setLayout(gl);
        setBackgroundColor(mainComp);

        initializeData();

        formatDateTimeString();
        createGraphComposite();
        createRightSideControls();

        setupUnderlayPlotControls();

        /*
         * Set the Basin Trend graphing parameters.
         */
        updateGraph();
    }

    /**
     * Initialize data.
     */
    private void initializeData() {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        largeLabelFont = new Font(getDisplay(), "Monospace", 12, SWT.NORMAL);
        medLabelFont = new Font(getDisplay(), "Monospace", 11, SWT.NORMAL);
        smallLabelFont = new Font(getDisplay(), "Sans", 10, SWT.BOLD);
        bgColor = new Color(getDisplay(), 251, 228, 182);

        underlayButtons = new ArrayList<Button>();
        plotButtons = new ArrayList<Button>();
        timeDurationButtons = new ArrayList<Button>();

        /*
         * Setup the plot colors
         */
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();

        String colorName = ffmpCfg
                .getBasinTrendPlotColorName(ThreshColNames.RATE.name());
        RGB rgb = RGBColors.getRGBColor(colorName);
        rateColor = new Color(getDisplay(), rgb);

        colorName = ffmpCfg.getBasinTrendPlotColorName(ThreshColNames.QPE
                .name());
        rgb = RGBColors.getRGBColor(colorName);
        qpeColor = new Color(getDisplay(), rgb);

        colorName = ffmpCfg.getBasinTrendPlotColorName(ThreshColNames.QPF
                .name());
        rgb = RGBColors.getRGBColor(colorName);
        qpfColor = new Color(getDisplay(), rgb);

        colorName = ffmpCfg.getBasinTrendPlotColorName("GUID");
        rgb = RGBColors.getRGBColor(colorName);
        guidColor = new Color(getDisplay(), rgb);

        colorName = ffmpCfg.getBasinTrendPlotColorName("VGB");
        vgbColor = new Color(getDisplay(), new RGB(135, 206, 235));
    }

    /**
     * Format the date/time string.
     */
    private void formatDateTimeString() {
        dateStr = sdf.format(currentDate);
    }

    /**
     * Create the graph composite.
     */
    private void createGraphComposite() {
        Composite graphControlComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 0;
        graphControlComp.setLayout(gl);
        graphControlComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                false));
        setBackgroundColor(graphControlComp);

        /*
         * Create the aggregate label
         */
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 700;
        aggregateNameLbl = new Label(graphControlComp, SWT.CENTER);
        aggregateNameLbl.setBackground(getDisplay().getSystemColor(
                SWT.COLOR_BLACK));
        aggregateNameLbl.setForeground(getDisplay().getSystemColor(
                SWT.COLOR_WHITE));
        aggregateNameLbl.setFont(largeLabelFont);
        aggregateNameLbl.setLayoutData(gd);
        updateAggregateName();

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 700;
        timeFrameLbl = new Label(graphControlComp, SWT.CENTER);
        timeFrameLbl
                .setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        timeFrameLbl
                .setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        timeFrameLbl.setFont(largeLabelFont);
        timeFrameLbl.setLayoutData(gd);

        /*
         * Create the hours radio buttons
         */
        Composite buttonComp = new Composite(graphControlComp, SWT.NONE);
        gl = new GridLayout(6, false);
        gl.horizontalSpacing = 30;
        buttonComp.setLayout(gl);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.verticalIndent = 5;
        buttonComp.setLayoutData(gd);
        setBackgroundColor(buttonComp);

        for (TimeDuration timeDur : TimeDuration.values()) {
            Button timeDurRdo = new Button(buttonComp, SWT.RADIO);
            timeDurRdo.setText(timeDur.getTimeDurName());
            timeDurRdo.setData(timeDur);
            setBackgroundColor(timeDurRdo);
            timeDurRdo.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    Button rdoBtn = (Button) event.getSource();
                    TimeDuration td = (TimeDuration) rdoBtn.getData();
                    handleTimeDurAction(td);
                }
            });

            if (timeDur == TimeDuration.ALL) {
                timeDurRdo.setSelection(true);
            }

            timeDurationButtons.add(timeDurRdo);
        }

        /*
         * Create the graph
         */
        createGraph(graphControlComp);

        /*
         * Create the time label below the graph.
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataTimeLbl = new Label(graphControlComp, SWT.CENTER);
        dataTimeLbl.setLayoutData(gd);
        dataTimeLbl.setFont(largeLabelFont);
        setBackgroundColor(dataTimeLbl);
        updateHourLabel(true);
    }

    /**
     * Create the basin trend graph.
     * 
     * @param graphComp
     *            Graph composite.
     */
    private void createGraph(Composite graphComp) {
        basinTrendGraph = new BasinTrendGraph(graphComp, bgColor, graphData,
                rateColor, qpeColor, qpfColor, guidColor, vgbColor,
                reverseXAxis);
    }

    /**
     * Create the plot/under-lay/legend controls on the right side of the graph.
     */
    private void createRightSideControls() {
        Composite rightSideComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginTop = 10;
        rightSideComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        rightSideComp.setLayoutData(gd);
        setBackgroundColor(rightSideComp);

        createUnderlayPlotControls(rightSideComp);
        createColorLegend(rightSideComp);

        createRightSideButtons(rightSideComp);
    }

    /**
     * Create the under-lay and plot controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createUnderlayPlotControls(Composite parentComp) {
        Composite plotComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.marginWidth = 10;
        gl.verticalSpacing = 3;
        gl.horizontalSpacing = 0;
        plotComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        plotComp.setLayoutData(gd);
        plotComp.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.verticalIndent = 10;
        gd.horizontalSpan = ((GridLayout) plotComp.getLayout()).numColumns;
        Label plotLbl = new Label(plotComp, SWT.CENTER);
        plotLbl.setText("Plots");
        plotLbl.setFont(largeLabelFont);
        plotLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        plotLbl.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        plotLbl.setLayoutData(gd);

        Label ulLbl = new Label(plotComp, SWT.CENTER);
        ulLbl.setText(" ul ");
        ulLbl.setFont(largeLabelFont);
        ulLbl.setBackground(getDisplay().getSystemColor(SWT.COLOR_GRAY));

        Label pLbl = new Label(plotComp, SWT.CENTER);
        pLbl.setText(" p ");
        pLbl.setFont(largeLabelFont);
        pLbl.setBackground(getDisplay().getSystemColor(SWT.COLOR_GRAY));

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Label itemLbl = new Label(plotComp, SWT.CENTER);
        itemLbl.setText("    Item ");
        itemLbl.setFont(largeLabelFont);
        itemLbl.setBackground(getDisplay().getSystemColor(SWT.COLOR_GRAY));
        itemLbl.setLayoutData(gd);

        /*
         * Rate
         */
        rateUlRdo = new Button(plotComp, SWT.RADIO);
        formatUnderlayButton(Underlays.RATE, rateUlRdo, plotComp);

        ratePlotChk = new Button(plotComp, SWT.CHECK);
        formatPlotButton(PlotItems.RATE, ratePlotChk, plotComp);

        createPlotColorLabel(rateColor, plotComp);

        Label rateItemLbl = new Label(plotComp, SWT.NONE);
        createItemLabel(PlotItems.RATE.getItemName(), rateItemLbl);

        /*
         * QPE
         */
        qpeUlRdo = new Button(plotComp, SWT.RADIO);
        formatUnderlayButton(Underlays.QPE, qpeUlRdo, plotComp);

        qpePlotChk = new Button(plotComp, SWT.CHECK);
        formatPlotButton(PlotItems.QPE, qpePlotChk, plotComp);

        createPlotColorLabel(qpeColor, plotComp);

        Label qpeItemLbl = new Label(plotComp, SWT.NONE);
        createItemLabel(PlotItems.QPE.getItemName(), qpeItemLbl);

        /*
         * QPF
         */
        // Filler
        new Label(plotComp, SWT.NONE);
        qpfPlotChk = new Button(plotComp, SWT.CHECK);

        formatPlotButton(PlotItems.QPF, qpfPlotChk, plotComp);
        createPlotColorLabel(qpfColor, plotComp);

        Label qpfItemLbl = new Label(plotComp, SWT.NONE);
        createItemLabel(PlotItems.QPF.getItemName(), qpfItemLbl);

        gd = new GridData();
        gd.horizontalSpan = 4;
        gd.horizontalIndent = 100;
        Composite qpfComp = new Composite(plotComp, SWT.NONE);
        qpfComp.setLayout(new GridLayout(1, false));
        qpfComp.setLayoutData(gd);
        qpfComp.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        // Set up the QPF Radio options
        FFMPMonitor monitor = FFMPMonitor.getInstance();
        ProductXML prodXml = monitor.getProductXML(resource.getPrimarySource());
        ProductRunXML prodRun = monitor.getRunConfig()
                .getRunner(resource.getResourceData().wfo)
                .getProduct(resource.getSiteKey());

        // get the selected QPF column type
        FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig.getInstance()
                .getTableConfigData(resource.getSiteKey());
        // FfmpTableConfig tableConf = FfmpTableConfig.getInstance();
        String columnName = ffmpTableCfgData.getTableColumnAttr(
                ffmpTableCfgData.getTableColumnKeys()[3])
                .getColumnNameWithSpace();
        String qpfType = columnName.substring(0, columnName.indexOf(" "));

        int i = 0;
        for (String name : prodRun.getQpfTypes(prodXml)) {
            Button qpfBtn = new Button(qpfComp, SWT.RADIO);
            qpfBtn.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
            qpfBtn.setText(name);
            qpfBtn.setData(name);
            if (name.equals(qpfType)) {
                qpfBtn.setSelection(true);
            }
            qpfRdos.add(qpfBtn);
            qpfBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {

                    Button qpfButton = (Button) e.getSource();

                    String currQpfType = FfmpTableConfig.getInstance()
                            .getTableConfigData(resource.getSiteKey())
                            .getQpfGraphType();

                    if (!((String) qpfButton.getData()).equals(currQpfType)) {
                        FfmpTableConfig.getInstance()
                                .getTableConfigData(resource.getSiteKey())
                                .setQpfGraphType((String) qpfButton.getData());

                        fireSourceUpdateEvent();
                    }
                }
            });

            i++;
        }

        /*
         * Guidance
         */
        // Filler
        new Label(plotComp, SWT.NONE);
        guidPlotChk = new Button(plotComp, SWT.CHECK);
        formatPlotButton(PlotItems.GUID, guidPlotChk, plotComp);
        createPlotColorLabel(guidColor, plotComp);
        Label guidItemLbl = new Label(plotComp, SWT.NONE);
        createItemLabel(PlotItems.GUID.getItemName(), guidItemLbl);

        gd = new GridData();
        gd.horizontalSpan = 4;
        gd.horizontalIndent = 100;
        Composite ffgComp = new Composite(plotComp, SWT.NONE);
        ffgComp.setLayout(new GridLayout(1, false));
        ffgComp.setLayoutData(gd);
        ffgComp.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        String ffgType = resource.getFFGName();

        int j = 0;
        for (String ffgname : prodXml.getAvailableGuidanceTypes()) {
            Button ffgBtn = new Button(ffgComp, SWT.RADIO);
            ffgBtn.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
            ffgBtn.setText(ffgname);
            ffgBtn.setData(new Integer(j));
            ffgRdos.add(ffgBtn);
            if (ffgType.equals(ffgname)) {
                ffgBtn.setSelection(true);
            }
            ffgBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Button ffgButton = (Button) e.getSource();
                    if (ffgButton.getSelection() == true) {
                        handleFFGSelection(ffgButton.getText());
                    }
                }
            });

            j++;
        }

        /*
         * Ratio
         */
        ratioUlRdo = new Button(plotComp, SWT.RADIO);
        formatUnderlayButton(Underlays.RATIO, ratioUlRdo, plotComp);

        // Filler
        new Label(plotComp, SWT.NONE);
        new Label(plotComp, SWT.NONE);

        ratioItemLbl = new Label(plotComp, SWT.NONE);
        createItemLabel(Underlays.RATIO.getUnderlayName(), ratioItemLbl);

        /*
         * Diff
         */
        diffUlRdo = new Button(plotComp, SWT.RADIO);
        formatUnderlayButton(Underlays.DIFF, diffUlRdo, plotComp);

        // Filler
        new Label(plotComp, SWT.NONE);
        new Label(plotComp, SWT.NONE);

        diffItemLbl = new Label(plotComp, SWT.NONE);
        createItemLabel(Underlays.DIFF.getUnderlayName(), diffItemLbl);

        /*
         * VGB
         */
        if (vgb == true) {
            // Filler
            new Label(plotComp, SWT.NONE);
            vgbPlotChk = new Button(plotComp, SWT.CHECK);

            formatPlotButton(PlotItems.VGB, vgbPlotChk, plotComp);
            createPlotColorLabel(vgbColor, plotComp);

            Label vgbItemLbl = new Label(plotComp, SWT.NONE);
            createItemLabel(PlotItems.VGB.getItemName(), vgbItemLbl);
        }
    }

    /**
     * Create the color legend.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createColorLegend(Composite parentComp) {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();

        Composite colorComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 5;
        gl.marginHeight = 10;
        gl.verticalSpacing = 0;
        colorComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.verticalIndent = 20;
        colorComp.setLayoutData(gd);
        colorComp.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 210;
        colorForLbl = new Label(colorComp, SWT.CENTER);
        colorForLbl.setFont(smallLabelFont);
        colorForLbl.setText(colorForStr + "XX (XX)");
        colorForLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        colorForLbl.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        colorForLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 190;
        gd.verticalIndent = 10;
        upperColorLbl = new Label(colorComp, SWT.CENTER);
        upperColorLbl.setFont(smallLabelFont);
        upperColorLbl.setText(">= XX");
        upperColorLbl.setBackground(ffmpCfg.getCellColor(TableCellColor.Upper));
        upperColorLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 190;
        midColorLbl = new Label(colorComp, SWT.CENTER);
        midColorLbl.setFont(smallLabelFont);
        midColorLbl.setText("XX >, >= XX");
        midColorLbl.setBackground(ffmpCfg.getCellColor(TableCellColor.Mid));
        midColorLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 190;
        lowerColorLbl = new Label(colorComp, SWT.CENTER);
        lowerColorLbl.setFont(smallLabelFont);
        lowerColorLbl.setText("XX >");
        lowerColorLbl.setBackground(ffmpCfg.getCellColor(TableCellColor.Lower));
        lowerColorLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 190;
        missingColorLbl = new Label(colorComp, SWT.CENTER);
        missingColorLbl.setFont(smallLabelFont);
        missingColorLbl.setText("Missing");
        missingColorLbl.setBackground(ffmpCfg
                .getCellColor(TableCellColor.BelowLower));
        missingColorLbl.setLayoutData(gd);
    }

    /**
     * Create the buttons on the right side of the graph.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createRightSideButtons(Composite parentComp) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 120;
        gd.verticalIndent = 15;
        reverseXAxisBtn = new Button(parentComp, SWT.PUSH);
        reverseXAxisBtn.setText("Reverse X-axis");
        reverseXAxisBtn.setLayoutData(gd);
        reverseXAxisBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                reverseXAxis = !reverseXAxis;
                basinTrendGraph.reverseXAxis();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.BOTTOM, true, true);
        gd.widthHint = 80;
        Button closeBtn = new Button(parentComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        sourceListeners.clear();
        largeLabelFont.dispose();
        medLabelFont.dispose();
        smallLabelFont.dispose();
        bgColor.dispose();
        rateColor.dispose();
        qpeColor.dispose();
        qpfColor.dispose();
        guidColor.dispose();
        vgbColor.dispose();
    }

    /**
     * Update the hour label.
     * 
     * @param isAllHours
     *            Flag indicating if All Hours is set.
     */
    private void updateHourLabel(boolean isAllHours) {
        if (isAllHours == true) {
            dataTimeLbl.setText("Hours before " + dateStr);
        } else {
            dataTimeLbl.setText(dateStr);
        }
    }

    /**
     * Format the under-lay button.
     * 
     * @param underlayItem
     *            Under-lay item.
     * @param ulBtn
     *            Under-lay button.
     * @param parentComp
     *            Parent composite.
     */
    private void formatUnderlayButton(Underlays underlayItem, Button ulBtn,
            Composite parentComp) {
        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = 18;
        gd.horizontalIndent = 8;
        ulBtn.setData(underlayItem);
        ulBtn.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        ulBtn.setLayoutData(gd);
        ulBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Button btn = ((Button) e.widget);
                if (btn.getSelection() == true) {
                    updateColorLegend();
                    updateGraph();
                }
            }
        });

        underlayButtons.add(ulBtn);
    }

    /**
     * Format the plot button.
     * 
     * @param plotItem
     *            Plot item.
     * @param plotBtn
     *            Plot button.
     * @param parentComp
     *            Parent composite.
     */
    private void formatPlotButton(PlotItems plotItem, Button plotBtn,
            Composite parentComp) {
        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = 20;
        gd.horizontalIndent = 3;
        plotBtn.setData(plotItem);
        plotBtn.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        plotBtn.setLayoutData(gd);
        plotBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Button plotBtn = (Button) e.getSource();
                updatePlotSelection(plotBtn, true);
            }
        });

        plotButtons.add(plotBtn);
    }

    /**
     * Create the plot color label.
     * 
     * @param c
     *            Color.
     * @param parentComp
     *            Parent composite.
     */
    private void createPlotColorLabel(Color c, Composite parentComp) {
        GridData gd = new GridData(15, 5);
        Label colorLbl = new Label(parentComp, SWT.BORDER);
        colorLbl.setLayoutData(gd);
        colorLbl.setBackground(c);
    }

    /**
     * Create an item label.
     * 
     * @param name
     *            Name displayed in the label.
     * @param itemLbl
     *            Item label.
     */
    private void createItemLabel(String name, Label itemLbl) {
        GridData gd = new GridData();
        gd.horizontalIndent = 10;
        itemLbl.setText(name);
        itemLbl.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        itemLbl.setFont(medLabelFont);
        itemLbl.setLayoutData(gd);
    }

    /**
     * Setup the plot controls.
     */
    private void setupUnderlayPlotControls() {
        FFMPConfig ffmpCfg = FFMPConfig.getInstance();

        String selectedUnderlay = ffmpCfg.getUnderlay();
        String guidSrc = null;
        if (selectedUnderlay.contains(",")) {
            String[] parts = selectedUnderlay.split(",");
            selectedUnderlay = parts[0];
            guidSrc = parts[1];
        }

        /*
         * Loop over the underlay buttons and select the one specified in the
         * configuration
         */
        for (Button ulBtn : underlayButtons) {
            Underlays ul = (Underlays) ulBtn.getData();

            if (ul.getUnderlayName().compareTo(selectedUnderlay) == 0) {
                ulBtn.setSelection(true);
            }
        }

        /*
         * Loop over the plot buttons and select the ones specified in the
         * configuration
         */
        String[] selectedPlots = ffmpCfg.getActiveBasinTrendPlots();

        for (String plotName : selectedPlots) {
            for (Button plotBtn : plotButtons) {
                PlotItems pi = (PlotItems) plotBtn.getData();
                if (pi.getItemName().compareTo(plotName) == 0) {
                    plotBtn.setSelection(true);
                }

                updatePlotSelection(plotBtn, false);
            }
        }

        if (guidSrc != null) {
            for (Button ffg : ffgRdos) {
                if (ffg.getText().equalsIgnoreCase(guidSrc)) {
                    ffg.setSelection(true);
                } else {
                    ffg.setSelection(false);
                }
            }
        }
        updateColorLegend();
        updateGraph();
    }

    /**
     * Set the background color of the specified control.
     * 
     * @param control
     *            Control.
     */
    private void setBackgroundColor(Control control) {
        control.setBackground(bgColor);
    }

    /**
     * Update the plot selection.
     * 
     * @param plotBtn
     *            Plot button.
     * @param updateLegendAndGraph
     *            Flag indicating the legend and graph should be updated.
     */
    private void updatePlotSelection(Button plotBtn,
            boolean updateLegendAndGraph) {
        PlotItems plotItem = (PlotItems) plotBtn.getData();

        if (plotItem == PlotItems.RATE) {
            rateUlRdo.setEnabled(plotBtn.getSelection());
        } else if (plotItem == PlotItems.QPE) {
            qpeUlRdo.setEnabled(plotBtn.getSelection());

            if (plotBtn.getSelection() == false) {
                ratioUlRdo.setEnabled(plotBtn.getSelection());
                diffUlRdo.setEnabled(plotBtn.getSelection());
                ratioItemLbl.setEnabled(plotBtn.getSelection());
                diffItemLbl.setEnabled(plotBtn.getSelection());
            } else {
                ratioUlRdo.setEnabled(guidPlotChk.getSelection());
                diffUlRdo.setEnabled(guidPlotChk.getSelection());
                ratioItemLbl.setEnabled(guidPlotChk.getSelection());
                diffItemLbl.setEnabled(guidPlotChk.getSelection());
            }
        } else if (plotItem == PlotItems.QPF) {
            for (Button qpfButton : qpfRdos) {
                qpfButton.setEnabled(plotBtn.getSelection());
            }
            formatTimeFrameLabel();
        } else if (plotItem == PlotItems.GUID) {
            qpfPlotChk.setEnabled(plotBtn.getSelection());
            if (qpfPlotChk.isEnabled() == true) {
                for (Button qpfButton : qpfRdos) {
                    qpfButton.setEnabled(qpfPlotChk.getSelection());
                }
            } else {
                for (Button qpfButton : qpfRdos) {
                    qpfButton.setEnabled(qpfPlotChk.isEnabled());
                }
            }

            for (Button ffgButton : ffgRdos) {
                ffgButton.setEnabled(plotBtn.getSelection());
            }

            if (plotBtn.getSelection() == false) {
                ratioUlRdo.setEnabled(plotBtn.getSelection());
                diffUlRdo.setEnabled(plotBtn.getSelection());
                ratioItemLbl.setEnabled(plotBtn.getSelection());
                diffItemLbl.setEnabled(plotBtn.getSelection());
            } else {
                ratioUlRdo.setEnabled(qpePlotChk.getSelection());
                diffUlRdo.setEnabled(qpePlotChk.getSelection());
                ratioItemLbl.setEnabled(qpePlotChk.getSelection());
                diffItemLbl.setEnabled(qpePlotChk.getSelection());
            }
        }

        if (updateLegendAndGraph == true) {
            updateColorLegend();
            updateGraph();
        }
    }

    /**
     * Update the color legend.
     */
    private void updateColorLegend() {
        ThresholdManager threshMgr = null;
        Underlays ul = null;

        for (Button ulBtn : underlayButtons) {
            if ((ulBtn.getSelection() == true) && (ulBtn.isEnabled() == true)) {
                ul = (Underlays) ulBtn.getData();
                threshMgr = FFMPConfig.getInstance().getThresholdManager(
                        ul.name());
            }
        }

        if (threshMgr == null) {
            colorForLbl.setText(colorForStr + "XX (XX)");
            upperColorLbl.setText(">= XX");
            midColorLbl.setText("XX >, >= XX");
            lowerColorLbl.setText("XX >");
        } else {
            String upperStr = String.format("%1.2f", threshMgr.getUpperValue());
            String midStr = String.format("%1.2f", threshMgr.getMidValue());

            StringBuilder sb = new StringBuilder(colorForStr);
            sb.append(ul.getUnderlayName());
            sb.append("(").append(threshMgr.getDisplayUnits()).append(")");
            colorForLbl.setText(sb.toString());

            upperColorLbl.setText(">= " + upperStr);
            midColorLbl.setText(upperStr + " > , >= " + midStr);
            lowerColorLbl.setText(midStr + " >");
        }
    }

    /**
     * Update the graph.
     */
    private void updateGraph() {
        setBasinTrendGraphParms();
    }

    /**
     * Format the time labels.
     */
    private void formatTimeFrameLabel() {
        int hours = 0;
        TimeDuration timeDur = TimeDuration.ALL;

        /*
         * Get the time duration from the selected time duration radio button.
         */
        for (Button timeDurBtn : timeDurationButtons) {
            if (timeDurBtn.getSelection() == true) {
                timeDur = (TimeDuration) timeDurBtn.getData();
                hours = timeDur.getHours();
            }
        }

        if (timeDur == TimeDuration.ALL) {
            timeFrameLbl.setText("");
        } else {
            String tmpStr;
            if (qpfPlotChk.getSelection() == true) {
                tmpStr = String.format("(%d hrs QPE : 1 hrs QPF)", (hours - 1));
            } else {
                tmpStr = String.format("(%d hrs QPE)", hours);
            }
            timeFrameLbl.setText(tmpStr);
        }
    }

    /**
     * Update the aggregate name.
     */
    private void updateAggregateName() {
        StringBuilder sb = new StringBuilder();

        sb.append(graphData.getState()).append(",")
                .append(graphData.getCounty());
        sb.append(" : ").append(graphData.getStreamName());

        aggregateNameLbl.setText(sb.toString());
    }

    /**
     * Handle the time duration action.
     * 
     * @param td
     *            Time duration.
     */
    private void handleTimeDurAction(TimeDuration td) {
        formatTimeFrameLabel();
        updateHourLabel(allHoursIsSelected());

        if (td == TimeDuration.ALL) {
            reverseXAxisBtn.setEnabled(true);
        } else {
            reverseXAxisBtn.setEnabled(false);
        }

        basinTrendGraph.resetReverseXAxis();
        basinTrendGraph.setTimeDurHours(td);
    }

    /**
     * Set the basin trend graph parameters from the configuration.
     */
    private void setBasinTrendGraphParms() {
        FFMPConfigBasinXML configBasin = FFMPConfig.getInstance()
                .getFFMPConfigData();
        /*
         * Time duration
         */
        TimeDuration timeDur = TimeDuration.ALL;

        for (Button timeDurBtn : timeDurationButtons) {
            if (timeDurBtn.getSelection() == true) {
                timeDur = (TimeDuration) timeDurBtn.getData();
            }
        }

        /*
         * Underlay
         */
        Underlays underlay = Underlays.RATE;
        for (Button underlayBtn : underlayButtons) {
            if (underlayBtn.getSelection() == true) {
                underlay = (Underlays) underlayBtn.getData();
            }
        }
        if ((underlay == Underlays.DIFF) || (underlay == Underlays.RATIO)) {
            String guidSource = null;
            for (Button ffg : ffgRdos) {
                if (ffg.getSelection()) {
                    guidSource = ffg.getText();
                }
            }
            if ((guidSource == null) && (ffgRdos.size() > 0)) {
                guidSource = ffgRdos.get(0).getText();
            }
            if (guidSource != null) {
                configBasin.setUnderlay(underlay.getUnderlayName() + ","
                        + guidSource);
            } else {
                configBasin.setUnderlay(underlay.getUnderlayName());
            }
        } else {
            configBasin.setUnderlay(underlay.getUnderlayName());
        }

        /*
         * Plot Items
         */
        ArrayList<PlotItems> plots = new ArrayList<PlotItems>();
        for (Button plotBtn : plotButtons) {
            if (plotBtn.getSelection() == true) {
                plots.add((PlotItems) plotBtn.getData());
            }
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < plots.size(); i++) {
            sb.append(plots.get(i).getItemName());

            if (i != plots.size() - 1) {
                sb.append(",");
            }
        }
        configBasin.setBasinTrendPlots(sb.toString());

        /*
         * Set the basin graph parameters.
         */
        basinTrendGraph.setGraphingParameters(timeDur, underlay, plots);
    }

    private boolean allHoursIsSelected() {
        boolean allHoursSelected = false;

        TimeDuration td;

        for (Button btn : timeDurationButtons) {
            td = (TimeDuration) btn.getData();

            if (td == TimeDuration.ALL) {
                if (btn.getSelection() == true) {
                    allHoursSelected = true;
                }
                break;
            }
        }

        return allHoursSelected;
    }

    /**
     * Set the graph data.
     * 
     * @param graphData
     *            Graph data.
     */
    public void setGraphData(FFMPGraphData graphData, Date date, String pfaf) {
        this.graphData = graphData;
        this.pfaf = pfaf;
        this.currentDate = date;

        this.graphData.setDisplayDate(date);

        System.out.println("this.graphData.getDate() = "
                + this.graphData.getDate().toString());
        System.out.println("        this.currentDate = "
                + this.currentDate.toString());

        formatDateTimeString();
        updateHourLabel(allHoursIsSelected());

        updateAggregateName();

        basinTrendGraph.setGraphData(graphData);
        shell.setCursor(null);
    }

    private void handleFFGSelection(String ffgType) {

        FfmpTableConfig.getInstance().getTableConfigData(resource.getSiteKey())
                .setFfgGraphType(ffgType);

        fireSourceUpdateEvent();
    }

    /**
     * Update since the threshold has changed.
     */
    public void thresholdChanged() {
        updateColorLegend();
        basinTrendGraph.redrawGraphCanvas();
    }

    /**
     * Get the current shell.
     * 
     * @return The current shell.
     */
    public Shell getCurrentShell() {
        return shell;
    }

    public String getPfaf() {
        return pfaf;
    }

    /**
     * Add listener
     * 
     * @param su
     */
    public synchronized void addListener(ISourceUpdate su) {
        sourceListeners.add(su);
    }

    /**
     * Remove listener
     * 
     * @param su
     */
    public synchronized void removeListener(ISourceUpdate su) {
        sourceListeners.remove(su);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.ISourceUpdate#
     * fireSourceUpdateEvent()
     */
    public void fireSourceUpdateEvent() {
        for (ISourceUpdate update : sourceListeners) {
            update.fireSourceUpdateEvent();
            shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        }
    }
}
