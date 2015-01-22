/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.palette;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;
import gov.noaa.nws.ncep.viz.rtkp.controls.EditNetworkDialog;
import gov.noaa.nws.ncep.viz.rtkp.rsc.GeoMagWorldActivityResource;
import gov.noaa.nws.ncep.viz.rtkp.rsc.GeoMagWorldActivityTitleDateResource;
import gov.noaa.nws.ncep.viz.rtkp.util.GeoMagTimeSeriesDataException;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil.GeoMagStationType;
import gov.noaa.nws.ncep.viz.ui.display.NatlCntrsEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.awt.Color;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.UiUtil;

/**
 * 
 * This java class performs the RTKp Data Block GUI construction.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- -----------------------------------
 * April 4, 2014 1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagRTKpDataBlockWindow extends ViewPart implements
        IGeoMagRTKpDataBlockListener, SelectionListener, DisposeListener,
        IPartListener {

    private static IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeoMagRTKpDataBlockWindow.class);

    private IWorkbenchPage page;

    private Shell shell;

    private boolean isState = false;

    private Group textGp;

    private Text text;

    private StringBuffer textStr = new StringBuffer("");

    private Button editNwBtn;

    private Date startTime = null;

    private boolean isEditorVisible = true;

    private int btnGapX = 5;

    private int staBtnWidth = 150;

    private int btnHeight = 20;

    private int pushbtnHeight = 20;

    private int labelGap = 15;

    private boolean showKpOnly = true;

    private String kType = "K";

    static final SimpleDateFormat dateFormat = new SimpleDateFormat(
            "yyyy-MMM-dd HH:mm:ss");

    static final SimpleDateFormat timeFormat = new SimpleDateFormat("HH:mm");

    private List<String> kp_last10_text = new ArrayList<String>();

    private List<Integer> kp_last10_values = new ArrayList<Integer>();

    private List<RGB> kp_last10_color = new ArrayList<RGB>();

    private RGB[] kindicesColors = { new RGB(0, 128, 0), new RGB(0, 128, 0),
            new RGB(0, 128, 0), new RGB(100, 228, 100), new RGB(9, 135, 205),
            new RGB(142, 53, 239), new RGB(205, 0, 205), new RGB(255, 255, 0),
            new RGB(215, 125, 0), new RGB(255, 0, 0) };

    private Color recentKpEstTitleColor = Color.decode("0xFFFFFF");

    protected Timer timer;

    protected TimerTask timerTask;

    protected boolean loadDone = false;

    protected Date curSynPerEndTime = null;

    // create this singleton object
    private static GeoMagRTKpDataBlockWindow instance = null;

    /**
     * Constructor
     * 
     */
    public GeoMagRTKpDataBlockWindow() {

        super();

        shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

        if (instance == null) {
            instance = this;
        } else {
            GeoMagRTKpDataBlockWindow tmp = instance;
            instance = this;
            instance.setState(tmp.isState());
        }
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    public static GeoMagRTKpDataBlockWindow getAccess() {
        return instance;
    }

    /**
     * Invoked by the workbench to initialize this View.
     */
    public void init(IViewSite site) {
        try {
            super.init(site);
        } catch (PartInitException pie) {
            pie.printStackTrace();
        }

        page = site.getPage();
        page.addPartListener(this);

        constructDataTimer();
    }

    protected void constructDataTimer() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {

                timer = new Timer("rtkpDataBlockDataRetrieve");
                timerTask = new TimerTask() {
                    @Override
                    public void run() {
                        setDataBlockContent();
                        loadDone = true;

                        fireUpdateEvent();
                    }
                };
                // update data every 10 seconds
                timer.schedule(timerTask, 0, 10000);
            }
        });
    }

    /**
     * Disposes resource. invoked by the workbench
     */
    public void dispose() {
        if (!isEditorVisible) {
            instance = null;
            return;
        } else {
            GeoMagWorldActivityResource geomagMapResource = GeoMagWorldActivityResource
                    .getGeoMagWorldActivityResource();
            geomagMapResource.dispose();

            super.dispose();
            isEditorVisible = false;

            NatlCntrsEditor editor = GeoMagWorldActivityResource.getMapEditor();

            if (editor != null) {
                for (IRenderableDisplay display : UiUtil
                        .getDisplaysFromContainer(editor)) {
                    for (ResourcePair rp : display.getDescriptor()
                            .getResourceList()) {
                        if (rp.getResource() instanceof GeoMagWorldActivityResource) {
                            GeoMagWorldActivityResource rsc = (GeoMagWorldActivityResource) rp
                                    .getResource();
                            rsc.unload();
                        }
                        if (rp.getResource() instanceof GeoMagWorldActivityTitleDateResource) {
                            GeoMagWorldActivityTitleDateResource rsc = (GeoMagWorldActivityTitleDateResource) rp
                                    .getResource();
                            rsc.unload();
                        }
                    }
                }
            }

            /*
             * remove the workbench part listener
             */
            page.removePartListener(this);

            if (timerTask != null) {
                timerTask.cancel();
            }

            if (timer != null) {
                timer.cancel();
            }

            instance = null;
        }
    }

    private void close() {
        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        if (wpage != null) {
            IViewPart vpart = wpage.findView(RTKpUtil.DATABLOCK_VIEW_ID);
            wpage.hideView(vpart);
        }
        NcDisplayMngr.setPanningMode();
    }

    /**
     * Invoked by the workbench, this method sets up the SWT controls for the
     * rtkp datablock palette
     */
    @Override
    public void createPartControl(Composite parent) {

        parent.setLayout(new GridLayout(1, false));
        // create textGp group. It contains text and textMode group
        textGp = new Group(parent, SWT.SHADOW_OUT);
        textGp.setLayout(new GridLayout());
        if ("Ks".equals(kType)) {
            textGp.setText("Recent Station Ks/Gamma Values");
        } else {
            textGp.setText("Recent Station K/Gamma Values");
        }
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        textGp.setLayoutData(data);

        createTextArea(textGp);
        createOptionsGp(parent);
    }

    public void createTextArea(Composite parent) {

        // Text display area
        text = new Text(parent, SWT.V_SCROLL | SWT.H_SCROLL);

        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        text.setLayoutData(data);
        Font font = text.getFont();
        FontData[] fontData = font.getFontData();
        for (int i = 0; i < fontData.length; i++) {
            // fontData[i].setHeight(9);
            fontData[i].setName("courier");
            // fontData[i].setStyle(1);
            fontData[i].setStyle(SWT.BOLD);
        }
        Font newFont = new Font(font.getDevice(), fontData);
        text.setFont(newFont);
        text.setEditable(false);
    }

    public void createOptionsGp(Composite parent) {

        Group optionsGrp = new Group(parent, SWT.SHADOW_OUT);
        optionsGrp.setLayout(new GridLayout(3, false));

        Group kKsGp = new Group(optionsGrp, SWT.SHADOW_OUT);

        Button showStationKBtn = new Button(kKsGp, SWT.RADIO);
        showStationKBtn.setText("Show Station K");
        showStationKBtn.setEnabled(true);
        showStationKBtn.setBounds(kKsGp.getBounds().x + btnGapX,
                kKsGp.getBounds().y - 5 + labelGap, staBtnWidth, btnHeight);
        showStationKBtn.setSelection(true);

        showStationKBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                setkType("K");
                setDataBlockContent();
                displayDataBlock();
            }
        });
        Button showStationKsBtn = new Button(kKsGp, SWT.RADIO);
        showStationKsBtn.setText("Show Station Ks");
        showStationKsBtn.setEnabled(true);
        showStationKsBtn.setBounds(showStationKBtn.getBounds().x
                + showStationKBtn.getBounds().width + btnGapX,
                kKsGp.getBounds().y - 7 + labelGap, staBtnWidth, btnHeight);

        showStationKsBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                setkType("Ks");
                setDataBlockContent();
                displayDataBlock();
            }
        });

        Group kpKGp = new Group(optionsGrp, SWT.SHADOW_OUT);

        Button showKpStationsBtn = new Button(kpKGp, SWT.RADIO);
        showKpStationsBtn.setText("Kp Stations Only");
        showKpStationsBtn.setEnabled(true);
        showKpStationsBtn.setBounds(kpKGp.getBounds().x + btnGapX,
                kpKGp.getBounds().y - 5 + labelGap, staBtnWidth, btnHeight);
        showKpStationsBtn.setSelection(true);

        showKpStationsBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                showKpOnly = true;
                setDataBlockContent();
                displayDataBlock();
            }
        });
        Button showAllKStationsBtn = new Button(kpKGp, SWT.RADIO);
        showAllKStationsBtn.setText("Show All K Stations");
        showAllKStationsBtn.setEnabled(true);
        showAllKStationsBtn
                .setBounds(
                        showKpStationsBtn.getBounds().x
                                + showKpStationsBtn.getBounds().width + btnGapX,
                        kpKGp.getBounds().y - 7 + labelGap, staBtnWidth + 25,
                        btnHeight);

        showAllKStationsBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                showKpOnly = false;
                setDataBlockContent();
                displayDataBlock();
            }
        });

        // Push buttons for Previous text info
        editNwBtn = new Button(optionsGrp, SWT.PUSH);
        editNwBtn.setText("Edit Network");
        editNwBtn.setEnabled(true);
        editNwBtn.setBounds(showAllKStationsBtn.getBounds().x
                + showAllKStationsBtn.getBounds().width + btnGapX,
                kpKGp.getBounds().y + labelGap, staBtnWidth, pushbtnHeight);

        editNwBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                openEditNetworkDialog();
            }
        });
    }

    public void setDataBlockContent() {

        endTime = RTKpUtil.calcCurTime();
        startTime = RTKpUtil.calcDataBlockStartTime(endTime);

        String filler = "   ";
        textStr = new StringBuffer("");
        kp_last10_text = new ArrayList<String>();
        kp_last10_values = new ArrayList<Integer>();

        try {

            List<String> kStations = RTKpUtil
                    .getGeoMagStationCodes(GeoMagStationType.KP);
            if (!showKpOnly) {
                kStations = RTKpUtil.getGeoMagStationCodes(GeoMagStationType.K);
            }
            int kStationsSize = (kStations != null) ? kStations.size() : 0;

            HashMap<String, Map<String, Object>> curKpStationsStates = RTKpUtil
                    .getStationsStatesMap(GeoMagStationType.KP, 0);

            // get list of kp stations based on state for current synoptic
            // period

            Date dbStartTime = RTKpUtil.calcDataBlockStartTime(endTime);

            List<Map<String, Object>> kpDataList = RTKpUtil.getEstKpIndex1min(
                    dbStartTime, endTime);

            List<GeoMagK1min> kDataList = RTKpUtil.getEstKIndex1min(kStations,
                    dbStartTime, endTime);

            float maxGamma = getMaxGamma(kDataList);
            double gammaSize = Math.floor(Math.log10(maxGamma)) + 1;

            String gammaFiller = "";
            String gammaFillerHeader = "";

            for (int i = 0; i < (int) gammaSize; i++) {
                gammaFiller += ".";
                gammaFillerHeader += " ";
            }

            // display header with list of station abbreviations
            // (stations not being used in kp average get [ and ] around the
            // names)
            textStr.append("  Time " + filler);
            for (int i = 0; i < kStationsSize; i++) {
                if (curKpStationsStates.containsKey(kStations.get(i))) {
                    Map<String, Object> stnMap = curKpStationsStates
                            .get(kStations.get(i));

                    if (stnMap != null) {
                        // Apply changes beginning with current synoptic
                        // period
                        boolean curStateActive = ((Integer) stnMap
                                .get("kp_active") == 1) ? true : false;
                        if (curStateActive) {
                            textStr.append(" " + kStations.get(i) + "*"
                                    + gammaFillerHeader + filler);
                        } else {
                            textStr.append("  " + kStations.get(i)
                                    + gammaFillerHeader + filler);
                        }
                    }
                } else {
                    textStr.append("  " + kStations.get(i) + gammaFillerHeader
                            + filler);
                }
            }
            textStr.append("  Kp" + filler + "<Ks>" + filler + "N\n");

            // minute lines with data
            int min = 0;
            do {

                // increment minute by 1
                Calendar startTimeCal = Calendar.getInstance(TimeZone
                        .getTimeZone("GMT"));
                startTimeCal.setTime(dbStartTime);
                startTimeCal.add(Calendar.MINUTE, 1);
                dbStartTime = startTimeCal.getTime();

                // add time
                String time = timeFormat.format(startTimeCal.getTime());
                textStr.append("  " + String.format("%-5s", time));

                textStr.append(filler);
                String kStr = " ";

                for (int i = 0; i < kStationsSize; i++) {
                    GeoMagK1min rec = getMatch1minRecord(kDataList,
                            dbStartTime, kStations.get(i));
                    if (rec == null) {
                        kStr += ".... " + gammaFiller + filler;
                        continue;
                    }

                    String k_disp = String.format("%.2f", rec.getKestReal());
                    if ("Ks".equals(kType)) {
                        k_disp = String.format("%.2f", rec.getKs());
                    }
                    String gamma = "" + (int) rec.getKestGamma();

                    if (gamma.length() < gammaSize) {
                        for (int j = gamma.length(); j < gammaSize; j++) {
                            gamma += " ";
                        }
                    }

                    if (("99999.00".equals(k_disp) || "33333.00".equals(k_disp))
                            && "99999".equals(gamma)) {
                        kStr += "" + ".... " + gammaFiller + filler;
                    } else {
                        kStr += "" + k_disp + "/" + gamma + filler;
                    }
                }

                // display kp data
                String kpStr = " ";

                Integer kpValue = 0;
                Map<String, Object> ksMap = getMatchKsRecord(kpDataList,
                        dbStartTime);
                if (ksMap != null) {
                    Date refTime = (Date) ksMap.get("reftime");
                    Double Kp_est = (Double) ksMap.get("Kp_est");
                    Double ks_avg = (Double) ksMap.get("ks_avg");
                    BigInteger station_count = (BigInteger) ksMap
                            .get("station_count");

                    kpValue = (int) Math.round(Kp_est);

                    double curKp = (double) (Math.round(3 * Kp_est));
                    kpStr += RTKpUtil.KpLu[(int) curKp] + filler;

                    kpStr += String.format("%.2f", ks_avg) + filler;
                    kpStr += station_count.toString();

                    double lbdry = 0d;
                    double ubdry = 0d;

                    recentKpEstTitleColor = Color.decode("0xFFFFFF");

                    if (Kp_est * 3d >= 11d) { // greater than or equal to 4M
                        lbdry = Math.round(Kp_est) - 0.5; // lower boundary
                        ubdry = lbdry + 1d / 6d;
                        // when in the 1/2 to 2/3 below, shade as dark
                        // orange
                        // (otherwise red)
                        if (lbdry <= ks_avg && ks_avg < ubdry) {
                            recentKpEstTitleColor = Color.decode("0xE56717"); // dark
                                                                              // orange
                        } else {
                            recentKpEstTitleColor = Color.decode("0xFF0000"); // red
                        }
                    }

                } else {
                    kpStr = " .." + filler + "(....)";
                }

                textStr.append(kStr + kpStr + "\n");

                if (!kpStr.equals(" .." + filler + "(....)") && min++ >= 10) {
                    kp_last10_text.add(String.format("%-5s", time) + "   "
                            + kpStr);
                    kp_last10_values.add(kpValue);
                }

            } while (dbStartTime.compareTo(endTime) != 0);

            setKp_last10_colors();

            loadDone = true;
        } catch (GeoMagTimeSeriesDataException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving ks_avg from geomag_k1min table.", e);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving ks_avg from geomag_k1min table.", e);
        }

    }

    public void displayDataBlock() {

        if (!loadDone) {
            setDataBlockContent();
        }

        text.setText(textStr.toString());
        RTKpUtil.showGeoMagWorldActivity(this);
        RTKpUtil.showRecentKpEstBlock(this);

    }

    private float getMaxGamma(List<GeoMagK1min> kDataList) {

        int kDataListSize = (kDataList != null) ? kDataList.size() : 0;

        float maxGamma = 0f;
        for (int i = 0; i < kDataListSize; i++) {
            GeoMagK1min k1minRec = kDataList.get(i);
            if (k1minRec.getKestGamma() > maxGamma
                    && k1minRec.getKestGamma() != 99999.99f) {
                maxGamma = k1minRec.getKestGamma();
            }
        }
        return maxGamma;
    }

    private GeoMagK1min getMatch1minRecord(List<GeoMagK1min> kDataList,
            Date refTime, String stationCode) {

        int kDataListSize = (kDataList != null) ? kDataList.size() : 0;

        for (int i = 0; i < kDataListSize; i++) {
            GeoMagK1min k1minRec = kDataList.get(i);
            if (k1minRec.match(refTime, stationCode)) {
                return k1minRec;
            }
        }

        return null;
    }

    public void openEditNetworkDialog() {

        EditNetworkDialog dialog;
        try {
            dialog = new EditNetworkDialog(shell, "Edit Network", this);
            dialog.open();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error while opening EditNetwork dialog.", e);
        }

    }

    private Map<String, Object> getMatchKsRecord(
            List<Map<String, Object>> kpDataList, Date refTime) {

        int kpDataListSize = (kpDataList != null) ? kpDataList.size() : 0;

        for (int i = 0; i < kpDataListSize; i++) {
            Map<String, Object> map = kpDataList.get(i);
            if (map != null && map.get("reftime") != null) {
                if (((Date) map.get("reftime")).compareTo(refTime) == 0) {
                    return map;
                }
            }
        }

        return null;
    }

    private Date getDBStartTime() {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(endTime);
        cal.add(Calendar.MINUTE, -20);
        return cal.getTime();
    }

    public Text getText() {
        return text;
    }

    public void setText(Text text) {
        this.text = text;
    }

    public StringBuffer getTextStr() {
        return textStr;
    }

    public void setTextStr(StringBuffer textStr) {
        this.textStr = textStr;
    }

    public void widgetDefaultSelected(SelectionEvent se) {

    }

    /*
     * invoked when widget is disposed
     * 
     * @see
     * org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt
     * .events.DisposeEvent)
     */
    public void widgetDisposed(DisposeEvent event) {

    }

    @Override
    public void partActivated(IWorkbenchPart part) {
    }

    @Override
    public void partBroughtToTop(IWorkbenchPart part) {
        partActivated(part);
    }

    @Override
    public void partClosed(IWorkbenchPart part) {
    }

    @Override
    public void partDeactivated(IWorkbenchPart part) {
    }

    @Override
    public void partOpened(IWorkbenchPart part) {
    }

    @Override
    public void setFocus() {
        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
                .setFocus();
    }

    @Override
    public void widgetSelected(SelectionEvent e) {
    }

    public boolean isState() {
        return isState;
    }

    public void setState(boolean isState) {
        this.isState = isState;
    }

    public void setEditorVisible(boolean isVisible) {
        this.isEditorVisible = isVisible;
    }

    public boolean getEditorVisible() {
        return this.isEditorVisible;
    }

    public Date getStartTime() {
        return startTime;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    private Date endTime = null;

    public Date getEndTime() {
        return endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

    public List<String> getKp_last10_text() {
        return kp_last10_text;
    }

    public void setKp_last10_text(List<String> kp_last10_text) {
        this.kp_last10_text = kp_last10_text;
    }

    public List<Integer> getKp_last10_values() {
        return kp_last10_values;
    }

    public void setKp_last10_values(List<Integer> kp_last10_values) {
        this.kp_last10_values = kp_last10_values;
    }

    public List<RGB> getKp_last10_color() {
        return kp_last10_color;
    }

    public void setKp_last10_color(List<RGB> kp_last10_color) {
        this.kp_last10_color = kp_last10_color;
    }

    public void setKp_last10_colors() {

        if (this.kindicesColors != null && kp_last10_values != null) {
            kp_last10_color = new ArrayList<RGB>();
            // set kp last10 text colors
            for (int i = 0; i < kp_last10_values.size(); i++) {
                Integer kp = kp_last10_values.get(i);
                kp_last10_color.add(kindicesColors[kp]);
            }
        }
    }

    public RGB[] getKindicesColors() {
        return kindicesColors;
    }

    public void setKindicesColors(RGB[] kindicesColors) {
        if (kindicesColors != null && kindicesColors.length == 10) {
            this.kindicesColors = kindicesColors;
        }
    }

    public Color getRecentKpEstTitleColor() {
        return recentKpEstTitleColor;
    }

    public void setRecentKpEstTitleColor(Color recentKpEstTitleColor) {
        this.recentKpEstTitleColor = recentKpEstTitleColor;
    }

    public boolean isShowKpOnly() {
        return showKpOnly;
    }

    public void setShowKpOnly(boolean showKpOnly) {
        this.showKpOnly = showKpOnly;
    }

    public String getkType() {
        return kType;
    }

    public void setkType(String kType) {
        this.kType = kType;

        if ("Ks".equals(kType)) {
            textGp.setText("Recent Station Ks/Gamma Values");
        } else {
            textGp.setText("Recent Station K/Gamma Values");
        }
    }

    @Override
    public void update() {
        displayDataBlock();
    }

    public void fireUpdateEvent() {
        final GeoMagRTKpDataBlockWindow gmDBWin = GeoMagRTKpDataBlockWindow
                .getAccess();

        VizApp.runAsync(new Runnable() {
            public void run() {
                if (gmDBWin != null) {
                    gmDBWin.update();
                }
            }
        });
    }
}
