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
package com.raytheon.viz.aviation.guidance;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.viz.core.jobs.IRequestCompleteListener;
import com.raytheon.viz.aviation.cachedata.CacheGuidanceRequest;
import com.raytheon.viz.aviation.cachedata.MosCacheGuidanceRequest;
import com.raytheon.viz.aviation.editor.HeaderTextToolTip;
import com.raytheon.viz.aviation.guidance.GuidanceRequest.GuidanceType;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;

/**
 * MosViewer class contains a basic setup for the Guidance tab on the TAF viewer
 * editor dialog. This setup is currently used by the GFS-MOS, GFSLAMP, ETA-MOS,
 * and NGM-MOS tabs.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 24 Jul 2009   2669       njensen    Renamed to MosViewer and hooked up guidance
 * 01 Dec 2010  3263       rferrel     Created generateToolTip to get table tool tips.
 * 28 APR 2011  8065       rferrel     Add flag to indicate display is current
 *                                     and implement data caching
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class MosViewer extends ViewerTab implements
        IRequestCompleteListener<String[]> {

    /**
     * Tool tip for values in the VIS/CVIS rows.
     */
    private static final Map<String, String> visDict = new HashMap<String, String>();
    static {
        visDict.put("1", "<1/2SM");
        visDict.put("2", "1/2-<1SM");
        visDict.put("3", "1-<2SM");
        visDict.put("4", "2-<3SM");
        visDict.put("5", "3-5SM");
        visDict.put("6", "6SM");
        visDict.put("7", ">6SM");
    }

    /**
     * Default value to use when no entry found in visDict.
     */
    private static final String visDefault = ">6SM";

    /**
     * Tool tip for values in the CIG/CCIG rows.
     */
    private static final Map<String, String> cigDict = new HashMap<String, String>();
    static {
        cigDict.put("1", "<200ft");
        cigDict.put("2", "200-400ft");
        cigDict.put("3", "500-900ft");
        cigDict.put("4", "1000-1900ft");
        cigDict.put("5", "2000-3000ft");
        cigDict.put("6", "3100-6500ft");
        cigDict.put("7", "6600-12000ft");
        cigDict.put("8", ">12000ft");
    }

    /**
     * Default value to use when no entry found in cigDict.
     */
    private static final String cigDefault = "Ulmtd";

    /**
     * Tool tip for values in the QPF06 row.
     */
    private static final Map<String, String> qpfDict = new HashMap<String, String>();
    static {
        qpfDict.put("1", "<0.1\"");
        qpfDict.put("2", "0.1-<1/4\"");
        qpfDict.put("3", "1/4-<1/2\"");
        qpfDict.put("4", "1/2-<1\"");
        qpfDict.put("5", "1-<2\"");
        qpfDict.put("6", ">=2\"");
    }

    /**
     * Default value to use when no entry found in qpfDict.
     */
    private static final String qpfDefault = "None";

    /**
     * All check box.
     */
    private Button allChk;

    /**
     * Routine check box.
     */
    private Button routineChk;

    /**
     * Table check box.
     */
    private Button tableRdo;

    /**
     * Long radio button.
     */
    private Button longRdo;

    /**
     * Short radio button.
     */
    private Button shortRdo;

    /**
     * Flight Category check box.
     */
    private Button flightCatChk;

    /**
     * Probabilities check box.
     */
    private Button probabilitiesChk;

    /**
     * The model type of mos data
     */
    private String model;

    /**
     * The station list.
     */
    private List<String> stationList;

    /**
     * Current header and data information displayed in the tab.
     */
    private String[] currentGuidance;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public MosViewer(Composite parent, String model) {
        super(parent);
        this.model = model;
    }

    /**
     * Method called after initializing all of the components.
     */
    @Override
    protected void finalInitialization() {
        // Do nothing
    }

    /**
     * Create the controls at the top of the composite.
     */
    @Override
    protected void createTopControls(ResourceConfigMgr configMgr) {
        Composite controlsComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(5, false);
        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        controlsComp.setLayout(gl);
        controlsComp.setLayoutData(gd);
        configMgr.setDefaultColors(controlsComp);

        // ------------------------------------------------
        // Create the All and Routine check buttons
        // ------------------------------------------------
        allChk = new Button(controlsComp, SWT.CHECK);
        allChk.setText("All");
        allChk.setToolTipText("Displays all data for all sites");
        configMgr.setDefaultFontAndColors(allChk);
        allChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                allChkIsSelected = allChk.getSelection();
                generateGuidance(siteID);
            }
        });

        routineChk = new Button(controlsComp, SWT.CHECK);
        routineChk.setText("Routine");
        routineChk.setToolTipText("Determines TAF issuance time");
        routineChk.setSelection(configMgr
                .getResourceAsBoolean(ResourceTag.ShowRoutine));
        configMgr.setDefaultFontAndColors(routineChk);
        routineChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                generateGuidance(siteID);
            }
        });

        // ------------------------------------------------
        // Create the format radio buttons
        // ------------------------------------------------
        Composite radioComp = new Composite(controlsComp, SWT.BORDER);
        gl = new GridLayout(4, false);
        radioComp.setLayout(gl);
        configMgr.setDefaultColors(radioComp);

        Label formatLBl = new Label(radioComp, SWT.NONE);
        formatLBl.setText("Format: ");
        configMgr.setDefaultFontAndColors(formatLBl);

        tableRdo = new Button(radioComp, SWT.RADIO);
        tableRdo.setText("table");
        tableRdo.setToolTipText("Display format");
        configMgr.setDefaultFontAndColors(tableRdo);
        tableRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (tableRdo.getSelection()) {
                    generateGuidance(siteID);
                }
            }
        });

        longRdo = new Button(radioComp, SWT.RADIO);
        longRdo.setText("long");
        longRdo.setToolTipText("Display format");
        configMgr.setDefaultFontAndColors(longRdo);
        longRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (longRdo.getSelection()) {
                    generateGuidance(siteID);
                }
            }
        });

        shortRdo = new Button(radioComp, SWT.RADIO);
        shortRdo.setText("short");
        shortRdo.setToolTipText("Display format");
        configMgr.setDefaultFontAndColors(shortRdo);
        shortRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (shortRdo.getSelection()) {
                    generateGuidance(siteID);
                }
            }
        });

        String format = configMgr.getDataAsString(ResourceTag.ShowFormatted);

        if (format.compareTo("raw") == 0) {
            tableRdo.setSelection(true);
        } else if (format.compareTo(longRdo.getText()) == 0) {
            longRdo.setSelection(true);
        } else if (format.compareTo(shortRdo.getText()) == 0) {
            shortRdo.setSelection(true);
        }

        // ------------------------------------------------
        // Create the Flight Categories and Probabilities
        // check buttons
        // ------------------------------------------------
        flightCatChk = new Button(controlsComp, SWT.CHECK);
        flightCatChk.setText("Flight Categories");
        configMgr.setDefaultFontAndColors(flightCatChk);
        flightCatChk.setSelection(configMgr
                .getResourceAsBoolean(ResourceTag.HighlightFlightCat));
        flightCatChk
                .setToolTipText("Background color depicts flight categories");
        flightCatChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                requestComplete(currentGuidance);
            }
        });

        probabilitiesChk = new Button(controlsComp, SWT.CHECK);
        probabilitiesChk.setText("Probabilities");
        configMgr.setDefaultFontAndColors(probabilitiesChk);
        probabilitiesChk.setSelection(configMgr
                .getResourceAsBoolean(ResourceTag.ShowProbs));
        probabilitiesChk.setToolTipText("Display Vsby and Cig probabilities");
        probabilitiesChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                requestComplete(currentGuidance);
            }
        });
    }

    @Override
    public void requestComplete(String[] newGuidance) {

        currentGuidance = newGuidance;

        boolean genToolTips = false;

        /*
         * Check if we need to remove probabilities
         */
        boolean removeProbs = false;
        if (!tableRdo.isDisposed() && tableRdo.getSelection() == true) {
            genToolTips = true;
            if (!probabilitiesChk.isDisposed()
                    && probabilitiesChk.getSelection() == false) {
                removeProbs = true;
            }
        }

        if (!textComp.isDisposed()) {
            if (newGuidance.length > 1) {
                StringBuilder sb = new StringBuilder();
                String header = newGuidance[0] + "\n" + newGuidance[1];
                textComp.getHeaderStTxt().setText(header);

                for (int i = 2; i < newGuidance.length; i++) {
                    if (removeProbs == true) {
                        if (containsProb(newGuidance[i]) == false) {
                            sb.append(newGuidance[i]).append("\n");
                        }
                    } else {
                        sb.append(newGuidance[i]).append("\n");
                    }
                }
                textComp.getDataStTxt().setText(sb.toString());
            } else if (newGuidance.length == 1) {
                textComp.getHeaderStTxt().setText("");
                textComp.getDataStTxt().setText(newGuidance[0]);
            }
        }

        // Update the displayed text
        if (!flightCatChk.isDisposed()) {
            updateTextMarkers(flightCatChk.getSelection(),
                    tableRdo.getSelection());
        }

        // Generate table's tool tips. Must be done after flight tags are
        // removed.
        if (!textComp.isDisposed()) {
            if (genToolTips) {
                generateToolTips();
            } else {
                textComp.getDataStTxt().setData(null);
            }
        }
        setDisplayCurrent(true);
        super.requestComplete();
    }

    /**
     * Generate the tool tip for the VIS, CVIS, CIG, CCIG row's entries and for
     * the cat# row headers.
     */
    private void generateToolTips() {
        String text = textComp.getDataStTxt().getText();
        ArrayList<HeaderTextToolTip> tipsList = new ArrayList<HeaderTextToolTip>();
        int offset = 0;
        int end = -1;
        String lastCategory = null;
        String tip = null;
        String index = null;
        String line = null;

        while (offset >= 0 && offset < text.length()) {
            end = text.indexOf("\n", offset);
            if (end < 0) {
                end = text.length();
                line = text.substring(offset);
            } else {
                line = text.substring(offset, end);
            }
            if (line.startsWith("VIS") || line.startsWith("CVIS")) {
                lastCategory = "VIS";
                line = line.trim();
                int c = 9;
                while (c < line.length()) {
                    index = line.substring(c, c + 3).trim();
                    if (index.length() != 0) {
                        tip = visDict.get(index);
                        if (tip == null) {
                            tip = visDefault;
                        }
                        tipsList.add(new HeaderTextToolTip(offset + c + 1,
                                offset + c + 4, tip));
                    }
                    c += 4;
                }
            } else if (line.startsWith("CIG") || line.startsWith("CCIG")) {
                lastCategory = "CIG";
                line = line.trim();
                int c = 9;
                while (c < line.length()) {
                    index = line.substring(c, c + 3).trim();
                    if (index.length() != 0) {
                        tip = cigDict.get(index);
                        if (tip == null) {
                            tip = cigDefault;
                        }
                        tipsList.add(new HeaderTextToolTip(offset + c + 1,
                                offset + c + 4, tip));
                    }
                    c += 4;
                }
            } else if (line.startsWith("QPF06")) {
                line = line.trim();
                int c = 17;
                while (c < line.length()) {
                    index = line.substring(c, c + 3).trim();
                    if (index.length() != 0) {
                        tip = qpfDict.get(index);
                        if (tip == null) {
                            tip = qpfDefault;
                        }
                        tipsList.add(new HeaderTextToolTip(offset + c + 1,
                                offset + c + 4, tip));
                    }
                    c += 8;
                }
            } else if (line.startsWith("cat")) {
                index = line.substring(3, 4);
                if (lastCategory == "VIS") {
                    tip = visDict.get(index);
                    if (tip == null) {
                        tip = visDefault;
                    }
                    tipsList.add(new HeaderTextToolTip(offset, offset + 5, tip));
                } else if (lastCategory == "CIG") {
                    tip = cigDict.get(index);
                    if (tip == null) {
                        tip = cigDefault;
                    }
                    tipsList.add(new HeaderTextToolTip(offset, offset + 5, tip));
                }
            }
            offset = end + 1;
        }

        HeaderTextToolTip[] tips = new HeaderTextToolTip[tipsList.size()];
        tipsList.toArray(tips);
        textComp.getDataStTxt().setData(tips);
    }

    @Override
    public int generateGuidance(String siteID) {
        int cnt = super.generateGuidance(siteID);
        if (allChk.getSelection() && tableRdo.getSelection()) {
            msgStatComp.setMessageText(
                    "Table display for all sites is not supported", allChk
                            .getDisplay().getSystemColor(SWT.COLOR_RED)
                            .getRGB());
            return cnt;
        }
        List<String> siteIDs;

        if (allChk.getSelection()) {
            siteIDs = stationList;
        } else {
            siteIDs = new ArrayList<String>();
            siteIDs.add(siteID);
        }

        String format = "short";
        if (longRdo.getSelection()) {
            format = "long";
        } else if (tableRdo.getSelection()) {
            format = "table";
        }
        MosGuidanceRequest req = new MosGuidanceRequest();
        req.setTag(getTag(siteID));
        req.setFormat(format);
        req.setGuidanceType(GuidanceType.MOS);
        req.setListener(this);
        req.setModel(this.model);
        ArrayList<String> siteObjs = new ArrayList<String>();
        ArrayList<CacheGuidanceRequest> cacheRequests = new ArrayList<CacheGuidanceRequest>();
        for (String sID : siteIDs) {
            String sIDtag = getTag(sID);
            String siteObj = getCacheSiteObj(sID, sIDtag);
            if (siteObj == null) {
                CacheGuidanceRequest cReq = createCacheRequest(sID, format);
                cacheRequests.add(cReq);
            } else {
                siteObjs.add(siteObj);
            }
        }
        if (cacheRequests.size() > 0) {
            queueCacheRequests(cnt, cacheRequests);
        } else {
            req.setSiteObjs(siteObjs);
            req.setRoutine(routineChk.getSelection());
            // textComp.getHeaderStTxt().setText("");
            // textComp.getDataStTxt().setText("");
            PythonGuidanceJob.getInstance().enqueue(req);
        }
        return cnt;
    }

    /**
     * Check if the text starts with any probability text.
     * 
     * @param textLine
     *            List of text to test against.
     * @return True if probability, false otherwise.
     */
    private boolean containsProb(String textLine) {
        if (textLine.startsWith("cat") || textLine.startsWith("PSKC")
                || textLine.startsWith("PFEW") || textLine.startsWith("PBKN")
                || textLine.startsWith("POVC") || textLine.startsWith("PSCT")
                || textLine.startsWith("PRain") || textLine.startsWith("PSnow")
                || textLine.startsWith("PFrz")) {
            return true;
        }

        return false;
    }

    /**
     * Getter/Accessor of stationList (based on configuration)
     * 
     * @return stationList
     */
    @Override
    public List<String> getStationList() {
        return stationList;
    }

    /**
     * Setter/Mutator of stationList (based on configuration)
     * 
     * @param stationList
     */
    @Override
    public void setStationList(List<String> stationList) {
        this.stationList = stationList;
    }

    private String getTag(String siteID) {
        return MosCacheGuidanceRequest.getTag(siteID, model);
    }

    private CacheGuidanceRequest createCacheRequest(String siteID, String format) {
        MosCacheGuidanceRequest req = new MosCacheGuidanceRequest();
        req.setTag(getTag(siteID));
        req.setGuidanceType(GuidanceType.MOS);
        req.setSiteID(siteID);
        req.setModel(this.model);
        req.setFormat(format);
        return req;
    }

    @Override
    public void generateCache(List<String> siteIDs) {
        for (String siteID : siteIDs) {
            CacheGuidanceRequest req = createCacheRequest(siteID, "short");
            cacheEnqueue(req);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.aviation.guidance.ViewerTab#getSite(java.lang.String)
     */
    @Override
    public String getSite(String site) {
        return chooseModel(site, model);
    }

    /**
     * Obtain the model.
     * 
     * @return model
     */
    public String getModel() {
        return model;
    }
}
