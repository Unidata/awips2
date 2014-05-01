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
import java.util.List;

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
import com.raytheon.viz.aviation.cachedata.GridCacheGuidanceRequest;
import com.raytheon.viz.aviation.guidance.GuidanceRequest.GuidanceType;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;

/**
 * GridViewer class contains a setup for the Guidance tab on the TAF viewer
 * editor dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation
 * 29 APR 2011  8065       rferrel     Add flag to indicate display is current
 *                                     and implement data caching
 * 13 FEB 2013  1549       rferrel     Changes to properly display grid data.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GridViewer extends ViewerTab implements
        IRequestCompleteListener<String[]> {
    /**
     * All check box.
     */
    private Button allChk;

    /**
     * Routine check box.
     */
    private Button routineChk;

    /**
     * Table radio button.
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
     * Flight Category check button.
     */
    private Button flightCatChk;

    private String[] currentGuidance;

    /**
     * The station list.
     */
    private List<String> stationList;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public GridViewer(Composite parent, String model) {
        super(parent);
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
        GridLayout gl = new GridLayout(4, false);
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
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.jobs.IRequestCompleteListener#requestComplete
     * (java.lang.Object)
     */
    @Override
    public void requestComplete(String[] newGuidance) {

        currentGuidance = newGuidance;

        if (!textComp.isDisposed()) {
            if (newGuidance == null) {
                textComp.getHeaderStTxt().setText("");
                textComp.getDataStTxt().setText("");
            } else {
                if (newGuidance.length > 0
                        && newGuidance[0].startsWith("++Cache out of date")) {
                    // Purge old cache and redo the request
                    ArrayList<String> siteIDs = new ArrayList<String>();
                    for (String line : newGuidance) {
                        String siteID = line.split(":")[1];
                        siteIDs.add(siteID);
                    }
                    generateCache(siteIDs);
                    reGenerateGuidance();
                } else if (newGuidance.length == 1) {
                    // Assume error message since header takes 2 lines.
                    textComp.getHeaderStTxt().setText("");
                    textComp.getDataStTxt().setText(newGuidance[0]);
                } else {
                    StringBuilder sb = new StringBuilder();
                    String header = newGuidance[0] + "\n" + newGuidance[1];
                    textComp.getHeaderStTxt().setText(header);
                    for (int i = 2; i < newGuidance.length; i++) {
                        sb.append(newGuidance[i]);
                        sb.append("\n");
                    }
                    textComp.getDataStTxt().setText(sb.toString());

                    if (!flightCatChk.isDisposed()) {
                        updateTextMarkers(flightCatChk.getSelection(),
                                tableRdo.getSelection());
                    }
                    setDisplayCurrent(true);
                    super.requestComplete();
                }
            }
        }
    }

    /**
     * Request desired data for display on the tab. A check is performed and if
     * all data is cached a request is queued to immediately update the tab.
     * Otherwise request is made to cache missing data and update the display
     * once it arrives.
     */
    @Override
    public int generateGuidance(String siteID) {
        int cnt = super.generateGuidance(siteID);
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

        GridGuidanceRequest req = new GridGuidanceRequest();
        req.setTag(getTag(siteID));
        req.setFormat(format);
        req.setGuidanceType(GuidanceType.GRID);
        req.setListener(this);
        List<String> siteObjs = new ArrayList<String>();
        List<CacheGuidanceRequest> cacheRequests = new ArrayList<CacheGuidanceRequest>();
        for (String sID : siteIDs) {
            String sIDtag = getTag(sID);
            String siteObj = getCacheSiteObj(sID, sIDtag);
            if (siteObj == null) {
                CacheGuidanceRequest sIDreq = createCacheRequest(sID);
                cacheRequests.add(sIDreq);
            } else {
                siteObjs.add(siteObj);
            }
        }
        if (cacheRequests.size() > 0) {
            queueCacheRequests(cnt, cacheRequests);
        } else {
            req.setSiteObjs(siteObjs);
            req.setRoutine(routineChk.getSelection());
            PythonGuidanceJob.getInstance().enqueue(req);
        }
        return cnt;
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

    /**
     * Queue a cache request for the list of sites.
     */
    @Override
    public void generateCache(List<String> siteIDs) {
        for (String siteID : siteIDs) {
            CacheGuidanceRequest req = createCacheRequest(siteID);
            cacheEnqueue(req);
        }
    }

    /**
     * Get grid style tag for the site ID.
     * 
     * @param siteID
     * @return tag
     */
    private String getTag(String siteID) {
        return GridCacheGuidanceRequest.getTag(siteID);
    }

    /**
     * Create grid cache request.
     * 
     * @param siteID
     * @return req
     */
    private CacheGuidanceRequest createCacheRequest(String siteID) {
        GridCacheGuidanceRequest req = new GridCacheGuidanceRequest();
        req.setTag(getTag(siteID));
        req.setSiteID(siteID);
        req.setGuidanceType(GuidanceType.GRID);
        return req;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.aviation.guidance.ViewerTab#getSite(java.lang.String)
     */
    @Override
    public String getSite(String site) {
        return site;
    }
}
