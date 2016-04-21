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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.aviation.cachedata.CacheGuidanceRequest;
import com.raytheon.viz.aviation.cachedata.PythonCacheGuidanceJob;
import com.raytheon.viz.aviation.editor.HeaderTextComp;
import com.raytheon.viz.aviation.editor.TafViewerEditorDlg;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;
import com.raytheon.viz.avnconfig.IStatusSettable;
import com.raytheon.viz.avnconfig.ITafSiteConfig;
import com.raytheon.viz.avnconfig.TafSiteConfigFactory;
import com.raytheon.viz.avnconfig.TafSiteData;

/**
 * 
 * Abstract class used as the base class for all of the guidance viewers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2009            lvenable     Initial creation
 * Apr 28,2011  8065       rferrel     Add flag to indicate display is current
 *                                     and implement data caching
 * Jun 1, 2011  9673       rferrel     Added fltCatFontColor.
 * 09Apr2014    #3005      lvenable    Marked currentTab as volatile, added call through
 *                                     methods to the HeaderTextComp class.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class ViewerTab extends Composite {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ViewerTab.class);

    /**
     * Header text composite/controls.
     */
    protected HeaderTextComp textComp;

    /**
     * VFR start tag.
     */
    protected static final String vfrStartTag = "<vfr>";

    /**
     * VFR end tag.
     */
    protected static final String vfrEndTag = "</vfr>";

    /**
     * LIFR start tag.
     */
    protected static final String lifrStartTag = "<lifr>";

    /**
     * LIFR end tag.
     */
    protected static final String lifrEndTag = "</lifr>";

    /**
     * IFR start tag.
     */
    protected static final String ifrStartTag = "<ifr>";

    /**
     * IFR end tag.
     */
    protected static final String ifrEndTag = "</ifr>";

    /**
     * MVFR start tag.
     */
    protected static final String mvfrStartTag = "<mvfr>";

    /**
     * MVFR end tag.
     */
    protected static final String mvfrEndTag = "</mvfr>";

    /**
     * MessageStatusComp instance
     */
    protected IStatusSettable msgStatComp;

    /**
     * Current site.
     */
    protected String siteID;

    /**
     * Taf data for the current site.
     */
    protected TafSiteData siteData;

    /**
     * Indicates when the tab is populated with the user's latest request.
     */
    private boolean displayCurrent = false;

    /**
     * True when tab is selected for display.
     */
    private volatile boolean currentTab = false;

    /**
     * Flight Category's font color.
     */
    private Color fltCatFontColor;

    /**
     * Flag set to true when allChk button is selected. Needed for check state
     * when not on the Display thread. Sub classes must maintain the status of
     * the flag
     */
    protected boolean allChkIsSelected;

    /**
     * Dialog the viewer is associated with.
     */
    protected TafViewerEditorDlg tafViewerEditorDlg;

    /**
     * Set dialog viewer is associated with.
     * 
     * @param tafViewerEditorDlg
     */
    public void setTafViewerEditorDlg(TafViewerEditorDlg tafViewerEditorDlg) {
        this.tafViewerEditorDlg = tafViewerEditorDlg;
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public ViewerTab(Composite parent) {
        super(parent, SWT.NONE);

        init();
        PythonGuidanceJob.addViewerTab(this);
    }

    /**
     * Method sub-classes must implement to request the caching of data
     * displayed by the tab.
     * 
     * @param siteIDs
     *            list of sites needing data cached
     */
    abstract public void generateCache(List<String> siteIDs);

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.widgets.Widget#dispose()
     */
    @Override
    public void dispose() {
        PythonGuidanceJob.removeViwerTab(this);
        if (fltCatFontColor != null) {
            fltCatFontColor.dispose();
            fltCatFontColor = null;
        }
        super.dispose();
    }

    /**
     * 
     * @return displayCurrent true when tab is populated with the current
     *         request
     */
    public boolean isDisplayCurrent() {
        return displayCurrent;
    }

    /**
     * Set the current display's status. When true the tab is populate with the
     * current request. When false the tab is waiting to be updated with the
     * current request.
     * 
     * @param displayCurrent
     */
    public void setDisplayCurrent(boolean displayCurrent) {
        this.displayCurrent = displayCurrent;
    }

    /**
     * Indicates the tab is selected as the current tab being displayed.
     * 
     * @return currentTab
     */
    public boolean isCurrentTab() {
        return currentTab;
    }

    /**
     * Set the current tab state.
     * 
     * @param currentTab
     */
    public void setCurrentTab(boolean currentTab) {
        this.currentTab = currentTab;
    }

    /**
     * Initialize composite.
     */
    private void init() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        this.setLayout(gl);
        this.setLayoutData(gd);

        initializeComponents();

        this.pack();
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        configMgr.setDefaultColors(this);

        createTopControls(configMgr);

        createTextControl();

        finalInitialization();
    }

    /**
     * Create the header text composite/controls.
     */
    protected void createTextControl() {
        textComp = new HeaderTextComp(this, true, true);
    }

    /**
     * Create the controls above the text controls.
     * 
     * @param configMgr
     *            Configuration manager.
     */
    protected abstract void createTopControls(ResourceConfigMgr configMgr);

    /**
     * This method is called so any extra initialization/setup can be performed
     * after the initializeComponents method is called.
     */
    protected abstract void finalInitialization();

    /**
     * From the GUI the user can select various options to queue another display
     * request prior to a previous request being finished. This variable is used
     * to determine the last request queued so it will be the one to populate
     * the tab.
     */
    private AtomicInteger generateGuidanceCount = new AtomicInteger(
            Integer.MIN_VALUE);

    /**
     * This method must be overridden by the implementing class to perform the
     * queue of the type of requested need to populate the tab. The first thing
     * the override method should do is call its super method to properly set up
     * the siteID, states and to get the count value to return.
     * 
     * @param siteID
     *            Site ID.
     * @return cnt unique count that increases each time the method is called.
     */
    public int generateGuidance(String siteID) {
        int cnt = generateGuidanceCount.incrementAndGet();
        this.siteID = siteID;
        setDisplayCurrent(false);
        return cnt;
    }

    /**
     * Used to force request to get the current sites data.
     */
    void reGenerateGuidance() {
        generateGuidance(siteID);
    }

    /**
     * This method must be called by the implementing class' requestComplete
     * method after it has populated the textComp header and data section. This
     * updates the highlighting of the TAF text in the viewer and adjusts the
     * width of the this tab's header and data text component so they will stay
     * aligned with each other when the horizontal scroll is moved.
     */
    public void requestComplete() {
        tafViewerEditorDlg.populateTafViewer();

        // Make the longest line in the header and data the same length.
        // This allows scrolling to work properly.
        StyledText st = textComp.getHeaderStTxt();
        int hdrLen = 0;
        int hdrIndex = -1;
        for (int i = 0; i < st.getLineCount(); ++i) {
            String line = st.getLine(i);
            if (hdrLen < line.length()) {
                hdrLen = line.length();
                hdrIndex = i;
            }
        }

        if (hdrIndex > -1) {
            int dataLen = 0;
            int dataIndex = -1;
            st = textComp.getDataStTxt();
            for (int i = 0; i < st.getLineCount(); ++i) {
                String line = st.getLine(i);
                if (dataLen < line.length()) {
                    dataLen = line.length();
                    dataIndex = i;
                }
            }
            if (dataIndex < 0) {
                // No data make blank line
                for (int i = 0; i < hdrLen; ++i) {
                    st.append(" ");
                }
            } else {
                int pos = -1;
                int cnt = -1;
                if (hdrLen < dataLen) {
                    st = textComp.getHeaderStTxt();
                    pos = st.getOffsetAtLine(hdrIndex) + hdrLen;
                    cnt = dataLen - hdrLen;
                } else {
                    pos = st.getOffsetAtLine(dataIndex) + dataLen;
                    cnt = hdrLen - dataLen;
                }
                while (cnt > 0) {
                    --cnt;
                    st.replaceTextRange(pos, 0, " ");
                }
            }
        }
    }

    /**
     * Get the header text composite.
     * 
     * @return The header text composite.
     */
    public HeaderTextComp getTextComp() {
        return textComp;
    }

    /**
     * Update the text to color flight category text and remove the marker tags.
     * 
     * @param fltCat
     *            True to color the text, false to leave text as is.
     * @param isTable
     *            True if formatting table data otherwise false
     */
    protected void updateTextMarkers(boolean fltCat, boolean isTable) {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        if (fltCat == true) {
            colorFlightCat(ResourceTag.VfrColor, vfrStartTag, vfrEndTag,
                    configMgr);
            colorFlightCat(ResourceTag.MvfrColor, mvfrStartTag, mvfrEndTag,
                    configMgr);
            colorFlightCat(ResourceTag.IfrColor, ifrStartTag, ifrEndTag,
                    configMgr);
            colorFlightCat(ResourceTag.LifrColor, lifrStartTag, lifrEndTag,
                    configMgr);
        }

        removeMarkers(vfrStartTag, vfrEndTag);
        removeMarkers(mvfrStartTag, mvfrEndTag);
        removeMarkers(ifrStartTag, ifrEndTag);
        removeMarkers(lifrStartTag, lifrEndTag);

        if (isTable) {
            // Trim trim/pad each line to same length as header so scrolling
            // data will stay aligned with the header.
            StyledText st = textComp.getHeaderStTxt();
            int hdrLen = -1;
            for (String l : st.getText().split("\n")) {
                if (hdrLen < l.length()) {
                    hdrLen = l.length();
                }
            }

            st = textComp.getDataStTxt();
            for (int i = 0; i < st.getLineCount(); ++i) {
                String line = st.getLine(i);
                int len = line.length();
                if (len < hdrLen) {
                    int start = st.getOffsetAtLine(i) + len;
                    for (int cnt = hdrLen - len; cnt > 0; --cnt) {
                        st.replaceTextRange(start, 0, " ");
                    }
                } else if (len > hdrLen) {
                    int cnt = len - hdrLen;
                    int start = st.getOffsetAtLine(i) + len - cnt;
                    for (; cnt > 0; --cnt) {
                        st.replaceTextRange(start, 1, "");
                    }
                }
            }
        }
    }

    /**
     * Color the flight category data on the display.
     * 
     * @param resTag
     *            Resource tag to determine the background color of the flight
     *            category text.
     * @param startTag
     *            Start tag to determine the start of the data.
     * @param endTag
     *            End tag to determine the end of the data.
     * @param configMgr
     *            Configuration manager to get the flight category color.
     */
    private void colorFlightCat(ResourceTag resTag, String startTag,
            String endTag, ResourceConfigMgr configMgr) {
        StyledText stText = textComp.getDataStTxt();
        StringBuilder sb = new StringBuilder(stText.getText());

        int startTagIdx = sb.indexOf(startTag);
        int endTagIdx = -1;

        if (fltCatFontColor == null) {
            fltCatFontColor = new Color(getParent().getDisplay(),
                    RGBColors.getRGBColor("white"));
        }

        while (startTagIdx != -1) {
            endTagIdx = sb.indexOf(endTag, startTagIdx);

            StyleRange sr = new StyleRange(startTagIdx + startTag.length(),
                    endTagIdx - (startTagIdx + startTag.length()),
                    fltCatFontColor, configMgr.getFlightCatColor(resTag));
            stText.setStyleRange(sr);

            startTagIdx = sb.indexOf(startTag, endTagIdx);
        }
    }

    /**
     * Remove the flight category XML tags from the text.
     * 
     * @param startTag
     *            Start tag.
     * @param endTag
     *            End tag.
     */
    protected void removeMarkers(String startTag, String endTag) {
        StyledText stText = textComp.getDataStTxt();

        boolean done = false;

        int startTagIdx = -1;
        int endTagIdx = -1;

        while (done == false) {
            done = true;

            /*
             * Remove the start tag.
             */
            startTagIdx = stText.getText().lastIndexOf(startTag);

            if (startTagIdx != -1) {
                stText.replaceTextRange(startTagIdx, startTag.length(), "");
                done = false;
            }

            /*
             * Remove the end tag.
             */
            endTagIdx = stText.getText().lastIndexOf(endTag);

            if (endTagIdx != -1) {
                stText.replaceTextRange(endTagIdx, endTag.length(), "");
                done = false;
            }
        }
    }

    /**
     * Clear the header and data text controls.
     */
    public void clearTextControls() {
        textComp.clearTextControls();
    }

    /**
     * Set the header and data text controls to show as updating.
     */
    public void markTextAsUpdating() {
        textComp.markTextAsUpdating();
    }

    /**
     * 
     * @return stationList list of sites tab needs to cache data for.
     */
    public abstract List<String> getStationList();

    /**
     * Set the list of stations tab needs to cache data for.
     * 
     * @param stationList
     */
    public abstract void setStationList(List<String> stationList);

    /**
     * @return the msgStatComp
     */
    public IStatusSettable getMsgStatComp() {
        return msgStatComp;
    }

    /**
     * @param msgStatComp
     *            the msgStatComp to set
     */
    public void setMsgStatComp(IStatusSettable msgStatComp) {
        this.msgStatComp = msgStatComp;
    }

    /**
     * Queue request onto the cache queue.
     * 
     * @param req
     *            - request to queue
     */
    protected void cacheEnqueue(CacheGuidanceRequest req) {
        // System.out.println("Queueing cache for tag: " + req.getTag());
        PythonCacheGuidanceJob.getInstance().enqueue(req);
    }

    /**
     * Queue the retrieval of requests and if still active update display once
     * data is retrieved. This starts up a thread to wait for the cache data to
     * arrive so this method does not block and freeze the GUI. Once the data
     * arrives the cnt is used to determine if the user still wants to display
     * the data.
     * 
     * @param cnt
     *            - Unique count to indicate this request is still active.
     * @param cacheRequests
     *            - List of data requests to retrieve
     */
    public void queueCacheRequests(final int cnt,
            final List<CacheGuidanceRequest> cacheRequests) {

        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                PythonGuidanceJob.getInstance().waitForCacheRequests(
                        cacheRequests);
                // Update tab if still current and waiting for this request
                if (ViewerTab.this.isDisposed() == false && isCurrentTab()
                        && generateGuidanceCount.get() == cnt) {
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            generateGuidance(siteID);
                        }
                    });
                }
            }
        });
        thread.start();
    }

    /**
     * Obtain the desired python pickle string which is the cached data.
     * 
     * @param siteID
     *            - Site data is for
     * @param tag
     *            - Unique tag for the desired data.
     * @return siteObj - The pickle string or null when current data not cached
     */
    public String getCacheSiteObj(String siteID, String tag) {
        return PythonCacheGuidanceJob.getInstance().getSiteObj(siteID, tag);
    }

    /**
     * Called when an alert arrives.
     * 
     * @param siteIDs
     *            - The site to clear
     */
    public void alertSites(final ArrayList<String> siteIDs) {
        if (isDisposed()) {
            return;
        }
        generateCache(siteIDs);
        if (allChkIsSelected || siteIDs.contains(siteID)) {
            setDisplayCurrent(false);
            if (isCurrentTab() && isDisposed() == false) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        generateGuidance(siteID);
                    }
                });
            }
        }
    }

    /**
     * Get the TAF's sites configuration information.
     * 
     * @param site
     * @return tafSiteData
     */
    protected TafSiteData getSiteData(String site) {
        ITafSiteConfig config;
        TafSiteData data = null;
        try {
            config = TafSiteConfigFactory.getInstance();
            data = config.getSite(site);
        } catch (ConfigurationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return data;
    }

    /**
     * Let the implementing class determine what should be returned for the
     * site.
     * 
     * @param site
     * @return
     */
    public abstract String getSite(String site);

    /**
     * Convince method used by implementing classes to determine what string to
     * return for the site base on the site and model.
     * 
     * @param site
     *            the site
     * @param model
     *            the model being used for the tab
     * @return siteId
     */
    public String chooseModel(String site, String model) {
        if ("gfslamp".equals(model)) {
            return getSiteData(site).gfslamp;
        } else if ("nammos".equals(model) || "etamos".equals(model)) {
            return getSiteData(site).nammos;
        } else if ("nam".equals(model) || "etabuf".equals(model)) {
            return getSiteData(site).nam;
        } else if ("metar".equals(model)) {
            return getSiteData(site).metar[0];
        } else if ("acars".equals(model)) {
            return getSiteData(site).acars;
        } else if ("gfsmos".equals(model)) {
            return getSiteData(site).gfsmos;
        } else if ("profilers".equals(model)) {
            return getSiteData(site).profilers[0];
        } else if ("radars".equals(model)) {
            return getSiteData(site).radars[0];
        } else {
            return site;
        }
    }
}
