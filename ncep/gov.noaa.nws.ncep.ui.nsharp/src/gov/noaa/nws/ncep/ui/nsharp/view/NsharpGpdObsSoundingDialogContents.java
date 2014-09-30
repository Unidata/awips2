/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.view.GpdProductDialogContents
 * 
 * This java class performs the NSHARP NsharpLoadDialog functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 08/2013	    			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.view;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfo;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery;

import java.sql.Timestamp;
import java.text.DateFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;

// import gov.noaa.nws.ncep.viz.rsc.gpd.query.GpdQuery;

public class NsharpGpdObsSoundingDialogContents {
    private Composite parent;

    private org.eclipse.swt.widgets.List sndTimeList = null, prodList = null;

    // private Text newProdNameText, curProdNameText;
    private Group sndTimeListGp, topGp, prodGp;

    private boolean timeLimit = false;

    // private boolean rawData = false;
    private Button timeBtn;

    // private String FILE_DROP = "DROP";
    private String currentProdName = "";

    private NsharpLoadDialog ldDia;

    // private Label curProdLbl, newProdLbl;
    private ArrayList<String> selectedTimeList = new ArrayList<String>();

    private Font newFont;

    private String[] sel = new String[1];

    public String getCurrentProdName() {
        return currentProdName;
    }

    public void setCurrentProdName(String currentProdName) {
        this.currentProdName = currentProdName;
    }

    public NsharpGpdObsSoundingDialogContents(Composite parent) {
        this.parent = parent;
        ldDia = NsharpLoadDialog.getAccess();
        newFont = ldDia.getNewFont();
    }

    private void createProdList() {
        if (prodList != null)
            prodList.removeAll();
        if (sndTimeList != null)
            sndTimeList.removeAll();
        ldDia.startWaitCursor();
        List<String> prodStrLst = null;
        // TBDGPD List<String> prodStrLst = GpdQuery
        // .getGpdAvailProducts(GenericPointDataReqType.GET_GPD_AVAILABLE_OBSERVED_SOUNDING_PRODUCTS);
        if (prodList != null) {
            for (String prod : prodStrLst)
                prodList.add(prod);
        }
        ldDia.stopWaitCursor();
    }

    private void createSndList() {
        /*
         * /testing List<String> prodList =
         * GpdQuery.getGpdAvailProducts(GenericPointDataReqType
         * .GET_GPD_AVAILABLE_SURFACE_PRODUCTS); if(prodList != null &&
         * prodList.size() > 0){ for(String prod: prodList)
         * System.out.println("GPD product = "+ prod); } //edn testing
         */
        if (currentProdName.equals("") == true)
            return;
        if (sndTimeList != null)
            sndTimeList.removeAll();

        // use NcSoundingQuery to query
        ldDia.startWaitCursor();
        NcSoundingTimeLines timeLines = null;
        // TBDGPD NcSoundingTimeLines timeLines =
        // GpdQuery.getGpdSoundingTimeLines(currentProdName);

        if (timeLines != null && timeLines.getTimeLines() != null) {
            DateFormatSymbols dfs = new DateFormatSymbols();
            String[] defaultDays = dfs.getShortWeekdays();
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

            for (Object timeLine : timeLines.getTimeLines()) {
                Timestamp synoptictime = (Timestamp) timeLine;
                if (synoptictime != null) {
                    // need to format synoptictime to GMT time string.
                    // Timestamp.toString produce a local time Not GMT time
                    cal.setTimeInMillis(synoptictime.getTime());
                    String dayOfWeek = defaultDays[cal
                            .get(Calendar.DAY_OF_WEEK)];
                    // String gmtTimeStr =
                    // String.format("%1$ty%1$tm%1$td/%1$tH%1$tM %2$s", cal,
                    // currentSndType.toString());
                    String gmtTimeStr = String.format(
                            "%1$ty%1$tm%1$td/%1$tH(%3$s) %2$s", cal,
                            currentProdName.toString(), dayOfWeek);
                    if (!timeLimit) {
                        // System.out.println("not 00z and 12z only");
                        sndTimeList.add(gmtTimeStr);
                    } else {
                        int hour = cal.get(Calendar.HOUR_OF_DAY);
                        // System.out.println("00z and 12z only hour = "+ hour);
                        if ((hour == 0) || (hour == 12))
                            sndTimeList.add(gmtTimeStr);
                    }
                }
            }
        } else
            System.out.println("EDEX timeline query return null");

        ldDia.stopWaitCursor();

    }

    private void queryAndMarkStn(String selectedSndTime) {
        String selectTimetr = NcSoundingQuery
                .convertSoundTimeDispStringToRangeStartTimeFormat(selectedSndTime);
        NsharpMapResource nsharpMapResource = NsharpMapResource
                .getOrCreateNsharpMapResource();
        double lat, lon;
        String stnInfoStr;

        // use NcSoundingQuery to query stn info

        // TBDGPD NcSoundingStnInfoCollection sndStnInfoCol =
        // GpdQuery.getGpdStationInfoCollection(currentProdName, selectTimetr);
        NcSoundingStnInfoCollection sndStnInfoCol = null;
        if (sndStnInfoCol != null && sndStnInfoCol.getStationInfo() != null) {

            NcSoundingStnInfo[] stnInfoAry = sndStnInfoCol.getStationInfo();
            // System.out.println("gpdobs station number = " +
            // stnInfoAry.length);
            // Note: A same station may have many reports
            for (int i = 0; i < stnInfoAry.length; i++) {
                NcSoundingStnInfo stnInfo = stnInfoAry[i];
                Timestamp synoptictime = null;
                stnInfoStr = stnInfo.getStnId();
                if (stnInfoStr == null || stnInfoStr.length() < 1)
                    stnInfoStr = "*";
                lat = stnInfo.getStationLatitude();
                lon = stnInfo.getStationLongitude();
                // elv = stnInfo.getStationElevation();
                synoptictime = (Timestamp) stnInfo.getSynopTime();

                // convert to Nsharp's own station info struct
                NsharpStationInfo stn = new NsharpStationInfo();
                String packedStnInfoStr = stnInfoStr.replace(" ", "_");
                int endIndex = Math.min(
                        NsharpConstants.MAX_SOUNDING_SOURCE_STR_LENGTH,
                        currentProdName.length());
                stn.setStnDisplayInfo(packedStnInfoStr + " " + selectedSndTime
                        + " " + currentProdName.substring(0, endIndex));
                stn.setLongitude(lon);
                stn.setLatitude(lat);
                stn.setStnId(stnInfoStr);
                stn.setReftime(synoptictime);
                stn.setRangestarttime(synoptictime);
                stn.setSndType(currentProdName.toString());
                // System.out.println("sndType= "+currentSndType);
                // System.out.println("stn  lat ="+stn.getLatitude() +
                // " lon="+stn.getLongitude());
                nsharpMapResource.addPoint(stn);
            }

            NsharpMapResource.bringMapEditorToTop();
        }
    }

    private void handleSndTimeSelection() {
        String selectedSndTime = null;
        if (sndTimeList.getSelectionCount() > 0) {
            NsharpMapResource nsharpMapResource = NsharpMapResource
                    .getOrCreateNsharpMapResource();// NsharpLoadDialog.getAccess().getNsharpMapResource();
            nsharpMapResource.setPoints(null);
            selectedTimeList.clear();
            ldDia.startWaitCursor();
            for (int i = 0; i < sndTimeList.getSelectionCount(); i++) {
                selectedSndTime = sndTimeList.getSelection()[i];
                selectedTimeList.add(selectedSndTime);
                // System.out.println("selected sounding time is " +
                // selectedSndTime);
                int endIndex = selectedSndTime.indexOf(" ");
                String queryingSndTime = selectedSndTime.substring(0, endIndex);
                queryAndMarkStn(queryingSndTime);

            }
            ldDia.setGpdSelectedTimeList(selectedTimeList);
            ldDia.stopWaitCursor();
        }
    }

    public void createGpdObsDialogContents() {
        currentProdName = ldDia.getActiveGpdProdName();

        // System.out.println("createGpdDialogContents");
        // timeLimit =false;
        // rawData = false;
        topGp = new Group(parent, SWT.SHADOW_ETCHED_IN);
        topGp.setLayout(new GridLayout(2, false));
        ldDia.createSndTypeList(topGp);
        /*
         * prodNameGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
         * prodNameGp.setLayout( new GridLayout( 2, true ) );
         * prodNameGp.setText("Product Name"); prodNameGp.setFont(newFont);
         * curProdLbl = new Label(prodNameGp, SWT.NONE | SWT.BORDER);
         * //curProdLbl.setBounds(prodNameGp.getBounds().x,
         * prodNameGp.getBounds().y + NsharpConstants.btnGapY,
         * prodNameGp.getBounds().width/2,NsharpConstants.btnHeight);
         * curProdLbl.setText("Current Product:"); curProdLbl.setFont(newFont);
         * curProdNameText = new Text(prodNameGp, SWT.NONE );
         * //curProdNameText.setBounds
         * (curProdLbl.getBounds().x+curProdLbl.getBounds().width,
         * prodNameGp.getBounds().y + NsharpConstants.btnGapY,
         * prodNameGp.getBounds().width/2,NsharpConstants.btnHeight);
         * curProdNameText.setText(currentProdName);
         * curProdNameText.setFont(newFont); curProdNameText.setTextLimit(20);
         * curProdNameText.setEditable(false); Color color = new
         * Color(Display.getDefault(), NsharpConstants.backgroundColor);
         * curProdNameText.setBackground(color); newProdLbl = new
         * Label(prodNameGp, SWT.NONE | SWT.BORDER);
         * //curProdLbl.setBounds(prodNameGp.getBounds().x,
         * prodNameGp.getBounds().y + NsharpConstants.btnGapY,
         * prodNameGp.getBounds().width/2,NsharpConstants.btnHeight);
         * newProdLbl.setText("New Product:"); newProdLbl.setFont(newFont);
         * 
         * newProdNameText = new Text(prodNameGp, SWT.BORDER | SWT.SINGLE);
         * //GridData data1 = new GridData (SWT.FILL,SWT.FILL, true, true);
         * //prodNameText.setLayoutData (data1);
         * //prodNameText.setBounds(stationBtn.getBounds().x,
         * locationLbl.getBounds().y,450,NsharpConstants.btnHeight);
         * newProdNameText.setTextLimit(20); newProdNameText.setFont(newFont);
         * //prodNameText.addListener (SWT.Verify, new Listener () { //public
         * void handleEvent (Event e) { //String userInputStr = e.text; //
         * System.out.println(userInputStr); //} //}); Button setBtn = new
         * Button(prodNameGp, SWT.PUSH); setBtn.setText("Set Product Name");
         * setBtn.setFont(newFont); setBtn.setEnabled( true );
         * setBtn.setBounds(prodNameGp.getBounds().x+ NsharpConstants.btnGapX,
         * curProdLbl.getBounds().y + curProdLbl.getBounds().height+
         * NsharpConstants.btnGapY,
         * NsharpConstants.btnWidth,NsharpConstants.btnHeight);
         * setBtn.addListener( SWT.MouseUp, new Listener() { public void
         * handleEvent(Event event) { String textStr =
         * newProdNameText.getText(); if((textStr != null) &&
         * !(textStr.isEmpty())){ //textStr = textStr.trim(); currentProdName =
         * textStr; currentProdName = currentProdName.trim();
         * //System.out.println(currentProdName);
         * curProdNameText.setText(currentProdName); curProdNameText.redraw();
         * createSndList(); ldDia.setActiveGpdProdName(currentProdName); } } }
         * );
         */
        prodGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
        prodGp.setText("Gpd Observed Sounding List");
        prodGp.setFont(newFont);
        prodList = new org.eclipse.swt.widgets.List(prodGp, SWT.BORDER
                | SWT.V_SCROLL);
        prodList.setBounds(prodGp.getBounds().x, prodGp.getBounds().y
                + NsharpConstants.labelGap, NsharpConstants.filelistWidth,
                NsharpConstants.listHeight);
        // query to get and add available sounding models from DB
        prodList.setFont(newFont);
        createProdList();

        // create a selection listener to handle user's selection on list
        prodList.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event e) {
                if (prodList.getSelectionCount() > 0) {
                    currentProdName = prodList.getSelection()[0];
                    ldDia.setActiveGpdProdName(currentProdName);
                    createSndList();
                }
            }
        });

        timeBtn = new Button(topGp, SWT.CHECK | SWT.BORDER);
        timeBtn.setText("00Z and 12Z only");
        timeBtn.setEnabled(true);
        timeBtn.setFont(newFont);
        timeBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (timeLimit)
                    timeLimit = false;
                else
                    timeLimit = true;

                // refresh sounding list if file type is selected already
                if (sndTimeList != null && currentProdName.equals("") == false) {
                    createSndList();
                }

            }
        });
        // create file widget list
        sndTimeListGp = new Group(parent, SWT.SHADOW_ETCHED_IN);
        sndTimeListGp.setText("Sounding Times:");
        sndTimeListGp.setFont(newFont);
        sndTimeList = new org.eclipse.swt.widgets.List(sndTimeListGp,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        sndTimeList.setBounds(sndTimeListGp.getBounds().x
                + NsharpConstants.btnGapX, sndTimeListGp.getBounds().y
                + NsharpConstants.labelGap, NsharpConstants.listWidth,
                NsharpConstants.listHeight * 7);
        sndTimeList.setFont(newFont);

        // create a selection listener to handle user's selection on list
        sndTimeList.addListener(SWT.Selection, new Listener() {
            // private String selectedSndTime=null;
            public void handleEvent(Event e) {
                handleSndTimeSelection();
            }
        });

        if (currentProdName.equals("") == false) {
            sel[0] = currentProdName;
            prodList.setSelection(sel);
            createSndList();
            selectedTimeList = ldDia.getGpdSelectedTimeList();
            Object[] selTimeObjectArray = selectedTimeList.toArray();
            String[] selTimeStringArray = Arrays.copyOf(selTimeObjectArray,
                    selTimeObjectArray.length, String[].class);
            sndTimeList.setSelection(selTimeStringArray);
            handleSndTimeSelection();
        }
    }

    public void cleanup() {
        if (sndTimeList != null) {
            sndTimeList.removeListener(SWT.Selection,
                    sndTimeList.getListeners(SWT.Selection)[0]);
            sndTimeList.dispose();
            sndTimeList = null;
        }
        if (sndTimeListGp != null) {
            sndTimeListGp.dispose();
            sndTimeListGp = null;
        }
        if (prodList != null) {
            if (prodList.getListeners(SWT.Selection).length > 0)
                prodList.removeListener(SWT.Selection,
                        prodList.getListeners(SWT.Selection)[0]);
            prodList.dispose();
            prodList = null;
        }

        if (prodGp != null) {
            prodGp.dispose();
            prodGp = null;
        }

        NsharpLoadDialog ldDia = NsharpLoadDialog.getAccess();
        ldDia.cleanSndTypeList();
        if (topGp != null) {
            topGp.dispose();
            topGp = null;
        }
    }
    /*
     * public static NcSoundingTimeLines gpdSoundingTimeLineQuery (String
     * productName){ NcSoundingTimeLines timeLines = null;
     * GenericPointDataReqMsg reqMsg = new GenericPointDataReqMsg();
     * reqMsg.setReqType
     * (GenericPointDataReqType.GET_GPD_PRODUCT_TIMELINE_OBJECT);
     * reqMsg.setProductName(productName); try { Object rslts =
     * ThriftClient.sendRequest( reqMsg ); if( (rslts instanceof
     * NcSoundingTimeLines) ) { // timeLines = (NcSoundingTimeLines)rslts; }
     * else { System.out.println("Request Failed: ");
     * 
     * } } catch (Exception e) { // TODO Auto-generated catch block
     * e.printStackTrace(); }
     * 
     * 
     * return timeLines;
     * 
     * }
     */
}
