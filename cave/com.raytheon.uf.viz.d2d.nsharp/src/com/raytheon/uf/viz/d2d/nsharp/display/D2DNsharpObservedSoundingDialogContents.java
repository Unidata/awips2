/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.view.ObservedSoundingDialogContents
 * 
 * This java class performs the NSHARP D2DNsharpLoadDialog functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 01/2011	    229			Chin Chen	Initial coding
 * 09/14/2011   457         S. Gurung   Renamed H5UAIR to NCUAIR
 * 07202015     RM#9173     Chin Chen   use NcSoundingQuery.genericSoundingDataQuery() to query grid model sounding data
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package com.raytheon.uf.viz.d2d.nsharp.display;

import gov.noaa.nws.ncep.viz.soundingrequest.NcSoundingQuery;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfo;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import com.raytheon.uf.viz.d2d.nsharp.display.map.D2DNsharpMapResource;

import java.sql.Timestamp;
import java.text.DateFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;

public class D2DNsharpObservedSoundingDialogContents {
    private Composite parent;

    private org.eclipse.swt.widgets.List sndTimeList;

    private Group sndTimeListGp, topGp;

    private boolean timeLimit = false;

    private boolean rawData = false;

    private Button timeBtn, bufruaBtn, uairBtn, rawBtn;

    private String FILE_UAIR = "UAIR";

    private String FILE_BUFRUA = "BUFRUA";

    // private String FILE_DROP = "DROP";
    private NcSoundingProfile.ObsSndType currentSndType = NcSoundingProfile.ObsSndType.NONE;

    private D2DNsharpLoadDialog ldDia;

    private ArrayList<String> selectedTimeList = new ArrayList<String>();

    private Font newFont;

    public boolean isRawData() {
        return rawData;
    }

    public NcSoundingProfile.ObsSndType getCurrentSndType() {
        return currentSndType;
    }

    public D2DNsharpObservedSoundingDialogContents(Composite parent) {
        this.parent = parent;
        ldDia = D2DNsharpLoadDialog.getAccess();
        newFont = ldDia.getNewFont();
    }

    private void createObsvdSndUairList() {
        sndTimeList.removeAll();

        // use NcSoundingQuery to query
        NcSoundingTimeLines timeLines = NcSoundingQuery
                .soundingTimeLineQuery(currentSndType.toString());

        if (timeLines != null && timeLines.getTimeLines() != null) {
            DateFormatSymbols dfs = new DateFormatSymbols();
            String[] defaultDays = dfs.getShortWeekdays();
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            ldDia.startWaitCursor();
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
                            currentSndType.toString(), dayOfWeek);
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
            ldDia.stopWaitCursor();
        } else
            System.out.println("EDEX timeline query return null");

    }

    private void queryAndMarkStn(String selectedSndTime) {
        String selectTimetr = NcSoundingQuery
                .convertSoundTimeDispStringToRangeStartTimeFormat(selectedSndTime);
        D2DNsharpMapResource nsharpMapResource = D2DNsharpMapResource
                .getOrCreateNsharpMapResource();
        // Chin float lat, lon;
        double lat, lon;
        String stnInfoStr;

        // use NcSoundingQuery to query stn info
        NcSoundingStnInfoCollection sndStnInfoCol = NcSoundingQuery.genericSoundingStnInfoQuery(currentSndType.toString(),null, selectTimetr) ;
               // .soundingStnInfoQuery(currentSndType.toString(), selectTimetr);
        if (sndStnInfoCol != null && sndStnInfoCol.getStationInfo() != null) {

            NcSoundingStnInfo[] stnInfoAry = sndStnInfoCol.getStationInfo();
            // System.out.println("obs station number = "+ stnInfoAry.length );
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
                stn.setStnDisplayInfo(packedStnInfoStr + " " + selectedSndTime
                        + " " + currentSndType.toString());
                stn.setLongitude(lon);
                stn.setLatitude(lat);
                stn.setStnId(stnInfoStr);
                stn.setReftime(synoptictime);
                stn.setRangestarttime(synoptictime);
                stn.setSndType(currentSndType.toString());
                // System.out.println("sndType= "+currentSndType);
                // System.out.println("stn  lat ="+stn.getLatitude() +
                // " lon="+stn.getLongitude());
                nsharpMapResource.addPoint(stn);
            }

            D2DNsharpMapResource.bringMapEditorToTop();
        }
    }

    private void handleSndTimeSelection() {
        String selectedSndTime = null;
        if (sndTimeList.getSelectionCount() > 0) {
            D2DNsharpMapResource nsharpMapResource = D2DNsharpMapResource
                    .getOrCreateNsharpMapResource();// D2DNsharpLoadDialog.getAccess().getD2DNsharpMapResource();
            nsharpMapResource.setPoints(null);
            selectedTimeList.clear();
            ldDia.startWaitCursor();
            for (int i = 0; i < sndTimeList.getSelectionCount(); i++) {
                selectedSndTime = sndTimeList.getSelection()[i];
                selectedTimeList.add(selectedSndTime);
                int endIndex = selectedSndTime.indexOf(" ");
                String queryingSndTime = selectedSndTime.substring(0, endIndex);
                queryAndMarkStn(queryingSndTime);

            }
            ldDia.setObsSelectedTimeList(selectedTimeList);
            ldDia.stopWaitCursor();
        }
    }

    public void createObsvdDialogContents() {
        currentSndType = ldDia.getActiveObsSndType();
        timeLimit = false;
        rawData = false;
        topGp = new Group(parent, SWT.SHADOW_ETCHED_IN);
        topGp.setLayout(new GridLayout(2, false));
        
        bufruaBtn = new Button(topGp, SWT.RADIO | SWT.BORDER);
        bufruaBtn.setText(FILE_BUFRUA);
        bufruaBtn.setEnabled(true);
        bufruaBtn.setBounds(topGp.getBounds().x + NsharpConstants.btnGapX,
        		topGp.getBounds().y + NsharpConstants.labelGap,
                NsharpConstants.btnWidth, NsharpConstants.btnHeight);
        bufruaBtn.setFont(newFont);
        bufruaBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                sndTimeList.removeAll();
                currentSndType = NcSoundingProfile.ObsSndType.BUFRUA;
                ldDia.setActiveObsSndType(currentSndType);
                createObsvdSndUairList();
            }
        });

        uairBtn = new Button(topGp, SWT.RADIO | SWT.BORDER);
        uairBtn.setText(FILE_UAIR);
        uairBtn.setEnabled(true);
        uairBtn.setBounds(topGp.getBounds().x + NsharpConstants.btnGapX,
        		bufruaBtn.getBounds().y + bufruaBtn.getBounds().height
                + NsharpConstants.btnGapY, NsharpConstants.btnWidth,
                NsharpConstants.btnHeight);
        uairBtn.setFont(newFont);
        uairBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                sndTimeList.removeAll();
                currentSndType = NcSoundingProfile.ObsSndType.NCUAIR;
                ldDia.setActiveObsSndType(currentSndType);
                createObsvdSndUairList();
            }
        });

        timeBtn = new Button(topGp, SWT.CHECK | SWT.BORDER);
        timeBtn.setText("00Z and 12Z only");
        timeBtn.setEnabled(true);
        timeBtn.setFont(newFont);
        timeBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (timeBtn.getSelection())
                    timeLimit = true;
                else
                    timeLimit = false;
                // refresh sounding list if file type is selected already
                if (currentSndType == NcSoundingProfile.ObsSndType.NCUAIR
                        || currentSndType == NcSoundingProfile.ObsSndType.BUFRUA) {
                    createObsvdSndUairList();
                }

            }
        });
        rawBtn = new Button(topGp, SWT.CHECK | SWT.BORDER);
        rawBtn.setText("raw data");
        rawBtn.setEnabled(true);
        rawBtn.setBounds(timeBtn.getBounds().x + timeBtn.getBounds().width,
                timeBtn.getBounds().y, timeBtn.getBounds().width,
                timeBtn.getBounds().height);
        rawBtn.setFont(newFont);
        rawBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (rawBtn.getSelection())
                    rawData = true;
                else
                    rawData = false;
                ;
            }
        });
        // create file widget list
        sndTimeListGp = new Group(parent, SWT.SHADOW_NONE);

        sndTimeListGp.setFont(newFont);
        sndTimeList = new org.eclipse.swt.widgets.List(sndTimeListGp,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        sndTimeList.setBounds(topGp.getBounds().x + NsharpConstants.btnGapX,
                sndTimeListGp.getBounds().y + NsharpConstants.labelGap,
                D2DNsharpLoadDialog.getDialogWidth()-40, NsharpConstants.listHeight * 6);
        sndTimeList.setFont(newFont);

        // create a selection listener to handle user's selection on list
        sndTimeList.addListener(SWT.Selection, new Listener() {
            // private String selectedSndTime=null;
            public void handleEvent(Event e) {
                handleSndTimeSelection();
            }
        });
        

        if (currentSndType == NcSoundingProfile.ObsSndType.NCUAIR
                || currentSndType == NcSoundingProfile.ObsSndType.BUFRUA) {
            if (currentSndType == NcSoundingProfile.ObsSndType.NCUAIR)
                uairBtn.setSelection(true);
            else
                bufruaBtn.setSelection(true);
            createObsvdSndUairList();
            selectedTimeList = ldDia.getObsSelectedTimeList();
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
        if (timeBtn != null) {
            timeBtn.removeListener(SWT.MouseUp,
                    timeBtn.getListeners(SWT.MouseUp)[0]);
            timeBtn.dispose();
            timeBtn = null;
        }
        if (rawBtn != null) {
            rawBtn.removeListener(SWT.MouseUp,
                    rawBtn.getListeners(SWT.MouseUp)[0]);
            rawBtn.dispose();
            rawBtn = null;
        }
        /*
         * if(browseBtn != null){ browseBtn.removeListener(SWT.MouseUp,
         * browseBtn.getListeners(SWT.MouseUp)[0]); browseBtn.dispose();
         * browseBtn = null; }
         * 
         * 
         * if(tamBtn != null){ tamBtn.removeListener(SWT.MouseUp,
         * tamBtn.getListeners(SWT.MouseUp)[0]); tamBtn.dispose(); tamBtn =
         * null; }
         */
        if (bufruaBtn != null) {
            bufruaBtn.removeListener(SWT.MouseUp,
                    bufruaBtn.getListeners(SWT.MouseUp)[0]);
            bufruaBtn.dispose();
            bufruaBtn = null;
        }
        if (uairBtn != null) {
            uairBtn.removeListener(SWT.MouseUp,
                    uairBtn.getListeners(SWT.MouseUp)[0]);
            uairBtn.dispose();
            uairBtn = null;
        }
        /*
         * if(newTabBtn != null){ newTabBtn.removeListener(SWT.MouseUp,
         * newTabBtn.getListeners(SWT.MouseUp)[0]); newTabBtn.dispose();
         * newTabBtn = null; }
         */
        D2DNsharpLoadDialog ldDia = D2DNsharpLoadDialog.getAccess();
        if (topGp != null) {
            topGp.dispose();
            topGp = null;
        }
    }
}
