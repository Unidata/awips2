/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.view.PfcSoundingDialogContents
 * 
 * This java class performs the NSHARP NsharpLoadDialog functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 01/2011	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.view;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
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
//import gov.noaa.nws.ncep.viz.rsc.gpd.query.GpdQuery;

public class NsharpGpdPfcSoundingDialogContents {
    private Composite parent;

    private org.eclipse.swt.widgets.List prodList, availablefileList,
            sndTimeList;

    private Group topGp, prodGp, bottomGp, availableFileGp, sndTimeListGp;

    private Button namBtn, gfsBtn, timeBtn;

    private boolean timeLimit = false;

    private List<String> selectedFileList = new ArrayList<String>();

    private List<String> selectedTimeList = new ArrayList<String>();

    private List<NsharpStationInfo> stnPoints = new ArrayList<NsharpStationInfo>();

    private NcSoundingProfile.PfcSndType currentSndType = NcSoundingProfile.PfcSndType.NONE;

    private NsharpLoadDialog ldDia;

    private Font newFont;

    private String currentProdName = "";

    private String[] sel = new String[1];

    public NcSoundingProfile.PfcSndType getCurrentSndType() {
        return currentSndType;
    }

    public NsharpGpdPfcSoundingDialogContents() {
    }

    public NsharpGpdPfcSoundingDialogContents(Composite parent) {
        this.parent = parent;
        ldDia = NsharpLoadDialog.getAccess();
        newFont = ldDia.getNewFont();
    }

    private void createPFCAvailableFileList() {
        sndTimeList.removeAll();
        ;
        availablefileList.removeAll();
        // query using NcSoundingQuery class to query
        NcSoundingTimeLines timeLines = null;
        // TBDGPD NcSoundingTimeLines timeLines =
        // GpdQuery.getGpdSoundingTimeLines(currentProdName);
        if (timeLines != null && timeLines.getTimeLines() != null) {
            ldDia.startWaitCursor();
            for (Object timeLine : timeLines.getTimeLines()) {
                Timestamp reftime = (Timestamp) timeLine;
                if (reftime != null) {
                    // need to format reftime to GMT time string.
                    // Timestamp.toString produce a local time Not GMT time
                    Calendar cal = Calendar.getInstance(TimeZone
                            .getTimeZone("GMT"));
                    cal.setTimeInMillis(reftime.getTime());
                    String gmtTimeStr = String.format(
                            "%1$tY-%1$tm-%1$td %1$tH", cal);
                    // System.out.println("GMT time " + gmtTimeStr);
                    availablefileList.add(gmtTimeStr);

                }

            }
            ldDia.stopWaitCursor();
        } else
            System.out.println("SQL: query return null");
    }

    private void createPFCSndTimeList(List<String> selectedFlLst) {
        if (selectedFlLst.size() <= 0)
            return;
        sndTimeList.removeAll();
        ldDia.startWaitCursor();
        String sndStr = currentProdName;
        int endIndex = Math.min(3, sndStr.length());
        String dispSndStr = sndStr.substring(0, endIndex);
        DateFormatSymbols dfs = new DateFormatSymbols();
        String[] defaultDays = dfs.getShortWeekdays();
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

        for (int i = 0; i < selectedFlLst.size(); i++) {
            String fl = selectedFlLst.get(i);
            long reftimeMs = NcSoundingQuery.convertRefTimeStr(fl);
            // System.out.println("reftime="+fl + " in ms="+reftimeMs);
            NcSoundingTimeLines timeLines = null;
            // TBDGPD NcSoundingTimeLines timeLines =
            // GpdQuery.getGpdRangeStartTimes(currentProdName, fl);
            if (timeLines != null && timeLines.getTimeLines().length > 0) {
                for (Object obj : timeLines.getTimeLines()) {
                    Timestamp rangestart = (Timestamp) obj;

                    // need to format rangestart to GMT time string.
                    // Timestamp.toString produce a local time Not GMT time
                    cal.setTimeInMillis(rangestart.getTime());
                    long vHour = (cal.getTimeInMillis() - reftimeMs) / 3600000;
                    String dayOfWeek = defaultDays[cal
                            .get(Calendar.DAY_OF_WEEK)];
                    // String gmtTimeStr =
                    // String.format("%1$ty%1$tm%1$td/%1$tH%1$tMV%2$03d %3$s",
                    // cal, vHour,dispSndStr);
                    String gmtTimeStr = String.format(
                            "%1$ty%1$tm%1$td/%1$tH(%4$s)V%2$03d %3$s", cal,
                            vHour, dispSndStr, dayOfWeek);
                    if (sndTimeList.indexOf(gmtTimeStr) != -1) {
                        // this indicate that gmtTimeStr is already in the
                        // sndTimeList, then we dont need to add it to list
                        // again.
                        continue;
                    }

                    // System.out.println("GMT time " + gmtTimeStr);
                    if (!timeLimit)
                        sndTimeList.add(gmtTimeStr);
                    else {
                        int hour = cal.get(Calendar.HOUR_OF_DAY);
                        if ((hour == 0) || (hour == 12))
                            sndTimeList.add(gmtTimeStr);
                    }

                }
            }
        }
        ldDia.stopWaitCursor();
    }

    private void handleAvailFileListSelection() {
        String selectedFile = null;
        if (availablefileList.getSelectionCount() > 0) {
            selectedFileList.clear();
            for (int i = 0; i < availablefileList.getSelectionCount(); i++) {
                selectedFile = availablefileList.getSelection()[i];
                // System.out.println("selected sounding file is " +
                // selectedFile);
                selectedFileList.add(selectedFile);
            }
            ldDia.setPfcSelectedFileList(selectedFileList);
            createPFCSndTimeList(selectedFileList);
        }
    }

    private void handleSndTimeSelection() {
        String selectedSndTime = null;
        if (sndTimeList.getSelectionCount() > 0) {
            NsharpMapResource nsharpMapResource = NsharpMapResource
                    .getOrCreateNsharpMapResource();
            nsharpMapResource.setPoints(null);
            selectedTimeList.clear();
            ldDia.startWaitCursor();
            List<String> queriedTimeList = new ArrayList<String>();
            for (int i = 0; i < sndTimeList.getSelectionCount(); i++) {
                selectedSndTime = sndTimeList.getSelection()[i];
                selectedTimeList.add(selectedSndTime);
                int endIndex = selectedSndTime.indexOf(" ");
                String querySndTime = selectedSndTime.substring(0, endIndex);
                // System.out.println("selected sounding time is " +
                // selectedSndTime);
                // refTimeStr is same as "PFC file" name in Load dialog display
                String refTimeStr = NcSoundingQuery
                        .convertSoundTimeDispStringToForecastTime(querySndTime);
                // while rangeStartStr is same as "sounding Times
                String rangeStartStr = NcSoundingQuery
                        .convertSoundTimeDispStringToRangeStartTimeFormat(querySndTime);
                if (queriedTimeList.contains(refTimeStr) == true) {
                    // for all range start times of one reference time, they
                    // should have same stations, therefore, only
                    // query once, the rest will be just marked.
                    addStnPtWithoutQuery(refTimeStr, rangeStartStr,
                            querySndTime);
                } else {
                    queriedTimeList.add(refTimeStr);
                    queryAndMarkStn(refTimeStr, rangeStartStr, querySndTime);
                }
            }

            ldDia.stopWaitCursor();

            nsharpMapResource.setPoints(stnPoints);
            NsharpMapResource.bringMapEditorToTop();
        }
    }

    private void createProdList() {
        if (prodList != null)
            prodList.removeAll();
        if (sndTimeList != null)
            sndTimeList.removeAll();
        if (availablefileList != null)
            availablefileList.removeAll();
        ldDia.startWaitCursor();
        List<String> prodStrLst = null;
        // TBDGPD List<String> prodStrLst =
        // GpdQuery.getGpdAvailProducts(GenericPointDataReqType.GET_GPD_AVAILABLE_MODEL_SOUNDING_PRODUCTS);
        for (String prod : prodStrLst)
            prodList.add(prod);
        ldDia.stopWaitCursor();
    }

    public void createGpdPfcDialogContents() {
        topGp = new Group(parent, SWT.SHADOW_ETCHED_IN);
        topGp.setLayout(new GridLayout(2, false));

        currentSndType = ldDia.getActivePfcSndType();
        ldDia.createSndTypeList(topGp);

        prodGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
        prodGp.setText("Gpd Pfc Sounding List");
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
                    createPFCAvailableFileList();
                }
            }
        });
        timeBtn = new Button(parent, SWT.CHECK | SWT.BORDER);
        timeBtn.setText("00Z and 12Z only");
        timeBtn.setEnabled(true);
        // timeBtn.setBounds(fileTypeGp.getBounds().x+ NsharpConstants.btnGapX,
        // browseBtn.getBounds().y + browseBtn.getBounds().height+
        // NsharpConstants.btnGapY,
        // NsharpConstants.btnWidth,NsharpConstants.btnHeight);
        timeBtn.setFont(newFont);
        timeBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (timeLimit)
                    timeLimit = false;
                else
                    timeLimit = true;

                // refresh sounding list if file type is selected already
                if (!currentSndType.equals("NA") && selectedFileList.size() > 0) {
                    createPFCSndTimeList(selectedFileList);
                }

            }
        });

        bottomGp = new Group(parent, SWT.SHADOW_ETCHED_IN);
        bottomGp.setLayout(new GridLayout(2, false));

        availableFileGp = new Group(bottomGp, SWT.SHADOW_ETCHED_IN);
        availableFileGp.setText("Available PFC files:");
        availableFileGp.setFont(newFont);
        availablefileList = new org.eclipse.swt.widgets.List(availableFileGp,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        availablefileList.setBounds(availableFileGp.getBounds().x,
                availableFileGp.getBounds().y + NsharpConstants.labelGap,
                NsharpConstants.filelistWidth,
                NsharpConstants.listHeight * 36 / 5);
        // create a selection listener to handle user's selection on list
        availablefileList.setFont(newFont);
        availablefileList.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event e) {
                handleAvailFileListSelection();
            }
        });

        // create Sounding Times widget list
        sndTimeListGp = new Group(bottomGp, SWT.SHADOW_ETCHED_IN);
        sndTimeListGp.setText("Sounding Times:");
        sndTimeListGp.setFont(newFont);
        sndTimeList = new org.eclipse.swt.widgets.List(sndTimeListGp,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        sndTimeList.removeAll();
        sndTimeList.setFont(newFont);
        sndTimeList.setBounds(sndTimeListGp.getBounds().x,
                sndTimeListGp.getBounds().y + NsharpConstants.labelGap,
                NsharpConstants.listWidth, NsharpConstants.listHeight * 36 / 5);
        sndTimeList.addListener(SWT.Selection, new Listener() {
            // private String selectedSndTime=null;
            public void handleEvent(Event e) {
                handleSndTimeSelection();
            }

        });
        if (currentProdName.equals("") == false) {
            sel[0] = currentProdName;
            prodList.setSelection(sel);
            createPFCAvailableFileList();
            selectedFileList = ldDia.getPfcSelectedFileList();
            Object[] selFileObjectArray = selectedFileList.toArray();
            String[] selFileStringArray = Arrays.copyOf(selFileObjectArray,
                    selFileObjectArray.length, String[].class);
            availablefileList.setSelection(selFileStringArray);
            handleAvailFileListSelection();
            selectedTimeList = ldDia.getPfcSelectedTimeList();
            Object[] selTimeObjectArray = selectedTimeList.toArray();
            String[] selTimeStringArray = Arrays.copyOf(selTimeObjectArray,
                    selTimeObjectArray.length, String[].class);
            sndTimeList.setSelection(selTimeStringArray);
            handleSndTimeSelection();
        }
    }

    private void addStnPtWithoutQuery(String refTimeStr, String rangeStartStr,
            String selectedSndTime) {
        long reftimeMs = NcSoundingQuery.convertRefTimeStr(refTimeStr);
        Timestamp refTime = new Timestamp(reftimeMs);
        for (NsharpStationInfo stn : stnPoints) {
            if (refTime.equals(stn.getReftime()) == true) {
                long rangetimeMs = NcSoundingQuery
                        .convertRefTimeStr(rangeStartStr);
                Timestamp rangeStartTime = new Timestamp(rangetimeMs);
                NsharpStationInfo.timeLineSpecific timeLinsSpc = stn.new timeLineSpecific();
                String sndTypeStr = currentProdName;
                int endIndex = Math.min(
                        NsharpConstants.MAX_SOUNDING_SOURCE_STR_LENGTH,
                        sndTypeStr.length());
                String dispInfo = stn.getStnId() + " " + selectedSndTime + " "
                        + sndTypeStr.substring(0, endIndex);
                timeLinsSpc.setDisplayInfo(dispInfo);
                timeLinsSpc.setTiemLine(rangeStartTime);
                stn.addToTimeLineSpList(timeLinsSpc);
            }
        }
        // System.out.println("addStnPtWithoutQuery stn num ="+
        // stnPoints.size()+ " for pfc refTime(file) "+refTimeStr);
    }

    private void queryAndMarkStn(String refTimeStr, String rangeStartStr,
            String selectedSndTime) {
        // use NcSoundingQuery to query stn info
        String sndTypeStr = currentProdName;
        NcSoundingStnInfoCollection sndStnInfoCol = null;
        // TBDGPD NcSoundingStnInfoCollection sndStnInfoCol
        // =GpdQuery.getGpdStationInfoCollection(currentProdName,refTimeStr,rangeStartStr);
        if (sndStnInfoCol != null && sndStnInfoCol.getStationInfo() != null) {

            NcSoundingStnInfo[] stnInfoAry = sndStnInfoCol.getStationInfo();
            System.out.println("gpdpfc station number = " + stnInfoAry.length);
            // System.out.println("queryAndMarkStn stn num ="+
            // stnInfoAry.length+ " for pfc refTime(file) "+refTimeStr);
            for (int i = 0; i < stnInfoAry.length; i++) {
                NcSoundingStnInfo stnInfo = stnInfoAry[i];
                NsharpStationInfo stn = new NsharpStationInfo();
                NsharpStationInfo.timeLineSpecific timeLinsSpc = stn.new timeLineSpecific();

                int endIndex = Math.min(
                        NsharpConstants.MAX_SOUNDING_SOURCE_STR_LENGTH,
                        sndTypeStr.length());
                String packedStnIdStr = stnInfo.getStnId().replace(" ", "_");
                String dispInfo = packedStnIdStr + " " + selectedSndTime + " "
                        + sndTypeStr.substring(0, endIndex);
                timeLinsSpc.setDisplayInfo(dispInfo);
                timeLinsSpc.setTiemLine(stnInfo.getRangeStartTime());
                stn.addToTimeLineSpList(timeLinsSpc);
                stn.setLongitude(stnInfo.getStationLongitude());
                stn.setLatitude(stnInfo.getStationLatitude());
                stn.setReftime(stnInfo.getSynopTime());
                stn.setStnId(stnInfo.getStnId());
                stn.setSndType(sndTypeStr);
                // if(i <10)
                // System.out.println(
                // "disP="+dispInfo+" refT= "+stnInfo.getSynopTime()+
                // " rangSt="+stnInfo.getRangeStartTime());
                stnPoints.add(stn);
            }

        }
    }

    public void cleanup() {
        if (namBtn != null && namBtn.isDisposed() == false) {
            namBtn.removeListener(SWT.MouseUp,
                    namBtn.getListeners(SWT.MouseUp)[0]);
            namBtn.dispose();
            namBtn = null;
        }
        if (gfsBtn != null) {
            gfsBtn.removeListener(SWT.MouseUp,
                    gfsBtn.getListeners(SWT.MouseUp)[0]);
            gfsBtn.dispose();
            gfsBtn = null;
        }
        NsharpLoadDialog ldDia = NsharpLoadDialog.getAccess();
        ldDia.cleanSndTypeList();
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
        if (topGp != null) {
            topGp.dispose();
            topGp = null;
        }

        if (timeBtn != null) {
            timeBtn.removeListener(SWT.MouseUp,
                    timeBtn.getListeners(SWT.MouseUp)[0]);
            timeBtn.dispose();
            timeBtn = null;
        }

        if (availablefileList != null) {
            availablefileList.removeListener(SWT.Selection,
                    availablefileList.getListeners(SWT.Selection)[0]);
            availablefileList.dispose();
            availablefileList = null;
        }

        if (availableFileGp != null) {
            availableFileGp.dispose();
            availableFileGp = null;
        }
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
        if (bottomGp != null) {
            bottomGp.dispose();
            bottomGp = null;
        }
    }
}
