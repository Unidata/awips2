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
package com.raytheon.uf.viz.d2d.nsharp.tool;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.SurfaceStationPointData;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpModelSoundingQuery;
import gov.noaa.nws.ncep.ui.nsharp.view.AbstractMdlSoundingDlgContents;

/**
 *
 * The class for D2D NSHARP Model Sounding Dialog Contents
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 20, 2020  73172    smanoj   Initial creation
 * Jun 25, 2020  80230    smanoj   Fixing some errors and enhancements.
 * Jul 17, 2020  80915    smanoj   Added queryLimit for NSHARP time queries.
 * Jan 27, 2021  87346    smanoj   Model Soundings, "Model Type" should be empty when
 *                                 gribModelTypeList is empty in the nsharpConfig.xml.
 * Oct 13, 2022  8946     mapeters Handle VizGlobalsManager.getProperty() method
 *                                 rename
 *
 * </pre>
 *
 * @author smanoj
 */
public class D2DNsharpMdlSoundingDlgContents
        extends AbstractMdlSoundingDlgContents {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DNsharpMdlSoundingDlgContents.class);

    private List<String> availableFiles = new ArrayList<>();

    private List<String> selectedTimes = new ArrayList<>();

    private int queryLimit;

    public D2DNsharpMdlSoundingDlgContents(Composite parent) {
        super(parent);
        ldDia = D2DNsharpLoadDialog.getAccess();
        newFont = ldDia.getNewFont();

        // get the number of Frames set to display in D2D.
        queryLimit = (int) (VizGlobalsManager.getCurrentInstance()
                .getProperty(VizConstants.FRAMES_ID));
        try {
            createModelTypeToRscDefNameMapping();
        } catch (VizException e) {
            statusHandler.error(
                    "Exception while createModelTypeToRscDefNameMapping.", e);
        }
    }

    @Override
    public void createMdlDialogContents() {
        topGp = new Group(parent, SWT.SHADOW_ETCHED_IN);
        topGp.setLayout(new GridLayout(2, false));
        selectedModelType = ldDia.getActiveMdlSndMdlType();
        selectedRscDefName = rscDefNameToGridModelMap.get(selectedModelType);
        ldDia.createSndTypeList(topGp);

        modelTypeGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
        modelTypeGp.setText("Model Type");
        modelTypeGp.setFont(newFont);
        modelTypeRscDefNameList = new org.eclipse.swt.widgets.List(modelTypeGp,
                SWT.BORDER | SWT.V_SCROLL);
        modelTypeRscDefNameList.setBounds(modelTypeGp.getBounds().x,
                modelTypeGp.getBounds().y + NsharpConstants.labelGap,
                NsharpConstants.filelistWidth, NsharpConstants.listHeight);
        // query to get and add available sounding models from DB
        modelTypeRscDefNameList.setFont(newFont);
        createModelTypeList();

        // create a selection listener to handle user's selection on list
        modelTypeRscDefNameList.addListener(SWT.Selection, e -> {
            if (modelTypeRscDefNameList.getSelectionCount() > 0) {
                selectedRscDefName = modelTypeRscDefNameList.getSelection()[0];
                // convert selectedModel, in resource definition name, to
                // grid model type name
                selectedModelType = rscDefNameToGridModelMap
                        .get(selectedRscDefName);
                ldDia.setActiveMdlSndMdlType(selectedModelType);
                createMDLAvailableFileList(selectedModelType);
                createMDLSndTimeList(availableFiles, selectedRscDefName,
                        selectedModelType);
                ldDia.getMapRsc().getOrCreateNsharpMapResource()
                        .setPoints(null);
                ldDia.getMapRsc().bringMapEditorToTop();
            }
        });

        locationMainGp = new Group(parent, SWT.SHADOW_ETCHED_IN);
        locationMainGp.setLayout(new GridLayout(5, false));
        locationMainGp.setText("Location");
        locationMainGp.setFont(newFont);
        latlonBtn = new Button(locationMainGp, SWT.RADIO | SWT.BORDER);
        latlonBtn.setText("Lat/Lon");
        latlonBtn.setFont(newFont);
        latlonBtn.setEnabled(true);
        latlonBtn.setSelection(true);
        latlonBtn.addListener(SWT.MouseUp, event -> {
            setCurrentLocType(LocationType.LATLON);
            locationText.setText("");
        });
        stationBtn = new Button(locationMainGp, SWT.RADIO | SWT.BORDER);
        stationBtn.setText("Station");
        stationBtn.setEnabled(true);
        stationBtn.setFont(newFont);
        stationBtn.addListener(SWT.MouseUp, event -> {
            setCurrentLocType(LocationType.STATION);
            locationText.setText("");
        });
        locationLbl = new Label(locationMainGp, SWT.NONE | SWT.BORDER);
        locationLbl.setText("Location:");
        locationLbl.setFont(newFont);
        locationText = new Text(locationMainGp, SWT.BORDER | SWT.SINGLE);
        GridData data1 = new GridData(SWT.FILL, SWT.FILL, true, true);
        locationText.setLayoutData(data1);
        locationText.setTextLimit(MAX_LOCATION_TEXT);
        locationText.setFont(newFont);
        locationText.addListener(SWT.Verify, e -> {
            String userInputStr = e.text;
            if (userInputStr.length() > 0) {

                if (currentLocType == LocationType.LATLON) {
                    // to make sure user enter digits and separated by ";"
                    // or ","only, if lat/lon is used
                    if (userInputStr.length() == 1) {
                        char inputChar = userInputStr.charAt(0);
                        if ((('0' > inputChar) || (inputChar > '9'))
                                && (inputChar != ';') && (inputChar != ',')
                                && (inputChar != '-') && (inputChar != '.')) {
                            e.doit = false;
                        }
                    }
                } else {
                    // do nothing when station type

                }
            }
        });

        loadBtn = new Button(locationMainGp, SWT.PUSH);
        loadBtn.setText("Load ");
        loadBtn.setFont(newFont);
        loadBtn.setEnabled(true);
        loadBtn.setBounds(
                locationMainGp.getBounds().x + NsharpConstants.btnGapX,
                locationLbl.getBounds().y + locationLbl.getBounds().height
                        + NsharpConstants.btnGapY,
                NsharpConstants.btnWidth, NsharpConstants.btnHeight);
        loadBtn.addListener(SWT.MouseUp, event -> {
            D2DNsharpLoadDialog ldDia = D2DNsharpLoadDialog.getAccess();

            // TODO should this be "if null or empty"?
            if (selectedTimes != null && selectedTimes.isEmpty()) {
                statusHandler.handle(Priority.WARN,
                        "Data not Available/Selected to load.");
                return;
            }
            String textStr = locationText.getText();
            if ((textStr != null) && !(textStr.isEmpty())) {
                if (currentLocType == LocationType.LATLON) {
                    // to make sure user enter digits and separated by ";"
                    // or ","only, if lat/lon is used
                    int dividerIndex = textStr.indexOf(';');
                    boolean indexFound = false;
                    if (dividerIndex != -1) {
                        indexFound = true;
                    }
                    if (!indexFound) {
                        dividerIndex = textStr.indexOf(',');
                        if (dividerIndex != -1) {
                            indexFound = true;
                        }
                    }
                    if (indexFound) {
                        try {
                            lat = Float.parseFloat(
                                    textStr.substring(0, dividerIndex));
                            lon = Float.parseFloat(
                                    textStr.substring(dividerIndex + 1));
                            if ((lat > 90) || (lat < -90) || (lon > 180)
                                    || (lon < -180)) {
                                statusHandler.handle(Priority.WARN,
                                        "lat/lon out of range (" + textStr
                                                + ") entered!\n"
                                                + GOOD_LATLON_STR);
                                locationText.setText("");
                                return;
                            }
                            ldDia.startWaitCursor();
                            NsharpEditor skewtEdt1 = NsharpEditor
                                    .createOrOpenEditor();
                            NsharpModelSoundingQuery qryAndLd1 = new NsharpModelSoundingQuery(
                                    "Querying Sounding Data...", queryLimit);
                            qryAndLd1.queryAndLoadData(false, skewtEdt1,
                                    soundingLysLstMap, selectedTimes,
                                    timeLineToFileMap, lat, lon, stnStr,
                                    selectedModelType, selectedRscDefName);
                            ldDia.stopWaitCursor();
                            ldDia.close();
                        } catch (Exception e) {
                            statusHandler.error(
                                    "Exception while parsing string to float.",
                                    e);
                            return;
                        }

                    } else {
                        statusHandler.handle(Priority.WARN, "Bad lat/lon ("
                                + textStr + ") entered!\n" + GOOD_LATLON_STR);
                        locationText.setText("");
                    }
                } else if (currentLocType == LocationType.STATION) {
                    // query station lat /lon
                    try {
                        // user may start with a space before enter station
                        // id
                        textStr = textStr.trim();
                        stnStr = textStr.toUpperCase(Locale.getDefault());
                        Coordinate co = SurfaceStationPointData
                                .getStnCoordinate(stnStr);
                        lat = (float) co.y;
                        lon = (float) co.x;
                        if (lat == SurfaceStationPointData.DEFAULT_LATLON) {
                            statusHandler.handle(Priority.WARN,
                                    "Bad station id (" + textStr
                                            + ") entered!\n" + GOOD_STN_STR);
                            locationText.setText("");
                            return;
                        }
                        ldDia.startWaitCursor();
                        NsharpEditor skewtEdt2 = NsharpEditor
                                .createOrOpenEditor();
                        NsharpModelSoundingQuery qryAndLd2 = new NsharpModelSoundingQuery(
                                "Querying Sounding Data...", queryLimit);
                        qryAndLd2.queryAndLoadData(true, skewtEdt2,
                                soundingLysLstMap, selectedTimes,
                                timeLineToFileMap, lat, lon, stnStr,
                                selectedModelType, selectedRscDefName);
                        ldDia.stopWaitCursor();
                        ldDia.close();
                    } catch (Exception e) {
                        statusHandler.error(
                                "Exception while parsing string to float.", e);
                        return;
                    }
                }
            }
        });
    }

    @Override
    public void cleanup() {
        if (modelTypeRscDefNameList != null) {
            if (modelTypeRscDefNameList
                    .getListeners(SWT.Selection).length > 0) {
                modelTypeRscDefNameList.removeListener(SWT.Selection,
                        modelTypeRscDefNameList.getListeners(SWT.Selection)[0]);
            }
            modelTypeRscDefNameList.dispose();
            modelTypeRscDefNameList = null;
        }
        if (modelTypeGp != null) {
            modelTypeGp.dispose();
            modelTypeGp = null;
        }

        D2DNsharpLoadDialog ldDia = D2DNsharpLoadDialog.getAccess();
        ldDia.cleanSndTypeList();
        ldDia.getMapRsc().setPoints(null);

        if (bottomGp != null) {
            bottomGp.dispose();
            bottomGp = null;
        }
        if (topGp != null) {
            topGp.dispose();
            topGp = null;
        }

        if (loadBtn != null) {
            loadBtn.removeListener(SWT.MouseUp,
                    loadBtn.getListeners(SWT.MouseUp)[0]);
            loadBtn.dispose();
            loadBtn = null;
        }
        if (stationBtn != null) {
            stationBtn.removeListener(SWT.MouseUp,
                    stationBtn.getListeners(SWT.MouseUp)[0]);
            stationBtn.dispose();
            stationBtn = null;
        }
        if (latlonBtn != null) {
            latlonBtn.removeListener(SWT.MouseUp,
                    latlonBtn.getListeners(SWT.MouseUp)[0]);
            latlonBtn.dispose();
            latlonBtn = null;
        }
        if (locationText != null) {
            locationText.removeListener(SWT.Verify,
                    locationText.getListeners(SWT.Verify)[0]);
            locationText.dispose();
            locationText = null;
        }

        if (locationLbl != null) {
            locationLbl.dispose();
            locationLbl = null;
        }
        if (locationMainGp != null) {
            locationMainGp.dispose();
            locationMainGp = null;
        }
        if (!this.selectedTimes.isEmpty()) {
            this.selectedTimes.clear();
        }
        if (!this.availableFiles.isEmpty()) {
            this.availableFiles.clear();
        }
        if (!this.timeLineToFileMap.isEmpty()) {
            this.timeLineToFileMap.clear();
        }
    }

    private void createModelTypeList() {
        if (modelTypeRscDefNameList != null) {
            modelTypeRscDefNameList.removeAll();
        }
        if (selectedTimes != null) {
            selectedTimes.clear();
        }
        if (availableFiles != null) {
            availableFiles.clear();
        }
        ldDia.startWaitCursor();
        NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
        NsharpConfigStore configStore = configMgr
                .retrieveNsharpConfigStoreFromFs();
        NsharpGraphProperty graphConfigProperty = configStore
                .getGraphProperty();
        List<String> cfgList = graphConfigProperty.getGribModelTypeList();
        DbQueryRequest dbQuery = new DbQueryRequest();
        dbQuery.setEntityClass(GridInfoRecord.class);
        dbQuery.setDistinct(true);
        dbQuery.addRequestField(GridInfoConstants.DATASET_ID);

        try {
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(dbQuery);
            String[] models = response.getFieldObjects(
                    GridInfoConstants.DATASET_ID, String.class);
            Arrays.sort(models, String.CASE_INSENSITIVE_ORDER);

            for (String modelName : models) {
                String rscDefName = gridModelToRscDefNameMap.get(modelName);
                if (cfgList != null && !cfgList.isEmpty()) {
                    if (cfgList.contains(rscDefName)) {
                        if (rscDefName != null) {
                            modelTypeRscDefNameList.add(rscDefName);
                        }
                    }
                }
            }

        } catch (VizException e) {
            statusHandler.error("Exception occured loading available models",
                    e);
        }
        ldDia.stopWaitCursor();
    }

    @Override
    protected void setAvailableFileList(List<String> availableFiles) {
        this.availableFiles = availableFiles;
    }

    @Override
    protected void setSndTimeList(List<String> selectedTimes) {
        this.selectedTimes = selectedTimes;
    }

    @Override
    protected void setTimeLineToFileMap(Map<String, String> timeLineToFileMap) {
        this.timeLineToFileMap = timeLineToFileMap;
    }

}