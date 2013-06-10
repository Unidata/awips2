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
package com.raytheon.viz.mpe.ui.dialogs.gagetable;

import java.awt.Point;
import java.awt.Rectangle;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.Vector;

import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPEGageData;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Gage Table Data Manager class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2009 2476       mpduff      Initial creation.
 * Jan 28, 2010 4415       mpduff      Fixed problem with column 
 *                                       header creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GageTableDataManager {
    private static GageTableDataManager instance = null;

    private static final SimpleDateFormat sdf;

    // The grid data
    private short[][] rMosaic = null;

    private short[][] avgrMosaic = null;

    private short[][] maxrMosaic = null;

    private short[][] bMosaic = null;

    private short[][] lMosaic = null;

    private short[][] gageOnly = null;

    private short[][] satPrecip = null;

    private short[][] lsatPrecip = null;

    private short[][] mMosaic = null;

    private short[][] mlMosaic = null;

    private short[][] p3Mosaic = null;

    private short[][] xmrg = null;// MPE_Bestqpe

    private short[][] rfcMosaic = null;

    private short[][] srMosaic = null;

    private short[][] sgMosaic = null;

    private short[][] srgMosaic = null;

    private short[][] rfcbMosaic = null;

    private short[][] rfcmMosaic = null;

    private short[][] qMosaic = null;

    private short[][] lqMosaic = null;

    private short[][] mlqMosaic = null;

    private short[] index = null;

    /**
     * Array of radar ids.
     */
    private String[] radarIds = null;

    /**
     * MPEGageData list.
     */
    private List<MPEGageData> mpeGageDataList = null;

    /**
     * The date of the data.
     */
    private Date dataDate = SimulatedTime.getSystemTime().getTime();

    /**
     * GageTableColumn list.
     */
    private List<GageTableColumn> columnDataList = null;

    /**
     * Rows Vector.
     */
    private Vector<Vector<String>> rows = null;

    /**
     * Columns Vector.
     */
    private Vector<String> columns = null;

    /**
     * Column Setting object.
     */
    private GageTableSortSettings columnSettings = null;

    /**
     * List of MPE Gage Data Records.
     */
    private List<MPEGageData> gageRecordList;

    /**
     * List of GageTableRowData objects.
     */
    private Set<GageTableRowData> gageTableRowList = null;

    /**
     * HashMap of column widths.
     */
    private Map<String, Integer> columnWidthMap = new HashMap<String, Integer>();

    /**
     * Data format.
     */
    private static final String format = "%5.2f";

    /**
     * Array of column names.
     */
    private String[] colArray = new String[1];

    /**
     * The selected grid.
     */
    private String selectedGrid = null;

    static {
        sdf = new SimpleDateFormat("yyyyMMddHH");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Private constructor
     */
    private GageTableDataManager() {

    }

    /**
     * Get an instance of the GageTableDataManager class.
     * 
     * @return an instance of this class
     */
    public static synchronized GageTableDataManager getInstance() {
        if (instance == null) {
            instance = new GageTableDataManager();
        }

        return instance;
    }

    /**
     * Get the list of Radar Ids.
     * 
     * @return the radarIds
     * @throws VizException
     */
    public String[] getRadarIds() throws VizException {
        if (radarIds == null) {
            String query = "select radid from radarloc order by radid asc";
            List<Object[]> rs = DirectDbQuery.executeQuery(query,
                    HydroConstants.IHFS, QueryLanguage.SQL);

            radarIds = new String[rs.size()];
            for (int i = 0; i < rs.size(); i++) {
                Object[] oa = rs.get(i);
                radarIds[i] = (String) oa[0];
            }
        }

        return radarIds;
    }

    /**
     * Get the list of Radar Ids.
     * 
     * @return the radarIds
     * @throws VizException
     */
    public String[] getActiveRadarIds() throws VizException {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String fxa_local_site = appsDefaults.getToken("fxa_local_site");
        if (radarIds == null) {
            String query = "select radid from radarloc where office_id = '"
                    + fxa_local_site + "' and use_radar='T' order by radid asc";
            List<Object[]> rs = DirectDbQuery.executeQuery(query,
                    HydroConstants.IHFS, QueryLanguage.SQL);

            radarIds = new String[rs.size()];
            for (int i = 0; i < rs.size(); i++) {
                Object[] oa = rs.get(i);
                radarIds[i] = (String) oa[0];
            }
        }

        return radarIds;
    }

    /**
     * Lookup the Radar Id for the gage.
     * 
     * @param gage
     *            The MPEGageData object
     * @return The radar id
     * @throws VizException
     * @throws IOException
     */
    public String lookupRadarId(MPEGageData gage) throws VizException,
            IOException {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        GageTableDataManager gageTableDataManager = GageTableDataManager
                .getInstance();
        MPEDataManager mpeDataManager = MPEDataManager.getInstance();
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();

        Date currentDate = displayManager.getCurrentEditDate();
        String radarId = "ZZZ";
        HRAPSubGrid subGrid = null;
        DisplayFieldData dataType = DisplayFieldData.Index;

        // Get the data if not already loaded
        if ((index == null) || !currentDate.equals(dataDate)) {
            // Set the dataDate
            dataDate = currentDate;

            // Get the file name and path
            String cv_use = dataType.getCv_use();
            String dirname = appsDefaults.getToken("rfcwide_index_dir");
            String fname = FileUtil.join(dirname, cv_use + sdf.format(dataDate)
                    + "z");

            XmrgFile file = new XmrgFile(fname);
            if (file.getFile().exists()) {
                file.load();
                index = file.getData();
            } else {
                // If the file doesn't exist return the default ZZZ
                return "ZZZ";
            }
        }

        // Find the location of the gage
        Point p = new Point((int) gage.getHrap().x, (int) gage.getHrap().y);
        Rectangle extent = null;
        short radarIndex = -999;
        try {
            subGrid = new HRAPSubGrid(mpeDataManager.getHRAPExtent());
        } catch (Exception e) {
            e.printStackTrace();
        }
        extent = subGrid.getExtent();

        if (extent.contains(p)) {
            int x = p.x - extent.x;
            int y = extent.height - 1 - (p.y - extent.y);

            radarIndex = index[y * subGrid.getNx() + x];
            if (radarIndex == 0) {
                return "ZZZ";
            }

            radarId = gageTableDataManager.getRadarIds()[radarIndex - 1];
        }

        return radarId;
    }

    /**
     * Get the Xmrg data for the specified type.
     * 
     * @param path
     *            Path to the Xmrg file
     * @param type
     *            The type of Xmrg file
     * @param extent
     *            The extent of the data
     * @return The data from the Xmrg file for that extent
     * @throws IOException
     */
    public short[][] getXmrgData(String path, String type, Rectangle extent)
            throws IOException {
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();
        Date currentDate = displayManager.getCurrentEditDate();

        if (type.equalsIgnoreCase(GageTableProductManager.MPE_AVGRMOSAIC)) {
            if ((avgrMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                avgrMosaic = file.getData(extent);
            }
            return avgrMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_BESTQPE)) {
            if ((xmrg == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                xmrg = file.getData(extent);
            }
            return xmrg;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_BMOSAIC)) {
            if ((bMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                bMosaic = file.getData(extent);
            }
            return bMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_GAGEONLY)) {
            if ((gageOnly == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                gageOnly = file.getData(extent);
            }
            return gageOnly;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_LMOSAIC)) {
            if ((lMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                lMosaic = file.getData(extent);
            }
            return lMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_LQMOSAIC)) {
            if ((lqMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                lqMosaic = file.getData(extent);
            }
            return lqMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_LSATPRE)) {
            if ((lsatPrecip == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                lsatPrecip = file.getData(extent);
            }
            return lsatPrecip;
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_MAXRMOSAIC)) {
            if ((maxrMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                maxrMosaic = file.getData(extent);
            }
            return maxrMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_MLMOSAIC)) {
            if ((mlMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                mlMosaic = file.getData(extent);
            }
            return mlMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_MLQMOSAIC)) {
            if ((mlqMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                mlqMosaic = file.getData(extent);
            }
            return mlqMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_MMOSAIC)) {
            if ((mMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                mMosaic = file.getData(extent);
            }
            return mMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_P3LMOSAIC)) {
            if ((p3Mosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                p3Mosaic = file.getData(extent);
            }
            return p3Mosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_QMOSAIC)) {
            if ((qMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                qMosaic = file.getData(extent);
            }
            return qMosaic;
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_RFCBMOSAIC)) {
            if ((rfcbMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                rfcbMosaic = file.getData(extent);
            }
            return rfcbMosaic;
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_RFCMMOSAIC)) {
            if ((rfcmMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                rfcmMosaic = file.getData(extent);
            }
            return rfcmMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_RFCMOSAIC)) {
            if ((rfcMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                rfcMosaic = file.getData(extent);
            }
            return rfcMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_RMOSAIC)) {
            if ((rMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                rMosaic = file.getData(extent);
            }
            return rMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_SATPRE)) {
            if ((satPrecip == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                satPrecip = file.getData(extent);
            }
            return satPrecip;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_SGMOSAIC)) {
            if ((sgMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                sgMosaic = file.getData(extent);
            }
            return sgMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_SRGMOSAIC)) {
            if ((srgMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                srgMosaic = file.getData(extent);
            }
            return srgMosaic;
        } else if (type.equalsIgnoreCase(GageTableProductManager.MPE_SRMOSAIC)) {
            if ((srMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                srMosaic = file.getData(extent);
            }
            return srMosaic;
        }
        return null;
    }

    /**
     * Get the data for populating the table.
     */
    private void getTableData() {
        rows = null;
        columns = null;
        gageTableRowList = new HashSet<GageTableRowData>();

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        GageTableProductManager prodManager = GageTableProductManager
                .getInstance();

        List<GageTableColumn> columnList = null;

        columnList = getColumnDataList();

        colArray = new String[columnList.size()];
        int index = 0;
        for (GageTableColumn column : columnList) {
            String name = column.getName();

            if (column.isDataColumn()) {
                colArray[index] = prodManager.lookupProductPrefix(name);
                index++;
            } else {
                colArray[index] = name;
                index++;
            }
        }

        String rfcmosaic = appsDefaults.getToken("mpe_generate_areal_qpe");

        rows = new Vector<Vector<String>>();

        gageRecordList = readGageData();
        index = 0;

        for (MPEGageData gage : gageRecordList) {
            if (!gage.getPe().equalsIgnoreCase("PP")) {
                continue;
            }

            Map<String, Double> productValueMap = new HashMap<String, Double>();

            for (String col : colArray) {
                if (col.equalsIgnoreCase(GageTableProductManager.MPE_AVGRMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_AVGRMOSAIC,
                            getData(DisplayFieldData.avgrMosaic, gage,
                                    GageTableProductManager.MPE_AVGRMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_BESTQPE)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_BESTQPE,
                            getData(DisplayFieldData.Xmrg, gage,
                                    GageTableProductManager.MPE_BESTQPE));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_BMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_BMOSAIC,
                            getData(DisplayFieldData.bMosaic, gage,
                                    GageTableProductManager.MPE_BMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_GAGEONLY)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_GAGEONLY,
                            getData(DisplayFieldData.gageOnly, gage,
                                    GageTableProductManager.MPE_GAGEONLY));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_LMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_LMOSAIC,
                            getData(DisplayFieldData.lMosaic, gage,
                                    GageTableProductManager.MPE_LMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_LQMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_LQMOSAIC,
                            getData(DisplayFieldData.lqmosaic, gage,
                                    GageTableProductManager.MPE_LQMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_LSATPRE)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_LSATPRE,
                            getData(DisplayFieldData.lsatPre, gage,
                                    GageTableProductManager.MPE_LSATPRE));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_MAXRMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_MAXRMOSAIC,
                            getData(DisplayFieldData.maxrMosaic, gage,
                                    GageTableProductManager.MPE_MAXRMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_MLMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_MLMOSAIC,
                            getData(DisplayFieldData.mlMosaic, gage,
                                    GageTableProductManager.MPE_MLMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_MLQMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_MLQMOSAIC,
                            getData(DisplayFieldData.mlqmosaic, gage,
                                    GageTableProductManager.MPE_MLQMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_MMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_MMOSAIC,
                            getData(DisplayFieldData.mMosaic, gage,
                                    GageTableProductManager.MPE_MMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_P3LMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_P3LMOSAIC,
                            getData(DisplayFieldData.p3lMosaic, gage,
                                    GageTableProductManager.MPE_P3LMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_QMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_QMOSAIC,
                            getData(DisplayFieldData.qmosaic, gage,
                                    GageTableProductManager.MPE_QMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_RFCMOSAIC)) {
                    if (rfcmosaic.equalsIgnoreCase("ON")) {
                        productValueMap.put(
                                GageTableProductManager.MPE_RFCMOSAIC,
                                getData(DisplayFieldData.rfcMosaic, gage,
                                        GageTableProductManager.MPE_RFCMOSAIC));
                    }
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_RFCBMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_RFCBMOSAIC,
                            getData(DisplayFieldData.rfcbMosaic, gage,
                                    GageTableProductManager.MPE_RFCBMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_RFCMMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_RFCMMOSAIC,
                            getData(DisplayFieldData.rfcmMosaic, gage,
                                    GageTableProductManager.MPE_RFCMMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_RMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_RMOSAIC,
                            getData(DisplayFieldData.rMosaic, gage,
                                    GageTableProductManager.MPE_RMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_SATPRE)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_SATPRE,
                            getData(DisplayFieldData.satPre, gage,
                                    GageTableProductManager.MPE_SATPRE));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_SGMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_SGMOSAIC,
                            getData(DisplayFieldData.sgMosaic, gage,
                                    GageTableProductManager.MPE_SGMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_SRGMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_SRGMOSAIC,
                            getData(DisplayFieldData.srgMosaic, gage,
                                    GageTableProductManager.MPE_SRGMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_SRMOSAIC)) {
                    productValueMap.put(
                            GageTableProductManager.MPE_SRMOSAIC,
                            getData(DisplayFieldData.srMosaic, gage,
                                    GageTableProductManager.MPE_SRMOSAIC));
                }
            }
            double editValue = -999;
            MPEGageData editedGage = MPEDataManager.getInstance()
                    .getEditedGage(gage.getId());
            if (editedGage != null) {
                String edit = editedGage.getEdit();
                if (edit != null && !edit.trim().equals("")
                        && !edit.trim().equalsIgnoreCase("M")) {
                    editValue = Double.parseDouble(editedGage.getEdit());                    
                } else {
                    gage.setManedit(false);
                }
            }
            // else if ((gage.getEdit() != null)
            // && !gage.getEdit().trim().equals("")
            // && !gage.getEdit().trim().equalsIgnoreCase("M")) {
            // editValue = Double.parseDouble(gage.getEdit());
            // }

            GageTableRowData rowData = new GageTableRowData(editValue,
                    productValueMap, gage);

            gageTableRowList.add(rowData);
            index++;
        }
        getTableRowData();
    }

    /**
     * Get the data for the table row.
     */
    private void getTableRowData() {
        GageTableProductManager prodManager = GageTableProductManager
                .getInstance();
        rows = new Vector<Vector<String>>();
        double diffGridValue = -999;
        String radarId = "ZZZ";

        List<GageTableColumn> columnDataList = getColumnDataList();
        Map<String, String> rowDataMap = new HashMap<String, String>();

        for (GageTableRowData rowData : gageTableRowList) {

            Vector<String> row = new Vector<String>();

            Map<String, Double> productValueMap = rowData.getProductValueMap();

            // LID
            rowDataMap.put("LID", rowData.getGageData().getId());

            // Value
            String value = null;
            if (rowData.getGageData().getGval() < 0) {
                value = "M";
            } else {
                value = String.format(format, rowData.getGageData().getGval());
                if (rowData.getGageData().getId().startsWith("PSEUDO")) {
                    value = String.format(format, rowData.getGageData()
                            .getGval() / 25.4);
                }
            }
            rowDataMap.put("Gage Value", value);

            // Edit value
            // Check to see if a value was edited
            String editValue = null;
            if (rowData.isValueEdited()) {
                editValue = String.valueOf(rowData.getEditValue());
            } else {
                editValue = rowData.getGageData().getEdit();
            }
            if ((editValue == null) || (editValue.equals(""))
                    || editValue.equalsIgnoreCase("M")) {
                editValue = "";
            } else if ((Float.parseFloat(editValue) == -999.0)) {
                editValue = "M";
            } else {
                editValue = String.format(format, Float.parseFloat(editValue));
            }
            rowDataMap.put("Edit Gage Value", editValue);

            // Diff value
            double diff = -999;
            for (int j = 0; j < colArray.length; j++) {
                if (getSelectedGrid().equalsIgnoreCase(colArray[j])) {
                    diffGridValue = productValueMap.get(colArray[j]);

                    if ((diffGridValue != -999)
                            && ((rowData.getGageData().getGval() != -999.0) || (rowData
                                    .getEditValue() != -999.0))) {
                        if (rowData.getEditValue() != -999) {
                            diff = rowData.getEditValue() - diffGridValue;
                        } else {
                            if (rowData.getGageData().getId()
                                    .startsWith("PSEUDO")) {
                                diff = rowData.getGageData().getGval() / 25.4
                                        - diffGridValue;
                            } else {
                                diff = rowData.getGageData().getGval()
                                        - diffGridValue;
                                break;
                            }
                        }
                    } else {
                        break;
                    }
                }
            }

            // Diff value
            String diffStr = null;
            if (diff != -999) {
                diffStr = String.format(format, diff);
            } else {
                diffStr = "M";
            }
            rowDataMap.put("Diff (Gage-Grid)", diffStr);

            // Radar ID
            try {
                radarId = rowData.getGageData().getRid();
                if (radarId.equalsIgnoreCase("ZZZ")) {
                    radarId = lookupRadarId(rowData.getGageData());
                    rowData.getGageData().setRid(radarId);
                }
            } catch (VizException e) {
                radarId = "ZZZ";
                e.printStackTrace();
            } catch (IOException e) {
                radarId = "ZZZ";
                e.printStackTrace();
            }

            rowDataMap.put("Radar ID", radarId);

            for (int j = 0; j < colArray.length; j++) {
                if (rowDataMap.containsKey(colArray[j])) {
                    row.add(rowDataMap.get(colArray[j]));
                } else {
                    if ((productValueMap.get(colArray[j]) == null)
                            || (productValueMap.get(colArray[j]) == -999.0)) {
                        row.add("M");
                    } else {
                        row.add(String.format(format,
                                productValueMap.get(colArray[j])));
                    }
                }
            }
            rows.add(row);
        }

        /* set the column headers */
        columns = new Vector<String>();
        for (GageTableColumn gtc : columnDataList) {
            if (rowDataMap.containsKey(gtc.getName())) {
                columns.add(gtc.getName());
            } else {
                Map<String, GageTableColumn> columnMap = prodManager
                        .getGageTableProductColumnMap();
                String prodPrefix = prodManager.lookupProductPrefix(gtc
                        .getName());

                if (prodPrefix != null) {
                    GageTableColumn col = columnMap.get(prodPrefix);
                    GageTableProductDescriptor descriptor = col
                            .getProductDescriptor();
                    String name = descriptor.getProductName();
                    columns.add(name);
                } else {
                    columns.add(gtc.getName());
                }
            }
        }
    }

    /**
     * Retrieve the data from the Xmrg grid.
     * 
     * @param dataType
     *            The type of data
     * @param gage
     *            The gage
     * @return The value as a String
     */
    private double getData(DisplayFieldData dataType, MPEGageData gage,
            String prodType) {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        MPEDataManager dataManager = MPEDataManager.getInstance();
        GageTableDataManager gageTableDataManager = GageTableDataManager
                .getInstance();
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();
        double returnValue = -999.0;

        try {
            String cv_use = dataType.getCv_use();
            String dirname = appsDefaults.getToken(dataType.getDirToken());
            String fname = FileUtil.join(dirname,
                    cv_use + sdf.format(displayManager.getCurrentEditDate()) + "z");

            Rectangle extent = dataManager.getHRAPExtent();

            short[][] data = gageTableDataManager.getXmrgData(fname, prodType,
                    extent);
            Coordinate coord = gage.getHrap();
            int x = (int) coord.x;
            int y = (int) coord.y;
            if (!extent.contains(x, y)) {
                returnValue = -999.0;
                return returnValue;
            }

            int gridX = x - extent.x;
            // int gridY = y - extent.y;
            // Needed to flip the grid
            int gridY = extent.height - (y - extent.y) - 1;
            short value = data[gridY][gridX];
            
            // Any value < 0 is considered missing
            if ((value == -899.0) || (value == -999.0) || (value < 0)) {
                returnValue = -999.0;
            } else {
                returnValue = (double) value / 100 / 25.4;
            }
        } catch (IOException e) {
            returnValue = -999.0;
        }

        return returnValue;
    }

    /**
     * Get the MPEGageData objects in a List.
     * 
     * @return List of MPEGageData objects
     */
    public List<MPEGageData> readGageData() {
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();
        Date currentDate = displayManager.getCurrentEditDate();

        if ((mpeGageDataList == null) || !currentDate.equals(dataDate)) {
            MPEDataManager mpeDataManager = MPEDataManager.getInstance();
            mpeGageDataList = mpeDataManager.readGageData(currentDate,
                    currentDate);
        }

        return mpeGageDataList;
    }

    /**
     * Reload the data.
     */
    public void reloadData() {
    	getTableData();
        getTableRowData();
    }

    /**
     * @return the dataDate
     */
    public Date getDataDate() {
        return dataDate;
    }

    /**
     * @return the columnSettings
     */
    public GageTableSortSettings getColumnSettings() {
        return columnSettings;
    }

    /**
     * @param columnSettings
     *            the columnSettings to set
     */
    public void setColumnSettings(GageTableSortSettings columnSettings) {
        this.columnSettings = columnSettings;
    }

    /**
     * @return the columnDataList
     */
    public List<GageTableColumn> getColumnDataList() {
        return columnDataList;
    }

    /**
     * @param columnDataList
     *            the columnDataList to set
     */
    public void setColumnDataList(List<GageTableColumn> columnDataList) {
        this.columnDataList = columnDataList;
    }

    /**
     * @return the selectedGrid
     */
    public String getSelectedGrid() {
        return selectedGrid;
    }

    /**
     * @param selectedGrid
     *            the selectedGrid to set
     */
    public void setSelectedGrid(String selectedGrid) {
        this.selectedGrid = selectedGrid;
    }

    /**
     * @return the rows
     */
    public Vector<Vector<String>> getRows() {
        if (rows == null) {
            getTableData();
        }
        return rows;
    }

    /**
     * @param rows
     *            the rows to set
     */
    public void setRows(Vector<Vector<String>> rows) {
        this.rows = rows;
    }

    /**
     * @return the columns
     */
    public Vector<String> getColumns() {
        if (columns == null) {
            getTableData();
        }
        return columns;
    }

    /**
     * @param columns
     *            the columns to set
     */
    public void setColumns(Vector<String> columns) {
        this.columns = columns;
    }

    /**
     * @return the columnWidthMap
     */
    public Map<String, Integer> getColumnWidthMap() {
        return columnWidthMap;
    }

    /**
     * @param columnWidthMap
     *            the columnWidthMap to set
     */
    public void setColumnWidthMap(Map<String, Integer> columnWidthMap) {
        this.columnWidthMap = columnWidthMap;
    }

    /**
     * @return the gageTableRowList
     */
    public List<GageTableRowData> getGageTableRowList() {
        return new ArrayList<GageTableRowData>(gageTableRowList);
    }

    /**
     * @param gageTableRowList
     *            the gageTableRowList to set
     */
    public void setGageTableRowList(List<GageTableRowData> gageTableRowList) {
        this.gageTableRowList = new HashSet<GageTableRowData>(gageTableRowList);
    }

    /**
     * Null the instance.
     */
    public static void setNull() {
        instance = null;
    }
}
