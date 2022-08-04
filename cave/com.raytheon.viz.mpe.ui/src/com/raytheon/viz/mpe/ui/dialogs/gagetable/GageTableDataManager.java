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
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.Vector;

import javax.xml.bind.JAXB;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.time.DateUtils;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.mpe.MPEDateFormatter;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPEGageData;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.gagetable.xml.GageTableColumnData;
import com.raytheon.viz.mpe.ui.dialogs.gagetable.xml.GageTableSettings;
import org.locationtech.jts.geom.Coordinate;

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
 * May 20, 2013 15962      lbousaidi   Added a new routine getRadarIdsTrue()
 *                                     for Radar Sites dialog.
 * Mar 05, 2014 17114      lbousaidi   display PC data in gage table.
 * Sep 04, 2014 16699      cgobs       Fixed 14.3.1 issue with reading MPE field data.
 * Oct 19, 2015 18090      lbousaidi   fixed best estimate qpe display.
 * Nov 18, 2015 18093      snaples     Added selectedGridIndex to maintain selected grid after table refresh.
 * Jan 13, 2016 18092      snaples     Removed redundant call, that resulted in a circle.
 * Mar 14, 2016 5467       bkowal      Handle the sat precip data hour difference.
 * Mar 01, 2017 6158       mpduff      Changed how sorting works.
 * May 12, 2017 6283       bkowal      Added {@link #updateVisibleColumns(List)}.
 * Jul 14, 2017 6358       mpduff      Changed how settings are handled.
 * Aug 02, 2017 6358       mpduff      Added ILocalizationPathObserver to listen for settings file changes.
 * Aug 07, 2017 6240       mpduff      Fix merge issues.
 * Sep 22, 2017 6161       bkowal      Fix revert issues.
 * Nov 16, 2017 6524       bkowal      Added {@link #clearCachedGages()}.
 * Aug 23, 2018 6953       tgurney     Don't filter radar locations by office id
 * Nov 26, 2018 7632       lsingh      Removed unused code.
 * </pre>
 *
 * @author mpduff
 */

public class GageTableDataManager implements ILocalizationPathObserver {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String SETTINGS_FILE = "hydro" + IPathManager.SEPARATOR
            + "MPEGageTableDisplaySettings.xml";

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

    // Dual Pol Fields

    private short[][] rdMosaic = null;

    private short[][] avgrdMosaic = null;

    private short[][] maxrdMosaic = null;

    private short[][] bdMosaic = null;

    private short[][] ldMosaic = null;

    private short[][] mdMosaic = null;

    private short[][] mldMosaic = null;

    private short[][] srdMosaic = null;

    private short[][] srdgMosaic = null;

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
    private GageTableSortSettings sortSettings = null;

    /**
     * List of GageTableRowData objects.
     */
    private Set<GageTableRowData> gageTableRowList = null;

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

    private int selectedGridIndex = 0;

    private Map<String, GageTableColumn> columnData;

    private List<String> columnOrder = new ArrayList<>();

    /**
     * List of non-data columns.
     */
    private final List<String> baseColumns = new ArrayList<>();

    private final Object gageLock = new Object();

    static {
        sdf = new SimpleDateFormat("yyyyMMddHH");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Private constructor
     */
    private GageTableDataManager() {
        IPathManager pm = PathManagerFactory.getPathManager();
        pm.addLocalizationPathObserver(SETTINGS_FILE, this);
        readSettingsFile();
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
        if (radarIds == null) {
            String query = "select radid from radarloc where use_radar='T' order by radid asc";
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
     * Get the list of Radar Ids from radarloc. only the one with use_radar= T
     *
     * @return the radarIds
     * @throws VizException
     */
    public String[] getRadarIdsTrue() throws VizException {

        if (radarIds == null) {
            String query = "select radid from radarloc where use_radar='T' "
                    + "order by radid asc";
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
    public String lookupRadarId(MPEGageData gage)
            throws VizException, IOException {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
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
            String fname = FileUtil.join(dirname,
                    cv_use + sdf.format(dataDate) + "z");

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
        Rectangle rect = mpeDataManager.getHRAPExtent();
        try {
            subGrid = new HRAPSubGrid(mpeDataManager.getHRAPExtent());
        } catch (Exception e) {
            statusHandler.debug("Failed to create subgrid from extent " + rect);
            throw new VizException(e);
        }
        extent = subGrid.getExtent();

        if (extent.contains(p)) {
            int x = p.x - extent.x;
            int y = extent.height - 1 - (p.y - extent.y);

            radarIndex = index[y * subGrid.getNx() + x];
            if (radarIndex == 0) {
                return "ZZZ";
            }

            radarId = getRadarIds()[radarIndex - 1];
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
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_GAGEONLY)) {
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
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_LQMOSAIC)) {
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
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_MLMOSAIC)) {
            if ((mlMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                mlMosaic = file.getData(extent);
            }
            return mlMosaic;
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_MLQMOSAIC)) {
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

            // ---------------------------------------
            // Dual Pol Fields

        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_RDMOSAIC)) {
            if ((rdMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                rdMosaic = file.getData(extent);
            }
            return rdMosaic;

        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_BDMOSAIC)) {
            if ((bdMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                bdMosaic = file.getData(extent);
            }
            return bdMosaic;

        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_LDMOSAIC)) {
            if ((ldMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                ldMosaic = file.getData(extent);
            }
            return ldMosaic;

        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_MDMOSAIC)) {
            if ((mdMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                mdMosaic = file.getData(extent);
            }
            return mdMosaic;

        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_MLDMOSAIC)) {
            if ((mldMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                mldMosaic = file.getData(extent);
            }
            return mldMosaic;

        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_AVGRDMOSAIC)) {
            if ((avgrdMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                avgrdMosaic = file.getData(extent);
            }
            return avgrdMosaic;

        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_MAXRDMOSAIC)) {
            if ((maxrdMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                maxrdMosaic = file.getData(extent);
            }
            return maxrdMosaic;

        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_SRDMOSAIC)) {
            if ((srdMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                srdMosaic = file.getData(extent);
            }
            return srdMosaic;

        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_SRDGMOSAIC)) {
            if ((srdgMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                srdgMosaic = file.getData(extent);
            }
            return srdgMosaic;

            // ------------------------------------------------------

            //
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_P3LMOSAIC)) {
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
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_RFCMOSAIC)) {
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
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_SGMOSAIC)) {
            if ((sgMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                sgMosaic = file.getData(extent);
            }
            return sgMosaic;
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_SRGMOSAIC)) {
            if ((srgMosaic == null) || !currentDate.equals(dataDate)) {
                // Set the dataDate
                dataDate = currentDate;

                XmrgFile file = new XmrgFile(path);
                file.load();
                srgMosaic = file.getData(extent);
            }
            return srgMosaic;
        } else if (type
                .equalsIgnoreCase(GageTableProductManager.MPE_SRMOSAIC)) {
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
        gageTableRowList = new HashSet<>();

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

        rows = new Vector<>();

        index = 0;

        for (MPEGageData gage : readGageData()) {

            Map<String, Double> productValueMap = new HashMap<>();

            for (String col : colArray) {
                if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_AVGRMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_AVGRMOSAIC,
                            getData(DisplayFieldData.avgrMosaic, gage,
                                    GageTableProductManager.MPE_AVGRMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_BESTQPE)) {
                    productValueMap.put(GageTableProductManager.MPE_BESTQPE,
                            getData(DisplayFieldData.Xmrg, gage,
                                    GageTableProductManager.MPE_BESTQPE));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_BMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_BMOSAIC,
                            getData(DisplayFieldData.bMosaic, gage,
                                    GageTableProductManager.MPE_BMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_GAGEONLY)) {
                    productValueMap.put(GageTableProductManager.MPE_GAGEONLY,
                            getData(DisplayFieldData.gageOnly, gage,
                                    GageTableProductManager.MPE_GAGEONLY));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_LMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_LMOSAIC,
                            getData(DisplayFieldData.lMosaic, gage,
                                    GageTableProductManager.MPE_LMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_LQMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_LQMOSAIC,
                            getData(DisplayFieldData.lqmosaic, gage,
                                    GageTableProductManager.MPE_LQMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_LSATPRE)) {
                    productValueMap.put(GageTableProductManager.MPE_LSATPRE,
                            getData(DisplayFieldData.lsatPre, gage,
                                    GageTableProductManager.MPE_LSATPRE));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_MAXRMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_MAXRMOSAIC,
                            getData(DisplayFieldData.maxrMosaic, gage,
                                    GageTableProductManager.MPE_MAXRMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_MLMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_MLMOSAIC,
                            getData(DisplayFieldData.mlMosaic, gage,
                                    GageTableProductManager.MPE_MLMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_MLQMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_MLQMOSAIC,
                            getData(DisplayFieldData.mlqmosaic, gage,
                                    GageTableProductManager.MPE_MLQMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_MMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_MMOSAIC,
                            getData(DisplayFieldData.mMosaic, gage,
                                    GageTableProductManager.MPE_MMOSAIC));

                    // -------------------------------------------
                    // Dual Pol Fields

                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_RDMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_RDMOSAIC,
                            getData(DisplayFieldData.rdMosaic, gage,
                                    GageTableProductManager.MPE_RDMOSAIC));

                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_BDMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_BDMOSAIC,
                            getData(DisplayFieldData.bdMosaic, gage,
                                    GageTableProductManager.MPE_BDMOSAIC));

                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_LDMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_LDMOSAIC,
                            getData(DisplayFieldData.ldMosaic, gage,
                                    GageTableProductManager.MPE_LDMOSAIC));

                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_MDMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_MDMOSAIC,
                            getData(DisplayFieldData.mdMosaic, gage,
                                    GageTableProductManager.MPE_MDMOSAIC));

                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_MLDMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_MLDMOSAIC,
                            getData(DisplayFieldData.mldMosaic, gage,
                                    GageTableProductManager.MPE_MLDMOSAIC));

                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_AVGRDMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_AVGRDMOSAIC,
                            getData(DisplayFieldData.avgrdMosaic, gage,
                                    GageTableProductManager.MPE_AVGRDMOSAIC));

                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_MAXRDMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_MAXRDMOSAIC,
                            getData(DisplayFieldData.maxrdMosaic, gage,
                                    GageTableProductManager.MPE_MAXRDMOSAIC));

                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_SRDMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_SRDMOSAIC,
                            getData(DisplayFieldData.srdMosaic, gage,
                                    GageTableProductManager.MPE_SRDMOSAIC));

                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_SRDGMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_SRDGMOSAIC,
                            getData(DisplayFieldData.srdgMosaic, gage,
                                    GageTableProductManager.MPE_SRDGMOSAIC));

                    // -------------------------------------------

                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_P3LMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_P3LMOSAIC,
                            getData(DisplayFieldData.p3lMosaic, gage,
                                    GageTableProductManager.MPE_P3LMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_QMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_QMOSAIC,
                            getData(DisplayFieldData.qmosaic, gage,
                                    GageTableProductManager.MPE_QMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_RFCMOSAIC)) {
                    if ("ON".equalsIgnoreCase(rfcmosaic)) {
                        productValueMap.put(
                                GageTableProductManager.MPE_RFCMOSAIC,
                                getData(DisplayFieldData.rfcMosaic, gage,
                                        GageTableProductManager.MPE_RFCMOSAIC));
                    }
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_RFCBMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_RFCBMOSAIC,
                            getData(DisplayFieldData.rfcbMosaic, gage,
                                    GageTableProductManager.MPE_RFCBMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_RFCMMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_RFCMMOSAIC,
                            getData(DisplayFieldData.rfcmMosaic, gage,
                                    GageTableProductManager.MPE_RFCMMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_RMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_RMOSAIC,
                            getData(DisplayFieldData.rMosaic, gage,
                                    GageTableProductManager.MPE_RMOSAIC));
                } else if (col
                        .equalsIgnoreCase(GageTableProductManager.MPE_SATPRE)) {
                    productValueMap.put(GageTableProductManager.MPE_SATPRE,
                            getData(DisplayFieldData.satPre, gage,
                                    GageTableProductManager.MPE_SATPRE));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_SGMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_SGMOSAIC,
                            getData(DisplayFieldData.sgMosaic, gage,
                                    GageTableProductManager.MPE_SGMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_SRGMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_SRGMOSAIC,
                            getData(DisplayFieldData.srgMosaic, gage,
                                    GageTableProductManager.MPE_SRGMOSAIC));
                } else if (col.equalsIgnoreCase(
                        GageTableProductManager.MPE_SRMOSAIC)) {
                    productValueMap.put(GageTableProductManager.MPE_SRMOSAIC,
                            getData(DisplayFieldData.srMosaic, gage,
                                    GageTableProductManager.MPE_SRMOSAIC));
                }
            }
            double editValue = -999;
            MPEGageData editedGage = MPEDataManager.getInstance()
                    .getEditedGage(gage.getId());
            if (editedGage != null) {
                String edit = editedGage.getEdit();
                if (edit != null && !edit.trim().isEmpty()
                        && !"M".equalsIgnoreCase(edit.trim())) {
                    editValue = Double.parseDouble(editedGage.getEdit());
                } else {
                    gage.setManedit(false);
                }
            }

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
        rows = new Vector<>();
        double diffGridValue = -999;
        String radarId = "ZZZ";

        List<GageTableColumn> columnDataList = getColumnDataList();
        Map<String, String> rowDataMap = new HashMap<>();

        for (GageTableRowData rowData : gageTableRowList) {

            Vector<String> row = new Vector<>();

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
                    value = String.format(format,
                            rowData.getGageData().getGval() / 25.4);
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
            if ((editValue == null) || (editValue.isEmpty())
                    || "M".equalsIgnoreCase(editValue)) {
                editValue = "";
            } else if ((Float.parseFloat(editValue) == -999.0)) {
                editValue = "M";
            } else {
                editValue = String.format(format, Float.parseFloat(editValue));
            }
            rowDataMap.put("Edit Gage Value", editValue);

            // Diff value
            double diff = -999;
            for (String element : colArray) {
                if (getSelectedGrid().equalsIgnoreCase(element)) {
                    diffGridValue = productValueMap.get(element);

                    if ((diffGridValue != -999)
                            && ((rowData.getGageData().getGval() != -999.0)
                                    || (rowData.getEditValue() != -999.0))) {
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
                if ("ZZZ".equalsIgnoreCase(radarId)) {
                    radarId = lookupRadarId(rowData.getGageData());
                    rowData.getGageData().setRid(radarId);
                }
            } catch (VizException | IOException e) {
                radarId = "ZZZ";
                statusHandler.debug("Failed to look up radar id for gage "
                        + rowData.getGageData().getId(), e);
            }

            rowDataMap.put("Radar ID", radarId);

            for (String element : colArray) {
                if (rowDataMap.containsKey(element)) {
                    row.add(rowDataMap.get(element));
                } else {
                    if ((productValueMap.get(element) == null)
                            || (productValueMap.get(element) == -999.0)) {
                        row.add("M");
                    } else {
                        row.add(String.format(format,
                                productValueMap.get(element)));
                    }
                }
            }
            rows.add(row);
        }

        /* set the column headers */
        columns = new Vector<>();
        for (GageTableColumn gtc : columnDataList) {
            if (rowDataMap.containsKey(gtc.getName())) {
                columns.add(gtc.getName());
            } else {
                Map<String, GageTableColumn> columnMap = prodManager
                        .getGageTableProductColumnMap();
                String prodPrefix = prodManager
                        .lookupProductPrefix(gtc.getName());

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
        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();
        double returnValue = -999.0;
        String ST3_FORMAT_STRING = MPEDateFormatter.yyyyMMddHH;
        /*
         * non need just change format of sdf
         */
        String date_form = appsDefaults.getToken("st3_date_form");
        if ((date_form == null) || date_form.isEmpty()
                || "mdY".equals(date_form)) {
            ST3_FORMAT_STRING = MPEDateFormatter.MMddyyyyHH;
        } else if ("Ymd".equals(date_form)) {
            ST3_FORMAT_STRING = MPEDateFormatter.yyyyMMddHH;
        }

        Date dataDate = displayManager.getCurrentEditDate();
        if (dataType == DisplayFieldData.satPre) {
            /*
             * SATPRE MPE file time stamp is the start time of the hour i.e. a
             * 12z -13z product has a time stamp of 12z. TODO: determining xmrg
             * file names needs to be standardized. This is at least the third
             * place that I have seen the use of this Satellite Precipitation
             * condition. Also used in
             * com.raytheon.viz.mpe.ui.rsc.MPEFieldResource
             * .getEditedData(MPEFieldFrame) and
             * com.raytheon.viz.mpe.ui.rsc.MPEFieldResource
             * .createFrame(DataTime). However, there may be additional places
             * where it is used as well.
             */
            dataDate = DateUtils.addHours(dataDate, -1);
        }
        String dirname = appsDefaults.getToken(dataType.getDirToken());
        String fname = null;
        if (dataType.getFileNamePrefix().contains("XMRG")) {
            String cdate = MPEDateFormatter.format(dataDate, ST3_FORMAT_STRING);
            fname = FileUtil.join(dirname,
                    dataType.getFileNamePrefix().toLowerCase() + cdate + "z");
        } else {
            fname = FileUtil.join(dirname,
                    dataType.getFileNamePrefix() + sdf.format(dataDate) + "z");

        }

        try {
            Rectangle extent = dataManager.getHRAPExtent();

            short[][] data = getXmrgData(fname, prodType, extent);
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
            statusHandler.debug("Failed to get xmrg data from " + fname, e);
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

        synchronized (gageLock) {
            if ((mpeGageDataList == null) || !currentDate.equals(dataDate)) {
                MPEDataManager mpeDataManager = MPEDataManager.getInstance();
                mpeGageDataList = mpeDataManager.readGageData(currentDate,
                        currentDate);
            }

            return mpeGageDataList;
        }
    }

    /**
     * Clears the currently cached gages that were previously read to force a
     * reload of the data the next time it is accessed.
     */
    public void clearCachedGages() {
        synchronized (gageLock) {
            mpeGageDataList = null;
        }
    }

    /**
     * Reload the data.
     */
    public void reloadData() {
        getTableData();
    }

    /**
     * @return the dataDate
     */
    public Date getDataDate() {
        return dataDate;
    }

    /**
     * @return the sortSettings
     */
    public GageTableSortSettings getSortSettings() {
        if (sortSettings == null) {
            sortSettings = new GageTableSortSettings();
        }
        return sortSettings;
    }

    /**
     * @param sortSettings
     *            the sortSettings to set
     */
    public void setSortSettings(GageTableSortSettings sortSettings) {
        this.sortSettings = sortSettings;
    }

    /**
     * @return the columnDataList
     */
    public List<GageTableColumn> getColumnDataList() {
        List<GageTableColumn> columnList = new ArrayList<>(columnOrder.size());
        for (String colName : columnOrder) {
            columnList.add(this.columnData.get(colName));
        }
        return columnList;
    }

    /**
     * @param columnDataList
     *            the columnDataList to set
     */
    public void updateVisibleColumns(List<GageTableColumn> columnDataList) {
        columnData.clear();
        columnOrder.clear();
        columns.clear();
        for (GageTableColumn gageTableColumn : columnDataList) {
            final String columnName = gageTableColumn.getName();
            columnOrder.add(columnName);
            columns.add(columnName);
            columnData.put(columnName, gageTableColumn);
        }

        /*
         * Eliminate any columns for the sort settings that are no longer
         * present in the table.
         */
        if (sortSettings != null) {
            if (CollectionUtils.isNotEmpty(sortSettings.getSortColumns())) {
                Iterator<String> sortIter = sortSettings.getSortColumns()
                        .iterator();
                while (sortIter.hasNext()) {
                    String sortColumn = sortIter.next();
                    if (!columns.contains(sortColumn)) {
                        /*
                         * Column is no longer visible. It should not longer be
                         * used as sorting criteria.
                         */
                        sortIter.remove();
                        sortSettings.getSortDirections().remove(sortColumn);
                    }
                }
            }
        }
    }

    /**
     * Read the settings XML file. There is a single file for the site.
     */
    private void readSettingsFile() {
        // Clear the previous settings
        columnOrder.clear();

        // Get a list of non-data column names
        for (String colName : GageTableConstants.BASE_COLUMNS) {
            baseColumns.add(colName);
        }

        IPathManager pm = PathManagerFactory.getPathManager();
        GageTableProductManager prodManager = GageTableProductManager
                .getInstance();
        Map<String, GageTableColumn> columnMap = prodManager
                .getGageTableProductColumnMap();

        Map<String, GageTableColumn> columnData = new HashMap<>();
        List<GageTableColumn> selectedColumns = new LinkedList<>();
        GageTableSettings settings = null;
        File f = pm.getStaticFile(LocalizationType.COMMON_STATIC,
                SETTINGS_FILE);

        if (f != null && f.exists()) {
            settings = JAXB.unmarshal(f, GageTableSettings.class);
        } else {
            settings = getDefaultSettings();
        }
        List<GageTableColumnData> columnSettingList = settings.getColumn();

        for (GageTableColumnData c : columnSettingList) {
            GageTableColumn column = null;
            if (prodManager.lookupProductPrefix(c.getName()) != null) {
                GageTableProductDescriptor prodDesc = columnMap
                        .get(prodManager.lookupProductPrefix(c.getName()))
                        .getProductDescriptor();
                column = new GageTableColumn(prodDesc);
                column.setName(prodDesc.getProductName());
                column.setToolTipText(prodDesc.getProductDescription());
            } else {
                // Non-data column, doesn't have a product descriptor
                column = new GageTableColumn(null);
                column.setName(c.getName());

                if ("LID".equalsIgnoreCase(column.getName())) {
                    column.setToolTipText("Location ID");
                } else if (column.getName().startsWith("Diff")) {
                    column.setToolTipText(
                            "Difference between Gage Value and Grid Data");
                } else {
                    column.setToolTipText(column.getName());
                }
            }
            column.setWidth(c.getWidth().intValue());

            if (baseColumns.contains(column.getName())) {
                column.setDataColumn(false);
            } else {
                column.setDataColumn(true);
            }

            columnData.put(column.getName(), column);
            columnOrder.add(column.getName());
            selectedColumns.add(column);
        }

        setColumnData(columnData);
        setColumnOrder(columnOrder);
        GageTableSortSettings sortSettings = new GageTableSortSettings();
        sortSettings.setColumnData(columnSettingList);
        setSortSettings(sortSettings);
        prodManager.setSelectedColumns(selectedColumns);
    }

    /**
     * Gets the default settings and creates a settings object.
     *
     * @return GageTableSettings data
     */
    private GageTableSettings getDefaultSettings() {
        GageTableProductManager prodManager = GageTableProductManager
                .getInstance();

        GageTableSettings settings = new GageTableSettings();

        // Get the non-data columns
        String[] baseColumns = GageTableConstants.BASE_COLUMNS;
        for (String s : baseColumns) {
            GageTableColumnData col = new GageTableColumnData();
            col.setName(s);
            col.setWidth(BigInteger.valueOf(GageTableConstants.DEFAULT_WIDTH));
            settings.getColumn().add(col);
        }

        // Get the data columns defined in Apps_defaults
        List<GageTableColumn> colList = prodManager
                .getAvailableGageTableColumnList();

        for (GageTableColumn tableCol : colList) {
            GageTableColumnData col = new GageTableColumnData();
            col.setName(tableCol.getName());
            col.setWidth(BigInteger.valueOf(GageTableConstants.DEFAULT_WIDTH));
            settings.getColumn().add(col);
        }

        return settings;
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
     * @return the selectedGridIndex value
     */
    public int getSelectedGridIndex() {
        return selectedGridIndex;
    }

    /**
     * @param selectedGridIndex
     *            the int value of the selected GridIndex
     */
    public void setSelectedGridIndex(int selectedGridIndex) {
        this.selectedGridIndex = selectedGridIndex;
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
     * @return the gageTableRowList
     */
    public List<GageTableRowData> getGageTableRowList() {
        return new ArrayList<>(gageTableRowList);
    }

    /**
     * @param gageTableRowList
     *            the gageTableRowList to set
     */
    public void setGageTableRowList(List<GageTableRowData> gageTableRowList) {
        this.gageTableRowList = new HashSet<>(gageTableRowList);
    }

    public void setColumnData(Map<String, GageTableColumn> columnData) {
        this.columnData = columnData;
    }

    public Map<String, GageTableColumn> getColumnData() {
        return columnData;
    }

    public void setColumnOrder(List<String> columnOrder) {
        this.columnOrder = columnOrder;
    }

    public List<String> getColumnOrder() {
        return columnOrder;
    }

    @Override
    public void fileChanged(ILocalizationFile file) {
        readSettingsFile();
    }
}
