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
package com.raytheon.viz.radar.rsc.graphic;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.xml.bind.JAXB;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.Layer;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.radar.rsc.graphic.RadarGraphicsPage.CoordinateSystem;
import com.raytheon.viz.radar.util.DmdModifier;
import com.raytheon.viz.radar.util.GraphicDataUtil;
import org.locationtech.jts.geom.Coordinate;

/**
 * Main class for displaying radar graphics products. Most of the actual drawing
 * logic is in {@link RadarGraphicsPage}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2009            chammack     Initial creation
 * 03/04/2013   DCS51      zwang        Handle GFM product
 * Sep 03, 2014  3574      njensen      Properly dispose objects
 * Aug 31, 2016  2671      tgurney      Factor out some logic to RadarRecordUtil
 *
 * </pre>
 *
 * @author chammack
 */

public class RadarGraphicsDisplay implements IRenderable {

    private final Map<Integer, RadarGraphicsPage> pageMap;

    private final ArrayList<RadarGraphicsPage> symbologyPages;

    private final Map<RadarDataKey, RadarDataPoint> symbologyData;

    private int currentPage;

    public RadarGraphicsDisplay(RadarRecord radarRecord, IGraphicsTarget target,
            IMapDescriptor mapDescriptor, Set<String> filteredStormIds,
            double magnification, RGB color) throws VizException {
        this.pageMap = new HashMap<>();
        this.symbologyPages = new ArrayList<>();
        this.currentPage = 0;

        // Only retrieve if this record has not been retrieved.
        if ((radarRecord.getSymbologyData() == null
                || radarRecord.getSymbologyData().isEmpty())
                && radarRecord.getGraphicBlock() == null) {
            File loc = HDF5Util.findHDF5Location(radarRecord);

            IDataStore dataStore = DataStoreFactory.getDataStore(loc);

            try {
                RadarDataRetriever.populateRadarRecord(dataStore, radarRecord);
            } catch (Exception e) {
                throw new VizException("Unable to retrieve radar graphics", e);
            }
        }
        GeneralGridGeometry gg = RadarRecordUtil
                .getRadarGraphicsGridGeometry(radarRecord);
        IWireframeShape ws = target.createWireframeShape(true, mapDescriptor);

        // Used for GFM forecast positions
        IWireframeShape gfmWs = target.createWireframeShape(true,
                mapDescriptor);

        symbologyData = radarRecord.getSymbologyData();
        if (symbologyData != null) {
            RadarGraphicsPage rgp = new RadarGraphicsPage(mapDescriptor, gg, ws,
                    gfmWs, target, color);

            this.symbologyPages.add(rgp);
            // Determine if each set of Storm data should be displayed
            RadarDataPoint currStorm;
            TreeMap<Integer, TreeMap<Integer, String>> tableData = new TreeMap<>(
                    Collections.reverseOrder());
            String featureData = null;
            boolean processTableData = false;

            // For DMD data.
            DmdModifier tableModifier = initializeDmdTablePreferences();
            for (RadarDataKey currLoc : symbologyData.keySet()) {
                currStorm = symbologyData.get(currLoc);
                // String dmdID = radarRecord.getDMDFeatureIDs();

                // Add the images that should be displayed as determined by the
                // logic in createSymbologyImages()
                rgp.addImages(currStorm, CoordinateSystem.LOCAL);

                // Handle DMD table data
                if (radarRecord.getProductCode() == 149) {
                    // Handle the tabular display data in the Generic Packet
                    String data = GraphicDataUtil.getDMDGraphicDataValue(
                            tableModifier, radarRecord,
                            new Coordinate(currLoc.getLon(), currLoc.getLat()),
                            false);

                    if (!data.equals("")) {
                        featureData = radarRecord.getIcao() + " " + data;
                        addTableRow(tableData, featureData);
                        processTableData = true;
                    }

                }
            }

            int pageNum = 0;
            int currentNumRecords = 0;
            if (processTableData) {
                for (TreeMap<Integer, String> strengthRank : tableData
                        .values()) {
                    for (String stormData : strengthRank.values()) {
                        this.currentPage = pageNum;

                        RadarGraphicsPage gab = this.pageMap.get(pageNum);
                        if (gab == null) {
                            gab = new RadarGraphicsPage(mapDescriptor, gg, ws,
                                    target, color, tableModifier);
                            gab.drawTableBorder(tableModifier.isShowTable());

                            // This should move to the DMD packet
                            String heading = "rda    mid   range@azm      base   depth  rank    msi   llrotv    llg2g   mxrotv   htmxrv";
                            gab.addTableRow(heading);
                            this.pageMap.put(pageNum, gab);
                        }

                        gab.addTableRow(stormData);

                        currentNumRecords++;

                        if (currentNumRecords >= gab.getRecordsPerPage()) {
                            currentNumRecords = 0;
                            pageNum++;
                        }
                    }
                }
            }
            // handle GFM product
            else {
                this.currentPage = pageNum;

                RadarGraphicsPage gab = this.pageMap.get(pageNum);
                if (gab == null) {
                    gab = new RadarGraphicsPage(mapDescriptor, gg, ws, gfmWs,
                            target, color);
                    this.pageMap.put(pageNum, gab);
                }
            }

            this.currentPage = 0;
        }

        // Graphic block is organized into pages for display. The data for each
        // page is contained in packets.
        GraphicBlock gb = radarRecord.getGraphicBlock();
        if (gb != null) {
            Layer[] pages = gb.getPages();

            // Go through each page
            for (int i = 0; i < pages.length; i++) {
                RadarGraphicsPage rgp = this.pageMap.get(i);
                if (rgp == null) {
                    rgp = new RadarGraphicsPage(mapDescriptor, gg, ws, target,
                            color);
                    this.pageMap.put(i, rgp);
                }

                SymbologyPacket[] packets = pages[i].getPackets();

                // Add the packet data (i.e. text, lines for tables, etc.) to
                // the display page
                for (SymbologyPacket sp : packets) {
                    rgp.addSymbologyPacket(sp, CoordinateSystem.SCREEN);
                }
            }

        }
        if ((symbologyData == null || symbologyData.isEmpty()) && gb == null) {
            String nullLegend = null;
            switch (radarRecord.getProductCode()) {
            case 139:
            case 141:
                nullLegend = "NO MESOS";
                break;
            case 143:
                nullLegend = "NO TVS";
                break;
            }
            if (nullLegend != null) {
                RadarGraphicsPage rgp = this.pageMap.get(0);
                if (rgp == null) {
                    rgp = new RadarGraphicsPage(mapDescriptor, gg, ws, target,
                            color);
                    this.pageMap.put(0, rgp);
                }
                rgp.addNullLegend(nullLegend);
            }
        }
        this.setMagnification(magnification);
    }

    /**
     * This method adds data to a row in the table. This is currently tailored
     * to DMD.
     *
     * @param featureData
     *            Data to put in a new row in the table.
     */
    public void addTableRow(
            TreeMap<Integer, TreeMap<Integer, String>> tableData,
            String featureData) {
        // organize Strings based on Strength Rank, then MSI
        int strengthLocation = featureData.indexOf(" r");

        int msi = 0;
        int strengthRank = 0;
        if (strengthLocation >= 0) {
            String startStrengthRank = featureData.substring(strengthLocation);

            String sortValues = startStrengthRank.substring(0, 12);

            String[] values = sortValues.trim().split("\\s+");

            if (values.length > 0 && values[0].length() > 1) {
                try {
                    strengthRank = Integer.parseInt(
                            values[0].substring(1, values[0].length()));
                } catch (NumberFormatException e) {
                    strengthRank = Integer.parseInt(
                            values[0].substring(1, values[0].length() - 1));
                }
            } else {
                strengthRank = 0;
            }

            if (values.length > 1) {
                msi = Integer.parseInt(values[1]);
            }
        }

        TreeMap<Integer, String> secondLevel = null;
        if (tableData.containsKey(strengthRank)) {
            secondLevel = tableData.get(strengthRank);
        } else {
            secondLevel = new TreeMap<>(Collections.reverseOrder());
            tableData.put(strengthRank, secondLevel);
        }

        secondLevel.put(msi, featureData);
    }

    /**
     * @return the currentPage
     */
    public int getCurrentPage() {
        return currentPage;
    }

    /**
     * @param currentPage
     *            the currentPage to set
     */
    public void setCurrentPage(int currentPage) {
        this.currentPage = currentPage;
    }

    public void setMagnification(double magnification) {
        synchronized (symbologyPages) {
            for (RadarGraphicsPage page : symbologyPages) {
                page.setMagnification(magnification);
            }
        }
        synchronized (pageMap) {
            for (RadarGraphicsPage page : pageMap.values()) {
                page.setMagnification(magnification);
            }
        }
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        synchronized (pageMap) {
            if (currentPage < 0 || currentPage >= this.pageMap.size()) {
                return;
            }

            RadarGraphicsPage page = pageMap.get(currentPage);
            if (page == null) {
                return;
            }

            page.paint(target, paintProps);
        }

        synchronized (symbologyPages) {
            for (RadarGraphicsPage currPage : symbologyPages) {
                if (currPage == null) {
                    return;
                }

                currPage.paint(target, paintProps);
            }
        }
    }

    public int getNumPages() {
        synchronized (pageMap) {
            return this.pageMap.size() > 0 ? this.pageMap.size() : 1;
        }
    }

    public void dispose() {
        synchronized (pageMap) {
            for (RadarGraphicsPage page : pageMap.values()) {
                page.dispose();
            }
            pageMap.clear();
        }

        synchronized (symbologyPages) {
            for (RadarGraphicsPage page : symbologyPages) {
                page.dispose();
            }
            symbologyPages.clear();
        }
    }

    private DmdModifier initializeDmdTablePreferences() {
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();

        LocalizationFile file = pm.getStaticLocalizationFile(
                "styleRules" + File.separator + "dmdModifier.xml");
        if (file.exists()) {
            return JAXB.unmarshal(file.getFile(), DmdModifier.class);
        }

        return null;
    }

}
