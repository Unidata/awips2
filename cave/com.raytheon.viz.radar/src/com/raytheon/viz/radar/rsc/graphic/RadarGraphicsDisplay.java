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
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint.RadarProductType;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.Layer;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.radar.rsc.graphic.RadarGraphicsPage.CoordinateSystem;
import com.raytheon.viz.radar.util.DmdModifier;
import com.raytheon.viz.radar.util.GraphicDataUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2009            chammack     Initial creation
 * 03/04/2013   DCS51      zwang        Handle GFM product
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class RadarGraphicsDisplay implements IRenderable {

    private Map<Integer, RadarGraphicsPage> pageMap;

    private ArrayList<RadarGraphicsPage> symbologyPages;

    private Map<RadarDataKey, RadarDataPoint> symbologyData;

    private int currentPage;

    /**
     * The offset needed to bring the radar coordinate system into something
     * geotools can handle
     */
    public static final int X_OFFSET = 2048;

    /**
     * The offset needed to bring the radar coordinate system into something
     * geotools can handle
     */
    public static final int Y_OFFSET = 2048;

    public RadarGraphicsDisplay(RadarRecord radarRecord,
            IGraphicsTarget target, IMapDescriptor mapDescriptor,
            Set<String> filteredStormIds, double magnification, RGB color)
            throws VizException {
        this.pageMap = new HashMap<Integer, RadarGraphicsPage>();
        this.symbologyPages = new ArrayList<RadarGraphicsPage>();
        this.currentPage = 0;

        // Only retrieve if this record has not been retrieved.
        if ((radarRecord.getSymbologyData() == null || radarRecord
                .getSymbologyData().isEmpty())
                && radarRecord.getGraphicBlock() == null) {
            File loc = HDF5Util.findHDF5Location(radarRecord);

            IDataStore dataStore = DataStoreFactory.getDataStore(loc);

            try {
                RadarDataRetriever.populateRadarRecord(dataStore, radarRecord);
            } catch (Exception e) {
                throw new VizException("Unable to retrieve radar graphics", e);
            }
        }

        ProjectedCRS crs = radarRecord.getCRS();
        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        // Per section 3.3.3
        generalEnvelope.setCoordinateReferenceSystem(crs);
        generalEnvelope.setRange(0, -256000 * 2, 256000 * 2);
        generalEnvelope.setRange(1, -256000 * 2, 256000 * 2);

        // [-2048, 2048] == range of 4095 (inclusive 0), plus 1 because
        // GGR is exclusive (?)
        GeneralGridGeometry gg = new GeneralGridGeometry(
                new GeneralGridEnvelope(new int[] { 0, 0 }, new int[] { 4096,
                        4096 }, false), generalEnvelope);
        IWireframeShape ws = target.createWireframeShape(true, mapDescriptor);
        
        // Used for GFM forecast positions
        IWireframeShape gfmWs = target.createWireframeShape(true, mapDescriptor);

        symbologyData = radarRecord.getSymbologyData();
        if (symbologyData != null) {
            RadarGraphicsPage rgp = new RadarGraphicsPage(mapDescriptor, gg,
                    ws, gfmWs, target, color);

            this.symbologyPages.add(rgp);
            // Determine if each set of Storm data should be displayed
            RadarDataPoint currStorm;
            TreeMap<Integer, TreeMap<Integer, String>> tableData = new TreeMap<Integer, TreeMap<Integer, String>>(
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

                //Handle DMD table data
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
                for (TreeMap<Integer, String> strengthRank : tableData.values()) {
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
                    strengthRank = Integer.parseInt(values[0].substring(1,
                            values[0].length()));
                } catch (NumberFormatException e) {
                    strengthRank = Integer.parseInt(values[0].substring(1,
                            values[0].length() - 1));
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
            secondLevel = new TreeMap<Integer, String>(
                    Collections.reverseOrder());
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
        for (RadarGraphicsPage page : symbologyPages) {
            page.setMagnification(magnification);
        }
        for (RadarGraphicsPage page : pageMap.values()) {
            page.setMagnification(magnification);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if (currentPage < 0 || currentPage >= this.pageMap.size()) {
            return;
        }

        RadarGraphicsPage page = pageMap.get(currentPage);
        if (page == null) {
            return;
        }
        page.paint(target, paintProps);

        for (RadarGraphicsPage currPage : symbologyPages) {
            if (currPage == null) {
                return;
            }

            currPage.paint(target, paintProps);
        }
    }

    public int getNumPages() {
        return this.pageMap.size() > 0 ? this.pageMap.size() : 1;
    }

    public void dispose() {
        for (RadarGraphicsPage page : pageMap.values()) {
            page.dispose();
        }
    }

    private DmdModifier initializeDmdTablePreferences() {
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();

        LocalizationFile file = pm.getStaticLocalizationFile("styleRules"
                + File.separator + "dmdModifier.xml");
        if (file.exists()) {
            return JAXB.unmarshal(file.getFile(), DmdModifier.class);
        }

        return null;
    }

}
