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
package com.raytheon.uf.edex.plugin.cwat;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataInterrogator;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.monitor.scan.LightningReport;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.SiteMessage;
import com.raytheon.uf.common.monitor.scan.ThreatLocation;
import com.raytheon.uf.common.monitor.scan.ThreatReport;
import com.raytheon.uf.common.monitor.scan.VILReport;
import com.raytheon.uf.edex.plugin.cwat.common.CWATConfig;
import com.raytheon.uf.edex.plugin.cwat.common.LocationCache;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * CWAThreat Product
 * 
 * Generates the CWA Threat messages SCAN
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/06/2009   2037       dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

// ----------------------------------------------------------------------------
public class CWAThreat {

    private CWATConfig cwa_config = null;

    private Coordinate site_mp = null;

    private GeodeticCalculator geoCalc = null;

    private int ivcp = 0;

    private short[] cz = null;

    private short[] vil = null;

    private short[] swp = null;

    private float[] u700Values = null;

    private float[] v700Values = null;

    private float[] u500Values = null;

    private HashMap<ThreatLocation, ThreatReport> conditionalThreats = new HashMap<ThreatLocation, ThreatReport>();

    // private HashMap<String, LocationBin> threatCoordinateList = new
    // HashMap<String, LocationBin>();

    private GridGeometry2D stationGeometry = null;

    private short[] cwaSCTI = new short[ScanUtils.SCAN_GRID_DIM_SQ];

    int maxSCTI = 0;

    private static int max_radius = (int) (ScanUtils.RADIUS_OF_10NM / (ScanUtils.SCAN_GRID_SIZE / ScanUtils.KM_PER_NM));

    /**
     * Default public constructor sets up CWA Threat analysis
     * 
     * @param cwa_config
     */
    public CWAThreat(CWATConfig cwa_config) {
        this.cwa_config = cwa_config;
        this.site_mp = cwa_config.getLatLon();
        this.stationGeometry = ScanUtils.getStationGeometry(site_mp,
                ScanUtils.SCAN_GRID_DIM_RESOLUTION, ScanUtils.SCAN_GRID_DIM);
        this.ivcp = cwa_config.getVil().getVolumeCoveragePattern();
        this.cz = ScanUtils.convertToGrid(cwa_config.getCZ(),
                ScanUtils.SCAN_GRID_DIM_SQ);
        this.vil = ScanUtils.convertToGrid(cwa_config.getVil(),
                ScanUtils.SCAN_GRID_DIM_SQ);
        this.swp = setSWP(new short[ScanUtils.SCAN_GRID_DIM_SQ]);

        this.geoCalc = getGeodeticCalculator();
        geoCalc.setStartingGeographicPoint(site_mp.x, site_mp.y);
    }

    // evaluateThreat.C

    // ----------------------------------------------------------------------------
    //
    // ::evaluateThreat()
    //
    // formerly named storm_logic():
    // This function is adapted from Andy Stern's "storm_position" function
    // in the original thunderstorm application. When called from the
    // analyzeSites
    // function, this code determines what type of thunderstorm threat warning,
    // if any, will be sent for a particular evaluation site.
    // While this is primarily a VIL-based decision algorithm, we have tried
    // to anticipate all realistic possibilities (given the available VIL,
    // composite reflectivity, and lightning data) that could pose a convective
    // threat. The threat message is returned to the analyzeSites routine.
    //
    // Variables:
    //
    // isite_id...............number id of the monitored site (input)
    // cg_near_site...........Number of CG strikes within 10 nmi of the
    // site (input)
    // ivcp...................Volume Coverage Pattern number of the
    // current volume scan (input)
    // threat.................flag for thunderstorm threat; value returned
    // to analyzeSites routine
    // threat detected = 1
    // no threat detected = 0
    // condition..............decision-tree's meteorological result that
    // determines the threat flag above.
    // Explanations for each condition value can
    // be found in the code where it is assigned.
    // ovhd_vil_pct...........areal percent coverage of all VIL within
    // 5nm of the site
    // site_mp................structure to hold lat & lon of a site
    // dist...................distance between two map points, in nm.
    // strong_vil_cell........integer boolean that indicates whether a
    // SCIT cell has a strong associated VIL value
    // (10g/kg/m^2 or above)
    // ltg_active_cell........integer boolean that indicates the presence
    // of CG lightning near a SCIT cell.
    //
    // Parent Routine: analyzeSites()
    // Routines Called: cdistance()
    //
    // History
    // May 1996 Mike Churma(TDL/RDC) - created
    //
    // Spring 1999 M. Tom Filiaggi (TDL) - Major SCANprocessor re-write!
    // June 2000 Tom Filiaggi(TDL) - changes the THUNDERSTORM
    // DECISION TREE a bit to fix a false message.
    // June 2001 Bill Mattison (GSC) - Changed to use Standard Library
    // vectors rather than SeqOf's as containers of
    // StormCellInfo objects.
    // December 2002 M. Churma (MDL) - Added VCP 77 to an "if" statement
    // that checks for precip-mode VCPs (new VCP 77 to appear
    // in ORPG4).
    // October 2003 Tom Filiaggi (MDL) - replaced use of vcp numbers with
    // the vcpinfo_contaner class.
    //
    // Written for Java AWIPS II implementation D. Hladky May 2009

    private ThreatReport evaluateThreat(ThreatLocation loc, ThreatReport report) {

        double dist;
        int jj = 0;
        int xx = 0;
        int yy = 0;
        int check = 0;
        int x1 = 0;
        int x2 = 0;
        int x1_ch = 0;
        int x2_ch = 0;
        int gr = 0;

        //
        // Enter the "evaluateThreat" function, which contains the
        // thunderstorm
        // decision-tree code. The function returns a value of 1 for a
        // thunderstorm threat, and 0 for no threat found.
        //

        ReferencedCoordinate rc = new ReferencedCoordinate(loc.getCoor());
        Coordinate coor = null;
        try {
            coor = rc.asGridCell(stationGeometry, PixelInCell.CELL_CENTER);
        } catch (TransformException e) {
            e.printStackTrace();
        } catch (FactoryException e) {
            e.printStackTrace();
        }
        GeodeticCalculator gc = new GeodeticCalculator(cwa_config.getCZ()
                .getCRS());
        for (jj = -1; jj <= 1; jj += 2) {
            x1 = (int) coor.x - max_radius;
            if (x1 < 0)
                x1 = 0;
            if (x1 > (ScanUtils.SCAN_GRID_DIM - 1))
                x1 = ScanUtils.SCAN_GRID_DIM - 1;
            x2 = (int) coor.x + max_radius;
            if (x2 < 0)
                x2 = 0;
            if (x2 > (ScanUtils.SCAN_GRID_DIM - 1))
                x2 = ScanUtils.SCAN_GRID_DIM - 1;
            for (gr = 0; gr <= max_radius; gr++) {
                if (jj == 1 && gr == 0)
                    gr++;
                yy = (int) coor.y + (gr * jj);
                if (yy < 0 || yy > (ScanUtils.SCAN_GRID_DIM - 1)) {
                    continue;
                }
                check = 0;
                while (check < 1) {
                    ReferencedCoordinate siteCoor1 = new ReferencedCoordinate(
                            new Coordinate(x1, yy), stationGeometry,
                            Type.GRID_CENTER);
                    dist = 0.0;
                    try {
                        dist = ScanUtils.getDistance(loc.getCoor(),
                                siteCoor1.asLatLon(), gc)
                                * ScanUtils.meterToNM;
                    } catch (TransformException e) {
                        e.printStackTrace();
                    } catch (FactoryException e) {
                        e.printStackTrace();
                    }
                    if (dist > ScanUtils.RADIUS_OF_10NM) {
                        x1++;
                        x1_ch = 0;
                    } else {
                        x1_ch = 1;
                    }
                    ReferencedCoordinate siteCoor2 = new ReferencedCoordinate(
                            new Coordinate(x2, yy), stationGeometry,
                            Type.GRID_CENTER);
                    try {
                        dist = ScanUtils.getDistance(loc.getCoor(),
                                siteCoor2.asLatLon(), gc)
                                * ScanUtils.meterToNM;
                    } catch (TransformException e) {
                        e.printStackTrace();
                    } catch (FactoryException e) {
                        e.printStackTrace();
                    }
                    if (dist > ScanUtils.RADIUS_OF_10NM) {
                        x2--;
                        x2_ch = 0;
                    } else {
                        x2_ch = 1;
                    }
                    check = x1_ch * x2_ch;
                    if (x1 >= x2) {
                        check = 1;
                    }
                } // end while
                for (xx = x1; xx <= x2; xx++) {
                    if (yy < 0 || xx < 0 || yy > (ScanUtils.SCAN_GRID_DIM - 1)
                            || xx > (ScanUtils.SCAN_GRID_DIM - 1)) {
                        continue;
                    }

                    if (cz[(ScanUtils.SCAN_GRID_DIM * yy) + xx] >= 40) {
                        report.setHighRefWithinTenNm(true);
                        // System.out.println("CZ > 40 within 10 NM....."
                        // + loc.getLocationName());
                        if (vil[(ScanUtils.SCAN_GRID_DIM * yy) + xx] >= 5) {
                            report.setCoincidentVil(true);
                            // System.out.println("Coincident VIL > 5....."
                            // + loc.getLocationName());
                        }
                    }
                    if (vil[(ScanUtils.SCAN_GRID_DIM * yy) + xx] >= 1) {
                        report.setVilWithinTenNm(true);
                        // System.out.println("VIL within 10 NM....."
                        // + loc.getLocationName());
                    }
                    if (report.isVilWithinTenNm() && report.isCoincidentVil()
                            && report.isHighRefWithinTenNm()) {
                        break;
                    }
                } // end xx loop
                if (report.isVilWithinTenNm() && report.isCoincidentVil()
                        && report.isHighRefWithinTenNm()) {
                    break;
                }
            } // end gr loop
            if (report.isVilWithinTenNm() && report.isCoincidentVil()
                    && report.isHighRefWithinTenNm()) {
                break;
            }
        } // end jj loop

        // Loop through the data for each cell
        for (String cellID : cwa_config.getCellMap().keySet()) {
            double distance = ScanUtils.getDistance(loc.getCoor(), cwa_config
                    .getCellMap().get(cellID), gc)
                    * ScanUtils.meterToNM;
            if (distance <= ScanUtils.RADIUS_OF_10NM) {
                report.setCellWithinTenNm(true);
                break;
                // System.out.println("Cell within 10 NM....." + cellID);
            }
        }

        int value = new RadarDataInterrogator(cwa_config.getVil())
                .getDataValue(loc.getCoor());
        double vilVal = 0.0;
        if (value > 0) {
            vilVal = ScanUtils.decodeVilValue(cwa_config.getVil(), value);
        }

        if (vilVal >= 10) {
            report.setStrongVilCell(true);
            // System.out.println("Strong VIL within 10 NM....."
            // + loc.getLocationName());
        }
        if (report.getCgRateTenNm() > 0) {
            report.setLtgActiveCell(true);
            // System.out.println("Lightning within 10 NM....."
            // + loc.getLocationName());
        }

        //
        // Calculations for least restrictive criterion for TRW trigger:
        // check if there are there are ANY vil within 10nm.
        //
        // =========================================================================
        // THUNDERSTORM DECISION TREE segment below. Note that the ltg-only
        // cases
        // (12&13) and data at all case (14) are handled in the analyzeSites
        // function.
        // =========================================================================
        //

        // ---------- Full data availability (radar & ltg) scenarios ----------

        if (report.isCellWithinTenNm() && report.isStrongVilCell()
                && report.isLtgActiveCell() && report.getCgNearSite() > 0) {
            // Lightning-active cell within 10nm (lightning also within 10nm)
            report.setCondition(1);
            report.setThreat(true);
            // System.out
            // .println("ThreatCondition 1....." + loc.getLocationName());
        } else if (report.getOvhdVilPct() >= 50.0 && report.getCgNearSite() > 0) {
            // No cell, but widespread VIL coverage with ltg
            report.setCondition(2);
            report.setThreat(true);
            // System.out
            // .println("ThreatCondition 2....." + loc.getLocationName());
        } else if (report.isCellWithinTenNm() && report.isStrongVilCell()
                && cwa_config.isLGT()) {
            // Cell nearby with no associated lightning.
            report.setCondition(3);
            report.setThreat(true);
            // System.out
            // .println("ThreatCondition 3....." + loc.getLocationName());
        } else if (cwa_config.isLGT() && report.isHighRefWithinTenNm()
                && report.isCoincidentVil() && report.getCgNearSite() > 0) {
            // High reflectivity with nearby ltg or coincident vil
            report.setCondition(4);
            report.setThreat(true);
            // System.out.println("ThreatCondition 4....."+loc.getLocationName());
        } else if (cwa_config.isLGT() && report.isVilWithinTenNm()
                && report.getCgNearSite() > 0) {
            // Least restrictive full data scenario: any vil and ltg nearby
            report.setCondition(5);
            report.setThreat(true);
            // System.out
            // .println("ThreatCondition 5....." + loc.getLocationName());
        } else if (cwa_config.isLGT() && !report.isCoincidentVil()
                && report.isHighRefWithinTenNm()) {
            // False echoes: nearby high reflectivity with no coincident vil
            report.setCondition(6);
            report.setThreat(false);
            // System.out
            // .println("ThreatCondition 6....." + loc.getLocationName());
        } else if (cwa_config.isLGT()) {
            // No threats identified in full data scenario.
            report.setCondition(7);
            report.setThreat(false);
            // System.out
            // .println("ThreatCondition 7....." + loc.getLocationName());
        }
        // ------------- Radar data only (no ltg data) scenarios ------------
        else if (report.isCellWithinTenNm() && report.isStrongVilCell()
                && !cwa_config.isLGT() && ivcp != 11) {
            // Cell nearby. VCP rules only allow for precip-more or severe-mode
            // TRW flag during evaluations that have LTG data missing.
            report.setCondition(8);
            report.setThreat(true);
            // System.out
            // .println("ThreatCondition 8....." + loc.getLocationName());
        } else if (!cwa_config.isLGT() && report.isHighRefWithinTenNm()
                && report.isCoincidentVil()) {
            // High reflectivity nearby with coincident vil.
            report.setCondition(9);
            report.setThreat(true);
            // System.out.println("ThreatCondition 9....."+loc.getLocationName());
        } else if (report.isHighRefWithinTenNm() && !report.isCoincidentVil()
                && !cwa_config.isLGT()) {
            // False echo check. High base ref with no coincident vil.
            report.setCondition(10);
            // System.out.println("ThreatCondition 10....."
            // + loc.getLocationName());
        }
        // No threats identified in radar-only mode.
        else if (!cwa_config.isLGT()) {
            report.setCondition(11);
            // System.out.println("ThreatCondition 11....."
            // + loc.getLocationName());
        }

        // -------------------- End of decision tree. -------------------------
        // get the messages for the lightning, vil and tstorm threat
        //
        return SiteMessage.getSiteMessage(report);
    }

    /**
     * Gets the threats for the CWA city display
     * 
     * @return
     */
    private void setThreatConditions() {
        for (ThreatLocation loc : cwa_config.getLocations()) {
            int total_cvr = 0;
            int ovhd_vil_count = 0;
            float total_grid_count = 0;
            float ovhd_total_grid_count = 0;

            ThreatReport threatReport = new ThreatReport();
            VILReport vil_report30 = null;
            VILReport vil_report10 = null;

            if (cwa_config.isLGT()) {

                LightningReport lgh_report30 = ScanUtils.getStrikeList(
                        cwa_config.getLightning(), loc.getCoor(),
                        stationGeometry, ScanUtils.RADIUS_OF_30NM,
                        cwa_config.previousVolScanTime, cwa_config.getCZ()
                                .getDataTime().getRefTime());
                LightningReport lgh_report10 = ScanUtils.getStrikeList(
                        cwa_config.getLightning(), loc.getCoor(),
                        stationGeometry, ScanUtils.RADIUS_OF_10NM,
                        cwa_config.previousVolScanTime, cwa_config.getCZ()
                                .getDataTime().getRefTime());

                threatReport.setCgNearSite(lgh_report10.getTotalCGStrikes());
                threatReport.setCgCountThirtyNm(lgh_report30
                        .getTotalCGStrikes());
                threatReport.setCgRateTenNm(lgh_report10.getCgRate());
            }

            //
            // ----------------------------------------------------------------------
            // If radar data is available (with or without lightning), perform
            // calculations to measure the coverage and strength of VIL near the
            // current evaluation site. Then call the "evaluateThreat" function.
            // ----------------------------------------------------------------------
            //

            if (cwa_config.getVil() != null && cwa_config.getCZ() != null) {
                //
                // Determine number and strength of VIL returns within 30 nm of
                // the
                // grid square by calculating pixel coverage. The first call to
                // vilcheck
                // counts light VIL gridpoint signals (from 1 to 9 kg/m^2); the
                // second
                // call counts the moderate signals (10 to 39 kg/m^2); the third
                // counts
                // the heavy VIL signals (40 kg/m^2 or greater).
                //

                // use wfoName and cz icao
                RadarRecord cz = cwa_config.getCZ();
                vil_report30 = new VILReport(loc.getWfoName(), loc.getCoor(),
                        cz.getIcao(), site_mp);
                vil_report30 = getVIL(vil_report30, cwa_config.getVil(),
                        ScanUtils.RADIUS_OF_30NM);
                vil_report10 = new VILReport(loc.getWfoName(), loc.getCoor(),
                        cz.getIcao(), site_mp);
                vil_report10 = getVIL(vil_report10, cwa_config.getVil(),
                        ScanUtils.RADIUS_OF_10NM);

                // Add up total number of VIL returns within 30 km.
                total_cvr = vil_report30.getTotalLgt()
                        + vil_report30.getTotalMdt()
                        + vil_report30.getTotalHvy();
                // Add the three results above for total VIL gridpoint count
                ovhd_vil_count = vil_report10.getOvhdLgt()
                        + vil_report10.getOvhdMdt() + vil_report10.getOvhdHvy();

                // Calculate percent coverage for the different VIL strengths.
                threatReport
                        .setLgtPct((vil_report30.getTotalLgt() / total_grid_count) * 100.0);
                threatReport
                        .setMdtPct((vil_report30.getTotalMdt() / total_grid_count) * 100.0);
                threatReport
                        .setHvyPct((vil_report30.getTotalHvy() / total_grid_count) * 100.0);

                // Total VIL coverage within 30 nm...area percent coverage
                threatReport.setAreaPct((total_cvr / total_grid_count) * 100.0);
                // Total area percent coverage within 10nm of the site of VILs
                threatReport
                        .setOvhdVilPct((ovhd_vil_count / ovhd_total_grid_count) * 100);
            } // end if goodvil and goodrefl

            //
            //
            // Thunderstorm threat evaluation using lightning data only is
            // performed
            // if radar data is unavailable.
            //
            else if (cwa_config.isLGT() && cwa_config.getVil() == null
                    && cwa_config.getCZ() == null) {
                // If there is cg ltg within 10 nm of the site then flag as a
                // threat

                if (threatReport.getCgNearSite() >= 1) {
                    threatReport.setThreat(true);
                    threatReport.setCondition(12);
                } else {
                    threatReport.setThreat(false);
                    threatReport.setCondition(13);
                }
            }

            //
            // The "else if" below accounts for a circumstance in which neither
            // radar nor lightning is available.
            //
            else if (cwa_config.getVil() == null && cwa_config.getCZ() == null
                    && !cwa_config.isLGT()) {
                threatReport.setThreat(false);
                threatReport.setCondition(14);
            }

            threatReport = evaluateThreat(loc, threatReport);

            conditionalThreats.put(loc, threatReport);
            // System.out.println("Location: " + loc.getLocationName() + " : "
            // + threatReport.toString());
        }
    }

    /**
     * Gets the int array for the values that get filled into the CWA Threat
     * grid
     * 
     * @return
     */
    public void genCWA() {

        int[] ltg = new int[ScanUtils.SCAN_GRID_DIM_SQ];
        boolean[] mesoGrid = new boolean[ScanUtils.SCAN_GRID_DIM_SQ];
        boolean[] tvsGrid = new boolean[ScanUtils.SCAN_GRID_DIM_SQ];

        int ii;
        boolean debug = cwa_config.getGenerator().logger.isDebugEnabled();

        // Initialize the grids.
        for (ii = 0; ii < ScanUtils.SCAN_GRID_DIM_SQ; ii++) {
            cwaSCTI[ii] = 0;
            ltg[ii] = 0;
            mesoGrid[ii] = false;
            tvsGrid[ii] = false;
        }

        // Match SWP (cell-based), MESO, and TVS data to coordinates in the
        // 116*116 grids.
        // loop through the lists of cells identified in the MESO and TVS algos.
        // Mark as true or false if a MESO or TVS exists.

        if (cwa_config.isMD()) {
            RadarRecord md = cwa_config.getMD();
            List<String> features = md
                    .getIds(RadarConstants.MapValues.MESO_TYPE);
            if (features != null && features.size() > 0) {
                for (String feature : features) {

                    double range = new Double(md.getProductVals(
                            RadarConstants.MapValues.MESO_TYPE, feature,
                            RadarConstants.MapValues.MESO_AZIMUTH_RANGE));
                    double azimuth = new Double(md.getProductVals(
                            RadarConstants.MapValues.MESO_TYPE, feature,
                            RadarConstants.MapValues.MESO_AZIMUTH_DIRECTION));

                    range = range * ScanUtils.NMI_TO_KM * 1000;

                    Coordinate mdCoor = RadarRecordUtil.getAzRangeLatLon(md,
                            feature, md.getSpatialObject().getLat(), md
                                    .getSpatialObject().getLon(), new Double(
                                    azimuth).toString(), new Double(range)
                                    .toString());

                    ReferencedCoordinate rc = new ReferencedCoordinate(mdCoor);
                    Coordinate coor = null;

                    if (debug) {
                        cwa_config.getGenerator().logger
                                .info("CWAThreat: MD feature: " + feature
                                        + " Lon/Lat: " + mdCoor.x + " : "
                                        + mdCoor.y);
                    }
                    try {
                        coor = rc.asGridCell(stationGeometry,
                                PixelInCell.CELL_CENTER);

                        if (ScanUtils.SCAN_GRID_DIM > (int) coor.y
                                && (int) coor.y > 0
                                && ScanUtils.SCAN_GRID_DIM > (int) coor.x
                                && (int) coor.x > 0) {

                            mesoGrid[(ScanUtils.SCAN_GRID_DIM * (int) coor.y)
                                    + (int) coor.x] = true;
                        }
                    } catch (TransformException te) {
                        cwa_config.getGenerator().logger.error("CWAThreat: "
                                + te);
                    } catch (FactoryException fe) {
                        cwa_config.getGenerator().logger.error("CWAThreat: "
                                + fe);
                    }
                }
            }
        }

        if (cwa_config.isTVS()) {
            RadarRecord td = cwa_config.getTVS();

            ArrayList<String> features = (ArrayList<String>) td
                    .getIds(MapValues.TVS_TYPE);

            if (features != null && features.size() > 0) {
                for (String feature : features) {

                    double range = new Double(td.getProductVals(
                            RadarConstants.MapValues.TVS_TYPE, feature,
                            RadarConstants.MapValues.TVS_RANGE));
                    double azimuth = new Double(td.getProductVals(
                            RadarConstants.MapValues.TVS_TYPE, feature,
                            RadarConstants.MapValues.TVS_AZIMUTH));

                    range = range * ScanUtils.NMI_TO_KM * 1000;

                    Coordinate tdCoor = RadarRecordUtil.getAzRangeLatLon(td,
                            feature, td.getSpatialObject().getLat(), td
                                    .getSpatialObject().getLon(), new Double(
                                    azimuth).toString(), new Double(range)
                                    .toString());

                    ReferencedCoordinate rc = new ReferencedCoordinate(tdCoor);
                    Coordinate coor = null;

                    if (debug) {
                        cwa_config.getGenerator().logger
                                .info("CWAThreat: TVS feature: " + feature
                                        + " Lon/Lat: " + tdCoor.x + " : "
                                        + tdCoor.y);
                    }
                    try {
                        coor = rc.asGridCell(stationGeometry,
                                PixelInCell.CELL_CENTER);

                        if (ScanUtils.SCAN_GRID_DIM > (int) coor.y
                                && (int) coor.y > 0
                                && ScanUtils.SCAN_GRID_DIM > (int) coor.x
                                && (int) coor.x > 0) {

                            tvsGrid[(ScanUtils.SCAN_GRID_DIM * (int) coor.y)
                                    + (int) coor.x] = true;
                        }
                    } catch (TransformException te) {
                        cwa_config.getGenerator().logger.error("CWAThreat: "
                                + te);
                    } catch (FactoryException fe) {
                        cwa_config.getGenerator().logger.error("CWAThreat: "
                                + fe);
                    }
                }
            }
        }

        // process most recent lightning strikes if we have lightning data
        if (cwa_config.isLGT()) {
            for (BinLightningRecord rec : cwa_config.getLightning()) {
                for (int i = 0; i < rec.getLatitudes().length; i++) {
                    ReferencedCoordinate rc = new ReferencedCoordinate(
                            new Coordinate((double) rec.getLongitudes()[i],
                                    (double) rec.getLatitudes()[i], 0.0));
                    Coordinate coor = null;

                    try {
                        coor = rc.asGridCell(stationGeometry,
                                PixelInCell.CELL_CENTER);

                        if (ScanUtils.SCAN_GRID_DIM > (int) coor.y
                                && (int) coor.y > 0
                                && ScanUtils.SCAN_GRID_DIM > (int) coor.x
                                && (int) coor.x > 0) {
                            // add a strike to the referenced grid
                            int current = ltg[(ScanUtils.SCAN_GRID_DIM * (int) coor.y)
                                    + (int) coor.x];
                            ltg[(ScanUtils.SCAN_GRID_DIM * (int) coor.y)
                                    + (int) coor.x] = current++;
                        }

                    } catch (Exception e) {
                        cwa_config.getGenerator().logger.error("CWAThreat: "
                                + e);
                    }
                }
            }
        }

        // Loop through the grids to look for correspondence between the various
        // gridded parameters. Assign unique Threat Index values accordingly.
        for (ii = 0; ii < ScanUtils.SCAN_GRID_DIM_SQ; ii++) {
            if ((ltg[ii] >= 2) || (vil[ii] >= 10)
                    || ((cz[ii] >= 40) && (ltg[ii] >= 1))) {
                cwaSCTI[ii] = 10;
            }
            // ///// First cell criterion : SWP < 30.
            if (swp[ii] < 30) {
                // System.out.println(" SWP[" + ii + "] value < 30: " +
                // swp[ii]);
                if (mesoGrid[ii] && tvsGrid[ii]) {
                    // System.out.println("MESO && TVS == true, 40");
                    cwaSCTI[ii] = 40;
                } else if (mesoGrid[ii] || tvsGrid[ii]) {
                    // System.out.println("MESO || TVS == true, 30");
                    cwaSCTI[ii] = 30;
                }
                // System.out.println(" value set too: " + cwaSCTI[ii]);
            }
            // ///// Second criterion : 30 <= SWP < 70
            else if ((swp[ii] >= 30) && (swp[ii] < 70)) {
                // System.out.println(" SWP[" + ii
                // + "] value >= 30 and value < 70: " + swp[ii]);
                if (mesoGrid[ii] && tvsGrid[ii]) {
                    // System.out.println("MESO and TVS == true, 70");
                    cwaSCTI[ii] = 70;
                } else if (mesoGrid[ii] || tvsGrid[ii]) {
                    // System.out.println("MESO || TVS == true, 60");
                    cwaSCTI[ii] = 60;
                } else {
                    // System.out.println("MESO && TVS == false, 50");
                    cwaSCTI[ii] = 50;
                }
                // System.out.println(" value set too: " + cwaSCTI[ii]);
            }
            // ///// Third criterion : SWP >= 70
            else if (swp[ii] >= 70) {
                // System.out.println(" SWP[" + ii + "] value >= 70: " +
                // swp[ii]);
                if (mesoGrid[ii] && tvsGrid[ii]) {
                    // System.out.println("MESO && TVS == true, 100");
                    cwaSCTI[ii] = 100;
                } else if (mesoGrid[ii] || tvsGrid[ii]) {
                    // System.out.println("MESO || TVS == true, 90");
                    cwaSCTI[ii] = 90;
                } else {
                    // System.out.println("MESO && TVS == false, 80");
                    cwaSCTI[ii] = 80;
                }
                // System.out.println(" value set too: " + cwaSCTI[ii]);
            }
            // Set the maxSCTI: If the current SCTI value is greater
            if (cwaSCTI[ii] > maxSCTI) {
                maxSCTI = cwaSCTI[ii];
            }
        }
        // set the treat conditional array
        setThreatConditions();
    }

    /**
     * Gets the Severe WX prob array
     * 
     * @param swp
     * @return
     */
    private short[] setSWP(short[] swp) {

        if (cwa_config.isModel()) {
            u700Values = ((FloatDataRecord) cwa_config.getU700()
                    .getMessageData()).getFloatData();
            v700Values = ((FloatDataRecord) cwa_config.getV700()
                    .getMessageData()).getFloatData();
            u500Values = ((FloatDataRecord) cwa_config.getU500()
                    .getMessageData()).getFloatData();
        }

        for (int x = 0; x < ScanUtils.SCAN_GRID_DIM; x++) {
            for (int y = 0; y < ScanUtils.SCAN_GRID_DIM; y++) {
                swp[(ScanUtils.SCAN_GRID_DIM * x) + y] = getSevereWxThreatProb(new Coordinate(
                        x, y));
            }
        }

        return swp;
    }

    /**
     * 
     * Obtain the severeWXProb for points in the SCAN grid.
     * 
     * @param Point
     * @return short
     */
    private short getSevereWxThreatProb(Coordinate gridPt) {

        float maxvil = 0.0f;
        float isvg10 = 0.0f;
        float isvg20 = 0.0f;
        float svrWxProb = 0.0f;
        int lower = 3;
        int upper = ScanUtils.SCAN_GRID_DIM - 4;
        int ii = 0;
        int jj = 0;
        boolean useVil = false;

        if (gridPt.x < lower)
            gridPt.x = lower;
        if (gridPt.x > upper)
            gridPt.x = upper;
        if (gridPt.y < lower)
            gridPt.y = lower;
        if (gridPt.y > upper)
            gridPt.y = upper;

        for (ii = (int) gridPt.x - 1; ii <= (int) gridPt.x + 1; ii++) {
            for (jj = (int) gridPt.y - 1; jj <= (int) gridPt.y + 1; jj++) {
                if (ii < 0 || jj < 0 || ii >= ScanUtils.SCAN_GRID_DIM
                        || jj >= ScanUtils.SCAN_GRID_DIM) {
                    continue;
                }
                if (vil[(ii * ScanUtils.SCAN_GRID_DIM) + jj] > maxvil) {
                    maxvil = vil[(ii * ScanUtils.SCAN_GRID_DIM) + jj];
                }
            }
        }

        for (ii = (int) gridPt.x - 3; ii <= (int) gridPt.x + 3; ii++) {
            for (jj = (int) gridPt.y - 3; jj <= (int) gridPt.y + 3; jj++) {
                if (ii < 0 || jj < 0 || ii >= ScanUtils.SCAN_GRID_DIM
                        || jj >= ScanUtils.SCAN_GRID_DIM) {
                    continue;
                }
                if (vil[(ii * ScanUtils.SCAN_GRID_DIM) + jj] > 20)
                    isvg20++;
                if (vil[(ii * ScanUtils.SCAN_GRID_DIM) + jj] > 10)
                    isvg10++;
            }
        }

        if (cwa_config.isModel()) {

            if (cwa_config.getVil().getLongitude() > -85.0) {

                float spd700 = getSpeed700(gridPt);
                svrWxProb = (float) (-16.37 + (2.33 * isvg20) + (1.02 * spd700) + (0.646 * maxvil));
            }

            else {
                if (cwa_config.isUA()) {

                    float u500 = getU500(gridPt);
                    svrWxProb = (float) (-16.49
                            + maxvil
                            * ((0.025 * maxvil) - (0.00206 * cwa_config
                                    .getSounding().firstFreezingLevel()))
                            + (0.365 * u500) + (0.341 * cwa_config
                            .getSounding().totalTotals()));
                } else {
                    useVil = true;
                }
            }

            if (svrWxProb < 0.0) {
                svrWxProb = 0.0f;
            } else if (svrWxProb > 100.0) {
                svrWxProb = 100.0f;
            }
        } else {
            useVil = true;
        }

        if (useVil) {

            // envDataBad
            //
            // If environmental data is missing or bizarre, use VIL-Only
            // relationships.
            //

            if (maxvil >= 70) {
                svrWxProb = 99.0f;
            } else if (maxvil >= 50) {
                svrWxProb = 40.0f;
            } else if (maxvil >= 40) {
                svrWxProb = 29.0f;
            } else if (maxvil >= 30) {
                svrWxProb = 21.0f;
            } else if (maxvil >= 20) {
                svrWxProb = 12.0f;
            } else if (maxvil >= 10) {
                svrWxProb = 8.0f;
            } else if (maxvil >= 5) {
                svrWxProb = 4.0f;
            } else {
                svrWxProb = 8.0f;
            }
        }

        return (short) svrWxProb;
    }

    /**
     * Gets the max value of SCTI
     * 
     * @return
     */
    public int getMaxSCTI() {
        return maxSCTI;
    }

    /**
     * Gets the CWA Threat Grid
     * 
     * @return
     */
    public short[] getCWAThreat() {
        return cwaSCTI;
    }

    /**
     * Gets the threat condition grid used for thunderstorm activity index
     * (Queried by City) on the front
     * 
     * @return
     */
    public HashMap<ThreatLocation, ThreatReport> getThreatConditions() {
        return conditionalThreats;
    }

    public GeodeticCalculator getGeodeticCalculator() {
        return new GeodeticCalculator();
    }

    /**
     * Gets a report of lgt, mod, hvy VIL
     * 
     * @param rec
     * @param cellCoor
     * @param radarSite
     * @param distance
     * @return
     * @throws Exception
     */
    private VILReport getVIL(VILReport report, RadarRecord vilRec,
            double distance) {

        int hvy = 0;
        int mod = 0;
        int light = 0;

        // go over the x, y scan grid finding lightning within
        // 10 and 30 nm of each point in grid
        // determine coordinate in X,Y relative to the Radar
        Coordinate[] coordArr = LocationCache.getInstance().getVilLatLons(
                report, vilRec, distance);
        int[] dataValues = new RadarDataInterrogator(vilRec)
                .getDataValues(coordArr);
        for (int dataValue : dataValues) {
            int value = (int) ScanUtils.decodeVilValue(vilRec, dataValue);
            // Determine number and strength of VIL returns within 30 nm
            // of the
            // grid square by calculating pixel coverage. The first call
            // to vilcheck
            // counts light VIL gridpoint signals (from 1 to 9 kg/m^2);
            // the second
            // call counts the moderate signals (10 to 39 kg/m^2); the
            // third counts
            // the heavy VIL signals (40 kg/m^2 or greater).
            //
            if (value <= 9 && value > 0) {
                light++;
            } else if (value > 9 && value < 40) {
                mod++;
            } else if (value > 40) {
                hvy++;
            }
        }

        report.setTotalLgt(light);
        report.setTotalMdt(mod);
        report.setTotalHvy(hvy);

        return report;
    }

    /**
     * Get speed at 700
     * 
     * @param coor
     * @return
     */
    private float getSpeed700(Coordinate coor) {

        // should be the same, I'd hope anyway (U&V)
        Coordinate gridCoor = getGridCoordinate(coor, cwa_config.getV700());

        float u = u700Values[(cwa_config.getU700().getSpatialObject().getNx() * (int) gridCoor.y)
                + (int) gridCoor.x];
        float v = v700Values[(cwa_config.getV700().getSpatialObject().getNx() * (int) gridCoor.y)
                + (int) gridCoor.x];

        // simple right triangle trig
        float windSpeed = (float) ((Math.sqrt((u * u) + (v * v))) * ScanUtils.M_PER_SEC_TO_KTS);

        return windSpeed;
    }

    /**
     * gets the u component of the wind at 500mb
     * 
     * @param coor
     * @return
     */
    private float getU500(Coordinate coor) {

        Coordinate gridCoor = getGridCoordinate(coor, cwa_config.getU500());

        float u500 = u500Values[(cwa_config.getU500().getSpatialObject()
                .getNx() * (int) gridCoor.y)
                + (int) gridCoor.x];

        return u500;
    }

    /**
     * Gets the swp referenced grid coordinate
     * 
     * @param coor
     * @param rec
     * @return
     */
    private Coordinate getGridCoordinate(Coordinate coor, GridRecord rec) {

        Coordinate latLonCoor = null;
        ReferencedCoordinate rcoor = new ReferencedCoordinate(coor,
                stationGeometry, Type.GRID_CENTER);

        try {
            latLonCoor = rcoor.asLatLon();
        } catch (TransformException e) {
            e.printStackTrace();
        } catch (FactoryException e) {
            e.printStackTrace();
        }

        // now reference back to the actual grib model
        Coordinate gridCoor = MapUtil.latLonToGridCoordinate(latLonCoor,
                PixelOrientation.CENTER, rec.getSpatialObject());

        return gridCoor;
    }
}
