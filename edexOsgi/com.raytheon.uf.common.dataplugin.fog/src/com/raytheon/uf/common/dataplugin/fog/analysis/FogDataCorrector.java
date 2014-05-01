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
package com.raytheon.uf.common.dataplugin.fog.analysis;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class essentially sets the Image Type grouping for Day(VIS), Night(IR), or Twilight(combo)
 * 
 * @author dhladky
 * 
 */
public class FogDataCorrector {

    public int grid_space = 1;

    public int satHeight = 0;

    public double twilight_angle_offset = 0;

    public FogSolarCalculator solarCal = null;

    public FogSatCalculator satCal = null;

    private final FogRecord rec;

    public FogDataCorrector(FogRecord rec, double twilight_angle_offset) {
        this.rec = rec;
        this.twilight_angle_offset = twilight_angle_offset;
    }

    // -Public---------------------------------------------------------------------
    // NAME: Execute()
    //
    // TYPE: Public Member function
    //
    // Description:
    // Execute the data correction.
    // Currently only VIS data is normalized, because the VIS data is
    // strongly affected by some factors like solar elevation angle,
    // Considering the computation time, the normalization is not done pixel
    // by pixel. Use a lower resolution grid to cover the original grid
    // Correction at original grid point uses the correction at
    // the nearest grid point in the lower resolution grid.
    // _grid_space denotes the span between the lower resolution grid.
    // Besides data correction, Fog_DataCorrector also build up
    // the ranges of the valid VIS data and IR data at each row.
    //
    // Note: the correction is based on the paper by albers from FSL.
    // 1992: Photometric correction of GOES visible satellite image
    // 6th conference on satellite meteorology and oceanography
    // Arguments: None
    //
    // History:
    // March 2004 Qin Zeng (GDMB/MDL) -- created
    // Sep. 2004 Qi Zhu (RSIS/MDL) -- Added code to set the satellite
    // height to the object of
    // Fog_SatCalculator class.
    // Sept. 2004 Bill Mattison (SAIC) -- updated interfaces to correct
    // interface with class SetupAccessor.
    // Dec 2009 D Hladky ported to JAVA
    // -----------------------------------------------------------------------------

    public FogRecord execute() {

        // TODO: Remember in bizarro AWIPS world J is X and I is Y
        // 58 degree is a semi-empirical parameter(angle) for VIS correction
        double EMPIRICAL_REFERENCE_ANGLE = 58.0; // UNIT DEGREE
        double SIN_MINIMUM_ELEVATION = Math.sin(Math.toRadians(8.0));
        // setup solar calculator
        if (solarCal == null) {
            solarCal = new FogSolarCalculator();
            solarCal.setDataTime(rec.getDataTime());
        }

        // setup satellite calcualtor
        if (satCal == null) {
            satCal = new FogSatCalculator();
            satCal.setLocation(new Coordinate(rec.getLon(), rec.getLat()));
            satCal.setSatLon(rec.getSatLon());
            satCal.setSatHeight((float) rec.getSatHeight());
        }

        int i = 0;
        int j = 0;
        double r1;
        ReferencedCoordinate tem_point;
        // SetupAccessor *_setup_accessor=
        // SetupAccessor::getInstance(_usingSAFESEAS);
        float sin_ref_angle = (float) Math.sin(Math.toRadians(EMPIRICAL_REFERENCE_ANGLE));
        float sin_twi = (float) Math.sin(Math.toRadians(twilight_angle_offset));
        float tem_sin_sunelev;

        // -------------------------------------------------------------------------
        int rso = rec.getNy();
        int cso = rec.getNx();
        float[][] sin_solar_elev;
        float[][] cos_p; // cos phase angle
        float[][] cos_e; // cos emission angle

        // TODO write function to determine
        // if by Solar Calculator you need to do the vis correction
        if (solarCal.visNeeded) {
            // Begin the VIS correction
            // _grid_space * _grid_space data correction

            // rso= Row Size Originally
            // cso= Column Size Orignally
            // rss= Row Size with Span
            // css= Column Size with Span
            int rss = rso / grid_space;
            int css = cso / grid_space;
            sin_solar_elev = new float[rss][css];
            cos_p = new float[rss][css];
            cos_e = new float[rss][css];

            // clear the elevation values
            for (i = 0; i < rss; ++i) {
                for (j = 0; j < css; ++j) {
                    sin_solar_elev[i][j] = -999.0f;
                }
            }

            // crs = Current Row with Span
            // ccs = Current Column with Span
            int crs = 0;
            int ccs = 0;
            Coordinate tem_loc = null;
            float CorrectionWeight;

            for (i = 0; i < rso; ++i) {
                crs = (int) (i / (grid_space * 1.0) + 0.5);
                rec.setRangeType(FogRecord.IMAGE_GROUP.TWILIGHT_GROUP, i);

                if (crs > rss - 1) {
                    crs = rss - 1;
                }
                // pointI, and pointJ are the i and j
                // of the point where day and night are divided in row i

                if (sin_solar_elev[crs][0] == -999.0) {
                    tem_point = new ReferencedCoordinate(new Coordinate(0, i, 0));
                    try {
                        tem_point.asGridCell(rec.getGridGeometry(), PixelInCell.CELL_CENTER);
                    } catch (TransformException e) {

                        e.printStackTrace();
                    } catch (FactoryException e) {

                        e.printStackTrace();
                    }
                    try {
                        tem_loc = tem_point.asLatLon();
                    } catch (TransformException e) {

                        e.printStackTrace();
                    } catch (FactoryException e) {

                        e.printStackTrace();
                    }
                    solarCal.setLocation(tem_loc);
                    sin_solar_elev[crs][0] = solarCal.getSinSolarElevation();
                }

                // Initialize as -1, if the pointJ.. NOT found, remain -1;
                // int pointI = i;
                int pointJ = -1;
                int pointJ_twi1 = -1;
                int pointJ_twi2 = -1;
                int twiC = 0;

                // start j loop
                for (j = 0; j < cso; ++j) {

                    ccs = (int) (j / (grid_space * 1.0) + 0.5);

                    if (ccs > css - 1) {
                        ccs = css - 1;
                    }

                    if (sin_solar_elev[crs][ccs] == -999.0) { // never been
                        // calculated
                        // awips world remember
                        tem_point = new ReferencedCoordinate(new Coordinate(j, i, 0));
                        try {
                            tem_point.asGridCell(rec.getGridGeometry(), PixelInCell.CELL_CENTER);
                        } catch (TransformException e) {

                            e.printStackTrace();
                        } catch (FactoryException e) {

                            e.printStackTrace();
                        }
                        try {
                            tem_loc = tem_point.asLatLon();
                        } catch (TransformException e) {

                            e.printStackTrace();
                        } catch (FactoryException e) {

                            e.printStackTrace();
                        }
                        // ================Solar
                        solarCal.setLocation(tem_loc);
                        sin_solar_elev[crs][ccs] = solarCal.getSinSolarElevation();
                        Coordinate tem_solarv3d = solarCal.solarV3D();
                        // ================Sat
                        satCal.setLocation(tem_loc);
                        Coordinate tem_satv3d = satCal.satV3D();
                        cos_e[crs][ccs] = FogMonitorUtils.CosZenith(tem_satv3d);
                        cos_p[crs][ccs] = FogMonitorUtils.CosAngleBetween(tem_solarv3d, tem_satv3d);
                    }
                    // sin_twi >0 so (sin_solar_elev >sin_twi) means daytime
                    // data.
                    if (sin_solar_elev[crs][ccs] > sin_twi) {
                        // begin the correction
                        double sinSolarElevation = sin_solar_elev[crs][ccs];

                        if (sinSolarElevation < SIN_MINIMUM_ELEVATION) {
                            sinSolarElevation = SIN_MINIMUM_ELEVATION;
                        }
                        // actual value for VIS satellite, only time it is
                        // accessed here
                        // (SK) r1 = rec.getVisArray()[(rec.getNy()*j)+i];
                        r1 = rec.getVisArray()[rec.getNx() * i + j];
                        // -------------------------------------------------------
                        //
                        // Fine tune applied here is to make the normalization
                        // more realistic.
                        // So the data used following are purely empirical.
                        // ------------------------------------------------------
                        if (r1 < 68 && r1 >= 30 && sin_solar_elev[crs][ccs] < SIN_MINIMUM_ELEVATION) {
                            CorrectionWeight = (float) (30 + 20 * sinSolarElevation + 10 * r1 / 68.0);
                        }
                        // This is a mix of cloud, noise or even cloud free area
                        // because the value and the elevation are too small.
                        else if (r1 < 30 && sin_solar_elev[crs][ccs] < SIN_MINIMUM_ELEVATION) {
                            CorrectionWeight = 30;
                        }
                        // This is a suspected cloud free area
                        else if (r1 < 68 && sin_solar_elev[crs][ccs] > SIN_MINIMUM_ELEVATION) {
                            CorrectionWeight = (float) (20 + 20 * r1 / 68);
                        } else { // This is the cloudy area.
                            CorrectionWeight = (float) (50 + 10 * r1 / 255.0);
                        }

                        r1 = r1 - CorrectionWeight * Math.log(sinSolarElevation / sin_ref_angle);
                        // float phasecorrection;
                        // if cos value of the phase angle less than 0.6
                        // If cos(phase angle) <0.6, the phase angle correction
                        // will be less than 1, so ignore it to reduce
                        // calculation
                        if (cos_p[crs][ccs] >= 0.6) {
                            float pCorrection = 20 * cos_p[crs][ccs] * cos_p[crs][ccs]
                                                                                  * cos_p[crs][ccs] * cos_p[crs][ccs] * cos_p[crs][ccs]
                                                                                                                                   * cos_p[crs][ccs] * cos_e[crs][ccs] * sin_solar_elev[crs][ccs];
                            r1 += pCorrection;
                        }

                        if (r1 > 255) {
                            r1 = 255; // Just in case
                        }
                        if (r1 < 0) {
                            r1 = 0; // Just in case
                        }

                        // (SK) rec.getVisArray()[rec.getNy() * j + i] = (short) r1;
                        rec.getVisArray()[rec.getNx() * i + j] = (short) r1;
                    } else {
                        // Nighttime pixel value will be set to zero in VIS
                        // array
                        // (SK) rec.getVisArray()[rec.getNy() * j + i] = 0;
                        rec.getVisArray()[rec.getNx() * i + j] = 0;
                    }

                    // Enter twilight area and leave twilight area
                    if (Math.abs(sin_solar_elev[crs][ccs]) < sin_twi && twiC == 0
                            || Math.abs(sin_solar_elev[crs][ccs]) > sin_twi && twiC == 1) {
                        if (ccs == 0) {
                            // first point entering twilight is the first
                            // point of the row
                            twiC++;
                            pointJ_twi1 = 0;
                        } else {
                            // Because the elevation calculation uses a lower
                            // resolution, so the first point entering twilight
                            // must be the point between the current and the
                            // last
                            // lower resolution points.
                            int k_end = ccs * grid_space;
                            // Search each points between two low resolution
                            // points
                            // at high resolution(not spanned) grid.
                            for (int k = (ccs - 1) * grid_space + 1; k <= k_end; ++k) {

                                tem_point = new ReferencedCoordinate(new Coordinate(k, i, 0));
                                try {
                                    tem_point.asGridCell(rec.getGridGeometry(),
                                            PixelInCell.CELL_CENTER);
                                } catch (TransformException e) {

                                    e.printStackTrace();
                                } catch (FactoryException e) {

                                    e.printStackTrace();
                                }
                                try {
                                    tem_loc = tem_point.asLatLon();
                                } catch (TransformException e) {

                                    e.printStackTrace();
                                } catch (FactoryException e) {

                                    e.printStackTrace();
                                }
                                solarCal.setLocation(tem_loc);
                                tem_sin_sunelev = solarCal.getSinSolarElevation();

                                if (twiC == 0) { // The first point at current
                                    // row
                                    if (Math.abs(tem_sin_sunelev) <= sin_twi) {
                                        pointJ_twi1 = k;
                                        twiC++;
                                        break;
                                    }
                                } else { // The second point at current row
                                    if (tem_sin_sunelev >= sin_twi) {
                                        pointJ_twi2 = k;
                                        twiC++;
                                        break;
                                    }
                                }
                            } // end of k loop
                        } // end of if
                    } // end of "if ------ "

                    // If the grid_span is too large, both twilight points may
                    // be
                    // missing(Because never enter the twilight area),
                    // then scan for day and night separator point.
                    // If twilight points are found then day/night separator
                    // point
                    // is omitted.
                    if (sin_solar_elev[crs][ccs] * sin_solar_elev[crs][0] < 0 && twiC == 0) {
                        int k_end = ccs * grid_space;
                        for (int k = (ccs - 1) * grid_space + 1; k < k_end + 1; ++k) {

                            tem_point = new ReferencedCoordinate(new Coordinate(k, i, 0));
                            try {
                                tem_point
                                .asGridCell(rec.getGridGeometry(), PixelInCell.CELL_CENTER);
                            } catch (TransformException e) {

                                e.printStackTrace();
                            } catch (FactoryException e) {

                                e.printStackTrace();
                            }
                            try {
                                tem_loc = tem_point.asLatLon();
                            } catch (TransformException e) {

                                e.printStackTrace();
                            } catch (FactoryException e) {

                                e.printStackTrace();
                            }
                            solarCal.setLocation(tem_loc);
                            tem_sin_sunelev = solarCal.getSinSolarElevation();

                            if (tem_sin_sunelev <= sin_twi) {
                                pointJ = k;
                                break;
                            }
                        }
                    }
                } // end of loop j

                // twiC ==1 but pointJ_twi2==0 means entering twilight area,
                // but never leaving it. So the last point of the row
                // is the twi2 point
                if (twiC == 1 && pointJ_twi2 == -1) {
                    pointJ_twi2 = cso - 1;
                }

                short vis_start;
                short vis_end;
                short ir_start;
                short ir_end;

                if (sin_solar_elev[crs][0] < 0) {
                    // the first point of the line is in night time area.
                    if (pointJ == -1) {
                        if (pointJ_twi1 == -1) { // Only night time area
                            ir_start = 0;
                            ir_end = (short) (cso - 1);
                            vis_start = (short) cso;
                            vis_end = (short) cso;
                        } else { // both day and night
                            ir_start = 0;
                            ir_end = (short) pointJ_twi1;
                            vis_start = (short) pointJ_twi2;
                            vis_end = (short) (cso - 1);

                            if (sin_solar_elev[crs][ccs] < 0) { // polar night
                                rec.setRangeType(FogRecord.IMAGE_GROUP.IR_GROUP, i);
                            }
                        }
                    } else { // Use day/night point, and this means no twilight
                        // area
                        ir_start = 0;
                        ir_end = (short) pointJ;
                        vis_start = (short) (pointJ + 1);
                        vis_end = (short) (cso - 1);

                        if (vis_start > vis_end) {
                            vis_start = vis_end;
                        }
                    }
                }
                // The first point of the row is in day time area.
                else if (sin_solar_elev[crs][0] > 0) {
                    if (pointJ == -1) {
                        if (pointJ_twi1 == -1) { // Whole day time
                            vis_start = 0;
                            vis_end = (short) (cso - 1);
                            ir_start = 0;
                            ir_end = 0;
                        } else {
                            vis_start = 0;
                            vis_end = (short) pointJ_twi1;
                            ir_start = (short) pointJ_twi2;
                            ir_end = (short) (cso - 1);
                            if (sin_solar_elev[crs][ccs] > 0) { // polar day
                                rec.setRangeType(FogRecord.IMAGE_GROUP.VIS_GROUP, i);
                            }
                        }
                    } else {
                        vis_start = 0;
                        vis_end = (short) pointJ;
                        ir_start = (short) (pointJ + 1);
                        ir_end = (short) (cso - 1);
                        if (ir_start > ir_end) {
                            ir_start = ir_end;
                        }
                    }
                }

                else {
                    if (sin_solar_elev[crs][ccs] > 0) { // ccs is the last
                        // spaned col
                        vis_start = (short) (pointJ_twi2 + 1);
                        vis_end = (short) (cso - 1);
                        ir_start = 0;
                        ir_end = 0;
                    } else {
                        vis_start = 0;
                        vis_end = 0;
                        ir_start = (short) (pointJ_twi2 + 1);
                        ir_end = (short) (cso - 1);
                    }
                }

                FogRange vis_range = new FogRange();
                FogRange ir_range = new FogRange();
                // Assumed that _vis_range and _ir_range of
                // _fog_image have been resize to appropriate size.
                vis_range.setStart(vis_start);
                vis_range.setEnd(vis_end);
                ir_range.setStart(ir_start);
                ir_range.setEnd(ir_end);
                rec.setVisRange(vis_range, i);
                rec.setIRRange(ir_range, i);
            } // end of loop i
        } // end of if VIS_needed -----
        else {
            for (i = 0; i < rso; ++i) {
                FogRange vis_range = new FogRange();
                FogRange ir_range = new FogRange();

                vis_range.setStart(-1);
                vis_range.setEnd(-1);
                ir_range.setStart(0);
                ir_range.setEnd(cso - 1);
                rec.setVisRange(vis_range, i);
                rec.setIRRange(ir_range, i);
            }
        }

        return rec;
    }
}
