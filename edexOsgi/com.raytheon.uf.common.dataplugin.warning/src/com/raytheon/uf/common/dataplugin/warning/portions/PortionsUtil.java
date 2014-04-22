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
package com.raytheon.uf.common.dataplugin.warning.portions;

import java.util.EnumSet;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil.Direction;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Port of A1 code that determines the portions of the county or zone
 * descriptions, such as NORTHWEST.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2013  2177       jsanchez     Initial creation
 * Sep 22, 2013 2177       jsanchez     Updated logic. Used GisUtil for very small portions.
 * Dec  4, 2013 2604       jsanchez     Moved out of viz.warngen.
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class PortionsUtil {

    private GridUtil gridUtil;

    private String threeLetterSiteID;

    public PortionsUtil(String threeLetterSiteID,
            GeneralGridGeometry localGridGeometry, MathTransform localToLatLon)
            throws Exception {
        this.threeLetterSiteID = threeLetterSiteID;
        gridUtil = new GridUtil(localGridGeometry, localToLatLon);
    }

    /**
     * Determines the the appropriate portion description for the warnedArea
     * intersecting the countyOrZone.
     * 
     * @param entityID
     * @param countyOrZone
     * @param warnedArea
     * @param useExtreme
     * @return
     * @throws Exception
     */
    public EnumSet<Direction> getPortions(String entityID,
            Geometry countyOrZone, Geometry warnedArea, boolean useExtreme)
            throws Exception {
        countyOrZone.getUserData();
        EntityData entityData = gridUtil.calculateGrids(countyOrZone,
                warnedArea);
        EnumSet<Direction> portions = null;
        if (entityData.getMeanMask() == 0 || entityData.getCoverageMask() == 0
                || entityData.getMeanMask() == entityData.getCoverageMask()) {
            // This takes into account the warned areas that are very small
            // the convex hull of the warned area is used for case the
            // warnedArea is a geometry collection.
            portions = GisUtil.calculateLocationPortion(countyOrZone,
                    warnedArea.convexHull(), useExtreme);
        } else {
            portions = getAreaDesc(entityData.getMeanMask(),
                    entityData.getCoverageMask(), entityData.getOctants(),
                    useExtreme);
        }
        return suppressPortions(entityID, portions);
    }

    /**
     * Looks up if the designated entity ID has an suppressed directions. For
     * example, a county or zone may not need to include the north and sound
     * direction description if it was included in the area.suppress file.
     * 
     * @param entityID
     * @param portions
     * @return
     */
    private EnumSet<Direction> suppressPortions(String entityID,
            EnumSet<Direction> portions) {
        Map<String, List<Direction>> suppressedCounties = SuppressMap
                .getInstance().getAreas(threeLetterSiteID);
        if (entityID != null && suppressedCounties != null
                && !suppressedCounties.isEmpty()) {
            List<Direction> suppressedDirections = suppressedCounties
                    .get(entityID.toUpperCase());
            if (suppressedDirections != null && !suppressedDirections.isEmpty()) {
                portions.removeAll(suppressedDirections);
            }
        }

        return portions;
    }

    /**
     * Port from A1 code of GeoEntityLookupTable::getAreaDesc.
     * 
     * @param meanMask
     * @param areaMask
     * @param octants
     * @param exYes
     */
    private static EnumSet<Direction> getAreaDesc(int meanMask, int areaMask,
            int octants, boolean exYes) {
        EnumSet<Direction> portions = EnumSet.noneOf(Direction.class);

        // Test for case where we cannot do portions
        if (meanMask == 0 || areaMask == 0) {
            return portions;
        }

        // The next block of code is the original port of A1 code but prevented
        // producing the correct result:
        // Test for case where area is completely within one subsection.
        // if (meanMask == areaMask) {
        // return getPointDesc(meanMask, exYes);
        // }

        // Test for central by not being near adjacent borders.
        // Another possible case of a stripe across the middle.
        if (octants == 0
                || ((octants & CoverageConstants.EXTREME_YES) == 0)
                && (meanMask & CoverageConstants.CENTER) == CoverageConstants.CENTER) {
            portions.add(Direction.CENTRAL);
            return portions;
        }

        if ((octants & 0xFFFF) == 0xFFFF) {
            return portions;
        }

        // Identify quadrants in use, q is typical, qq is diagonal.
        int xoctant = octants >> 8;
        int xxoctant = octants >> 16;
        int nn, ss, ee, ww, ne, nw, se, sw;
        nn = ss = ee = ww = ne = nw = se = sw = 0;
        int omerge = xxoctant | xoctant | octants;
        if ((omerge & (CoverageConstants.NNE | CoverageConstants.ENE)) != 0) {
            ne = 1;
        }
        if ((omerge & (CoverageConstants.SSE | CoverageConstants.ESE)) != 0) {
            se = 1;
        }
        if ((omerge & (CoverageConstants.NNW | CoverageConstants.WNW)) != 0) {
            nw = 1;
        }
        if ((omerge & (CoverageConstants.SSW | CoverageConstants.WSW)) != 0) {
            sw = 1;
        }
        if ((omerge & (CoverageConstants.NNE | CoverageConstants.NNW)) != 0) {
            nn = 1;
        }
        if ((omerge & (CoverageConstants.SSE | CoverageConstants.SSW)) != 0) {
            ss = 1;
        }
        if ((omerge & (CoverageConstants.WNW | CoverageConstants.WSW)) != 0) {
            ww = 1;
        }
        if ((omerge & (CoverageConstants.ENE | CoverageConstants.ESE)) != 0) {
            ee = 1;
        }
        if ((areaMask & CoverageConstants.NORTH_SOUTH) == 0) {
            nn = ss = ne = nw = se = sw = 0;
        }
        if ((areaMask & CoverageConstants.EAST_WEST) == 0) {
            ee = ww = ne = nw = se = sw = 0;
        }
        int q = ne + nw + se + sw;
        int qq = nn + ss + ee + ww;

        // Identify extremes in use.
        int nnx, ssx, eex, wwx;
        nnx = ssx = eex = wwx = 0;
        if ((areaMask & CoverageConstants.XNORTH) != 0) {
            nnx = 1;
        }
        if ((areaMask & CoverageConstants.XSOUTH) != 0) {
            ssx = 1;
        }
        if ((areaMask & CoverageConstants.XWEST) != 0) {
            wwx = 1;
        }
        if ((areaMask & CoverageConstants.XEAST) != 0) {
            eex = 1;
        }
        int xxx = nnx + ssx + eex + wwx;

        // Modify masks based on whether we can use extreme.
        if ((octants & CoverageConstants.EXTREME_NO) != 0
                && (areaMask & CoverageConstants.EXTREME) != 0) {
            areaMask &= CoverageConstants.NOT_EXTREME;
            meanMask &= CoverageConstants.NOT_EXTREME;
        }

        // Possible case of a stripe across the middle
        if (q == 0) {
            ;// Only one direction encoded
        } else if (q == 2 && nw == se || q == 2 && ne == sw || qq == 2
                && nn == ss || qq == 2 && ee == ww) {
            if ((meanMask & CoverageConstants.CENTRAL) == CoverageConstants.CENTRAL
                    || nnx == ssx && wwx == eex) {
                portions.add(Direction.CENTRAL);
                return portions;
            }
            return getPointDesc2(meanMask, exYes, nn, ss, ee, ww);
        }

        // Modify masks based on whether we can use central.
        if (xxx > 2 || nnx != ssx && wwx != eex) {
            areaMask &= CoverageConstants.NOT_CENTRAL;
            meanMask &= CoverageConstants.NOT_CENTRAL;
        }

        // All quadrants in use.
        if (q == 4 && qq == 4) {
            return EnumSet.noneOf(Direction.class);
        }

        // Only one typical quadrant in use.
        if (q == 1) {
            return getPointDesc2(meanMask, exYes, nn, ss, ee, ww);
        }

        // Further modify masks based on whether we can use central.
        if (xxx >= 2) {
            areaMask &= CoverageConstants.NOT_CENTRAL;
            meanMask &= CoverageConstants.NOT_CENTRAL;
        }

        // No more than two quadrants of any kind in use, or all quadrants.
        if (q < 3 && qq < 3) {
            if (nnx != ssx && wwx != eex
                    || (meanMask & CoverageConstants.CENTRAL) != 0) {
                return getPointDesc2(meanMask, exYes, nn, ss, ee, ww);

            } else {
                return getPointDesc2(areaMask, exYes, nn, ss, ee, ww);
            }
        }

        // Three typical quadrants in use.
        if (q == 3 && qq != 3) {

            if (ne == 0) {
                // The next line is the original port of A1 code but prevented
                // producing the correct result:
                // if (ne == 0 && (xxoctant & (SSW | WSW)) != 0) {
                portions.add(Direction.SOUTH);
                portions.add(Direction.WEST);

            } else if (se == 0) {
                // The next line is the original port of A1 code but prevented
                // producing the correct result:
                // } else if (se == 0 && (xxoctant & (NNW | WNW)) != 0) {
                portions.add(Direction.NORTH);
                portions.add(Direction.WEST);

            } else if (nw == 0) {
                // The next line is the original port of A1 code but prevented
                // producing the correct result:
                // } else if (nw == 0 && (xxoctant & (SSE | ESE)) != 0) {
                portions.add(Direction.SOUTH);
                portions.add(Direction.EAST);

            } else if (sw == 0) {
                // The next line is the original port of A1 code but prevented
                // producing the correct result:
                // } else if (sw == 0 && (xxoctant & (NNE | ENE)) != 0) {
                portions.add(Direction.NORTH);
                portions.add(Direction.EAST);
            }
            // The next line is the original port of A1 code but prevented
            // producing the correct result:
            // return getPointDesc(meanMask, exYes);
        }

        // Three diagonal quadrants in use.
        if (qq == 3 && portions.isEmpty()) {
            if (nn == 0) {
                portions.add(Direction.SOUTH);
            } else if (ss == 0) {
                portions.add(Direction.NORTH);
            } else if (ww == 0) {
                portions.add(Direction.EAST);
            } else if (ee == 0) {
                portions.add(Direction.WEST);
            }
        }

        // add extreme for three quadrant case.
        if (!portions.isEmpty()) {
            if (exYes && ((areaMask & CoverageConstants.EXTREME)) != 0) {
                portions.add(Direction.EXTREME);
            }
            return portions;
        }

        // All of either type of quadrant in use.
        if (q == 4 || qq == 4) {
            return EnumSet.noneOf(Direction.class);
        }

        // Case of a pure simple direction.
        nn = areaMask & CoverageConstants.NORTHERN;
        ss = areaMask & CoverageConstants.SOUTHERN;
        ee = areaMask & CoverageConstants.EASTERN;
        ww = areaMask & CoverageConstants.WESTERN;
        if (ss != 0 && nn != 0 || q == 0) {
            if (ee == 0 && ww != 0) {
                portions.add(Direction.WEST);
            }
            if (ww == 0 && ee != 0) {
                portions.add(Direction.EAST);
            }
        } else if (ee != 0 && ww != 0 || q == 0) {
            if (nn == 0 && ss != 0) {
                portions.add(Direction.SOUTH);
            }
            if (ss == 0 && nn != 0) {
                portions.add(Direction.NORTH);
            }
        }

        // add extreme for simple direction case.
        if (!portions.isEmpty()) {
            if (exYes && ((areaMask & CoverageConstants.EXTREME)) != 0) {
                portions.add(Direction.EXTREME);
            }
            return portions;
        }

        // Catch with the point descriptor one last time
        return getPointDesc2(meanMask, exYes, nn, ss, ee, ww);
    }

    /**
     * Port from A1 code of GeoEntityLookupTable::getPointDesc.
     * 
     * @param mask
     * @param exYes
     * @return
     */
    private static EnumSet<Direction> getPointDesc(int mask, boolean exYes) {
        EnumSet<Direction> portions = EnumSet.noneOf(Direction.class);

        int cc = mask & CoverageConstants.CENTRAL;
        if (cc == CoverageConstants.CENTRAL) {
            portions.add(Direction.CENTRAL);
            return portions;
        }

        if ((mask & CoverageConstants.NORTH_SOUTH) == 0) {
            ;
        } else if ((mask & CoverageConstants.SOUTHERN) == (mask & CoverageConstants.NORTH_SOUTH)) {
            portions.add(Direction.SOUTH);
        } else if ((mask & CoverageConstants.NORTHERN) == (mask & CoverageConstants.NORTH_SOUTH)) {
            portions.add(Direction.NORTH);
        }

        if ((mask & CoverageConstants.EAST_WEST) == 0) {
            ;
        } else if ((mask & CoverageConstants.WESTERN) == (mask & CoverageConstants.EAST_WEST)) {
            portions.add(Direction.WEST);
        } else if ((mask & CoverageConstants.EASTERN) == (mask & CoverageConstants.EAST_WEST)) {
            portions.add(Direction.EAST);
        }

        if (portions.isEmpty()) {
            return portions;
        }

        if (cc != 0) {
            portions.add(Direction.CENTRAL);
        }

        if (exYes && ((int) (mask & CoverageConstants.EXTREME) != 0)) {
            portions.add(Direction.EXTREME);
        }

        return portions;
    }

    /**
     * This method is not a direct port from A1. The original getPointDesc did
     * not produce the expected results. This method is a modified version of
     * getPointDesct that uses the calculated qq values instead of just the
     * meanMask.
     * 
     * @param mask
     * @param exYes
     * @return
     */
    private static EnumSet<Direction> getPointDesc2(int mask, boolean exYes,
            int nn, int ss, int ee, int ww) {
        EnumSet<Direction> portions = EnumSet.noneOf(Direction.class);

        if (mask == 0) {
            return portions;
        }

        int counter = 0;
        if (nn != 0 && ss != 0) {
            ;
        } else if (ss != 0) {
            portions.add(Direction.SOUTH);
            counter++;
        } else if (nn != 0) {
            portions.add(Direction.NORTH);
            counter++;
        }

        if (ee != 0 && ww != 0) {
            ;
        } else if (ww != 0) {
            portions.add(Direction.WEST);
            counter++;
        } else if (ee != 0) {
            portions.add(Direction.EAST);
            counter++;
        }

        if (portions.isEmpty()) {
            return portions;
        }

        int cc = mask & CoverageConstants.CENTRAL;
        boolean useCentral = counter < 2;
        if (useCentral && cc != 0) {
            portions.add(Direction.CENTRAL);
        }

        if (exYes && ((int) (mask & CoverageConstants.EXTREME) != 0)) {
            portions.add(Direction.EXTREME);
        }

        return portions;
    }
}
