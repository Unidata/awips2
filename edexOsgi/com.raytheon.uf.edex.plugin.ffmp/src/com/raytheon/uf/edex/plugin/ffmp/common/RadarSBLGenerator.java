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
package com.raytheon.uf.edex.plugin.ffmp.common;

import java.awt.geom.Point2D;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.JTS;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.MathTransform2D;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.ffmp.SourceBin;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBinEntry;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBinList;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.TopologyException;

/**
 * Creates a SourceBinList for given radar and set of basins.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 6, 2011   10593     dfriedma    Initial creation
 * May 1, 2013   15684     zhao        Add code to handle possible TopologyException 
 * 
 * </pre>
 * 
 * @author dfriedma
 * @version 1.0
 */

public class RadarSBLGenerator {
    private enum MappingMethod {
        BIN_CENTER, ALL_BINS
    }
    private static final String DEBUG_FILE_NAME_PATTERN = "ffmp/sources/human-readable-%s.txt";
    private static final String CONFIG_FILE_NAME = "ffmp/RadarSBLGenerator.txt";
    private Log logger;
    private boolean debug;
    private MappingMethod mappingMethod = MappingMethod.BIN_CENTER;
    private PrintStream debugStream;
    
    public RadarSBLGenerator(FFMPConfig config) {
        logger = config.getGenerator().logger;
        loadConfiguration();
    }
    
    public SourceBinList generate(String sourceId, Set<Long> basinSet, Map<Long, Geometry> basinMap, RadarRecord radarRec) throws InvalidGridGeometryException, FactoryException, MismatchedDimensionException, TransformException {
        SourceBinList result = null;
        logger.info(String.format("RadarSBLGenerator: Using %s method", mappingMethod));
        if (debug)
            debugStream = getDebugStream(sourceId);
        try {
            long t0 = System.currentTimeMillis();
            if (mappingMethod == MappingMethod.BIN_CENTER)
                result = generateWithBinCenters(sourceId, basinSet, basinMap, radarRec);
            else if (mappingMethod == MappingMethod.ALL_BINS)
                result = generateWithAllBins(sourceId, basinSet, basinMap, radarRec);
            else
                return null;
            long t = System.currentTimeMillis();
            logger.info(String.format("RadarSBLGenerator: source %s, %s: %1.1f seconds", sourceId, mappingMethod, 
                    (double) (t-t0) / 1000));
        } finally {
            if (debugStream != null)
                debugStream.close();
        }
        return result;
            
    }
    
    private SourceBinList generateWithBinCenters(String sourceId, Set<Long> basinSet, Map<Long, Geometry> basinMap, RadarRecord radarRec) throws InvalidGridGeometryException, FactoryException, MismatchedDimensionException, TransformException {
        SourceBinList sbl = new SourceBinList(sourceId);
        
        Map<Long, Geometry> xformedBasinMap = new HashMap<Long, Geometry>(basinSet.size());
        
        MathTransform mt = CRSCache.getInstance().getTransformFromLatLon(radarRec.getCRS());
        MathTransform mtToLL = CRSCache.getInstance().getTransformToLatLon(radarRec.getCRS());
        
        int searchGridLen = Math.max(1, (int) (Math.sqrt(basinSet.size()) + 0.5));
        GridGeometry2D searchGridGeometry = getSearchGridGeometry(radarRec, searchGridLen);
        ArrayList<Long>[] searchGrid = new ArrayList[searchGridLen * searchGridLen];
        for (int i = 0; i < searchGrid.length; i++)
            searchGrid[i] = new ArrayList<Long>(4);
        
        HashMap<Long,ArrayList<Short>> rblMap = null;
        if (debugStream != null)
            rblMap = new HashMap<Long, ArrayList<Short>>();

        MathTransform2D w2g = searchGridGeometry.getCRSToGrid2D(PixelOrientation.UPPER_LEFT);
        DirectPosition2D pw = new DirectPosition2D();
        DirectPosition2D pg = new DirectPosition2D();
        DirectPosition2D pe = new DirectPosition2D();
        for (Long key : basinSet) {
            Geometry basinGeom = basinMap.get(key);
            if (basinGeom == null)
                continue;
            basinGeom.getCentroid();
            Geometry xformedGeom = JTS.transform(basinGeom, mt);
            xformedBasinMap.put(key, xformedGeom);
            Geometry bb = xformedGeom.getEnvelope();
            Coordinate[] bbCoords = bb.getCoordinates();
            pw.x = bbCoords[0].x;
            pw.y = bbCoords[0].y;
            w2g.transform((Point2D)pw, pg);
            int minx = Math.max(0, (int) Math.floor(pg.x));
            int miny = Math.max(0, (int) Math.floor(pg.y));

            pw.x = bbCoords[2].x;
            pw.y = bbCoords[2].y;
            w2g.transform((Point2D)pw, pg);
            int maxx = Math.min(searchGridLen, (int) Math.floor(pg.x));
            int maxy = Math.min(searchGridLen, (int) Math.floor(pg.y));
            
            // Grid y-coords are flipped.
            if (maxy < miny) {
                int t = maxy; 
                maxy = miny;
                miny = t;
            }
            
            int b = miny * searchGridLen;
            for (int j = miny; j <= maxy; ++j) {
                for (int i = minx; i <= maxx; ++i) {
                    searchGrid[b + i].add(key);
                }
                b += searchGridLen;
            }
        }
        
        if (xformedBasinMap.isEmpty())
            return sbl;
        
        HashMap<Long, ArrayList<SourceBinEntry>> sbeMap = new HashMap<Long, ArrayList<SourceBinEntry>>(basinSet.size());
        
        GeometryFactory gf = xformedBasinMap.values().iterator().next().getFactory();
        
        double eFactor = Math.cos(Math.toRadians(radarRec.getTrueElevationAngle()));
        double[] binSizes = new double[radarRec.getNumBins()];
        double cc = Math.PI / 180 / 2;
        double r0 = 0;
        for (int ib = 0; ib < radarRec.getNumBins(); ++ib) {
            double r1 = (ib + 1) * radarRec.getGateResolution() * eFactor;
            binSizes[ib] = cc * (r1*r1 - r0*r0);
            r0 = r1;
        }

        //Long lastPfaf = null;
        Geometry lastBasinGeometry = null;
        ArrayList<SourceBinEntry> lastSBEL = null;
        ArrayList<Short> lastRBL = null;
        Coordinate cw = new Coordinate();
        for (int ir = 0; ir < radarRec.getNumRadials(); ++ir) {
            double a = (ir + 0.5) * Math.PI / 180; // center of the radial
            double sin = Math.sin(a);
            double cos = Math.cos(a);
            
            for (int ib = 0; ib < radarRec.getNumBins(); ++ib) {
                double r = (ib + 0.5) * radarRec.getGateResolution() * eFactor;
                pw.x = r * sin; // clockwise from north
                pw.y = r * cos;
                cw.x = pw.x;
                cw.y = pw.y;
                
                Point p = gf.createPoint(cw);
                if (lastBasinGeometry != null && lastBasinGeometry.contains(p)) {
                    // nothing
                } else {
                    lastBasinGeometry = null;
                    
                    w2g.transform((Point2D)pw, pg);
                    int i = (int) pg.x;
                    int j = (int) pg.y;
                    if (i >= 0 && i < searchGridLen && j >= 0 && j < searchGridLen) {
                        for (Long pfaf : searchGrid[j * searchGridLen + i]) {
                            Geometry geom = xformedBasinMap.get(pfaf);
                            //DR15684
                            boolean geomContainsPointP = false;
                            try {
                            	geomContainsPointP = geom.contains(p);
                            } catch (TopologyException e) {
                            	logger.warn(String.format("RadarSBLGenerator: caught a TopologyException: %s", e.getMessage()));
                            	throw e;
                            }
                            if (geomContainsPointP) {
                                //lastPfaf = pfaf;
                                lastBasinGeometry = geom;
                                lastSBEL = sbeMap.get(pfaf);
                                if (lastSBEL == null) {
                                    lastSBEL = new ArrayList<SourceBinEntry>();
                                    sbeMap.put(pfaf, lastSBEL);
                                }
                                if (debugStream != null) {
                                    lastRBL = rblMap.get(pfaf);
                                    if (lastRBL == null) {
                                        lastRBL = new ArrayList<Short>();
                                        rblMap.put(pfaf, lastRBL);
                                    }
                                }
                                break;
                            }
                        }
                    }
                }
                
                if (lastBasinGeometry != null) {
                    SourceBinEntry sbe = new SourceBinEntry();
                    mtToLL.transform(pw, pe);
                    sbe.coor = new Coordinate(pe.x, pe.y);
                    sbe.area = binSizes[ib];
                    lastSBEL.add(sbe);
                    if (lastRBL != null) {
                        lastRBL.add((short) ir);
                        lastRBL.add((short) ib);
                    }
                }
            }
        }
        
        // Assign a bin to any basins that were missed above
        for (Long key : basinSet) {
            ArrayList<SourceBinEntry> sbel = sbeMap.get(key);
            Geometry geom;
            if (sbel == null && (geom = basinMap.get(key)) != null) {
                sbel = new ArrayList<SourceBinEntry>();
                sbeMap.put(key, sbel);
                
                SourceBinEntry sbe = new SourceBinEntry();
                sbe.coor = new Coordinate(geom.getCentroid().getCoordinate());
                sbe.area = 1;
                sbel.add(sbe);
            }
        }
        
        for (Entry<Long, ArrayList<SourceBinEntry>> entry : sbeMap.entrySet()) {
            sbl.addBin(entry.getKey(), new SourceBin(entry.getValue()));
        }
        
        if (debugStream != null) {            
            PrintStream dbg = debugStream;
            for (Entry<Long, ArrayList<Short>> entry : rblMap.entrySet()) {
                dbg.format("%012d : ", entry.getKey());
                int lrp = -1;
                ArrayList<Short> a = entry.getValue();
                ArrayList<SourceBinEntry> sbel = sbeMap.get(entry.getKey());
                for (int i = 0; i < a.size(); ) {
                    int ir = a.get(i++);
                    int ib = a.get(i++);
                    if (lrp != ir) {
                        if (lrp != -1)
                            dbg.print(") ");
                        dbg.format("%d(", ir);
                        lrp = ir;
                    } else
                        dbg.print(' ');
                    dbg.print(ib);
                }
                if (lrp != -1)
                    dbg.print(") ");
                dbg.print(" : ");
                for (SourceBinEntry sbe : sbel)
                    dbg.format("%1.2f ", sbe.area);
                dbg.print('\n');
            }
        }
        
        return sbl;
    }

    private GridGeometry2D getSearchGridGeometry(RadarRecord radarRec, int gridLen) {
        ProjectedCRS crs = radarRec.getCRS();
        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        generalEnvelope.setCoordinateReferenceSystem(crs);

        double range = radarRec.getGateResolution()
                * radarRec.getNumBins()
                * Math.cos(Math.toRadians(radarRec.getTrueElevationAngle()));

        generalEnvelope.setRange(0, -range, range);
        generalEnvelope.setRange(1, -range, range);

        GridGeometry2D searchGridGeometry = new GridGeometry2D(new GeneralGridEnvelope(
                new int[] { 0, 0 }, new int[] { gridLen, gridLen }, false), generalEnvelope);
        return searchGridGeometry;
    }
    
    private SourceBinList generateWithAllBins(String sourceId, Set<Long> basinSet, Map<Long, Geometry> basinMap, RadarRecord radarRec) throws InvalidGridGeometryException, FactoryException, MismatchedDimensionException, TransformException {
        SourceBinList sbl = new SourceBinList(sourceId);
        int nRadials = radarRec.getNumRadials();
        int nBins = radarRec.getNumBins();
        
        MathTransform mt = CRSCache.getInstance().getTransformFromLatLon(radarRec.getCRS());
        MathTransform mtToLL = CRSCache.getInstance().getTransformToLatLon(radarRec.getCRS());
        
        double eFactor = Math.cos(Math.toRadians(radarRec.getTrueElevationAngle()));
        
        GeometryFactory gf = new GeometryFactory(); //.values().iterator().next().getFactory();

        Geometry[] radarBinGeometries = new Geometry[nRadials * nBins];
        Coordinate[] radarBinLLs = new Coordinate[nRadials * nBins];
        Coordinate[] radialCoords0 = new Coordinate[nBins + 1];
        Coordinate[] radialCoords1 = new Coordinate[nBins + 1];
        Coordinate[] tCoords;
        DirectPosition2D pw = new DirectPosition2D();
        DirectPosition2D pe = new DirectPosition2D();
        for (int ir = 0; ir <= nRadials; ++ir) {
            double a = ir * Math.PI / 180;
            double sin = Math.sin(a);
            double cos = Math.cos(a);
            
            for (int ib = 0; ib <= nBins; ++ib) {
                double r = ib * radarRec.getGateResolution() * eFactor;
                radialCoords1[ib] = new Coordinate(
                        r * sin, // clockwise from north
                        r * cos);
            }
            
            if (ir > 0) {
                double am = (ir + 0.5) * Math.PI / 180;
                double sinm = Math.sin(am);
                double cosm = Math.cos(am);
                for (int ib = 1; ib <= nBins; ++ib) {
                    Coordinate[] coors = new Coordinate[5];
                    coors[0] = radialCoords0[ib - 1];
                    coors[1] = radialCoords0[ib];
                    coors[2] = radialCoords1[ib];
                    coors[3] = radialCoords1[ib - 1];
                    // make a closed object
                    coors[4] = coors[0];
    
                    LinearRing lr = gf.createLinearRing(coors);
                    Polygon poly = gf.createPolygon(lr, null);
    
                    int bidx = (ir - 1) * nBins + (ib - 1);
                    radarBinGeometries[bidx] = poly;
                    double rm = (ib + 0.5) * radarRec.getGateResolution() * eFactor;
                    pw.x = rm * sinm;
                    pw.y = rm * cosm;
                    mtToLL.transform(pw, pe);
                    radarBinLLs[bidx] = new Coordinate(pe.x, pe.y);
                }

            }
            
            tCoords = radialCoords0;
            radialCoords0 = radialCoords1;
            radialCoords1 = tCoords;
        }
                
        int[] polarRange = new int[4]; // bin0,rad0, b1,r1, inclusive
        int[] binCoords = new int[80]; // b1,r1, b2,r2 ...
        Coordinate[] polarCoords = new Coordinate[4];
        for (int i = 0; i < 4; ++i)
            polarCoords[i] = new Coordinate();
        
        PrintStream dbg = debugStream;
        
        for (Long key : basinSet) {
            Geometry basinGeom = basinMap.get(key);
            if (basinGeom == null)
                continue;
            basinGeom.getCentroid();
            Geometry xformedGeom = JTS.transform(basinGeom, mt);
            Coordinate[] shell;
            if (xformedGeom instanceof Polygon)
                shell = ((Polygon) xformedGeom).getExteriorRing().getCoordinates();
            else
                shell = xformedGeom.convexHull().getCoordinates();
            
            if (binCoords.length < shell.length * 2)
                binCoords = new int[shell.length * 2];
            for (int i = 0; i < shell.length; ++i) {
                double x = shell[i].x;
                double y = shell[i].y;
                binCoords[i * 2] = (int) (Math.sqrt(x*x + y*y) / (radarRec.getGateResolution() * eFactor));
                double atan = Math.atan2(x, y); // clockwise from north
                /* Make the angle positive so that truncating results in 
                 * consistent rounding for all angles.
                 */
                if (atan < 0)
                    atan += 2 * Math.PI; 
                int b = (int) (atan * 180 / Math.PI);
                binCoords[i * 2 + 1] = b;
            }
            // TODO: first two range gates are blank so don't bother with them?
            calcPolarRange(binCoords, shell.length, polarRange);

            ArrayList<SourceBinEntry> sbel = new ArrayList<SourceBinEntry>();
            double basinArea = xformedGeom.getArea();
            
            if (dbg != null)
                dbg.format("%012d : ", key);
            int lrp = -1;
            
            int ir = polarRange[1] - 1;
            do {
                ++ir;
                if (ir == 360)
                    ir = 0;
                for (int ib = polarRange[0]; ib <= polarRange[2]; ++ib) {
                    int bidx = ir * nBins + ib;

                    Geometry intersectGeo = xformedGeom.intersection(radarBinGeometries[bidx]);
                    if (! intersectGeo.isEmpty()) {
                        SourceBinEntry sbe = new SourceBinEntry();
                        sbe.coor = new Coordinate(radarBinLLs[bidx]);
                        sbe.area = intersectGeo.getArea() / basinArea;;
                        sbel.add(sbe);
                        
                        if (dbg != null) {
                            if (lrp != ir) {
                                if (lrp != -1)
                                    dbg.print(") ");
                                dbg.format("%d(", ir);
                                lrp = ir;
                            } else
                                dbg.print(' ');
                            dbg.print(ib);
                        }
                    }
                }
            } while (ir != polarRange[3]);
            
            if (dbg != null) {
                if (lrp != -1)
                    dbg.print(") ");
                dbg.print(" : ");
                for (SourceBinEntry sbe : sbel)
                    dbg.format("%1.2f ", sbe.area);
                dbg.print('\n');
            }
                
            sbl.addBin(key, new SourceBin(sbel));
        }
        
        return sbl;
    }

    private int[] a = new int[40];
    private int[] b = new int[40];
    
    /** Given a list of radar bin coordinates, return the range of radials/bins that
     * contains all of them.
     * @param binCoords Radar bin coordinates radial0, bin0, radial1, bin1, ...
     * @param nCoords The number of (radial,bin) pairs in binCoords
     * @param polarRange Upon return, contains startRadial, startBin, endRadial, endBin.  
     * The values are inclusive.  startBin may be greater than endBin, indicating 
     * wrap-around from 359 to 0.  
     */
    private void calcPolarRange(int[] binCoords, int nCoords, int[] polarRange) {
        /* Implementation: If all points fit within a less-than-180-degree 
         * sector, we only need to examine bins within the sector (further 
         * bounded by the min and max range.)  Otherwise, we must examine all
         * bins within the circle defined by the max range.
         * 
         * Actual test is "less than 179 degrees" to be safe.
         */
        
        if (a.length < nCoords) {
            a = new int[nCoords];
            b = new int[nCoords];
        }
        int minr = binCoords[0];
        int maxr = minr;
        
        int nc2 = nCoords * 2;
        for (int i = 2; i < nc2; i += 2) {
            minr = Math.min(minr, binCoords[i]);
            maxr = Math.max(maxr, binCoords[i]);
        }
        polarRange[0] = minr;
        polarRange[2] = maxr;
            
        int j = 0;
        for (int i = 1; i < nc2; i += 2)
            a[j++] = binCoords[i];
        Arrays.sort(a, 0, nCoords);

        /* If there is a greater-than-179-degree gap between two points,
         * try scanning from the other side.
         */
        int trys = 0;
        Ltry:
        while (true) {
            for (int i = 1; i < nCoords; ++i) {
                if (a[i] - a[i-1] >= 179) {
                    if (trys == 0) {
                        for (j = i; j < nCoords; ++j)
                            a[j] = a[j] - 360;
                        System.arraycopy(a, 0, b, nCoords - i, i);
                        System.arraycopy(a, i, b, 0, nCoords - i);
                        System.arraycopy(b, 0, a, 0, nCoords);                        
                        ++trys;
                        continue Ltry;
                    } else {
                        polarRange[0] = 0;
                        polarRange[1] = 0;
                        polarRange[3] = 359;
                        return;
                    }
                }
            }
            break;
        }
        
        int an = a[nCoords - 1];
        if (an - a[0] < 179) {
            polarRange[1] = a[0] >= 0 ? a[0] : a[0] + 360;            
            polarRange[3] = an >= 0 ? an : an + 360;
        } else {
            polarRange[0] = 0;
            polarRange[1] = 0;
            polarRange[3] = 359;
        }
    }

    private PrintStream getDebugStream(String sourceId) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);

        LocalizationFile lf = pm.getLocalizationFile(lc, 
                String.format(DEBUG_FILE_NAME_PATTERN, sourceId));
        
        File file = lf.getFile();
        
        try {
            logger.info(String.format("Saving human-readable bin list to %s",
                    lf));
            return new PrintStream(file);
        } catch (IOException e) {
            logger.error(String.format("%s: %s", file, e.getMessage()));
            return null;
        }
    }
    
    private void loadConfiguration() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);

        LocalizationFile lf = pm.getLocalizationFile(lc, CONFIG_FILE_NAME);
        
        File file = lf.getFile();
        if (! file.exists())
            return;
        Properties p = new Properties();
        
        try {
            InputStream inStream = new FileInputStream(file);
            try {
                p.load(inStream);
            } finally {
                inStream.close();
            }
        } catch (IOException e) {
            logger.error(String.format("%s: %s", file, e.getMessage()));
        }
        
        try {
            debug = Boolean.parseBoolean(p.getProperty("debug"));
        } catch (Exception e) {
            // ignore
        }
        try {
            mappingMethod = MappingMethod.valueOf(p.getProperty("method"));
        } catch (Exception e) {
            // ignore
        }
    }
    
}
