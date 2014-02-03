/*
 * gov.noaa.nws.ncep.ui.pgen.graphToGrid.ContoursToGrid
 * 
 * January 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.graphtogrid;

import gov.noaa.nws.ncep.gempak.parameters.core.categorymap.CatMap;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourCircle;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourMinmax;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.util.GridUtil;
import com.raytheon.viz.core.contours.util.ContourContainer;
import com.raytheon.viz.core.contours.util.FortConBuf;
import com.raytheon.viz.core.contours.util.FortConConfig;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;

/**
 * Class for Graph-to-Grid to generate grids from a Contours element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#215		J. Wu   	Initial Creation.
 * 06/10		#215		J. Wu   	Added support for Min/Max.
 * 07/10		#215		J. Wu   	Added support for Outlook.
 * 07/10		#215		J. Wu   	Added support for writing grid to
 * 											a GEMPAK grid file
 * 09/10		#215		J. Wu   	Checked working directory and PATH.
 * 11/10		#345		J. Wu   	Added support for circle.
 * 
 * </pre>
 * 
 * @author J. Wu
 */

public class ContoursToGrid extends GraphToGrid {

    private static float smallestContourValue = GridUtil.GRID_FILL_VALUE - 1;

    private static float largestContourValue = GridUtil.GRID_FILL_VALUE + 1;

    /** Factory */
    private GeometryFactory geometryFactory = new GeometryFactory();

    /**
     * Constructor
     */
    public ContoursToGrid(Contours currentGraph,
            HashMap<String, String> gridParameters) {
        super(currentGraph, gridParameters);
    }

    /**
     * Do grid calculation.
     */
    @Override
    public void makeGrid() {

        /*
         * Build a grid space from the given projection, garea, kx, ky.
         */
        String proj = getParamValues(gridParameters, "PROJ");
        String garea = getParamValues(gridParameters, "GRDAREA");
        String kxky = getParamValues(gridParameters, "KXKY");

        String[] nkxky = kxky.split(";");

        int kx = 63;
        int ky = 28;
        if (nkxky.length > 1) {
            kx = Integer.parseInt(nkxky[0]);
            ky = Integer.parseInt(nkxky[1]);
        }

        /*
         * Find a coordinate transformation between world and the custom grid
         * space.
         */
        CoordinateTransform gtrans = new CoordinateTransform(proj, garea, kx,
                ky);

        /*
         * Start a grid.
         */
        float[] grid = new float[kx * ky];
        float[] hist = new float[kx * ky];

        for (int kk = 0; kk < kx * ky; kk++) {
            grid[kk] = G2GCommon.RMISSD;
            hist[kk] = G2GCommon.INIT;
        }

        Coordinate[] gridPts = new Coordinate[kx * ky];
        double[] newPt = new double[2], lonlat;
        Coordinate c;
        for (int jj = 0; jj < ky; jj++) {
            for (int ii = 0; ii < kx; ii++) {

                // Grid starts at (1, 1)
                newPt[0] = ii + 1;
                newPt[1] = jj + 1;
                lonlat = gtrans.gridToWorld(newPt);

                c = new Coordinate(lonlat[0], lonlat[1]);

                gridPts[ii + jj * kx] = c;
            }
        }

        /*
         * Process the user-specified bounds.
         */
        String bound = getParamValues(gridParameters, "BOUNDS");
        Contours bndContours = contoursFromBounds(bound, gtrans, hist, gridPts,
                kx, ky);

        /*
         * Process the user-specified category map.
         */
        String cmapString = getParamValues(gridParameters, "CATMAP");
        CatMap cmap = new CatMap(cmapString);

        /*
         * Smooth and extend the lines - in grid space.
         * 
         * 1. Convert the text label to float using the CATMAP. 2. Add bound
         * lines as part of the Contours. 3. Extend the lines to the boundary of
         * the GAREA
         * 
         * Note that bound lines is also processed here but it won't be smoothed
         * and extended since they are closed lines with smoothing level 0.
         */
        Contours extContours = ((Contours) currentGraph).copy();

        // Find bound lines.
        checkSpecialBounds(extContours, hist, kx, ky, gtrans, gridPts, cmap);

        for (ContourLine cline : bndContours.getContourLines()) {
            extContours.add(cline);
        }

        // Convert circles to contour lines for G2G processing
        ArrayList<ContourCircle> ccircle = extContours.getContourCircles();
        for (ContourCircle cc : ccircle) {
            ArrayList<Coordinate> pts = generateArcPoints((Arc) cc.getCircle(),
                    20.0);
            if (pts.size() > 1) {
                ContourLine ncl = new ContourLine(pts, true,
                        cc.getLabelString(), 1);
                extContours.add(ncl);
            }
        }

        // Do extensions
        ContoursExtension cntExt = new ContoursExtension(extContours, gtrans,
                kx, ky, new Color[] { Color.blue }, cmap);

        /*
         * Draw bounds and grids for diagnosis
         */
        // drawBoundsAndGrid( bndContours, hist, kx, ky, gridPts );

        /*
         * Prepare data to be set into g2g_driver
         * 
         * Note that if no extension is required (bounds is empty or the line is
         * a closed line), fi_ext and fj_ext should use the the original line
         * points.
         * 
         * ?It is reasonable to always extend the lines regardless of bounds?
         */
        String bnds = getParamValues(gridParameters, "BOUNDS");
        boolean extend = true;
        // if ( bnds != null && bnds.trim().length() > 0 ) {
        // extend = false;
        // }

        float[][] flat, flon;
        float[][] fi_orig, fj_orig;
        float[][] fi_ext, fj_ext;

        int nlines = cntExt.getNlines();

        int[] npoints = cntExt.getNpts();
        int[] norigpts = cntExt.getNOrigPts();

        flat = cntExt.getFlat();
        flon = cntExt.getFlon();
        fi_orig = cntExt.getFiOrig();
        fj_orig = cntExt.getFjOrig();

        int[] nextpts;
        if (extend) {
            nextpts = cntExt.getNExtPts();
            fi_ext = cntExt.getFiExt();
            fj_ext = cntExt.getFjExt();
        } else {
            nextpts = cntExt.getNOrigPts();
            fi_ext = cntExt.getFiOrig();
            fj_ext = cntExt.getFjOrig();
        }

        /*
         * Convert 2-D arrays into 1-D to call the C driver. First do the
         * original lat/lon points
         */
        int ntotal = 0;
        for (int kk = 0; kk < nlines; kk++) {
            ntotal += npoints[kk];
        }

        float[] latPts = new float[ntotal];
        float[] lonPts = new float[ntotal];
        int np = 0;
        for (int kk = 0; kk < nlines; kk++) {
            for (int mm = 0; mm < npoints[kk]; mm++) {
                latPts[np + mm] = flat[kk][mm];
                lonPts[np + mm] = flon[kk][mm];
            }

            np += npoints[kk];
        }

        /*
         * Smoothed line points in grid space
         */
        ntotal = 0;
        for (int kk = 0; kk < nlines; kk++) {
            ntotal += norigpts[kk];
        }

        float[] smthLat = new float[ntotal];
        float[] smthLon = new float[ntotal];
        np = 0;
        for (int kk = 0; kk < nlines; kk++) {
            for (int mm = 0; mm < norigpts[kk]; mm++) {
                smthLat[np + mm] = fi_orig[kk][mm];
                smthLon[np + mm] = fj_orig[kk][mm];
            }

            np += norigpts[kk];
        }

        /*
         * Smoothed/extended line points in grid space
         */
        ntotal = 0;
        for (int kk = 0; kk < nlines; kk++) {
            ntotal += nextpts[kk];
        }

        float[] extLat = new float[ntotal];
        float[] extLon = new float[ntotal];
        np = 0;
        for (int kk = 0; kk < nlines; kk++) {
            for (int mm = 0; mm < nextpts[kk]; mm++) {
                extLat[np + mm] = fi_ext[kk][mm];
                extLon[np + mm] = fj_ext[kk][mm];
            }

            np += nextpts[kk];
        }

        /*
         * Line values, smooth level, closed flag.
         * 
         * Note that the bounds with a value of "-RMISSD" should be reset to
         * "RMISSD" for g2g calculation.
         */
        int[] ismth = cntExt.getIsmth();
        int[] iclosed = cntExt.getClosed();
        float[] values = cntExt.getValue();
        float[] linevalues = new float[values.length];
        for (int ii = 0; ii < values.length; ii++) {
            if (values[ii] == -G2GCommon.RMISSD) {
                linevalues[ii] = -values[ii];
            } else {
                linevalues[ii] = values[ii];
            }
        }

        /*
         * Get the minimum and maximums in Contours
         */
        ArrayList<ContourMinmax> cminmax = extContours.getContourMinmaxs();
        int mmnum = cminmax.size();
        double[] mmlonlat = new double[mmnum * 2];
        double[] mmgrid = new double[mmnum * 2];
        float[] mmvalue = new float[mmnum];
        float[] mmlat = new float[mmnum];
        float[] mmlon = new float[mmnum];
        float[] mmfi = new float[mmnum];
        float[] mmfj = new float[mmnum];

        int ii = 0;
        for (ContourMinmax cmm : cminmax) {
            Coordinate p = ((SinglePointElement) (cmm.getSymbol()))
                    .getLocation();
            mmlon[ii] = (float) p.x;
            mmlat[ii] = (float) p.y;

            mmlonlat[ii * 2] = p.x;
            mmlonlat[ii * 2 + 1] = p.y;
            mmvalue[ii] = getValueForLabel(cmap, cmm.getLabelString()[0]);
            ;
            ii++;
        }

        mmgrid = gtrans.worldToGrid(mmlonlat);
        for (int jj = 0; jj < mmnum; jj++) {
            mmfi[jj] = (float) mmgrid[jj * 2];
            mmfj[jj] = (float) mmgrid[jj * 2 + 1];
        }

        /*
         * Call the native libg2g to do the actual calculation
         */
        G2GNativeLibrary g2gNative = G2GNativeLibrary.getInstance();

        String catmap = getParamValues(gridParameters, "CATMAP");
        String gglims = getParamValues(gridParameters, "GGLIMS");
        String histgrd = getParamValues(gridParameters, "HISTGRD");
        String discrete = getParamValues(gridParameters, "DISCRETE");
        String dlines = getParamValues(gridParameters, "DLINES");
        String edgeopts = getParamValues(gridParameters, "EDGEOPTS");

        /*
         * Run the native Graph-to-Grid algorithm.
         */
        g2gNative.g2g_compute(grid, hist, kx, ky, nlines, npoints, latPts,
                lonPts, norigpts, smthLat, smthLon, nextpts, extLat, extLon,
                linevalues, ismth, iclosed, mmnum, mmlat, mmlon, mmfi, mmfj,
                mmvalue, catmap, histgrd, discrete, dlines, gglims, edgeopts);

        /*
         * Generate contour lines from the grid and display
         */
        String cint = getParamValues(gridParameters, "CINT");
        contoursFromGrid(grid, kx, ky, cint, gtrans, cmap);

        /*
         * Write the G2G grid to a GEMPAK grid file.
         */
        String gdoutf = getParamValues(gridParameters, "GDOUTF");
        String maxgrd = getParamValues(gridParameters, "MAXGRD");
        String gdatim = getParamValues(gridParameters, "GDATTIM");
        String gvcord = getParamValues(gridParameters, "GVCORD");

        String ckxky = new String("" + gtrans.getKx() + ";" + gtrans.getKy());
        Coordinate[] gext = gtrans.getExtent();
        String gdarea = new String("" + gext[0].y + ";" + gext[0].x + ";"
                + +gext[1].y + ";" + gext[1].x);

        String gparm = getParamValues(gridParameters, "GPARM");
        if (gparm == null || gparm.trim().length() == 0) {
            gparm = extContours.getParm();
        }

        String glevel = getParamValues(gridParameters, "GLEVEL");
        if (glevel == null || glevel.trim().length() == 0) {
            glevel = extContours.getLevel();
        }

        if (gparm == null || gparm.trim().length() == 0) {
            return;
        }

        if (glevel == null || glevel.trim().length() == 0) {
            return;
        }

        gdatim = new String(PgenUtil.calendarToGempakDattim(extContours
                .getTime1()) + extContours.getForecastHours());

        /*
         * Find the correct current working directory in case.
         */
        String path = getParamValues(gridParameters, "PATH");
        if (path == null || path.trim().length() == 0) {
            path = new String(".");
        }

        if (path.trim().startsWith(".")) {
            String workingDir = PgenUtil.getWorkingDirectory();
            if (workingDir != null) {
                path = new String(path.trim().replaceFirst(".", workingDir));
            }
        }

        String fullFile = new String(path + "/" + gdoutf);

        /*
         * Check if the PATH is valid and exists
         */
        File grdfile = new File(path);

        Pattern ptn = Pattern.compile("[A-Z]");
        Matcher mt = ptn.matcher(path);

        String msg = null;
        if (mt.find()) {
            msg = new String("The PATH cannot have upper case in it");
        } else {
            if (!grdfile.exists()) {
                msg = new String(
                        "The PATH does not exist, please create it first");
            }
        }

        if (msg != null) {

            MessageDialog msgDlg = new MessageDialog(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell(), "Warning", null,
                    "Fail to write to " + fullFile + " because:\n\n" + msg,
                    MessageDialog.INFORMATION, new String[] { "OK" }, 0);
            msgDlg.open();

            return;
        }

        /*
         * Now try to save to grd file
         */
        String cpyfil = new String(" ");
        String anlyss = new String(" ");

        g2gNative.g2g_write(grid, hist, histgrd, fullFile, proj, cpyfil,
                gdarea, anlyss, ckxky, maxgrd, gparm, gdatim, gvcord, glevel);

    }

    /**
     * Generate contours from a grid and display it a Contours element
     */
    private void contoursFromGrid(float[] grid, int kx, int ky, String cint,
            CoordinateTransform gtrans, CatMap cmap) {

        /*
         * Generate contour lines from the grid
         */
        long[] sz = new long[] { kx, ky };

        int minX = 0;
        int minY = 0;
        int maxX = (int) (sz[0] - 1);
        int maxY = (int) (sz[1] - 1);

        int szX = (maxX - minX) + 1;
        int szY = (maxY - minY) + 1;

        int[] work = new int[10 * grid.length];
        float[] xPoints = new float[10 * grid.length];
        float[] yPoints = new float[10 * grid.length];
        int[] numPoints = new int[1];

        /*
         * Build an array of contour levels
         */
        float gmax = Float.MIN_VALUE;
        float gmin = Float.MAX_VALUE;

        for (int ii = 0; ii < grid.length; ii++) {
            if (grid[ii] > gmax) {
                gmax = grid[ii];
            }

            if (grid[ii] < gmin) {
                gmin = grid[ii];
            }
        }

        ArrayList<String> contourVals = new ArrayList<String>();
        for (ContourLine cline : ((Contours) currentGraph).getContourLines()) {
            contourVals.add(cline.getLabelString()[0]);
        }

        if (cint != null) {
            float interval = 0;
            if (!cint.contains(";")) {
                try {
                    interval = Math.abs(Float.parseFloat(cint));
                } catch (Exception e) {
                }

                if (interval > 0. && (gmin + interval) < gmax) {

                    float start = getValueForLabel(cmap, contourVals.get(0));
                    while (start > gmin) {
                        start -= interval;
                    }

                    contourVals.clear();
                    while (start < gmax) {
                        contourVals.add("" + start);
                        start += interval;
                    }

                }
            } else {
                String[] cintArray = cint.split(";");
                ArrayList<String> cintList = new ArrayList<String>();
                for (String str : cintArray) {
                    try {
                        Float.parseFloat(str);
                        cintList.add(str);
                    } catch (Exception e) {
                    }
                }

                if (cintList.size() > 0) {
                    contourVals.clear();
                    contourVals.addAll(cintList);
                }
            }
        }

        float[] contVals = new float[contourVals.size()];
        for (int ii = 0; ii < contourVals.size(); ii++) {
            contVals[ii] = getValueForLabel(cmap, contourVals.get(ii));
        }

        /*
         * Call AWIPS contouring code to generate contour lines
         */
        float[][] grid2d = new float[szX][szY];
        int idx = 0;
        for (int j = 0; j < szY; j += 1) {
            for (int i = 0; i < szX; i += 1) {
                grid2d[i][j] = grid[idx];
                idx += 1;
            }
        }
        FortConConfig config = new FortConConfig();
        config.mode = contVals.length;
        config.seed = contVals;
        config.badlo = smallestContourValue;
        config.badhi = largestContourValue;
        config.xOffset = 1f;
        config.yOffset = 1f;
        ContourContainer container = FortConBuf.contour(grid2d, config);

        int nContours = container.contourVals.size();
        int numPoints2 = 0;
        for (float[] line : container.xyContourPoints) {
            numPoints2 += line.length / 2;
        }

        /*
         * Put all line points into an array to be converted to lat/lon
         */
        int[] nContourPts = new int[nContours];
        float[] contourValue = new float[nContours];

        double[] vals = new double[numPoints2 * 2];
        int ncnt = 0;
        int npts = 0;
        for (int ii = 0; ii < nContours; ii++) {
            float[] line = container.xyContourPoints.get(ii);
            contourValue[ncnt] = ((int) (container.contourVals.get(ii) * 10)) / 10.0f;
            nContourPts[ncnt] = line.length / 2;
            for (int jj = 0; jj < line.length; jj += 1) {
                vals[npts] = line[jj];
                npts += 1;
            }
            ncnt += 1;
        }

        double[] latlons = gtrans.gridToWorld(vals);

        /*
         * Create and display the new Contours element
         */
        String[] cntStrings = new String[nContours];
        for (int kk = 0; kk < nContours; kk++) {
            cntStrings[kk] = getLabelForValue(cmap, contourValue[kk]);
        }

        Contours gridContours = ((Contours) currentGraph).createContours(
                nContours, nContourPts, latlons, cntStrings, Color.green);

        if (gridContours.getContourLines().size() > 0) {

            String dispOpt = getParamValues(gridParameters, "DISPOPT");
            boolean dispAsGhost = true;
            if (dispOpt != null && dispOpt.equalsIgnoreCase("FALSE")) {
                dispAsGhost = false;
            }

            PgenResource drawingLayer = PgenSession.getInstance()
                    .getPgenResource();
            if (dispAsGhost) {
                drawingLayer.setGhostLine(gridContours);
            } else {
                drawingLayer.addElement(gridContours);
            }

            PgenUtil.refresh();
        }

    }

    /**
     * Bound processing - By default, set the grid points inside a bound as
     * BOUNDED and their values as RMISSD (-9999). However, the assignments are
     * reversed if the "inout" flag is explicitly set to "false" by the user. In
     * that case, the grid points outside a bound are set as BOUNDED and their
     * values as RMISSD (-9999).
     * 
     * Note: need to access the "hist" array as it is an FORTRAN array (column
     * first).
     */
    private Contours contoursFromBounds(String bnds,
            CoordinateTransform gtrans, float hist[], Coordinate[] grdPts,
            int kx, int ky) {

        ArrayList<BoundPolygon> boundPolys = BoundPolygon.getAllBounds(bnds);

        Contours bndContours = new Contours();

        /*
         * Check if a grid point is inside .
         */
        int ks = 0;
        Boolean inout = true;
        Point p;
        for (BoundPolygon bpoly : boundPolys) {

            inout = bpoly.getInout();
            for (MultiPolygon mpoly : bpoly.getBoundPolygons()) {
                ks++;
                /*
                 * Mask the grid points inside the bound as bounded. If the
                 * inout is set to false, set the grid point outside the bound
                 * as BOUNDED.
                 */
                for (int jj = 0; jj < ky; jj++) {
                    for (int ii = 0; ii < kx; ii++) {

                        p = geometryFactory.createPoint(grdPts[ii + jj * kx]);

                        if (hist[ii + jj * kx] != G2GCommon.BOUNDED) {
                            if (BoundPolygon.pointInPolygon(p, mpoly) ^ inout) {
                                hist[ii + jj * kx] = G2GCommon.BOUNDED;
                            }
                        }
                    }
                }
            }

            /*
             * Construct bounds as ContourLines
             */
            Contours bContours = bpoly.getBoundsAsContours();
            if (bContours.getContourLines().size() > 0) {
                for (ContourLine cline : bContours.getContourLines()) {
                    bndContours.add(cline);
                }
            }

        }

        return bndContours;
    }

    /**
     * Special bound processing -
     * 
     * 1. A closed line labeled "-9999.0" is treated as a bound - the grid
     * points inside such a bound are set as "BOUNDED" and values as "RMISSD".
     * 2. A closed line labeled "9999.0" is also treated as a bound - the grid
     * points outside such a bound are set as "BOUNDED" and values as "RMISSD".
     * 3. An unlabeled closed line is treated as a line labeled as "-9999.0"
     * 
     * Note: need to access the "hist" array as it is an FORTRAN array (column
     * first).
     */
    private void checkSpecialBounds(Contours cnt, float[] hist, int kx, int ky,
            CoordinateTransform gtrans, Coordinate[] grdPts, CatMap cmap) {

        ArrayList<ContourLine> cntline = new ArrayList<ContourLine>();
        if (cnt != null) {
            cntline = cnt.getContourLines();
        }

        ArrayList<Polygon> polys = new ArrayList<Polygon>();
        boolean[] inout = new boolean[cntline.size()];

        int nb = 0;
        for (ContourLine cline : cntline) {
            Line ln = cline.getLine();
            String[] label = cline.getLabelString();

            float lblValue = getValueForLabel(cmap, label[0]);

            if (ln.isClosedLine()
                    && (lblValue == G2GCommon.RMISSD || lblValue == -G2GCommon.RMISSD)) {
                ArrayList<Coordinate> coords = ln.getPoints();
                coords.add(new Coordinate(coords.get(0).x, coords.get(0).y));

                Coordinate[] cd = new Coordinate[coords.size()];
                CoordinateSequence sequence = new CoordinateArraySequence(
                        coords.toArray(cd));
                LinearRing ring = new LinearRing(sequence, geometryFactory);
                Polygon g = new Polygon(ring, null, geometryFactory);

                polys.add(g);

                if (lblValue == G2GCommon.RMISSD) {
                    inout[nb] = true;
                } else {
                    inout[nb] = false;
                }

                nb++;
            }
        }

        if (polys.size() > 0) {

            /*
             * Check if a grid point is inside .
             */
            Point p;
            int mb = 0;
            for (Polygon poly : polys) {
                /*
                 * Mask the grid points inside the bound as bounded. If the
                 * inout is set to false, set the grid point outside the bound
                 * as BOUNDED.
                 */
                for (int jj = 0; jj < ky; jj++) {
                    for (int ii = 0; ii < kx; ii++) {

                        p = geometryFactory.createPoint(grdPts[ii + jj * kx]);

                        if (hist[ii + jj * kx] != G2GCommon.BOUNDED) {
                            if ((!poly.contains(p)) ^ inout[mb]) {
                                hist[ii + jj * kx] = G2GCommon.BOUNDED;
                            }
                        }
                    }
                }

                mb++;
            }
        }

    }

    /*
     * Display bounds and grid points.
     */
    private void drawBoundsAndGrid(Contours cnt, float[] hist, int kx, int ky,
            Coordinate[] grdPts) {

        PgenResource drawingLayer = PgenSession.getInstance().getPgenResource();

        /*
         * Build grid points as a a collection of markers - for debug. if a grid
         * point is BOUNDED, it is drawn as red, otherwise, blue.
         */
        DECollection nde = new DECollection();
        nde.setPgenCategory("");
        nde.setPgenType("");
        for (int jj = 0; jj < ky; jj++) {
            for (int ii = 0; ii < kx; ii++) {

                if (hist[ii + jj * kx] == G2GCommon.BOUNDED) {
                    nde.add(new Symbol(null, new Color[] { Color.red }, 0.5f,
                            0.5, false, grdPts[ii + jj * kx], "Marker",
                            "FILLED_BOX"));
                } else {
                    nde.add(new Symbol(null, new Color[] { Color.blue }, 0.5f,
                            0.5, false, grdPts[ii + jj * kx], "Marker",
                            "FILLED_BOX"));

                }
            }
        }

        /*
         * Display bounds and grid points.
         */
        if (cnt != null && cnt.getContourLines().size() > 0) {
            drawingLayer.addElement(cnt);
        }

        drawingLayer.addElement(nde);

    }

    /*
     * Generate a list of points from a circle (Arc) to from a line.
     * 
     * This is adapted from Steve's DisplayElementFactory->
     * createDisplayElements(IArc arc, PaintProperties paintProps)
     */
    private ArrayList<Coordinate> generateArcPoints(Arc arc, double interval) {

        ArrayList<Coordinate> points = new ArrayList<Coordinate>();

        PgenResource drawingLayer = PgenSession.getInstance().getPgenResource();

        /*
         * Convert center and circumference point from lat/lon to pixel
         * coordinates.
         */
        double[] tmp = { arc.getCenterPoint().x, arc.getCenterPoint().y, 0.0 };
        double[] center = drawingLayer.getDescriptor().worldToPixel(tmp);
        double[] tmp2 = { arc.getCircumferencePoint().x,
                arc.getCircumferencePoint().y, 0.0 };
        double[] circum = drawingLayer.getDescriptor().worldToPixel(tmp2);

        /*
         * calculate angle of major axis
         */
        double axisAngle = Math.toDegrees(Math.atan2((circum[1] - center[1]),
                (circum[0] - center[0])));
        double cosineAxis = Math.cos(Math.toRadians(axisAngle));
        double sineAxis = Math.sin(Math.toRadians(axisAngle));

        /*
         * calculate half lengths of major and minor axes
         */
        double diff[] = { circum[0] - center[0], circum[1] - center[1] };
        double major = Math.sqrt((diff[0] * diff[0]) + (diff[1] * diff[1]));
        double minor = major * arc.getAxisRatio();

        /*
         * Calculate points along the arc
         */
        double increment = interval; // degrees
        double angle = arc.getStartAngle();
        int numpts = (int) (Math.round(arc.getEndAngle() - arc.getStartAngle()
                + 1.0) / increment);

        double[][] path = new double[numpts][3];
        for (int j = 0; j < numpts; j++) {
            double thisSine = Math.sin(Math.toRadians(angle));
            double thisCosine = Math.cos(Math.toRadians(angle));
            // Can maybe use simpler less expensive calculations for circle,
            // if ever necessary.
            // if ( arc.getAxisRatio() == 1.0 ) {
            // path[j][0] = center[0] + (major * thisCosine );
            // path[j][1] = center[1] + (minor * thisSine );
            // }
            // else {
            path[j][0] = center[0] + (major * cosineAxis * thisCosine)
                    - (minor * sineAxis * thisSine);
            path[j][1] = center[1] + (major * sineAxis * thisCosine)
                    + (minor * cosineAxis * thisSine);
            // }

            double[] pt = drawingLayer.getDescriptor().pixelToWorld(
                    new double[] { path[j][0], path[j][1], 0.0 });

            points.add(new Coordinate(pt[0], pt[1]));

            angle += increment;
        }

        return points;
    }

}
