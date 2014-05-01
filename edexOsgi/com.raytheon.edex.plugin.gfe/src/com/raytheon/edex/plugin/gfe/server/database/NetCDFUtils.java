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
package com.raytheon.edex.plugin.gfe.server.database;

import java.awt.Point;
import java.awt.Rectangle;
import java.nio.FloatBuffer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import ucar.ma2.ArrayFloat;
import ucar.ma2.DataType;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Utility class for netcdf file readers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2012            randerso    Initial creation
 * Oct 10  2012     #1260  randerso    Cleaned up getSubGridDims to better match A1
 *                                     Changed subGridGl to use new GridLocation constructor
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class NetCDFUtils {
    public static ServerResponse<ProjectionData> getProj(NetcdfFile cdf) {
        ServerResponse<ProjectionData> sr = new ServerResponse<ProjectionData>();

        String projName = "";
        ServerResponse<String> ssr = getStringFileAtt(cdf, "projName");
        if (ssr.isOkay()) {
            projName = ssr.getPayload();
        } else {
            sr.addMessages(ssr);
        }

        int x = 0, y = 0;
        ServerResponse<Integer> isr = getDim(cdf, "x");
        if (isr.isOkay()) {
            x = isr.getPayload();
        } else {
            sr.addMessages(isr);
        }
        isr = getDim(cdf, "y");
        if (isr.isOkay()) {
            y = isr.getPayload();
        } else {
            sr.addMessages(isr);
        }

        float lon00 = 0.0f, lat00 = 0.0f, latNxNy = 0.0f, lonNxNy = 0.0f;
        ServerResponse<Float> fsr = getFloatFileAtt(cdf, "lon00");
        if (fsr.isOkay()) {
            lon00 = fsr.getPayload();
        } else {
            sr.addMessages(fsr);
        }
        fsr = getFloatFileAtt(cdf, "lat00");
        if (fsr.isOkay()) {
            lat00 = fsr.getPayload();
        } else {
            sr.addMessages(fsr);
        }
        fsr = getFloatFileAtt(cdf, "latNxNy");
        if (fsr.isOkay()) {
            latNxNy = fsr.getPayload();
        } else {
            sr.addMessages(fsr);
        }
        fsr = getFloatFileAtt(cdf, "lonNxNy");
        if (fsr.isOkay()) {
            lonNxNy = fsr.getPayload();
        } else {
            sr.addMessages(fsr);
        }
        if (!sr.isOkay()) {
            return sr;
        }

        if (lon00 == 0.0 && lat00 == 0.0 && latNxNy == 0.0 && lonNxNy == 0.0) {
            sr.addMessage("Geo coords lon00, lat00, latNxNy, lonNxNy invalid");
            return sr;
        }

        if (projName.equals("LAMBERT_CONFORMAL")) {
            float centralLat = 0.0f, centralLon = 0.0f;

            fsr = getFloatFileAtt(cdf, "centralLat");
            if (fsr.isOkay()) {
                centralLat = fsr.getPayload();
            } else {
                sr.addMessages(fsr);
            }
            fsr = getFloatFileAtt(cdf, "centralLon");
            if (fsr.isOkay()) {
                centralLon = fsr.getPayload();
            } else {
                sr.addMessages(fsr);
            }
            if (centralLat == 0.0f && centralLon == 0.0f) {
                sr.addMessage("Geo coords centralLat and centralLon are invalid");
                return sr;
            }
            ProjectionData proj = new ProjectionData("D2DProj",
                    ProjectionType.LAMBERT_CONFORMAL, new Coordinate(lon00,
                            lat00), new Coordinate(lonNxNy, latNxNy),
                    new Coordinate(centralLon, centralLat), centralLat,
                    centralLat, new Point(1, 1), new Point(x, y), 0, 0, 0);
            sr.setPayload(proj);
        }

        else if (projName.equals("MERCATOR")) {
            float centralLon = 0.0f;
            fsr = getFloatFileAtt(cdf, "centralLon");
            if (fsr.isOkay()) {
                centralLon = fsr.getPayload();
            } else {
                sr.addMessages(fsr);
            }
            if (centralLon == 0.0f) {
                sr.addMessage("Geo coords centralLon is invalid");
                return sr;
            }
            ProjectionData proj = new ProjectionData("D2DProj",
                    ProjectionType.MERCATOR, new Coordinate(lon00, lat00),
                    new Coordinate(lonNxNy, latNxNy), new Coordinate(0.0, 0.0),
                    0.0f, 0.0f, new Point(1, 1), new Point(x, y), 0,
                    centralLon, 0);
            sr.setPayload(proj);
        }

        else if (projName.equals("STEREOGRAPHIC")) {
            float centralLon = 0.0f;
            fsr = getFloatFileAtt(cdf, "centralLon");
            if (fsr.isOkay()) {
                centralLon = fsr.getPayload();
            } else {
                sr.addMessages(fsr);
            }
            if (centralLon == 0.0f) {
                sr.addMessage("Geo coords centralLon is invalid");
                return sr;
            }
            ProjectionData proj = new ProjectionData("D2DProj",
                    ProjectionType.POLAR_STEREOGRAPHIC, new Coordinate(lon00,
                            lat00), new Coordinate(lonNxNy, latNxNy),
                    new Coordinate(0, 0), 0, 0, new Point(1, 1),
                    new Point(x, y), 0, 0, centralLon);
            sr.setPayload(proj);
        }

        else if (projName.equals("LATLON")
                || projName.equals("CYLINDRICAL_EQUIDISTANT")) {
            float centralLon = 0.0f;
            fsr = getFloatFileAtt(cdf, "centralLon");
            if (fsr.isOkay()) {
                centralLon = fsr.getPayload();
            } else {
                sr.addMessages(fsr);
            }
            ProjectionData proj = new ProjectionData("D2DProj",
                    ProjectionType.LATLON, new Coordinate(lon00, lat00),
                    new Coordinate(lonNxNy, latNxNy), new Coordinate(0, 0), 0f,
                    0f, new Point(1, 1), new Point(x, y), 0f, centralLon, 0.0f);
            sr.setPayload(proj);
        } else {
            sr.addMessage("unknown projection: " + projName);
        }

        return sr;
    }

    public static ServerResponse<Date> parseTime(String str) {
        ServerResponse<Date> sr = new ServerResponse<Date>();

        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd_HHmm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        try {
            Date modelTime = sdf.parse(str);
            sr.setPayload(modelTime);
        } catch (ParseException e) {
            sr.addMessage(str
                    + " does not folow the D2D convention for a date.");
        }

        return sr;
    }

    public static ServerResponse<String> getStringFileAtt(NetcdfFile cdf,
            String name) {
        ServerResponse<String> sr = new ServerResponse<String>();
        Attribute att = cdf.findGlobalAttribute(name);
        if (att == null || !att.getDataType().equals(DataType.STRING)) {
            sr.addMessage("Missing or invalid attribute: " + name);
            return sr;
        }

        String val = att.getStringValue();
        sr.setPayload(val);

        return sr;
    }

    public static ServerResponse<Float> getFloatFileAtt(NetcdfFile cdf,
            String name) {
        ServerResponse<Float> sr = new ServerResponse<Float>();
        Attribute att = cdf.findGlobalAttribute(name);
        if (att == null || !att.getDataType().equals(DataType.FLOAT)) {
            sr.addMessage("Missing or invalid attribute: " + name);
            return sr;
        }

        float val = att.getNumericValue().floatValue();
        sr.setPayload(val);

        return sr;
    }

    public static ServerResponse<Integer> getDim(NetcdfFile cdf, String name) {
        ServerResponse<Integer> sr = new ServerResponse<Integer>();

        Dimension dim = cdf.findDimension(name);
        if (dim == null) {
            sr.addMessage("Missing or invalid dimension: " + name);
            return sr;
        }

        int val = dim.getLength();
        sr.setPayload(val);

        return sr;
    }

    public static ServerResponse<Grid2DFloat> getFloatGrid(NetcdfFile cdf,
            String varName, int index, int level, Rectangle subdomain) {
        ServerResponse<Grid2DFloat> sr = new ServerResponse<Grid2DFloat>();
        Variable var = cdf.findVariable(varName);
        if (var == null || !var.getDataType().equals(DataType.FLOAT)) {
            sr.addMessage("Missing or invalid var: " + varName);
            return sr;
        }

        int[] dims = var.getShape();

        Rectangle nativeDomain = new Rectangle(0, 0, dims[dims.length - 1],
                dims[dims.length - 2]);

        Rectangle subd = (subdomain == null || subdomain.isEmpty() ? nativeDomain
                : subdomain);

        int[] edges = new int[dims.length];
        Arrays.fill(edges, 0);
        int[] count = new int[dims.length];
        Arrays.fill(count, 0, dims.length - 2, 1);

        count[dims.length - 1] = subd.width;
        count[dims.length - 2] = subd.height;
        edges[dims.length - 1] = subd.x;
        edges[dims.length - 2] = subd.y;
        if (dims.length >= 3) {
            if (index < 0 || index >= dims[0]) {
                sr.addMessage("index out of bounds: " + index + ", len = "
                        + dims[0]);
                return sr;
            }
            edges[0] = index;
            edges[1] = level;
        }

        try {
            // flip data in the vertical direction for A2
            ArrayFloat dta = (ArrayFloat) var.read(edges, count).reduce()
                    .flip(0);
            FloatBuffer f = dta.getDataAsByteBuffer().asFloatBuffer();

            if (sr.isOkay()) {
                Grid2DFloat grid = new Grid2DFloat(subd.width, subd.height, f);
                sr.setPayload(grid);
            }
        } catch (Exception e) {
            sr.addMessage(e.getLocalizedMessage());
        }
        return sr;
    }

    public static Rectangle getSubGridDims(GridLocation igloc,
            GridLocation ogloc) {
        List<Integer> xindex = new ArrayList<Integer>();
        List<Integer> yindex = new ArrayList<Integer>();
        for (int x = 0; x < ogloc.gridSize().x; x++) {
            Coordinate ll = ogloc.latLonCenter(new Coordinate(x, 0));

            Point igc = igloc.gridCell((float) ll.y, (float) ll.x);
            xindex.add(igc.x);
            yindex.add(igc.y);

            ll = ogloc.latLonCenter(new Coordinate(x, ogloc.gridSize().y - 1));
            igc = igloc.gridCell((float) ll.y, (float) ll.x);
            xindex.add(igc.x);
            yindex.add(igc.y);
        }
        for (int y = 0; y < ogloc.gridSize().y; y++) {
            Coordinate ll = ogloc.latLonCenter(new Coordinate(0, y));
            Point igc = igloc.gridCell((float) ll.y, (float) ll.x);
            xindex.add(igc.x);
            yindex.add(igc.y);

            ll = ogloc.latLonCenter(new Coordinate(ogloc.gridSize().x - 1, y));
            igc = igloc.gridCell((float) ll.y, (float) ll.x);
            xindex.add(igc.x);
            yindex.add(igc.y);
        }

        // find min/max plus a little extra for so interpolation doesn't have
        // edge effects
        int xmin = Collections.min(xindex) - 2;
        int xmax = Collections.max(xindex) + 2;
        int ymin = Collections.min(yindex) - 2;
        int ymax = Collections.max(yindex) + 2;

        xmin = Math.max(0, xmin);
        ymin = Math.max(0, ymin);
        xmax = Math.min(xmax, igloc.gridSize().x - 1);
        ymax = Math.min(ymax, igloc.gridSize().y - 1);

        Rectangle rval = new Rectangle(xmin, ymin, xmax - xmin, ymax - ymin);

        // fix up coordinates for 0,0 in upper left in A2
        rval.y = igloc.gridSize().y - rval.y - rval.height;

        return rval;
    }
}
