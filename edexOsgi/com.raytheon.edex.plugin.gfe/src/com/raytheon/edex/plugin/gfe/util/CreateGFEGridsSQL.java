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
package com.raytheon.edex.plugin.gfe.util;

import java.awt.Point;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.util.LinkedHashMap;
import java.util.TimeZone;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;

public class CreateGFEGridsSQL {
    static final CreateGFEGridsSQL x = new CreateGFEGridsSQL();

    // public class ProjectionData {
    // String projectionType;
    //
    // Coordinate latLonLL;
    //
    // Coordinate latLonUR;
    //
    // Coordinate latLonOrigin;
    //
    // double stdParallelOne;
    //
    // double stdParallelTwo;
    //
    // Coordinate gridPointLL;
    //
    // Coordinate gridPointUR;
    //
    // double latIntersect;
    //
    // double lonCenter;
    //
    // double lonOrigin;
    //
    // public ProjectionData() {
    // latLonLL = new Coordinate();
    // latLonUR = new Coordinate();
    // latLonOrigin = new Coordinate();
    // gridPointLL = new Coordinate();
    // gridPointUR = new Coordinate();
    // }
    //
    // /*
    // * (non-Javadoc)
    // *
    // * @see java.lang.Object#toString()
    // */
    // @Override
    // public String toString() {
    // String result = "{";
    //
    // result += projectionType + ", ";
    // result += latLonLL + ", ";
    // result += latLonUR + ", ";
    // result += latLonOrigin + ", ";
    // result += stdParallelOne + ", ";
    // result += stdParallelTwo + ", ";
    // result += gridPointLL + ", ";
    // result += gridPointUR + ", ";
    // result += latIntersect + ", ";
    // result += lonCenter + ", ";
    // result += lonOrigin;
    //
    // result += "}";
    // return result;
    // }
    //
    // }

    public class GridData {
        Coordinate gridSize;

        Coordinate origin;

        Coordinate extent;

        String timeZone;

        String projName;

        public GridData() {
            gridSize = new Coordinate();
            origin = new Coordinate();
            extent = new Coordinate();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            String result = "{";

            result += gridSize + ", ";
            result += origin + ", ";
            result += extent + ", ";
            result += timeZone + ", ";
            result += projName;

            result += "}";
            return result;
        }

    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        try {
            BufferedReader in = new BufferedReader(new FileReader(
                    "/home/randerso/gfe/GFEProjs.txt"));
            StreamTokenizer st = new StreamTokenizer(in);
            st.eolIsSignificant(false);
            st.parseNumbers();
            st.quoteChar('\'');
            st.commentChar('#');
            st.wordChars('_', '_');

            LinkedHashMap<String, ProjectionData> projMap = new LinkedHashMap<String, ProjectionData>();
            while (st.ttype != StreamTokenizer.TT_EOF) {
                if (getToken(st) != StreamTokenizer.TT_WORD) {
                    continue;
                }

                ProjectionData proj = readProj(st);
                if (proj != null) {
                    projMap.put(proj.getProjectionID(), proj);
                }
            }
            in.close();

            in = new BufferedReader(new FileReader(
                    "/home/randerso/gfe/GFEGrids.txt"));

            st = new StreamTokenizer(in);
            st.eolIsSignificant(false);
            st.parseNumbers();
            st.ordinaryChar('\'');
            st.commentChar('#');
            st.wordChars('_', '_');
            st.wordChars('/', '/');

            LinkedHashMap<String, GridData> gridMap = new LinkedHashMap<String, GridData>();
            while (st.ttype != StreamTokenizer.TT_EOF) {
                if (getToken(st) != StreamTokenizer.TT_WORD) {
                    continue;
                }
                String id = st.sval;

                GridData grid = readGrid(st);
                if (grid != null) {
                    if (projMap.containsKey(grid.projName)) {
                        gridMap.put(id, grid);
                    } else {
                        System.out.println("Grid: " + id
                                + " refers to unknown projection "
                                + grid.projName);
                    }
                }
            }
            in.close();

            BufferedWriter out = new BufferedWriter(new FileWriter(
                    "/home/randerso/gfe/spatial_gfe.sql"));
            out.write("DROP TABLE IF EXISTS awips.spatial_gfe;\n");
            out.write("CREATE TABLE awips.spatial_gfe\n");
            out.write("(\n");
            out.write(" siteId  character varying(4),\n");
            out.write(" nx      integer,\n");
            out.write(" ny      integer,\n");
            out.write(" timeZone character varying(32),\n");
            out.write(" crs     character varying(2047),\n");
            out.write(" coverage    geometry,\n");
            out.write(" CONSTRAINT spatial_gfe_pkey PRIMARY KEY (siteId)\n");
            out.write(")\n");
            out.write("WITHOUT OIDS;\n");
            out.write("ALTER TABLE awips.spatial_gfe OWNER TO awips;\n");
            out.write("\n\n");

            for (String gridId : gridMap.keySet()) {
                GridData grid = gridMap.get(gridId);
                String sql = createInsert(gridId, grid, projMap
                        .get(grid.projName));
                out.write(sql);
                out.write("\n\n");
            }
            out.close();

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    public static int getToken(StreamTokenizer st) throws IOException {
        loop: while (st.nextToken() != StreamTokenizer.TT_EOF) {
            switch (st.ttype) {
            case StreamTokenizer.TT_NUMBER:
            case StreamTokenizer.TT_WORD:
                break loop;
            }
        }
        return st.ttype;
    }

    public static GridData readGrid(StreamTokenizer st) throws IOException {
        GridData grid = x.new GridData();

        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        grid.gridSize.x = st.nval;
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        grid.gridSize.y = st.nval;

        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        grid.origin.x = st.nval;
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        grid.origin.y = st.nval;

        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        grid.extent.x = st.nval;
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        grid.extent.y = st.nval;

        if (getToken(st) != StreamTokenizer.TT_WORD) {
            return null;
        }
        grid.timeZone = st.sval;

        if (getToken(st) != StreamTokenizer.TT_WORD) {
            return null;
        }
        grid.projName = st.sval;

        TimeZone tz = TimeZone.getTimeZone(grid.timeZone);

        if (!grid.timeZone.equals(tz.getID())) {
            System.out
                    .println("ERROR: unrecognized time zone " + grid.timeZone);
        }

        return grid;
    }

    public static ProjectionData readProj(StreamTokenizer st)
            throws IOException {

        String id = st.sval;
        if (getToken(st) != StreamTokenizer.TT_WORD) {
            return null;
        }
        String s = st.sval;
        ProjectionType projectionType = ProjectionType.valueOf(s);

        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        Coordinate latLonLL = new Coordinate();
        latLonLL.x = MapUtil.correctLon(st.nval);
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        latLonLL.y = st.nval;

        Coordinate latLonUR = new Coordinate();
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        latLonUR.x = MapUtil.correctLon(st.nval);
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        latLonUR.y = st.nval;

        Coordinate latLonOrigin = new Coordinate();
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        latLonOrigin.x = MapUtil.correctLon(st.nval);
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        latLonOrigin.y = st.nval;

        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        float stdParallelOne = (float) st.nval;

        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        float stdParallelTwo = (float) st.nval;

        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        Point gridPointLL = new Point();
        gridPointLL.x = (int) st.nval;
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        gridPointLL.y = (int) st.nval;

        Point gridPointUR = new Point();
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        gridPointUR.x = (int) st.nval;
        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        gridPointUR.y = (int) st.nval;

        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        float latIntersect = (float) MapUtil.correctLat(st.nval);

        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        float lonCenter = (float) MapUtil.correctLon(st.nval);

        if (getToken(st) != StreamTokenizer.TT_NUMBER) {
            return null;
        }
        float lonOrigin = (float) MapUtil.correctLon(st.nval);

        ProjectionData proj = new ProjectionData(id, projectionType.ordinal(),
                latLonLL, latLonUR, latLonOrigin, stdParallelOne,
                stdParallelTwo, gridPointLL, gridPointUR, latIntersect,
                lonCenter, lonOrigin);
        return proj;
    }

    public static String createInsert(String gridId, GridData grid,
            ProjectionData proj) {
        try {
            // construct the appropriate CRS based on the projection type
            CoordinateReferenceSystem crs;
            switch (proj.getProjectionType()) {
            case LAMBERT_CONFORMAL:
                crs = MapUtil.constructLambertConformal(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        proj.getStdParallelOne(), proj.getStdParallelTwo(),
                        proj.getLatLonOrigin().x);
                break;

            case MERCATOR:
                crs = MapUtil.constructMercator(MapUtil.AWIPS_EARTH_RADIUS,
                        MapUtil.AWIPS_EARTH_RADIUS, proj.getStdParallelOne(),
                        proj.getLonCenter());
                break;

            case POLAR_STEREOGRAPHIC:
                crs = MapUtil.constructNorthPolarStereo(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        60.0, proj.getLonOrigin());
                break;

            case LATLON:
                crs = MapUtil.LATLON_PROJECTION;
                break;

            case NONE:
            default:
                System.out.println("ERROR: unknown projection type: "
                        + proj.getProjectionType());
                return "";
            }

            // transform the projection corner points to CRS units
            MathTransform mt = MapUtil.getTransformFromLatLon(crs);
            double[] output = new double[4];
            mt.transform(new double[] { proj.getLatLonLL().x,
                    proj.getLatLonLL().y, proj.getLatLonUR().x,
                    proj.getLatLonUR().y }, 0, output, 0, 2);

            // create a grid geometry for the projection
            GeneralEnvelope ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(crs);
            ge.setRange(0, Math.min(output[0], output[2]), Math.max(output[0],
                    output[2]));
            ge.setRange(1, Math.min(output[1], output[3]), Math.max(output[1],
                    output[3]));

            GeneralGridEnvelope gr = new GeneralGridEnvelope(new int[] {
                    proj.getGridPointLL().x, proj.getGridPointLL().y },
                    new int[] { proj.getGridPointUR().x,
                            proj.getGridPointUR().y }, false);

            GridGeometry2D projGeom = new GridGeometry2D(gr, ge);

            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            double[] latLon = new double[8];

            // check to see if the projection looks right
            // mt = dmtf.createConcatenatedTransform(projGeom
            // .getGridToCRS(PixelOrientation.UPPER_LEFT), MapUtil
            // .getTransformToLatLon(crs));
            // mt.transform(new double[] { proj.getGridPointLL().x,
            // proj.getGridPointUR().y, proj.getGridPointLL().x,
            // proj.getGridPointLL().y, proj.getGridPointUR().x,
            // proj.getGridPointLL().y, proj.getGridPointUR().x,
            // proj.getGridPointUR().y }, 0, latLon, 0, 4);
            //
            // System.out.println();
            // System.out.println(grid.projName);
            // for (int i = 0; i < latLon.length; i += 2) {
            // System.out
            // .println("(" + latLon[i + 1] + ", " + latLon[i] + ")");
            // }

            // transform the grid corners from grid coordinates to CRS units
            // need to adjust for the fact that AWIPS considers 1,1 to be in
            // lower left
            // and GeoTools considers 1,1 to be in upper left
            Coordinate ll = new Coordinate(grid.origin.x,
                    proj.getGridPointUR().y - grid.origin.y
                            + proj.getGridPointLL().y);
            Coordinate ur = new Coordinate(grid.origin.x + grid.extent.x, ll.y
                    - grid.extent.y);

            mt.transform(new double[] { ll.x, ll.y, ur.x, ur.y }, 0, output, 0,
                    2);

            // System.out.println();
            // for (int i = 0; i < output.length; i += 2) {
            // System.out
            // .println("(" + output[i + 1] + ", " + output[i] + ")");
            // }

            mt = projGeom.getGridToCRS(PixelOrientation.UPPER_LEFT);
            output = new double[4];
            mt.transform(new double[] { ll.x, ll.y, ur.x, ur.y }, 0, output, 0,
                    2);

            // construct the grid geometry that covers the GFE grid
            ge.setRange(0, Math.min(output[0], output[2]), Math.max(output[0],
                    output[2]));
            ge.setRange(1, Math.min(output[1], output[3]), Math.max(output[1],
                    output[3]));
            GridGeometry2D gridGeom = new GridGeometry2D(
                    new GeneralGridEnvelope(new int[] { 0, 0 }, new int[] {
                            (int) grid.gridSize.x - 1,
                            (int) grid.gridSize.y - 1 }, false), ge);

            // set up the transform from grid coordinates to lon/lat
            mt = dmtf.createConcatenatedTransform(gridGeom
                    .getGridToCRS(PixelOrientation.CENTER), MapUtil
                    .getTransformToLatLon(crs));

            // transform grid corner points to Lat/Lon
            mt.transform(new double[] { -1.0, grid.gridSize.y - 1, -1.0, -1.0,
                    grid.gridSize.x - 1, -1.0, grid.gridSize.x - 1,
                    grid.gridSize.y - 1 }, 0, latLon, 0, 4);

            // System.out.println();
            // System.out.println(gridId);
            // for (int i = 0; i < latLon.length; i += 2) {
            // System.out
            // .println("(" + latLon[i + 1] + ", " + latLon[i] + ")");
            // }

            StringBuffer sql = new StringBuffer();
            sql
                    .append("INSERT INTO awips.spatial_gfe (siteId,nx,ny,timeZone,crs,coverage) VALUES (");

            sql.append("'" + gridId + "',");
            sql.append((int) grid.gridSize.x + ",");
            sql.append((int) grid.gridSize.y + ",");
            sql.append("'" + grid.timeZone + "',");
            sql.append("'" + crs.toWKT() + "',\n");
            sql.append("GeomFromText('POLYGON ((" + latLon[0] + " " + latLon[1]
                    + ",\n" + latLon[2] + " " + latLon[3] + ",\n" + latLon[4]
                    + " " + latLon[5] + ",\n" + latLon[6] + " " + latLon[7]
                    + ",\n" + latLon[0] + " " + latLon[1] + "))',-1)");

            sql.append(");");
            return sql.toString();

        } catch (TransformException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (FactoryException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return "";
    }
}
