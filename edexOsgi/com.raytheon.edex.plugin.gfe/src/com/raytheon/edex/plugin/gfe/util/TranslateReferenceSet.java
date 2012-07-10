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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.operation.polygonize.Polygonizer;
import com.vividsolutions.jts.operation.valid.IsValidOp;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/07/2008
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TranslateReferenceSet {
    public static class DefaultFilter implements FileFilter {

        @Override
        public boolean accept(File f) {
            return (!f.isHidden() && f.canRead());
        }

    }

    public static class ExtensionFilter extends DefaultFilter {
        private List<String> extensions;

        public ExtensionFilter(String... extensions) {
            super();
            this.extensions = Arrays.asList(extensions);
        }

        @Override
        public boolean accept(File f) {
            if (!super.accept(f)) {
                return false;
            }

            if (f.isDirectory()) {
                return true;
            }

            String name = f.getName();
            int pos = name.lastIndexOf(".");
            if (pos < 0) {
                return true;
            }

            String ext = name.substring(pos);
            return extensions.contains(ext);
        }
    }

    public static ReferenceData translate(File file) {
        ReferenceData refData = null;
        try {
            BufferedReader in = new BufferedReader(new FileReader(file));
            String query = in.readLine();
            if ("NOQUERY".equals(query)) {
                query = null;
            }

            int numPolygons = Integer.parseInt(in.readLine());

            GeometryFactory gf = new GeometryFactory();
            List<LineString> incPolys = new ArrayList<LineString>();
            List<LineString> excPolys = new ArrayList<LineString>();
            for (int i = 0; i < numPolygons; i++) {
                String s = in.readLine();
                boolean include;
                if ("INC".equals(s)) {
                    include = true;
                } else if ("EXC".equals(s)) {
                    include = false;
                } else {
                    throw new IOException("Invalid inclusion flag: \"" + s
                            + "\"");
                }
                int numVertices = Integer.parseInt(in.readLine());
                Coordinate[] vertices = new Coordinate[Math.max(4, numVertices)];
                for (int j = 0; j < numVertices; j++) {
                    s = in.readLine();
                    String[] tokens = s.split(" ");
                    double x = Double.parseDouble(tokens[0]);
                    double y = Double.parseDouble(tokens[1]);
                    vertices[j] = new Coordinate(x, y);
                }

                LineString ls = gf.createLineString(vertices);

                if (include) {
                    incPolys.add(ls);
                } else {
                    excPolys.add(ls);
                }
            }
            in.close();

            Geometry mls = gf.buildGeometry(incPolys);
            Point pt = gf.createPoint(mls.getCoordinate());
            Geometry nodedLines = mls.union(pt);

            Polygonizer polygonizer = new Polygonizer();
            polygonizer.add(nodedLines);
            Collection<?> dangles = polygonizer.getDangles();
            if (dangles != null && dangles.size() > 0) {
                System.out.println("WARNING: " + file.getAbsolutePath()
                        + " contains dangling lines.");
                for (Object g : dangles) {
                    System.out.println(g);
                }
            }
            Collection<?> cutEdges = polygonizer.getCutEdges();
            if (cutEdges != null && cutEdges.size() > 0) {
                System.out.println("WARNING: " + file.getAbsolutePath()
                        + " contains cut edges.");
                for (Object g : cutEdges) {
                    System.out.println(g);
                }
            }
            Collection<?> included = polygonizer.getPolygons();

            MultiPolygon polygons = gf.createMultiPolygon(included
                    .toArray(new Polygon[included.size()]));
            if (!polygons.isValid()) {
                System.out.println("WARNING: " + file.getAbsolutePath()
                        + " contains invalid polygons.");
                IsValidOp validOp = new IsValidOp(polygons);
                System.out.println(validOp.getValidationError());
                for (int i = 0; i < polygons.getNumGeometries(); i++) {
                    Polygon g = (Polygon) polygons.getGeometryN(i);
                    if (!g.isValid()) {
                        System.out.println(g);
                    }
                }
            }

            mls = gf.buildGeometry(excPolys);
            pt = gf.createPoint(mls.getCoordinate());
            nodedLines = mls.union(pt);

            polygonizer = new Polygonizer();
            polygonizer.add(nodedLines);
            dangles = polygonizer.getDangles();
            if (dangles != null && dangles.size() > 0) {
                System.out.println("WARNING: " + file.getAbsolutePath()
                        + " contains dangling lines.");
                for (Object g : dangles) {
                    System.out.println(g);
                }
            }
            cutEdges = polygonizer.getCutEdges();
            if (cutEdges != null && cutEdges.size() > 0) {
                System.out.println("WARNING: " + file.getAbsolutePath()
                        + " contains cut edges.");
                for (Object g : cutEdges) {
                    System.out.println(g);
                }
            }
            Collection<?> excluded = polygonizer.getPolygons();

            if (excluded.size() > 0) {
                MultiPolygon holes = gf.createMultiPolygon(excluded
                        .toArray(new Polygon[excPolys.size()]));
                if (!holes.isValid()) {
                    System.out.println("WARNING: " + file.getAbsolutePath()
                            + " contains invalid polygons.");
                    for (int i = 0; i < holes.getNumGeometries(); i++) {
                        Geometry g = holes.getGeometryN(i);
                        if (!g.isValid()) {
                            System.out.println(g);
                        }
                    }
                }

                Geometry mp = polygons.difference(holes);
                if (mp instanceof MultiPolygon) {
                    polygons = (MultiPolygon) mp;
                } else if (mp instanceof Polygon) {
                    polygons = gf
                            .createMultiPolygon(new Polygon[] { (Polygon) mp });
                }

                if (!polygons.isValid()) {
                    System.out.println("WARNING: " + file.getAbsolutePath()
                            + " contains invalid polygons.");
                    for (int i = 0; i < polygons.getNumGeometries(); i++) {
                        Geometry g = polygons.getGeometryN(i);
                        if (!g.isValid()) {
                            System.out.println(g);
                        }
                    }
                }
            }

            String name = file.getName();
            name = name.substring(0, name.lastIndexOf('.'));
            ReferenceID id = new ReferenceID(name);
            refData = new ReferenceData((GridLocation) null, id, query,
                    polygons, ReferenceData.CoordinateType.LATLON);

        } catch (Exception e) {
            System.out.println("ERROR while translating file: "
                    + file.getAbsolutePath());
            e.printStackTrace();
        }

        return refData;
    }

    /**
     * @param file
     * @throws SerializationException
     * @throws JAXBException
     */
    public static void translateFile(File file) throws SerializationException,
            JAXBException {
        JAXBManager jbm = new JAXBManager(ReferenceData.class);
        if (file.isDirectory()) {
            for (File f : file.listFiles(new ExtensionFilter(".REFERENCE"))) {
                System.out.println("Translating: " + f.getAbsoluteFile());
                translateFile(f);
            }
        } else {
            ReferenceData refData = translate(file);
            if (refData != null) {
                String path = file.getAbsolutePath();
                path = path.substring(0, path.lastIndexOf('.')) + ".xml";
                jbm.jaxbMarshalToXmlFile(refData, path);
            }
        }
    }

    public static void main(String[] args) {
        if (args.length > 0) {
            for (String arg : args) {
                File file = new File(arg);
                try {
                    translateFile(file);
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        } else {

            System.out.println("Usage: TranslateReferenceSet path");
        }
    }
}
