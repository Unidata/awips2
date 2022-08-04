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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.operation.polygonize.Polygonizer;
import org.locationtech.jts.operation.valid.IsValidOp;

/**
 * Command line utility to convert A1 edit area files to A2 edit area files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2008                         Initial creation
 * Apr 01, 2015  #4353     dgilling     Improve logging of Geometry validation errors.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TranslateReferenceSet {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TranslateReferenceSet.class);

    private static JAXBManager jbm;
    static {
        try {
            jbm = new JAXBManager(ReferenceData.class);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            System.exit(-1);
        }
    }

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
        try (BufferedReader in = new BufferedReader(new FileReader(file))) {
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
            IsValidOp validOp = new IsValidOp(polygons);
            if (!validOp.isValid()) {
                System.out.println("WARNING: " + file.getAbsolutePath()
                        + " contains invalid polygons.");
                System.out.println(validOp.getValidationError());
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
                validOp = new IsValidOp(holes);
                if (!validOp.isValid()) {
                    System.out.println("WARNING: " + file.getAbsolutePath()
                            + " contains invalid polygons.");
                    System.out.println(validOp.getValidationError());
                }

                Geometry mp = polygons.difference(holes);
                if (mp instanceof MultiPolygon) {
                    polygons = (MultiPolygon) mp;
                } else if (mp instanceof Polygon) {
                    polygons = gf
                            .createMultiPolygon(new Polygon[] { (Polygon) mp });
                }

                validOp = new IsValidOp(polygons);
                if (!validOp.isValid()) {
                    System.out.println("WARNING: " + file.getAbsolutePath()
                            + " contains invalid polygons.");
                    System.out.println(validOp.getValidationError());
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
     */
    public static void translateFile(File file) throws SerializationException {
        System.out.println("Translating file: " + file.getAbsoluteFile());
        if (file.isDirectory()) {
            for (File f : file.listFiles(new ExtensionFilter(".REFERENCE"))) {
                translateFile(f);
            }
        } else {
            ReferenceData refData = translate(file);
            if (refData != null) {
                String dir = file.getParentFile().getAbsolutePath();
                String fname = file.getName();
                fname = fname.replace(".REFERENCE", ".xml");
                fname = FileUtil.unmangle(fname);
                fname = fname.replace(" ", "_");
                StringBuilder sb = new StringBuilder();
                for (char c : fname.toCharArray()) {
                    if (FileUtil.VALID_FILENAME_CHARS.indexOf(c) == -1) {
                        sb.append("-");
                    } else {
                        sb.append("c");
                    }
                }
                String path = FileUtil.join(dir, fname);
                jbm.marshalToXmlFile(refData, path);
            }
        }
    }

    public static void main(String[] args) {
        if (args.length > 0) {
            for (String arg : args) {
                File file = new File(arg);
                try {
                    translateFile(file);
                } catch (SerializationException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        } else {

            System.out.println("Usage: TranslateReferenceSet path");
        }
    }
}
