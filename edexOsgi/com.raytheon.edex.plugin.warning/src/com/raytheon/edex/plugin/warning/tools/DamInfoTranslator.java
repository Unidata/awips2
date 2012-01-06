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
package com.raytheon.edex.plugin.warning.tools;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Translate dam_info.txt file into SQL to create CustomLocations table in
 * postgis
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 11, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class DamInfoTranslator {

    private static String CWA = "OAX";

    private static String RFC = "MBRFC";

    private static Pattern pattern = Pattern
            .compile("\\{<(.*?)>=(.*?)=.*LAT\\.\\.\\.LON([\\s0-9]*).*\\}");

    private static GeometryFactory geomFactory = new GeometryFactory();

    private static int bullet = 1;;

    public static void translate(File damInfoTxt) {
        try {
            BufferedReader in = new BufferedReader(new FileReader(damInfoTxt));

            System.out.println("DROP TABLE mapdata.customlocations;");

            System.out.println("CREATE TABLE mapdata.customlocations");
            System.out.println("(");
            System.out.println("gid serial PRIMARY KEY,");
            System.out.println("bullet character varying(16),");
            System.out.println("\"name\" character varying(64),");
            System.out.println("cwa character varying(12),");
            System.out.println("rfc character varying(8),");
            System.out.println("lon double precision,");
            System.out.println("lat double precision);");

            System.out
                    .println("SELECT AddGeometryColumn('mapdata','customlocations','the_geom',4326,'MULTIPOLYGON',2);");
            System.out
                    .println("SELECT AddGeometryColumn('mapdata','customlocations','the_geom_0_00',4326,'MULTIPOLYGON',2);");
            System.out
                    .println("CREATE INDEX customlocations_the_geom_gist ON mapdata.customlocations USING gist(the_geom);");

            int lineNum = 0;
            String line;
            StringBuilder itemBuilder = null;
            String item;
            while ((line = in.readLine()) != null) {
                lineNum++;

                // skip comment lines
                if (line.startsWith("//")) {
                    continue;
                }

                if (line.startsWith("{")) {
                    itemBuilder = new StringBuilder();
                }

                if (itemBuilder != null) {
                    itemBuilder.append(line);
                    itemBuilder.append(' ');
                }

                if (line.endsWith("}")) {
                    if (itemBuilder == null) {
                        System.out.println("Unbalanced ending brace at line "
                                + lineNum + " in "
                                + damInfoTxt.getAbsolutePath());
                    }
                    item = itemBuilder.toString();
                    itemBuilder = null;

                    parseItem(item);
                }
            }

            System.out
                    .println("UPDATE mapdata.customlocations SET the_geom_0_00=ST_Segmentize(the_geom,0.1);");
            System.out
                    .println("CREATE INDEX customlocations_the_geom_0_00_gist ON mapdata.customlocations USING gist(the_geom_0_00);");

        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private static void parseItem(String item) {
        Matcher matcher = pattern.matcher(item);
        if (matcher.find()) {
            String type = matcher.group(1);
            String name = matcher.group(2);
            String coords = matcher.group(3);

            Polygon polygon = parseCoords(coords);
            MultiPolygon mp = geomFactory
                    .createMultiPolygon(new Polygon[] { polygon });

            Point centroid = polygon.getCentroid();

            System.out
                    .println("INSERT INTO mapdata.customlocations (\"bullet\",\"name\",\"cwa\",\"rfc\",\"lon\",\"lat\",the_geom) VALUES('"
                            + "bullet"
                            + bullet++
                            + "','"
                            + name
                            + "','"
                            + CWA
                            + "','"
                            + RFC
                            + "','"
                            + centroid.getX()
                            + "','"
                            + centroid.getY()
                            + "', GeomFromText('"
                            + mp.toText() + "',4326));");
        }
    }

    private static Polygon parseCoords(String coords) {
        String[] ll = coords.trim().split("\\s");

        Coordinate[] c = new Coordinate[ll.length / 2 + 1];
        int j = 0;
        for (int i = 0; i < ll.length; i += 2) {
            Double lat = Integer.parseInt(ll[i]) / 100.0;
            Double lon = Integer.parseInt(ll[i + 1]) / 100.0;
            c[j++] = new Coordinate(-lon, lat);
        }
        c[j] = c[0];

        LinearRing lr = geomFactory.createLinearRing(c);
        Polygon poly = geomFactory.createPolygon(lr, null);
        return poly;
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out
                    .println("usage: DamInfoTranslator [cwa [rfc]] xxx-dam_info.txt ");
            return;
        }

        if (args.length > 1) {
            CWA = args[0];
        }

        if (args.length > 2) {
            RFC = args[1];
        }
        translate(new File(args[args.length - 1]));
    }
}
