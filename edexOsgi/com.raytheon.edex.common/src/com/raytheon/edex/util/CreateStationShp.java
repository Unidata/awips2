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

package com.raytheon.edex.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import org.geotools.data.FeatureStore;
import org.geotools.data.FeatureWriter;
import org.geotools.data.Transaction;
import org.geotools.data.shapefile.indexed.IndexType;
import org.geotools.data.shapefile.indexed.IndexedShapefileDataStore;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * Creates a station catalog shapefile
 * 
 * @author chammack
 * 
 */
public class CreateStationShp {

    public static void main(String[] args) {

        File theShape = new File("D:/temp/stations.shp");
        GeometryFactory geomFactory = new GeometryFactory();
        try {
            // Set up what our data schema will look like
            SimpleFeatureTypeBuilder builder = new SimpleFeatureTypeBuilder();
            builder.setName("stations");
            builder.add("the_geom", Point.class);
            builder.add("icao", String.class);
            builder.add("attrib2", String.class);
            builder.add("attrib3", String.class);
            builder.add("name", String.class);
            builder.add("state", String.class);
            builder.add("country", String.class);
            builder.add("attrib7", String.class);
            builder.add("attrib12", String.class);
            builder.add("attrib13", String.class);

            SimpleFeatureType featureType = builder.buildFeatureType();

            // Load up the indexed datastore
            IndexedShapefileDataStore shpDS = new IndexedShapefileDataStore(
                    theShape.toURI().toURL(), null, true, true, IndexType.QIX);

            // Tell the datastore to create the dbf schema if it does not
            // exist
            shpDS.createSchema(featureType);

            // Get the transaction object
            FeatureStore<SimpleFeatureType, SimpleFeature> fs = (FeatureStore<SimpleFeatureType, SimpleFeature>) shpDS
                    .getFeatureSource();
            Transaction trx = (fs).getTransaction();

            // Open a feature writer with the transaction
            FeatureWriter<SimpleFeatureType, SimpleFeature> writer = shpDS
                    .getFeatureWriter(trx);

            // Open the textfile
            BufferedReader br = new BufferedReader(new FileReader(
                    "D:/temp/nsd_cccc.txt"));
            while (br.ready()) {

                // Read and parse the file
                String line = br.readLine();
                String[] fields = line.split(";", -1);
                if (fields.length < 12) {
                    System.out.println("Parse error on line " + line + " ("
                            + fields.length + ")");
                    System.exit(0);
                }

                // Load the feature to write, and set all of the properties by
                // name
                SimpleFeature feature = writer.next();
                feature.setAttribute("icao", fields[0]);
                feature.setAttribute("attrib2", fields[1]);
                feature.setAttribute("attrib3", fields[2]);
                feature.setAttribute("name", fields[3]);
                feature.setAttribute("state", fields[4]);
                feature.setAttribute("country", fields[5]);
                feature.setAttribute("attrib7", fields[6]);

                // Parse out dms
                double lat1 = extractDegrees(fields[7]);
                double lon1 = extractDegrees(fields[8]);

                // Construct the point geometry object and add to the feature
                Coordinate pt1 = new Coordinate(lon1, lat1);
                Point p = geomFactory.createPoint(pt1);
                feature.setDefaultGeometry(p);

                feature.setAttribute("attrib12", fields[11]);
                if (fields.length >= 13) {
                    feature.setAttribute("attrib13", fields[12]);
                }

                writer.write();

            }

            writer.close();
            System.out.println("Done");

        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    private static double extractDegrees(String rawField) {
        String[] dms = rawField.split("-");
        int deg = Integer.parseInt(dms[0].trim());

        char dir;

        int min = 0;
        int sec = 0;
        if (dms.length == 2) {
            dir = dms[1].charAt(dms[1].length() - 1);
            min = Integer.parseInt(dms[1].substring(0, dms[1].length() - 1));

        } else {
            min = Integer.parseInt(dms[1]);

            dir = dms[2].charAt(dms[2].length() - 1);
            String substr = dms[2].substring(0, dms[2].length() - 1);
            if (substr.trim().length() > 0) {
                sec = Integer.parseInt(substr);
            }

        }
        if (dir == 'S' || dir == 'W') {
            return dmsToDecimal(deg, min, sec) * -1;
        } else {
            return dmsToDecimal(deg, min, sec);
        }

    }

    private static double dmsToDecimal(int deg, int min, int sec) {
        return ((min) / 60.0) + ((sec) / 3600.0) + deg;
    }

}
