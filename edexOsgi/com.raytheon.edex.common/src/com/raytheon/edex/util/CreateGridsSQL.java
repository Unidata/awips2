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
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.geospatial.MapUtil;

public class CreateGridsSQL {

    private static Document document;

    /**
     * @param args
     */
    public static void main(String[] args) {

        try {

            File srcDir = new File("D:/Working Folder/grids");

            String[] files = srcDir.list();

            BufferedWriter out = new BufferedWriter(new FileWriter(
                    "D:/Working Folder/grids_spatial.sql"));
            out.write("CREATE TABLE awips.spatial_grids\n");
            out.write("(\n");
            out.write(" grid        integer,\n");
            out.write(" description character varying(2047),\n");
            out.write(" nx      integer,\n");
            out.write(" ny      integer,\n");
            out.write(" dx      float,\n");
            out.write(" dy      float,\n");
            out.write(" crs     character varying(2047),\n");
            out.write(" coverage    geometry,\n");
            out.write(" CONSTRAINT spatial_grids_pkey PRIMARY KEY (grid)\n");
            out.write(")\n");
            out.write("WITHOUT OIDS;\n");
            out.write("ALTER TABLE awips.spatial_grids OWNER TO awips;\n");
            out.write("\n\n");

            for (String fileName : files) {

                StringBuffer fileBuffer = new StringBuffer();
                BufferedReader in = new BufferedReader(new FileReader(
                        "D:/Working Folder/grids/" + fileName));
                String str;
                while ((str = in.readLine()) != null) {
                    fileBuffer.append(str);
                }
                in.close();

                // System.out.println(getSQL(fileBuffer));

                out.write(getSQL(fileBuffer));

            }
            out.close();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public static String getSQL(StringBuffer fileBuffer) {

        String number = null;
        String description = null;
        String nx = null;
        String ny = null;
        String dx = null;
        String dy = null;
        String lov = null;
        String latin1 = null;
        String latin2 = null;
        String lat1 = null;
        String lon1 = null;
        String lat2 = null;
        String lon2 = null;
        String lat3 = null;
        String lon3 = null;
        String lat4 = null;
        String lon4 = null;

        try {

            document = XMLUtils.scanXMLtoDOM(fileBuffer.toString());
            number = getNode("number");
            description = getNode("description");
            nx = getNode("nx");
            ny = getNode("ny");
            dx = getNode("dx");
            dy = getNode("dy");
            lov = getNode("lov");
            latin1 = getNode("latin1");
            latin2 = getNode("latin2");
            lat1 = getNode("lat1");
            lon1 = getNode("lon1");
            lat2 = getNode("lat2");
            lon2 = getNode("lon2");
            lat3 = getNode("lat3");
            lon3 = getNode("lon3");
            lat4 = getNode("lat4");
            lon4 = getNode("lon4");

        } catch (Exception e) {
            e.printStackTrace();
        }

        StringBuffer sql = new StringBuffer();

        sql
                .append("INSERT INTO awips.spatial_grids (grid,description,nx,ny,dx,dy,crs,coverage) VALUES (");

        sql.append(number + ",");
        sql.append("'" + description + "',");
        sql.append(nx + ",");
        sql.append(ny + ",");
        sql.append(dx + ",");
        sql.append(dy + ",");

        if (description.toLowerCase().contains("lambert")) {

            sql.append("'"
                    + MapUtil
                            .constructLambertConformal(
                                    MapUtil.AWIPS_EARTH_RADIUS,
                                    MapUtil.AWIPS_EARTH_RADIUS,
                                    Double.parseDouble(latin1),
                                    Double.parseDouble(latin2),
                                    Double.parseDouble(lov)).toWKT() + "'");
        } else if (description.toLowerCase().contains("stereo")) {
            if (latin1.equals("null")) {
                latin1 = "90";
            }
            // sql.append("'"
            // +
            // MapUtil.constructPolarStereo(Double.parseDouble(lov),Double.parseDouble(latin1))
            // .toWKT() + "'");
        } else if (description.toLowerCase().contains("mercator")) {
            sql.append("'"
                    + MapUtil.constructMercator(MapUtil.AWIPS_EARTH_RADIUS,
                            MapUtil.AWIPS_EARTH_RADIUS,
                            Double.parseDouble(latin1), 0).toWKT() + "'");
        } else if (description.toLowerCase().contains("latitude grid")) {
            sql.append("'" + MapUtil.getLatLonProjection().toWKT() + "'");
        }
        sql.append(",");

        sql.append("GeomFromText('POLYGON ((");

        sql.append(lon1 + " " + lat1 + "," + lon2 + " " + lat2 + "," + lon3
                + " " + lat3 + "," + lon4 + " " + lat4 + "," + lon1 + " "
                + lat1);

        sql.append("))',-1)");
        sql.append(");\n\n");
        return sql.toString();
    }

    public static String getNode(String tagName) {
        NodeList list = document.getElementsByTagName(tagName);
        String retVal = list.item(0).getTextContent();
        if (retVal == null || retVal.length() < 1) {
            retVal = "null";
        }

        return retVal;
    }

}
