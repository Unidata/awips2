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
package com.raytheon.uf.edex.ohd.areal;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import com.raytheon.uf.common.hydro.areal.ArealTypeSelection;
import com.raytheon.uf.common.hydro.areal.GeoAreaData;

/**
 * JUnit test for the GeoDataReader class.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2018   6979     mduff       Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class GeoDataReaderTest {

    @Test
    public void testReadSingleBasin() {
        List<String> content = getSingleBasinContent();
        GeoDataReader reader = new GeoDataReader();
        reader.importGeoArea(content, ArealTypeSelection.BASINS);
        List<GeoAreaData> geoDataList = reader.getGeoDataList();
        for (GeoAreaData data : geoDataList) {
            assertTrue("Invalid BasinID: " + data.getAreaId(),
                    "BasinID".equals(data.getAreaId()));
            assertTrue("Ivalid Boundary Type: " + data.getBoundaryType(),
                    "BASIN".equals(data.getBoundaryType()));
            assertTrue("Invalid interior latitude: " + data.getInteriorLat(),
                    40.10000 == data.getInteriorLat());
            assertTrue("Invalid interior longitude: " + data.getInteriorLon(),
                    98.14803 == data.getInteriorLon());
            assertTrue("Invalid basin name: " + data.getName(),
                    "Basin Name".equals(data.getName()));
            assertTrue("Invalid number of points: " + data.getNumberPoints(),
                    data.getNumberPoints() == data.getLat().length);
            double lastLat = data.getLat()[data.getLat().length - 1];
            double lastLon = data.getLon()[data.getLon().length - 1];
            assertTrue("Last Point Wrong: " + lastLat + ", " + lastLon,
                    lastLat == 40.2029900 && lastLon == -98.2218000);
        }
    }

    @Test
    public void testReadMultiBasin() {
        List<String> content = getMultiBasinContent();
        GeoDataReader reader = new GeoDataReader();
        reader.importGeoArea(content, ArealTypeSelection.BASINS);
        List<GeoAreaData> geoDataList = reader.getGeoDataList();

        int i = 1;
        for (GeoAreaData data : geoDataList) {
            String basinId = "BasinID" + i;
            String basinName = "Basin Name " + i;
            assertTrue("Invalid BasinID: " + data.getAreaId(),
                    basinId.equals(data.getAreaId()));
            assertTrue("Ivalid Boundary Type: " + data.getBoundaryType(),
                    "BASIN".equals(data.getBoundaryType()));
            if (i == 2) {
                assertTrue(
                        "Invalid interior latitude: " + data.getInteriorLat(),
                        41.99500 == data.getInteriorLat());
                assertTrue(
                        "Invalid interior longitude: " + data.getInteriorLon(),
                        94.76000 == data.getInteriorLon());
            } else {
                assertTrue(
                        "Invalid interior latitude: " + data.getInteriorLat(),
                        40.10000 == data.getInteriorLat());
                assertTrue(
                        "Invalid interior longitude: " + data.getInteriorLon(),
                        98.14803 == data.getInteriorLon());
            }
            assertTrue("Invalid basin name: " + data.getName(),
                    basinName.equals(data.getName()));
            assertTrue("Invalid number of points: " + data.getNumberPoints(),
                    data.getNumberPoints() == data.getLat().length);
            i++;
        }
    }

    private List<String> getSingleBasinContent() {
        List<String> content = new ArrayList<>();
        content.add("BasinID Basin Name -1 5 40.10000  98.14803");
        content.add("40.2029900 98.2218000");
        content.add("40.2691300 98.3675100");
        content.add("40.2504600 98.3146700");
        content.add("40.2308200 98.3140300");
        content.add("40.2029900 98.2218000");
        return content;
    }

    private List<String> getMultiBasinContent() {
        List<String> content = new ArrayList<>();
        content.add("BasinID1 Basin Name 1 -1 25 40.10000  98.14803");
        content.add("40.2029900 98.2218000");
        content.add("40.1545600 98.1811800");
        content.add("40.1655000 98.1163200");
        content.add("40.1364600 98.0893900");
        content.add("40.1471400 98.0375500");
        content.add("40.1277000 98.0239600");
        content.add("40.0990600 97.9710500");
        content.add("40.0994700 97.9450000");
        content.add("39.9719800 97.9285400");
        content.add("39.9707600 98.0065400");
        content.add("39.9410900 98.0187100");
        content.add("39.9308500 98.0444100");
        content.add("39.9504900 98.0449800");
        content.add("39.9684000 98.1495200");
        content.add("39.9478600 98.2009000");
        content.add("39.9770900 98.2148100");
        content.add("39.9651600 98.3314800");
        content.add("39.9845600 98.3451200");
        content.add("40.0341500 98.3207000");
        content.add("40.1618200 98.3248400");
        content.add("40.2102000 98.3655600");
        content.add("40.2691300 98.3675100");
        content.add("40.2504600 98.3146700");
        content.add("40.2308200 98.3140300");
        content.add("40.2029900 98.2218000");
        content.add("BasinID2 Basin Name 2 -1 18 41.99500  94.76000");
        content.add("42.2200000 95.0000000");
        content.add("42.1800000 94.8900000");
        content.add("42.1200000 94.7500000");
        content.add("42.0200000 94.6100000");
        content.add("41.9900000 94.5300000");
        content.add("41.9700000 94.5100000");
        content.add("41.8600000 94.5100000");
        content.add("41.8400000 94.4900000");
        content.add("41.7800000 94.5000000");
        content.add("41.7700000 94.5300000");
        content.add("41.8300000 94.6700000");
        content.add("41.9000000 94.7500000");
        content.add("41.9300000 94.8200000");
        content.add("42.0500000 94.9500000");
        content.add("42.1200000 95.0200000");
        content.add("42.1500000 95.0300000");
        content.add("42.2000000 95.0200000");
        content.add("42.2200000 95.0000000");
        content.add("BasinID3 Basin Name 3 -1 25 40.10000  98.14803");
        content.add("40.2029900 98.2218000");
        content.add("40.1545600 98.1811800");
        content.add("40.1655000 98.1163200");
        content.add("40.1364600 98.0893900");
        content.add("40.1471400 98.0375500");
        content.add("40.1277000 98.0239600");
        content.add("40.0990600 97.9710500");
        content.add("40.0994700 97.9450000");
        content.add("39.9719800 97.9285400");
        content.add("39.9707600 98.0065400");
        content.add("39.9410900 98.0187100");
        content.add("39.9308500 98.0444100");
        content.add("39.9504900 98.0449800");
        content.add("39.9684000 98.1495200");
        content.add("39.9478600 98.2009000");
        content.add("39.9770900 98.2148100");
        content.add("39.9651600 98.3314800");
        content.add("39.9845600 98.3451200");
        content.add("40.0341500 98.3207000");
        content.add("40.1618200 98.3248400");
        content.add("40.2102000 98.3655600");
        content.add("40.2691300 98.3675100");
        content.add("40.2504600 98.3146700");
        content.add("40.2308200 98.3140300");
        content.add("40.2029900 98.2218000");
        return content;
    }
}