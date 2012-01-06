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
package com.raytheon.viz.grid.gridcache;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Used to testing the Data cube cache functionality. Can be removed from the
 * baseline
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public class TestDriver {
    public void printcache() {
        System.out.println(DataCache.getInstance().toString());
    }

    public void createCache() {
        // DataCache.getInstance().clearCache();
        DataCache cache = DataCache.getInstance();
        DataCacheManager dman = DataCacheManager.getInstance();

        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add("T");
        paramList.add("RH");
        paramList.add("GH");
        LayerProperty lpParm = new LayerProperty();

        // String modelName = "NAM12";
        String modelName = "NAM212";
        String levelType = "MB";

        for (int i = 0; i < paramList.size(); i++) {
            DataCubeFactory factory = new DataCubeFactory();
            HashMap<String, RequestConstraint> query = new HashMap<String, RequestConstraint>();
            query.put("pluginName", new RequestConstraint("grib"));

            // Model parameter
            query.put("modelInfo.parameterAbbreviation", new RequestConstraint(
                    paramList.get(i)));

            // model name
            query.put("modelInfo.modelName", new RequestConstraint(modelName));

            // level type
            query.put("modelInfo.level.masterLevel.name",
                    new RequestConstraint(levelType));

            query.put("dataTime.refTime", new RequestConstraint(
                    "2009-11-17 06:00:00"));

            query.put("dataTime.fcstTime", new RequestConstraint("0"));
            // query.put("dataTime.fcstTime", new RequestConstraint("10800"));
            // query.put("dataTime.fcstTime", new RequestConstraint("21600"));

            // model identifying data
            // query.put("modelInfo.gridid", new RequestConstraint("211"));
            // query.put("modelInfo.genprocess", new RequestConstraint("84"));
            // query.put("modelInfo.centerid", new RequestConstraint("7"));
            // query.put("modelInfo.subcenterid", new RequestConstraint("0"));

            // The number of records to return
            lpParm.setNumberOfImages(9999);

            try {
                Date d1 = new Date();
                lpParm.setEntryQueryParameters(query, false);
                DataCacheCube cube = factory.createCube(lpParm);
                cache.addData(cube, true);

                Date d2 = new Date();
                System.out.println("Cube generation time:  "
                        + (d2.getTime() - d1.getTime()));

                // ArrayList<GribRecord> gribRecList =
                // cube.getGribRecordStack();

                // GribRecord rec = gribRecList.get(0);
                // float[] fa = dman.getData(rec);
                // System.out.println(fa.length + " elements in the array");
            } catch (VizException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    public void getData() {
        DataCacheManager dman = DataCacheManager.getInstance();

        HashMap<String, RequestConstraint> query = new HashMap<String, RequestConstraint>();
        query.put("pluginName", new RequestConstraint("grib"));

        // Model parameter
        query
                .put("modelInfo.parameterAbbreviation", new RequestConstraint(
                        "T"));

        // model name
        query.put("modelInfo.modelName", new RequestConstraint("NAM212"));

        // level type
        query.put("modelInfo.level.masterLevel.name", new RequestConstraint(
                "MB"));

        query.put("dataTime.refTime", new RequestConstraint(
                "2009-11-17 06:00:00"));

        query.put("dataTime.fcstTime", new RequestConstraint("0"));
        // query.put("dataTime.fcstTime", new RequestConstraint("10800"));
        // query.put("dataTime.fcstTime", new RequestConstraint("21600"));

        // model identifying data
        // query.put("modelInfo.gridid", new RequestConstraint("211"));
        // query.put("modelInfo.genprocess", new RequestConstraint("84"));
        // query.put("modelInfo.centerid", new RequestConstraint("7"));
        // query.put("modelInfo.subcenterid", new RequestConstraint("0"));

        // query.put("modelInfo.level.id", new
        // RequestConstraint("-796167917"));

        LayerProperty lpParm = new LayerProperty();
        // The number of records to return
        lpParm.setNumberOfImages(9999);

        try {
            Date d1 = new Date();
            lpParm.setEntryQueryParameters(query, false);

            float[] fa = dman.getDataCube(lpParm);
            Date d2 = new Date();

            System.out.println(fa.length + " elements in the array");
            System.out.println(d2.getTime() - d1.getTime()
                    + " millis for the query");

            DataCubeFactory factory = new DataCubeFactory();
            DataCacheCube cube = factory.createCube(lpParm);

            ArrayList<GribRecord> gribRecList = cube.getGribRecordStack();

            GribRecord rec = gribRecList.get(0);
            Date d3 = new Date();
            fa = dman.getData(rec);
            Date d4 = new Date();
            System.out.println(fa.length + " elements in the array");
            System.out.println(d4.getTime() - d3.getTime()
                    + " millis for the query 2");
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public void getLayerData() {
        DataCacheManager dman = DataCacheManager.getInstance();
        HashMap<String, RequestConstraint> query = new HashMap<String, RequestConstraint>();
        query.put("pluginName", new RequestConstraint("grib"));

        // Model parameter
        query
                .put("modelInfo.parameterAbbreviation", new RequestConstraint(
                        "T"));

        // model name
        query.put("modelInfo.modelName", new RequestConstraint("NAM218"));

        // level type
        query
                .put("modelInfo.level.levelonevalue", new RequestConstraint(
                        "500"));

        query.put("dataTime.refTime", new RequestConstraint(
                "2009-11-17 06:00:00"));

        query.put("dataTime.fcstTime", new RequestConstraint("0"));
        // query.put("dataTime.fcstTime", new RequestConstraint("10800"));
        // query.put("dataTime.fcstTime", new RequestConstraint("21600"));

        LayerProperty lpParm = new LayerProperty();
        lpParm.setNumberOfImages(9999);

        try {
            lpParm.setEntryQueryParameters(query, false);
            float[] layer = dman.getDataLayer(lpParm);
            System.out.println(layer.length + " elements in the layer array");
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

}
