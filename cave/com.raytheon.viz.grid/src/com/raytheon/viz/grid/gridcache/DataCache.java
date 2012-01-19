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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * This is the 3 dimensional data cube cache for D2D.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2009 3579       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataCache {
    /** The only instance of this cache. */
    private static DataCache instance = null;

    private static final int SECONDS_PER_MINUTE = 60;

    private static final int MILLIS_PER_SECOND = 1000;

    private static SimpleDateFormat sdf = null;

    static {
        sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /** Date Format yyyy-MM-dd HH:mm:ss */
    public static final SimpleDateFormat DATE_FORMAT = sdf;

    /**
     * List of Model Nodes.
     */
    private List<ModelNode> modelNodeList = new ArrayList<ModelNode>();

    /**
     * How long (in millis) to keep the data cubes around.
     */
    private int retentionTime = 10 * SECONDS_PER_MINUTE * MILLIS_PER_SECOND;

    /**
     * The timer delay, how long to wait to run.
     */
    private int delay = 10 * SECONDS_PER_MINUTE * MILLIS_PER_SECOND;

    /**
     * How often to repeat.
     */
    private int period = 10 * SECONDS_PER_MINUTE * MILLIS_PER_SECOND;

    /**
     * Cache Purge Timer.
     */
    private Timer timer = new Timer();

    /**
     * Private, invisible constructor.
     */
    private DataCache() {
        // Set the purge timer to run
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                purge();
            }
        }, delay, period);

    }

    /**
     * Get an instance of the DataCache.
     * 
     * This class should not be used directly. Use the DataCacheManager to
     * interface with this class.
     * 
     * @return An instance of this DataCache
     */
    public static synchronized DataCache getInstance() {
        if (instance == null) {
            instance = new DataCache();
        }

        return instance;
    }

    /**
     * Add a cube to the cache.
     * 
     * @param cube
     *            The DataCacheCube to add
     * @param replace
     *            true = replace an existing cube, false = do not replace
     */
    public synchronized void addData(DataCacheCube cube, boolean replace) {
        boolean modelMatch = false;
        boolean parameterMatch = false;
        boolean levelMatch = false;
        boolean refTimeMatch = false;
        for (ModelNode mnode : modelNodeList) {
            if ((mnode.getModelName() != null) && mnode.getModelName().equals(cube.getMetadata().getModelName())) {
                modelMatch = true;

                /* Check if this parameter is in the list */
                if (mnode.getParameterNodeList().size() > 0) {
                    for (ParameterNode pnode : mnode.getParameterNodeList()) {
                        if (pnode.getParameter().equalsIgnoreCase(
                                cube.getMetadata().getParameter())) {
                            parameterMatch = true;

                            /* Check if this level type is in the list */
                            if (pnode.getLevelTypeList().size() > 0) {
                                for (LevelNode lnode : pnode.getLevelTypeList()) {
                                    if (lnode.getLevelType().equals(
                                            cube.getMetadata().getLevelType())) {
                                        levelMatch = true;

                                        /*
                                         * Check if the reference time is in the
                                         * list
                                         */
                                        if (lnode.getReferenceTimeList().size() > 0) {
                                            for (TimeNode tnode : lnode
                                                    .getReferenceTimeList()) {
                                                if (tnode.getReferenceTime() == cube
                                                        .getMetadata()
                                                        .getRefTime()) {
                                                    refTimeMatch = true;
                                                    tnode.addDataCube(cube);
                                                }
                                            }
                                            if (refTimeMatch == false) {
                                                // Found a new reference time
                                                addNewTimeNode(cube, lnode);
                                            }
                                        } else {
                                            // Found a new reference time
                                            addNewTimeNode(cube, lnode);
                                        }
                                    }
                                }
                                if (levelMatch == false) {
                                    // Found a new level type
                                    addNewLevelTypeNode(cube, pnode);
                                }
                            } else {
                                // Found a new level type
                                addNewLevelTypeNode(cube, pnode);
                            }
                        }
                    }
                    if (parameterMatch == false) {
                        // Found a new parameter
                        addNewParameterNode(cube, mnode);
                    }
                } else {
                    // Found a new parameter
                    addNewParameterNode(cube, mnode);
                }
            }
        }
        if (modelMatch == false) {
            // Found a new model
            addNewModelNode(cube);
        }
    }

    /**
     * Add a new Model Node.
     * 
     * @param cube
     *            The cube of data
     */
    private void addNewModelNode(DataCacheCube cube) {
        ModelNode mnode = new ModelNode();
        mnode.setModelName(cube.getMetadata().getModelName());

        addNewParameterNode(cube, mnode);

        modelNodeList.add(mnode);
    }

    /**
     * Add a new parameter node.
     * 
     * @param cube
     *            The cube of data
     * @param mnode
     *            The model node that holds this node
     */
    private void addNewParameterNode(DataCacheCube cube, ModelNode mnode) {
        ParameterNode pnode = new ParameterNode();
        pnode.setModelName(cube.getMetadata().getModelName());
        pnode.setParameter(cube.getMetadata().getParameter());

        mnode.addParameterNode(pnode);

        /* Add new level type node */
        addNewLevelTypeNode(cube, pnode);
    }

    /**
     * Add a level type node.
     * 
     * @param cube
     *            The cube of data
     * @param pnode
     *            The parameter node that holds this node
     */
    private void addNewLevelTypeNode(DataCacheCube cube, ParameterNode pnode) {
        LevelNode lnode = new LevelNode();
        lnode.setModelName(cube.getMetadata().getModelName());
        lnode.setLevelType(cube.getMetadata().getLevelType());

        pnode.addLevelNode(lnode);

        /* Add new reference time node */
        addNewTimeNode(cube, lnode);
    }

    /**
     * Add a Time Node.
     * 
     * @param cube
     *            The cube of data
     * @param lnode
     *            The level node that holds this node
     */
    private void addNewTimeNode(DataCacheCube cube, LevelNode lnode) {
        TimeNode tnode = new TimeNode();
        tnode.setModelName(cube.getMetadata().getModelName());
        tnode.setReferenceTime(cube.getMetadata().getRefTime());

        tnode.addDataCube(cube);
        lnode.addTimeNode(tnode);
    }

    /**
     * If the cache has the cube return it, otherwise build it, put it in the
     * cache, then return it.
     * 
     * @param lProp
     *            The layer properties
     * @return The data cube
     * @throws VizException
     */
    public DataCacheCube getCube(LayerProperty lProp) throws VizException {
        DataCubeMetadata metadata = new DataCubeMetadata();
        DataCacheCube cube = null;
        String tableScript = ScriptCreator.createScript(lProp);
        List<Object> rs = Loader.loadData(tableScript, 10000);

        if (rs.size() > 0) {

            /* Build the metadata */
            if (rs.get(0) instanceof GribRecord) {
                GribRecord record = (GribRecord) rs.get(0);
                metadata.setModelName(record.getModelInfo().getModelName());
                metadata.setLevelType(record.getModelInfo().getLevel()
                        .getMasterLevel().getName());
                metadata.setParameter(record.getModelInfo()
                        .getParameterAbbreviation());
                Calendar cal = Calendar
                        .getInstance(TimeZone.getTimeZone("GMT"));
                cal
                        .setTimeInMillis(record.getDataTime().getRefTime()
                                .getTime());
                metadata.setRefTime(cal.getTimeInMillis());
                metadata.setFcstHr(record.getDataTime().getFcstTime());
              
                // Look for the cube
                for (ModelNode mnode : modelNodeList) {
                    for (ParameterNode pnode : mnode.getParameterNodeList()) {
                        for (LevelNode lnode : pnode.getLevelTypeList()) {
                            for (TimeNode tnode : lnode.getReferenceTimeList()) {
                                for (int fcstTime : tnode.getFcstTimes()) {
                                    if (tnode.getCube(fcstTime).getMetadata()
                                            .equals(metadata)) {
                                        return tnode.getCube(fcstTime);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Cube not in the cache, create one
        DataCubeFactory factory = new DataCubeFactory();
        cube = factory.createCube(lProp);
        addData(cube, true);

        return cube;
    }

    /**
     * Get a DataCacheCube from the cache. Check the GribRecord for layer data.
     * If layerOneValue and layerTwoValue are both null, request the entire
     * cube, otherwise just get the data for the layer.
     * 
     * @param rec
     *      The GribRecord
     * @return
     *      The DataCacheCube
     */
    public DataCacheCube getCube(GribRecord record) {
        String levelType = record.getModelInfo().getLevel().getMasterLevel()
            .getName();

        DataCacheCube cube = null;
        DataCubeMetadata metadata = new DataCubeMetadata();

        metadata.setModelName(record.getModelInfo().getModelName());
        metadata.setLevelType(record.getModelInfo().getLevel()
                .getMasterLevel().getName());
        metadata.setParameter(record.getModelInfo().getParameterAbbreviation());
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis(record.getDataTime().getRefTime().getTime());
        metadata.setRefTime(cal.getTimeInMillis());
        metadata.setFcstHr(record.getDataTime().getFcstTime());

        // Look for the cube
        for (ModelNode mnode : modelNodeList) {
            for (ParameterNode pnode : mnode.getParameterNodeList()) {
                for (LevelNode lnode : pnode.getLevelTypeList()) {
                    for (TimeNode tnode : lnode.getReferenceTimeList()) {
                        for (int fcstTime : tnode.getFcstTimes()) {
                            if (tnode.getCube(fcstTime).getMetadata()
                                    .equals(metadata)) {
                                return tnode.getCube(fcstTime);
                            }
                        }
                    }
                }
            }
        }
        
        // Build the query.
        DataCubeFactory factory = new DataCubeFactory();
        HashMap<String, RequestConstraint> query = new HashMap<String, RequestConstraint>();
        query.put("pluginName", new RequestConstraint("grib"));

        // Model parameter
        query.put("modelInfo.parameterAbbreviation", new RequestConstraint(record
                .getModelInfo().getParameterAbbreviation()));

        // Level type
        query.put("modelInfo.level.masterLevel.name",
                new RequestConstraint(levelType));

        // model name
        query.put("modelInfo.modelName", new RequestConstraint(record
                .getModelInfo().getModelName()));

        query.put("dataTime.refTime", new RequestConstraint(DATE_FORMAT
                .format(record.getDataTime().getRefTime())));

        query.put("dataTime.fcstTime", new RequestConstraint(String.valueOf(record
                .getDataTime().getFcstTime())));
        
        // No data cube available in cache, Check for layer data
        if ((record.getModelInfo().getLevelOneValue() == null)
                && (record.getModelInfo().getLevelTwoValue() == null)) {

            LayerProperty lp = new LayerProperty();

            // The number of records to return
            lp.setNumberOfImages(9999);

            try {
                lp.setEntryQueryParameters(query, false);
                cube = factory.createCube(lp);
                DataCache.getInstance().addData(cube, true);
                
                return cube;
            } catch (VizException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        
        return cube;
    }

    /**
     * Clear the cache.
     */
    public void clearCache() {
        modelNodeList.clear();
    }

    private void purge() {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

        // Find and remove the cubes and nodes
        for (int i = 0; i < modelNodeList.size(); i++) {
            ModelNode mnode = modelNodeList.get(i);
            if (mnode.getModelName() == null) {
                modelNodeList.remove(i);
            }
            for (int j = 0; j < mnode.getParameterNodeList().size(); j++) {
                ParameterNode pnode = mnode.getParameterNodeList().get(j);

                for (int k = 0; k < pnode.getLevelTypeList().size(); k++) {
                    LevelNode lnode = pnode.getLevelTypeList().get(k);

                    for (int ii = 0; ii < lnode.getReferenceTimeList().size(); ii++) {
                        TimeNode tnode = lnode.getReferenceTimeList().get(ii);

                        for (int fcstTime : tnode.getFcstTimes()) {
                            DataCacheCube cube = tnode.getCube(fcstTime);
                            long creationTime = cube.getCreationTime();
                            if (cal.getTimeInMillis() - creationTime > retentionTime) {
                                // Delete the cube.
                                if (tnode.getFcstTimes().length > 0) {
                                    tnode.removeCube(cube.getMetadata()
                                            .getFcstHr());
                                }
                            }
                        }
                        if (tnode.getFcstTimes().length == 0) {
                            lnode.getReferenceTimeList().remove(ii);
                        }
                    }
                    if (lnode.getReferenceTimeList().size() == 0) {
                        pnode.getLevelTypeList().remove(k);
                    }
                }
                if (pnode.getLevelTypeList().size() == 0) {
                    mnode.getParameterNodeList().remove(j);
                }
            }
            if (mnode.getParameterNodeList().size() == 0) {
                modelNodeList.remove(i);
            }
        }
    }

    /**
     * @return the retentionTime in milliseconds
     */
    public int getRetentionTime() {
        return retentionTime;
    }

    /**
     * @param retentionTime
     *            the retentionTime to set in milliseconds
     */
    public void setRetentionTime(int retentionTime) {
        this.retentionTime = retentionTime;
    }

    @Override
    public String toString() {
        StringBuilder buf = new StringBuilder();

        for (ModelNode mnode : modelNodeList) {
            buf.append("******************\n");
            buf.append("* " + mnode.getModelName() + "\n");

            List<ParameterNode> parameterNodeList = mnode
                    .getParameterNodeList();
            for (ParameterNode pnode : parameterNodeList) {
                buf.append("*-------------------\n");
                buf.append("* " + pnode.getParameter() + "\n");

                List<LevelNode> levelList = pnode.getLevelTypeList();
                for (LevelNode lnode : levelList) {
                    buf.append("* " + lnode.getLevelType() + "\n");

                    List<TimeNode> timeList = lnode.getReferenceTimeList();
                    for (TimeNode tnode : timeList) {
                        Calendar c = Calendar.getInstance(TimeZone
                                .getTimeZone("GMT"));
                        c.setTimeInMillis(tnode.getReferenceTime());
                        buf.append("* " + (c.get(Calendar.MONTH) + 1) + "/"
                                + c.get(Calendar.DAY_OF_MONTH) + " "
                                + c.get(Calendar.HOUR_OF_DAY) + "Z\n");

                        int[] fcstTimes = tnode.getFcstTimes();

                        buf.append("* " + fcstTimes.length + " datacubes\n");
                        buf.append("* Fcst Hrs: ");
                        for (int i = 0; i < fcstTimes.length; i++) {
                            if (i > 0) {
                                buf.append(", ");
                            }
                            buf.append(fcstTimes[i] / 60 / 60);
                        }
                        buf.append("\n");
                    }
                }
            }
            buf.append("******************\n");
        }

        return buf.toString();
    }
}
