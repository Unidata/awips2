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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.FloodCategoryData;
import com.raytheon.viz.hydrocommon.util.DbUtils;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;

/**
 * Class for managing database query calls. FloodCategoryDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2008 1661       askripsky   Initial Creation
 * March 3, 2011           JingtaoD    Modification - display blank if stored as null in database, be able to save null into
 *                                     database if displayed as blank in the Flood Catergroy GUI.
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class FloodCategoryDataManager extends HydroDataManager {
    protected static FloodCategoryDataManager manager = null;

    private static final String INSERT_STATEMENT = "INSERT INTO floodcat (lid, major_stage, moderate_stage, minor_stage, major_flow, moderate_flow, minor_flow) VALUES ('%s', %s, %s, %s, %s, %s, %s)";

    private static final String SELECT_STATEMENT = "SELECT lid, major_stage, moderate_stage, minor_stage, major_flow, moderate_flow, minor_flow FROM floodcat";

    private static final String DELETE_STATEMENT = "DELETE from floodcat WHERE %s";

    private static final String UPDATE_STATEMENT = "UPDATE floodcat SET major_stage=%s, moderate_stage=%s, minor_stage=%s, major_flow=%s, moderate_flow=%s, minor_flow=%s WHERE %s";

    /**
     * Private constructor.
     */
    protected FloodCategoryDataManager() {
        super();
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized FloodCategoryDataManager getInstance() {
        if (manager == null) {
            manager = new FloodCategoryDataManager();
        }

        return (FloodCategoryDataManager) manager;
    }

    /**
     * Deletes each record passed in from floodcat table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecords(ArrayList<FloodCategoryData> recordsToDelete)
            throws VizException {
        for (FloodCategoryData currData : recordsToDelete) {
            deleteRecord(currData);
        }
    }

    /**
     * Deletes each record passed in from floodcat table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecord(FloodCategoryData recordToDelete)
            throws VizException {
        runStatement(String.format(DELETE_STATEMENT,
                HydroDataUtils.getPKStatement(recordToDelete)));
    }

    /**
     * Deletes each record passed in from floodcat table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecord(String lid) throws VizException {
        runStatement(String.format(DELETE_STATEMENT, "lid='" + lid + "'"));
    }

    /**
     * Checks to see if the record exists by checking by PK, should only return
     * 0 or 1 records.
     * 
     * @param currData
     * @return
     * @throws VizException
     * @throws VizException
     */
    public FloodCategoryData getFloodCategoryData(String lid)
            throws VizException {
        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE lid='"
                + lid + "'");

        return (result.getResultCount() > 0) ? new FloodCategoryData(
                result.getRows()[0], result.getColumnNames())
                : new FloodCategoryData();
    }

    /**
     * Checks to see if the record already exists
     * 
     * @param lid
     * @return
     * @throws VizException
     */
    public int checkFloodCategoryData(String lid) throws VizException {
        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE lid='"
                + lid + "'");

        return result.getResultCount();
    }

    private void updateFloodCategoryData(FloodCategoryData data)
            throws VizException {
        Double majorS = null, minorS = null, moderateS = null, majorD = null, minorD = null, moderateD = null;

        DbUtils.escapeSpecialCharforData(data);

        if (data.getMajorStage() != HydroConstants.MISSING_VALUE)
            majorS = data.getMajorStage();
        if (data.getModerateStage() != HydroConstants.MISSING_VALUE)
            moderateS = data.getModerateStage();
        if (data.getMinorStage() != HydroConstants.MISSING_VALUE)
            minorS = data.getMinorStage();
        if (data.getMajorDischarge() != HydroConstants.MISSING_VALUE)
            majorD = data.getMajorDischarge();
        if (data.getModerateDischarge() != HydroConstants.MISSING_VALUE)
            moderateD = data.getModerateDischarge();
        if (data.getMinorDischarge() != HydroConstants.MISSING_VALUE)
            minorD = data.getMinorDischarge();

        /*
         * check if any of the stages or flows is missing, then assign null into
         * database;
         */
        runStatement(String.format(UPDATE_STATEMENT, majorS, moderateS, minorS,
                majorD, moderateD, minorD, HydroDataUtils.getPKStatement(data)));
    }

    private void insertFloodData(FloodCategoryData currData)
            throws VizException {

        Double majorS = null, minorS = null, moderateS = null, majorD = null, minorD = null, moderateD = null;

        DbUtils.escapeSpecialCharforData(currData);

        if (currData.getMajorStage() != HydroConstants.MISSING_VALUE)
            majorS = currData.getMajorStage();
        if (currData.getModerateStage() != HydroConstants.MISSING_VALUE)
            moderateS = currData.getModerateStage();
        if (currData.getMinorStage() != HydroConstants.MISSING_VALUE)
            minorS = currData.getMinorStage();
        if (currData.getMajorDischarge() != HydroConstants.MISSING_VALUE)
            majorD = currData.getMajorDischarge();
        if (currData.getModerateDischarge() != HydroConstants.MISSING_VALUE)
            moderateD = currData.getModerateDischarge();
        if (currData.getMinorDischarge() != HydroConstants.MISSING_VALUE)
            minorD = currData.getMinorDischarge();

        /* if any of the stage or flows is missing, the assign null to database */
        runStatement(String.format(INSERT_STATEMENT, currData.getLid(), majorS,
                moderateS, minorS, majorD, moderateD, minorD));
    }

    public boolean putFloodCategoryData(String lid, String majorStage,
            String modStage, String minorStage, String majorDischarge,
            String modDischarge, String minorDischarge, Shell shell)
            throws VizException {
        boolean rval = false;
        String blankStr = "";

        FloodCategoryData newData = new FloodCategoryData();

        newData.setLid(lid);

        try {
            if (majorDischarge.equals(blankStr))
                newData.setMajorDischarge((double) HydroConstants.MISSING_VALUE);
            else
                newData.setMajorDischarge(Double.parseDouble(majorDischarge));

            if (modDischarge.equals(blankStr))
                newData.setModerateDischarge((double) HydroConstants.MISSING_VALUE);
            else
                newData.setModerateDischarge(Double.parseDouble(modDischarge));

            if (minorDischarge.equals(blankStr))
                newData.setMinorDischarge((double) HydroConstants.MISSING_VALUE);
            else
                newData.setMinorDischarge(Double.parseDouble(minorDischarge));

            if (majorStage.equals(blankStr))
                newData.setMajorStage((double) HydroConstants.MISSING_VALUE);
            else
                newData.setMajorStage(Double.parseDouble(majorStage));

            if (modStage.equals(blankStr))
                newData.setModerateStage((double) HydroConstants.MISSING_VALUE);
            else
                newData.setModerateStage(Double.parseDouble(modStage));

            if (minorStage.equals(blankStr))
                newData.setMinorStage((double) HydroConstants.MISSING_VALUE);
            else
                newData.setMinorStage(Double.parseDouble(minorStage));
        } catch (Exception e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Value");
            mb.setMessage("Please enter a valid value.");
            mb.open();

            rval = false;

            return rval;
        }
        // Check if it's going to be an update or insert
        if (checkFloodCategoryData(lid) > 0) {
            // Do an update
            updateFloodCategoryData(newData);
        } else {
            // Do an insert
            insertFloodData(newData);
        }
        rval = true;

        return rval;
    }
}
