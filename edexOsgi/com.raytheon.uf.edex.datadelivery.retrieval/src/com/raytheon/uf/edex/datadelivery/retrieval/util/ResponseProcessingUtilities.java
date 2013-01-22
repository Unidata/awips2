package com.raytheon.uf.edex.datadelivery.retrieval.util;

import java.text.ParseException;
import java.util.ArrayList;

import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;

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

/**
 * Response processing related Utilities
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2011            dhladky     Initial creation
 * Aug 20, 2012 0743       djohnson    Fix cache lookup to use the model name and not hashcode.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ResponseProcessingUtilities {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ResponseProcessingUtilities.class);

    public static GridRecord getGridRecord(String name, Parameter parm,
            Level level, String ensembleId,
            GridCoverage gridCoverage) {

        com.raytheon.uf.common.parameter.Parameter parameter = new com.raytheon.uf.common.parameter.Parameter();
        parameter.setAbbreviation(parm.getName());
        parameter.setName(parm.getDefinition());
        parameter.setUnitString(parm.getUnits());

        GridRecord record = new GridRecord();
        record.setPluginName("grid");
        record.setLocation(gridCoverage);
        record.setLevel(level);
        record.setParameter(parameter);
        record.setDatasetId(name);
        record.setEnsembleId(ensembleId);
        return record;
    }

    public static GridCoverage getCoverageFromCache(GridCoverage coverage) {
        return GridCoverageLookup.getInstance().getCoverage(coverage, true);
    }

    /**
     * get the number of times in a request
     * 
     * @param time
     * @return
     */
    public static int getOpenDAPGridNumTimes(Time time) {

        int start = time.getRequestStartTimeAsInt();
        int end = time.getRequestEndTimeAsInt();

        return (end - start) + 1;
    }

    /**
     * get number of levels in a request
     * 
     * @param parm
     * @return
     */
    public static int getOpenDAPGridNumLevels(Parameter parm) {
        if (parm.getLevels().getSelectedLevelIndices() == null) {
            return 1;
        }
        int numLevs = parm.getLevels().getSelectedLevelIndices().size();
        if (numLevs == 0) {
            numLevs = 1;
        }
        return numLevs;
    }

    /**
     * Gather the prospective data times for a request
     * 
     * @param time
     * @return
     */
    public static ArrayList<DataTime> getOpenDAPGridDataTimes(Time time) {

        ArrayList<DataTime> dt = new ArrayList<DataTime>();

        int numTimes = getOpenDAPGridNumTimes(time);
        int reqStartInt = time.getRequestStartTimeAsInt();

        for (int i = 0; i < numTimes; i++) {

            int increment = time.findForecastStepUnit() * reqStartInt;
            DataTime dataTime = null;
            try {
                dataTime = new DataTime(time.getStartDate(), increment);
                dt.add(dataTime);
                reqStartInt++;
            } catch (ParseException e) {
                statusHandler.error("Date failed to parse! " + e.getMessage());
            }
        }

        return dt;
    }

    /**
     * determine levels for this parameter
     * 
     * @param parm
     * @return
     */
    public static ArrayList<Level> getOpenDAPGridLevels(Levels plevels) {

        ArrayList<Level> levels = new ArrayList<Level>();

        try {
            int startLevel = plevels.getRequestLevelStart();
            int endLevel = plevels.getRequestLevelEnd();

            for (int index = startLevel; index <= endLevel; index++) {
                double levelOneValue = plevels.getLevelAt(index);
                String masterLevelName = LevelType
                        .getLevelTypeIdName(plevels.getLevelType());
                MasterLevel masterLevel = LevelFactory.getInstance()
                        .getMasterLevel(masterLevelName);
                Level level = LevelFactory.getInstance().getLevel(
                        masterLevelName, levelOneValue);
                level.setMasterLevel(masterLevel);
                levels.add(level);
            }
        } catch (Exception e) {
            statusHandler.error("Couldn't retrieve the levels : "
                    + plevels.getLevelType() + " ERROR: " + e);
        }

        return levels;

    }

}
