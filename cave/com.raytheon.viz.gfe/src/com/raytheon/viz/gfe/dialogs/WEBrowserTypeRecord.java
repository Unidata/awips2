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
package com.raytheon.viz.gfe.dialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.core.IParmManager;

/**
 * Class for tracking sources, fields, pressures, misc maps for a specific
 * selected type.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/10/2008              Eric Babin  Initial Creation
 * 04/30/2009   2282       rjpeter     Refactored interfaces.
 * 08/19/2009   2547       rjpeter     Implement Test/Prac database display.
 * 02/22/2012	14351	   mli		   update with incoming databases
 * 09/12/2012   #1117      dgilling    Revert previous changes, force 
 *                                     source list to always rebuild to ensure
 *                                     up to date db list.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class WEBrowserTypeRecord {

    private String type;

    private List<String> sources = new ArrayList<String>();

    public static final SimpleDateFormat SOURCE_FORMAT = new SimpleDateFormat(
            "dd/HHmm");

    private Map<String, ParmID[]> fieldMap = new HashMap<String, ParmID[]>();

    /**
     * Maps a model run to a map of its parm names and possible pressure level
     * values.
     */
    private Map<String, Map<String, List<String>>> pressureMap = new HashMap<String, Map<String, List<String>>>();

    /**
     * Maps a model run to a map of its parm names and possible misc level
     * values (pressures that are not MBxxxx).
     */
    private Map<String, Map<String, List<String>>> miscMap = new HashMap<String, Map<String, List<String>>>();

    private ParmID[] possibleParms;

    private ParmID[] fields;

    private final CAVEMode mode;

    private final IParmManager parmMgr;

    static {
        SOURCE_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * @return the type
     */
    public WEBrowserTypeRecord(String type, CAVEMode mode, IParmManager parmMgr) {
        this.type = type;
        this.mode = mode;
        this.parmMgr = parmMgr;
        // create and fill these entries on creation of this type.
        makeSources();
        // go ahead and fill the ParmID[] array.
        getPossibleParmIDs();
    }

    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return the sources
     */
    public java.util.List<String> getSources() {
        makeSources();
        return sources;
    }

    /**
     * @return the fields
     */
    public ParmID[] getFields(String type) {
        if (fieldMap.containsKey(type)) {
            return fieldMap.get(type);
        }
        return fields;
    }

    private List<DatabaseID> getDbsForType(String typeLabel) {
        boolean pracFlag = false;
        boolean testFlag = false;

        if (typeLabel.equalsIgnoreCase("IFP")) {
            typeLabel = "";
            pracFlag = mode.equals(CAVEMode.PRACTICE);
            testFlag = mode.equals(CAVEMode.TEST);
        }
        List<DatabaseID> dbs = parmMgr.getAvailableDbs();

        List<DatabaseID> filtDB = new ArrayList<DatabaseID>();

        for (DatabaseID db : dbs) {
            if (db.getDbType().equalsIgnoreCase(typeLabel)) {
                filtDB.add(db);
            } else if (db.getDbType().equalsIgnoreCase("Prac") && pracFlag) {
                filtDB.add(db);
            } else if (db.getDbType().equalsIgnoreCase("Test") && testFlag) {
                filtDB.add(db);
            }
        }

        return filtDB;
    }

    private void makeSources() {
        // clear all existing data as we will end up rebuilding it here...
        sources.clear();
        fieldMap.clear();
        miscMap.clear();
        pressureMap.clear();

        List<DatabaseID> databases = getDbsForType(type);
        final DatabaseID mutableDb = parmMgr.getMutableDatabase();

        Collections.sort(databases, new Comparator<DatabaseID>() {
            @Override
            public int compare(DatabaseID left, DatabaseID right) {
                // sorting rules for WeatherElementBrowser:
                // mutable first
                // singletons next in alpha order
                // rest in order first by name, then time (newest to oldest)
                if (left.equals(right)) {
                    return 0;
                }

                if (left.equals(mutableDb)) {
                    return -1;
                } else if (right.equals(mutableDb)) {
                    return 1;
                }

                String leftModelTime = (left.getModelTime() != null ? left
                        .getModelTime() : "");
                String rightModelTime = (right.getModelTime() != null ? right
                        .getModelTime() : "");
                if (leftModelTime.equals(DatabaseID.NO_MODEL_TIME)
                        && (!rightModelTime.equals(DatabaseID.NO_MODEL_TIME))) {
                    return -1;
                } else if (!leftModelTime.equals(DatabaseID.NO_MODEL_TIME)
                        && (rightModelTime.equals(DatabaseID.NO_MODEL_TIME))) {
                    return 1;
                } else if (leftModelTime.equals(DatabaseID.NO_MODEL_TIME)
                        && (rightModelTime.equals(DatabaseID.NO_MODEL_TIME))) {
                    return left.getModelName().compareTo(right.getModelName());
                }

                int modelNameCompare = left.getModelName().compareTo(
                        right.getModelName());
                if (modelNameCompare != 0) {
                    return modelNameCompare;
                }
                return -1 * leftModelTime.compareTo(rightModelTime);
            }
        });

        for (DatabaseID dbId : databases) {
            String sourceString = dbId.getModelName();
            if (dbId.getModelDate() != null) {
                synchronized (SOURCE_FORMAT) {
                    sourceString += " "
                            + SOURCE_FORMAT.format(dbId.getModelDate());
                }
            }
            sources.add(sourceString);

            ParmID[] availParms = parmMgr.getAvailableParms(dbId);
            List<ParmID> sortedParms = new ArrayList<ParmID>(availParms.length);
            for (ParmID parm : availParms) {
                if (parm.getParmLevel().startsWith("MB")) {
                    addToMap(pressureMap, sourceString, parm.getParmName(),
                            parm.getParmLevel());
                } else {
                    addToMap(miscMap, sourceString, parm.getParmName(),
                            parm.getParmLevel());
                }

                sortedParms.add(parm);
            }

            Collections.sort(sortedParms);
            fieldMap.put(sourceString,
                    sortedParms.toArray(new ParmID[sortedParms.size()]));
        }
    }

    private static void addToMap(Map<String, List<String>> map, String name,
            String value) {
        if (map.containsKey(name)) {
            List<String> list = map.get(name);
            if (!list.contains(value)) {
                list.add(value);
            }
        } else {
            List<String> list = new ArrayList<String>();
            list.add(value);
            map.put(name, list);
        }
    }

    private static void addToMap(Map<String, Map<String, List<String>>> map,
            String model, String parmName, String value) {
        if (map.containsKey(model)) {
            if (map.get(model).containsKey(parmName)) {
                List<String> list = map.get(model).get(parmName);
                if (!list.contains(value)) {
                    list.add(value);
                }
            } else {
                List<String> list = new ArrayList<String>();
                list.add(value);
                map.get(model).put(parmName, list);
            }
        } else {
            Map<String, List<String>> internalMap = new HashMap<String, List<String>>();
            List<String> list = new ArrayList<String>();
            list.add(value);
            internalMap.put(parmName, list);
            map.put(model, internalMap);
        }
    }

    /**
     * @return the fieldMap
     */
    public Map<String, ParmID[]> getFieldMap() {
        return fieldMap;
    }

    /**
     * @param fieldMap
     *            the fieldMap to set
     */
    public void setFieldMap(HashMap<String, ParmID[]> fieldMap) {
        this.fieldMap = fieldMap;
    }

    /**
     * Given a list of model runs, returns a map of parm names and corresponding
     * pressure level values.
     * 
     * @param modelNames
     *            A list of model runs. Format for the strings should be model
     *            DD/HH (e.g., "RUC13 03/12" or "GFS40 30/00")
     * @return A single map containing all parms present in the model runs
     *         mapped to all possible pressure values.
     */
    public Map<String, List<String>> getPressureMap(String... modelNames) {
        Map<String, List<String>> filteredMap = new HashMap<String, List<String>>();

        for (String model : modelNames) {
            if (pressureMap.containsKey(model)) {
                Map<String, List<String>> modelPressureMap = pressureMap
                        .get(model);
                for (String parm : modelPressureMap.keySet()) {
                    for (String value : modelPressureMap.get(parm)) {
                        addToMap(filteredMap, parm, value);
                    }
                }
            }
        }

        return filteredMap;
    }

    /**
     * Given a list of model runs, returns a map of parm names and corresponding
     * "misc." pressure level values.
     * 
     * @param modelNames
     *            A list of model runs. Format for the strings should be model
     *            DD/HH (e.g., "RUC13 03/12" or "GFS40 30/00")
     * @return A single map containing all parms present in the model runs
     *         mapped to all possible "misc." pressure values.
     */
    public Map<String, List<String>> getMiscMap(String... modelNames) {
        Map<String, List<String>> filteredMap = new HashMap<String, List<String>>();

        for (String model : modelNames) {
            if (miscMap.containsKey(model)) {
                Map<String, List<String>> modelMiscMap = miscMap.get(model);
                for (String parm : modelMiscMap.keySet()) {
                    for (String value : modelMiscMap.get(parm)) {
                        addToMap(filteredMap, parm, value);
                    }
                }
            }
        }

        return filteredMap;
    }

    /**
     * Given a model run and parm name, returns an array of corresponding
     * pressure level values.
     * 
     * @param model
     *            The model run name and time, specified in "model DD/HH" format
     *            (e.g., "RUC13 03/12" or "GFS40 30/00").
     * @param parmName
     *            The parm name (e.g., "t" or "pop").
     * @return An array containing all valid pressure level values for the
     *         specified parm in the model run.
     */
    public String[] getPress(String model, String parmName) {
        if (pressureMap.containsKey(model)
                && pressureMap.get(model).containsKey(parmName)) {
            List<String> pressures = pressureMap.get(model).get(parmName);

            return pressures.toArray(new String[pressures.size()]);
        }

        return new String[0];
    }

    /**
     * Given a model run and parm name, returns an array of corresponding
     * "misc." pressure level values.
     * 
     * @param model
     *            The model run name and time, specified in "model DD/HH" format
     *            (e.g., "RUC13 03/12" or "GFS40 30/00").
     * @param parmName
     *            The parm name (e.g., "t" or "pop").
     * @return An array containing all valid "misc." pressure level values for
     *         the specified parm in the model run.
     */
    public String[] getMisc(String model, String parmName) {
        if (miscMap.containsKey(model)
                && miscMap.get(model).containsKey(parmName)) {
            List<String> miscValues = miscMap.get(model).get(parmName);

            return miscValues.toArray(new String[miscValues.size()]);
        }

        return new String[0];
    }

    /**
     * @param pressureMap
     *            the pressureMap to set
     */
    public void setPressureMap(
            Map<String, Map<String, List<String>>> pressureMap) {
        this.pressureMap = pressureMap;
    }

    /**
     * Get the list of possible parms for this specific type.
     * 
     * @return ParmID[] possibleParmIDs.
     */
    public ParmID[] getPossibleParmIDs() {
        if (possibleParms == null) {
            List<DatabaseID> databases = getDbsForType(type);
            ArrayList<ParmID> parmIds = new ArrayList<ParmID>();
            for (DatabaseID db : databases) {
                ParmID ids[] = parmMgr.getAvailableParms(db);
                for (int i = 0; i < ids.length; i++) {
                    parmIds.add(ids[i]);
                }
            }

            possibleParms = new ParmID[parmIds.size()];
            parmIds.toArray(possibleParms);
        }

        return possibleParms;
    }

    public ArrayList<ParmID> getFilteredParmIDs(String sources[],
            String fields[], String planes[]) {
        ArrayList<ParmID> listToReturn = new ArrayList<ParmID>();
        if ((sources.length != 0) && (fields.length != 0)
                && (planes.length != 0)) {
            java.util.List<String> sourceList = java.util.Arrays
                    .asList(sources);
            java.util.List<String> fieldList = java.util.Arrays.asList(fields);
            java.util.List<String> planesList = java.util.Arrays.asList(planes);

            ParmID potentialList[] = getPossibleParmIDs();

            for (ParmID potParm : potentialList) {
                DatabaseID potDatabase = potParm.getDbId();
                String s = potDatabase.getModelName();
                if (potDatabase.getModelDate() != null) {
                    synchronized (SOURCE_FORMAT) {
                        s += " "
                                + SOURCE_FORMAT.format(potDatabase
                                        .getModelDate());
                    }

                }
                if (sourceList.contains(s)
                        && fieldList.contains(potParm.getParmName())
                        && planesList.contains(potParm.getParmLevel())) {
                    listToReturn.add(potParm);
                }
            }

        }
        return listToReturn;
    }
}
