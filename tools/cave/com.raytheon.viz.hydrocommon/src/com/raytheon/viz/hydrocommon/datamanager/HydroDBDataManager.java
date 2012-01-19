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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.HydroDBData;

/**
 * Class for managing database query calls.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2008 1697       askripsky   Initial Creation
 * Nov 21, 2008 1697       askripsky   Changed to use reflection and filter generic methods
 * Nov 03, 2011 11273      lbousaidi   added updateNewData and putNewData.
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class HydroDBDataManager extends HydroDataManager {
    private static HydroDBDataManager manager = null;

    /**
     * Private constructor.
     */
    private HydroDBDataManager() {
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized HydroDBDataManager getInstance() {
        if (manager == null) {
            manager = new HydroDBDataManager();
        }

        return manager;
    }

    /**
     * Deletes each record passed in from the respective table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public <T extends HydroDBData> void deleteRecords(
            ArrayList<T> recordsToDelete) throws VizException {
        for (T currData : recordsToDelete) {
            deleteRecord(currData);
        }
    }

    /**
     * Deletes each record passed in from the table. Retrieves the delete SQL
     * from the getDeleteStatement method of the data object passed in.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public <T extends HydroDBData> void deleteRecord(T recordToDelete)
            throws VizException {
        try {
            String deleteQuery = (String) recordToDelete.getClass().getMethod(
                    "getDeleteStatement").invoke(recordToDelete);

            runStatement(deleteQuery);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Get records from DB.
     * 
     * @param clazz
     *            The data type to run the query for.
     * @return ArrayList containing the data type that is passed in containing
     *         data from the DB.
     * @throws VizException
     */
    public <T extends HydroDBData> ArrayList<T> getData(Class<T> clazz)
            throws VizException {
        ArrayList<T> rval = new ArrayList<T>();

        try {
            String selectStatement = (String) clazz.getMethod(
                    "getSelectStatement").invoke(clazz.newInstance());

            QueryResult result = runMappedQuery(selectStatement);

            // Find the constuctor using reflection to accept the DB results
            Constructor<T> dataConstructor = getDBConstructor(clazz);

            if (result.getResultCount() > 0) {
                for (QueryResultRow currRow : result.getRows()) {
                    rval.add(dataConstructor.newInstance(currRow, result
                            .getColumnNames()));
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return rval;
    }

    /**
     * Uses reflection to return the constructor that accepts the raw DB data.
     * 
     * @param <T>
     * @param clazz
     * @return
     * @throws SecurityException
     * @throws NoSuchMethodException
     */
    private <T extends HydroDBData> Constructor<T> getDBConstructor(
            Class<T> clazz) throws SecurityException, NoSuchMethodException {
        return clazz.getConstructor(QueryResultRow.class, Map.class);
    }

    /**
     * Get records from DB.
     * 
     * @param clazz
     *            The data type to run the query for.
     * @return ArrayList containing the data type that is passed in containing
     *         data from the DB.
     * @throws VizException
     */
    @SuppressWarnings("unchecked")
    public <T extends HydroDBData> ArrayList<T> getData(T data)
            throws VizException {
        ArrayList<T> rval = new ArrayList<T>();

        try {
            String selectQuery = (String) data.getClass().getMethod(
                    "getConstrainedSelectStatement").invoke(data);

            QueryResult result = runMappedQuery(selectQuery);

            // Find the constuctor using reflection to accept the DB results
            Constructor<T> dataConstructor = (Constructor<T>) getDBConstructor(data
                    .getClass());

            for (QueryResultRow currRow : result.getRows()) {
                rval.add(dataConstructor.newInstance(currRow, result
                        .getColumnNames()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return rval;
    }

    /**
     * Checks to see if the record already exists
     * 
     * @param lid
     * @return
     * @throws VizException
     */
    public <T extends HydroDBData> int checkData(T data) throws VizException {
        String dataQuery = null;
        int rval = 0;

        try {
            dataQuery = (String) data.getClass()
                    .getMethod("getExistsStatement").invoke(data);

            if (dataQuery != null) {
                rval = runMappedQuery(dataQuery).getResultCount();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return rval;
    }

    /**
     * Updates the respective DB table with the data passed in.
     * 
     * @param <T>
     * @param data
     * @throws VizException
     */
    public <T extends HydroDBData> void updateData(T data) throws VizException {
        try {
            // Get the update statement with the values filled in
            String updateQuery = (String) data.getClass().getMethod(
                    "getUpdateStatement").invoke(data);

            if (updateQuery != null) {
                runStatement(updateQuery);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Updates the respective DB table updateData with the newData passed in.
     * 
     * @param <T>
     * @param newData
     * @param updateData
     * @throws VizException
     * */
    public <T extends HydroDBData> void updateNewData(T newData, T updateData) throws VizException {
    	try {
    		
        	String updateQuery = (String) newData.getClass().getMethod(
        	"getUpdateStatement").invoke(newData);
        	        	
        	String pkquery= (String) updateData.getClass().getMethod(
        	"getPKStatement").invoke(updateData);
        	        	
        	String updateQueryToRun= updateQuery + "WHERE " + pkquery ;
        	
        	if (updateQueryToRun != null) {
        		runStatement(updateQueryToRun);
        	}
        } catch (Exception e) {
        	e.printStackTrace();
        }
       
    }
    /**
     * Inserts into the respective DB table with the data passed in.
     * 
     * @param <T>
     * @param currData
     * @throws VizException
     */
    private <T extends HydroDBData> void insertData(T currData)
            throws VizException {
        String insertQuery = null;

        try {
            // if (currData.getClass() == LocationAgencyOfficeData.class) {
            Method getSQLMethod = currData.getClass().getMethod(
                    "getInsertStatement");

            insertQuery = (String) getSQLMethod.invoke(currData);
            // }

            if (insertQuery != null) {
                runStatement(insertQuery);
            }
        } catch (SecurityException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (NoSuchMethodException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IllegalArgumentException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Checks to see if the data passed in already exists in the DB based on the
     * primary key for the data's respective table. If the data exists, an
     * UPDATE is performed. If not, an INSERT is performed.
     * 
     * @param <T>
     * @param lid
     * @param newData
     * @throws VizException
     */
    public <T extends HydroDBData> void putData(T newData) throws VizException {
        // Check if it's going to be an update or insert
        if (checkData(newData) > 0) {
            // Do an update
            updateData(newData);
        } else {
            // Do an insert
            insertData(newData);
        }
    }
    
    /**
     * Checks to see if the newData passed in already exists in the DB based on the
     * primary key for the data's respective table. If the data exists, an
     * UPDATE is performed by replacing the updateData with NewData. 
     * If not, an INSERT of newData is performed.
     * 
     * @param <T>
     * @param lid
     * @param newData
     * @throws VizException
     */
    public <T extends HydroDBData> void putNewData(T newData, T updateData, boolean insert) throws VizException {
        // Check if it's going to be an update 
    	
    	if ((insert) && (checkData(newData)==0) ) {
    			// Do an insert    	
    			insertData(newData);
    			 
    	} else if (checkData(updateData) > 0) {
    			// Do an update
            	updateNewData(newData, updateData);
        }
    }
    
}
