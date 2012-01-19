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

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Generates the 3-D data cubes for the 3-D cache.  
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

public class DataCubeFactory {
    /**
     * The cube to build.
     */
    private DataCacheCube cube = new DataCacheCube();

    /**
     * List of GribRecord objects for the cube.
     */
    private ArrayList<GribRecord> recList = new ArrayList<GribRecord>();
    
    /**
     * List of FloatDataRecord objects for the cube
     */
    private ArrayList<FloatDataRecord> fdrList = new ArrayList<FloatDataRecord>();

    /**
     * Constructor.
     */
    public DataCubeFactory() {
    }

    /**
     * Create a DataCacheCube from a LayerProperty object.
     * 
     * @param lProp
     *      The LayerProperty object
     * @return
     *      The DataCacheCube
     * @throws VizException
     */
    public DataCacheCube createCube(LayerProperty lProp) throws VizException {
        String tableScript = null;

        tableScript = ScriptCreator.createScript(lProp);
        List<Object> rs = Loader.loadData(tableScript, 10000);
        for (int i = 0; i < rs.size(); i++) {
            if (rs.get(i) instanceof GribRecord) {
                GribRecord record = (GribRecord) rs.get(i);
                if (i == 0) {
                    // Set the metadata the first time through the loop
                    cube.getMetadata().setModelName(record.getModelInfo().getModelName());
                    cube.getMetadata().setLevelType(record.getModelInfo().getLevel()
                            .getMasterLevel().getName());
                    cube.getMetadata().setParameter(record.getModelInfo()
                            .getParameterAbbreviation());
                    Calendar cal = Calendar.getInstance(TimeZone
                            .getTimeZone("GMT"));
                    // Set the creation time to the current time
                    cube.setCreationTime(cal.getTimeInMillis());
                    
                    // Set the reference time
                    cal.setTimeInMillis(record.getDataTime().getRefTime()
                            .getTime());
                    cube.getMetadata().setRefTime(cal.getTimeInMillis());
                    cube.getMetadata().setFcstHr(record.getDataTime().getFcstTime());
                    cube.getMetadata().setNx(record.getModelInfo().getLocation().getNx());
                    cube.getMetadata().setNy(record.getModelInfo().getLocation().getNy());
                }

                if (record.getModelInfo().getLevelTwoValue() == null) {
                    recList.add(record);
                }
            }
        }
        
        // Sort the stack by level
        java.util.Collections.sort(recList, new LevelComparator());
        
        cube.setGribRecordStack(recList);
        
        createStack();
        
        return cube;
    }

    /**
     * Create a stack of grids.
     * 
     * @param record
     *            A GribRecord
     */
    private void createStack() {
        for (GribRecord record : recList) {
            try {
                File loc = HDF5Util.findHDF5Location(record);
                IDataStore dataStore = DataStoreFactory.getDataStore(loc);
                IDataRecord[] dataRec;
                dataRec = dataStore.retrieve(record.getDataURI());
                if (dataRec.length > 0) {
                    if (dataRec[0] instanceof FloatDataRecord) {
                        fdrList.add((FloatDataRecord) dataRec[0]);
                    } else {
                        // Log message
                        System.err.println("Grib data not of type float: "
                                + record.getModelInfo().getModelName() + ": "
                                + record.getModelInfo().getParameterName());
                    }
                }
            } catch (FileNotFoundException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (StorageException e) {
                // TODO Auto-generated catch block
                // e.printStackTrace();
                System.err.println("Storage Error: " + e.getMessage());
                System.err.println("    Caused by: " + e.getCause());
            }
        }

        cube.setFdrStackData(fdrList);
    }
}
