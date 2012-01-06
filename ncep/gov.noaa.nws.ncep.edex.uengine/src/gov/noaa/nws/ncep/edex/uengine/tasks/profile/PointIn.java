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

package gov.noaa.nws.ncep.edex.uengine.tasks.profile;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import gov.noaa.nws.ncep.edex.plugin.ncgrib.dao.NcgribDao;

import java.awt.Point;

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;

/**
 * PointIn task derived from original uEngine PointIn task. Reads a file in from
 * the data store.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 29, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class PointIn extends ScriptTask {

    private PluginDataObject dataRecord;

    private PluginDao dao;

    private int indX;
    private int indY;

    /**
     * Constructor
     * 
     * @param aPlugin
     *            the plugin
     * @param aDataRecord
     *            the data record to read in
     */
    public PointIn(String aPlugin, PluginDataObject aDataRecord, int xInd, int yInd) {
        dataRecord = aDataRecord;
        indX = xInd;
        indY = yInd;
        try {
            dao = PluginFactory.getInstance().getPluginDao(aPlugin);
//                    dataRecord.getPluginName());
        } catch (PluginException e) {
            System.out.println("Unable to get " + dataRecord.getPluginName()
                            + " dao");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() throws PluginException {
        IDataRecord record = getHDF5DataPoint(dataRecord, indX, indY );
        FloatDataRecord fdr = (FloatDataRecord)record;
        return fdr.getFloatData()[0];
    }
    
    public float getPointData() throws PluginException {
    	return ((FloatDataRecord)getHDF5DataPoint(dataRecord, indX, indY )).getFloatData()[0];
    }
    

    public Object[] retrieveGroup() throws PluginException {
        return dao.getHDF5Data(dataRecord, -1);
    }
    
    public IDataRecord getHDF5DataPoint(PluginDataObject object,
            int xInd, int yInd) throws PluginException {

        Request pointRequest = Request.buildPointRequest(new Point(xInd, yInd) );
        IDataRecord[] record = null;
        record = new IDataRecord[1];

        if (object instanceof IPersistable) {
            /* connect to the data store and retrieve the data */
        	//chin remove this line NcgribDao dao = new NcgribDao();
        	IDataStore dataStore = dao.getDataStore((IPersistable) object);
            try {
                record[0] = dataStore.retrieve(object.getDataURI(),
                        "Data", pointRequest);

            } catch (Exception e) {
                throw new PluginException("Error getting HDF5 data", e);
            }
        }
        return record[0];
    }

}