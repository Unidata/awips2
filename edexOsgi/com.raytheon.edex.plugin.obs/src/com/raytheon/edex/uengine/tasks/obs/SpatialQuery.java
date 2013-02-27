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

package com.raytheon.edex.uengine.tasks.obs;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * SpatialQuery task derived from original uEngine SpatialQuery task.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 11, 2007                     njensen             Initial Creation
 * Feb 27, 2013     1638            mschenke            Moved ObStationDao to edex pointdata plugin
 *
 * </PRE>
 *
 */
public class SpatialQuery extends ScriptTask
{
    
    private ArrayList<String> fields = new ArrayList<String>();

    private double upperLeftLat;
    private double upperLeftLon;
    private double lowerRightLat;
    private double lowerRightLon;
    
    private ObStationDao obStationDao;

    public SpatialQuery(){
    }
    
    /* (non-Javadoc)
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute()
    {
        List<ObStation> results = null;
        try {
            obStationDao = new ObStationDao();
            results = obStationDao.queryBySpatialBox(upperLeftLat, upperLeftLon, lowerRightLat, lowerRightLon);
        } catch (Exception e) {
            throw new MicroEngineException("Unable to obtain spatial data",e);
        }
        return results;
    }
    
    public void addField(String aField)
    {
        fields.add(aField);
    }

    public ArrayList<String> getFields()
    {
        return fields;
    }

    public void setFields(ArrayList<String> aFields)
    {
        fields = aFields;
    }

    public double getLowerRightLat()
    {
        return lowerRightLat;
    }

    public void setLowerRightLat(double aLowerRightLat)
    {
        lowerRightLat = aLowerRightLat;
    }

    public double getLowerRightLon()
    {
        return lowerRightLon;
    }

    public void setLowerRightLon(double aLowerRightLon)
    {
        lowerRightLon = aLowerRightLon;
    }

    public double getUpperLeftLat()
    {
        return upperLeftLat;
    }

    public void setUpperLeftLat(double aUpperLeftLat)
    {
        upperLeftLat = aUpperLeftLat;
    }

    public double getUpperLeftLon()
    {
        return upperLeftLon;
    }

    public void setUpperLeftLon(double aUpperLeftLon)
    {
        upperLeftLon = aUpperLeftLon;
    }    
    

}
