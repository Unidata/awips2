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
package com.raytheon.edex.uengine.tasks.process;

import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.edex.uengine.util.JMath;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07Jun2007    TO6         MW Fegan    Created.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */

public class ConvertWindsData extends ScriptTask {
    private boolean speed = true;

    private IDataRecord uWinds = null;

    private IDataRecord vWinds = null;

    /**
     * Constructor.
     * 
     * @param uWinds
     * @param vWinds
     * @param speed
     */
    public ConvertWindsData(IDataRecord uWinds, IDataRecord vWinds,
            boolean speed) {
        this.uWinds = uWinds;
        this.vWinds = vWinds;
        this.speed = speed;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        FloatDataRecord retVal = null;
        if (!(uWinds instanceof FloatDataRecord)
                || !(vWinds instanceof FloatDataRecord)) {
            return retVal;
        }
        JMath math = new JMath();
        float[] data;
        if (speed) {
            data = math.awndspd((float[]) uWinds.getDataObject(),
                    (float[]) vWinds.getDataObject());
        } else {
            data = math.awnddir((float[]) uWinds.getDataObject(),
                    (float[]) vWinds.getDataObject());
        }
        retVal = new FloatDataRecord(uWinds.getName(), "", data, uWinds
                .getDimension(), uWinds.getSizes());

        return retVal;
    }
}
