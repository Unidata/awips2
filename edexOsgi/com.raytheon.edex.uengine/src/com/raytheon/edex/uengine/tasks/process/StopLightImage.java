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

/**TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 	
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class StopLightImage extends ScriptTask {
    private IDataRecord dataRecord = null;
    private float red = (float)2.0;
    private float yellow = (float)1.0;
    private float green = (float)0.0;

    /**
     * Constructor.
     * @param dataRecord data record to be stop-lighted.
     */
    public StopLightImage(IDataRecord dataRecord) {
        this.dataRecord = dataRecord;
    }
    /* (non-Javadoc)
     * @see com.raytheon.edex.uengine.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        // TODO Auto-generated method stub
        JMath math = new JMath();
        if (!(dataRecord instanceof FloatDataRecord)) {
            return dataRecord;
        }
        float[] data = (float[]) dataRecord.getDataObject();
        float[] filtered = math.astoplight(data, this.red, this.yellow, this.green);
        ((FloatDataRecord)dataRecord).setFloatData(filtered);
        return dataRecord;
    }
    public void setColors(float red, float yellow, float green) {
        this.red = red;
        this.yellow = yellow;
        this.green = green;
    }
}
