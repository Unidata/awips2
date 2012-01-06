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
package com.raytheon.uf.viz.derivparam.data;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 3, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AggregateRequestableData extends AbstractRequestableData {

    protected List<AbstractRequestableData> sourceRecords;

    public AggregateRequestableData(List<AbstractRequestableData> sourceRecords) {
        super(sourceRecords.get(0));
        this.sourceRecords = sourceRecords;
    }

    public List<AbstractRequestableData> getSourceRecords() {
        return sourceRecords;
    }

    public void setSourceRecords(List<AbstractRequestableData> sourceRecords) {
        this.sourceRecords = sourceRecords;
    }

    public Object getDataValue(Object arg) throws VizException {
        List<Object> args = new ArrayList<Object>(sourceRecords.size());
        for (AbstractRequestableData sourceRecord : sourceRecords) {
            args.add(sourceRecord.getDataValue(arg));
        }
        return args;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractRequestableData#getDependencies
     * ()
     */
    @Override
    public List<AbstractRequestableData> getDependencies() {
        return sourceRecords;
    }

}
