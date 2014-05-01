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
package com.raytheon.viz.pointdata.util;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;

/**
 * Wraps a data record from a PointDataContainer in the AbstractRequestableData
 * api so it can be used in Derived Parameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class PointRequestableData extends AbstractRequestableData {

    private IDataRecord rec;

    public PointRequestableData(IDataRecord rec, Unit<?> unit) {
        this.unit = unit;
        this.rec = rec;
        this.level = PointDataInventory.getStationLevel();
        this.parameter = rec.getName();
        this.dataTime = TimeAndSpace.TIME_AGNOSTIC;
        this.space = TimeAndSpace.SPACE_AGNOSTIC;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractRequestableData#getDataValue
     * ()
     */
    @Override
    public Object getDataValue(Object arg) throws VizException {
        return rec;
    }

}
