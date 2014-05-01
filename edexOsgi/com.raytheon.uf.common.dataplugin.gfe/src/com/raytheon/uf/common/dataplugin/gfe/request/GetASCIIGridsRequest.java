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
package com.raytheon.uf.common.dataplugin.gfe.request;

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2011  #8983     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class GetASCIIGridsRequest extends AbstractGfeRequest {

    @DynamicSerializeElement
    private List<DatabaseID> databaseIds;

    @DynamicSerializeElement
    private List<ParmID> parmIds;

    @DynamicSerializeElement
    private TimeRange timeRange;

    @DynamicSerializeElement
    private String coordConversionString;

    public List<DatabaseID> getDatabaseIds() {
        return databaseIds;
    }

    public void setDatabaseIds(List<DatabaseID> databaseIds) {
        this.databaseIds = databaseIds;
    }

    public List<ParmID> getParmIds() {
        return parmIds;
    }

    public void setParmIds(List<ParmID> parmIds) {
        this.parmIds = parmIds;
    }

    public TimeRange getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

    public String getCoordConversionString() {
        return coordConversionString;
    }

    public void setCoordConversionString(String coordConversionString) {
        this.coordConversionString = coordConversionString;
    }

}
