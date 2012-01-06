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
package gov.noaa.nws.ncep.common.dataplugin.ncgrib;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.request.NcgridDataRequestMessage;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2010            brockwoo     Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
@DynamicSerialize
public class NcgribThriftRecord {

    @DynamicSerializeElement
    private NcgridDataRequestMessage message;

    @DynamicSerializeElement
    private float[] data;

    @DynamicSerializeElement
    private boolean localSection = false;

    @DynamicSerializeElement
    private int[] localSectionData;

    @DynamicSerializeElement
    private boolean hybridGrid = false;

    @DynamicSerializeElement
    private float[] hybridGridData;

    @DynamicSerializeElement
    private boolean thinnedGrid = false;

    @DynamicSerializeElement
    private int[] thinnedGridData;

    public NcgridDataRequestMessage getMessage() {
        return message;
    }

    public void setMessage(NcgridDataRequestMessage message) {
        this.message = message;
    }

    public float[] getData() {
        return data;
    }

    public void setData(float[] data) {
        this.data = data;
    }

    public boolean isLocalSection() {
        return localSection;
    }

    public void setLocalSection(boolean localSection) {
        this.localSection = localSection;
    }

    public int[] getLocalSectionData() {
        return localSectionData;
    }

    public void setLocalSectionData(int[] localSectionData) {
        this.localSectionData = localSectionData;
    }

    public boolean isHybridGrid() {
        return hybridGrid;
    }

    public void setHybridGrid(boolean hybridGrid) {
        this.hybridGrid = hybridGrid;
    }

    public float[] getHybridGridData() {
        return hybridGridData;
    }

    public void setHybridGridData(float[] hybridGridData) {
        this.hybridGridData = hybridGridData;
    }

    public boolean isThinnedGrid() {
        return thinnedGrid;
    }

    public void setThinnedGrid(boolean thinnedGrid) {
        this.thinnedGrid = thinnedGrid;
    }

    public int[] getThinnedGridData() {
        return thinnedGridData;
    }

    public void setThinnedGridData(int[] thinnedGridData) {
        this.thinnedGridData = thinnedGridData;
    }

    public void setDataFields(IDataRecord[] data) {
        if (data[0] != null) {
            this.data = ((FloatDataRecord) data[0]).getFloatData();
        }
        if (data[1] != null) {
            this.localSectionData = ((IntegerDataRecord) data[1]).getIntData();
            this.localSection = true;
        }
        if (data[2] != null) {
            this.hybridGridData = ((FloatDataRecord) data[2]).getFloatData();
            this.hybridGrid = true;
        }
        if (data[3] != null) {
            this.thinnedGridData = ((IntegerDataRecord) data[3]).getIntData();
            this.thinnedGrid = true;
        }
    }

}
