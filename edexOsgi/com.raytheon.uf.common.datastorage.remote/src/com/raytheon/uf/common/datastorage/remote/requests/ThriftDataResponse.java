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
package com.raytheon.uf.common.datastorage.remote.requests;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Data response object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class ThriftDataResponse implements ISerializableObject {

    @DynamicSerializeElement
    private String[] datasets;

    @DynamicSerializeElement
    private IDataRecord[] records;

    @DynamicSerializeElement
    private IDataRecord record;

    public ThriftDataResponse() {

    }

    public ThriftDataResponse(String[] datsets) {
        this.datasets = datsets;
    }

    public ThriftDataResponse(IDataRecord[] records) {
        this.records = records;
    }

    public ThriftDataResponse(IDataRecord record) {
        this.record = record;
    }

    public String[] getDatasets() {
        return datasets;
    }

    public void setDatasets(String[] datasets) {
        this.datasets = datasets;
    }

    public IDataRecord[] getRecords() {
        return records;
    }

    public void setRecords(IDataRecord[] records) {
        this.records = records;
    }

    public IDataRecord getRecord() {
        return record;
    }

    public void setRecord(IDataRecord record) {
        this.record = record;
    }

}
