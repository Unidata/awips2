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
package com.raytheon.uf.edex.plugin.pointset;

import java.nio.Buffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.pointset.PointSetConstants;
import com.raytheon.uf.common.dataplugin.pointset.PointSetData;
import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * 
 * {@link PluginDao} for {@link PointSetRecord}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * Jan 21, 2016  5208     bsteffen  Store scale and offset
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class PointSetDao extends PluginDao {
    
    public PointSetDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    public PointSetDao() throws PluginException {
        super("pointset");
    }

    @Override
    public PluginDataObject[] persistToDatabase(PluginDataObject... records) {
        return super.persistToDatabase(verifyRecords(records));
    }

    @Override
    public StorageStatus persistToHDF5(PluginDataObject... records)
            throws PluginException {
        return super.persistToHDF5(verifyRecords(records));
    }

    private PluginDataObject[] verifyRecords(PluginDataObject... records) {
        List<PluginDataObject> toPersist = new ArrayList<PluginDataObject>(
                records.length);
        for (PluginDataObject record : records) {
            PointSetRecord rec = (PointSetRecord) record;
            if (validateParameter(rec)) {
                toPersist.add(rec);
            }
        }
        return toPersist.toArray(new PointSetRecord[toPersist.size()]);
    }

    protected boolean validateParameter(PointSetRecord record) {
        Parameter parameter = record.getParameter();
        boolean result = true;
        if (parameter == null) {
            result = false;
        } else if (parameter.getName() == null) {
            result = false;
        } else if (parameter.getName().equals("Missing")) {
            result = false;
        } else {
            Parameter dbParameter = ParameterLookup.getInstance().getParameter(
                    parameter, true);
            if (!parameter.equals(dbParameter)) {
                UnitConverter converter = Parameter.compareUnits(parameter,
                        dbParameter);
                    record.getData().convert(converter);
            }
            record.setParameter(dbParameter);
        }
        if (!result) {
            logger.info("Discarding record due to missing or unknown parameter mapping: "
                    + record);
        }
        return result;
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        if (obj instanceof PointSetRecord) {
            PointSetRecord points = (PointSetRecord) obj;
            PointSetData data = points.getData();
            Buffer buffer = data.getData();
            IDataRecord dataRecord = DataStoreFactory.createStorageRecord(
                    "Data", points.getDataURI(), buffer.array());
            if (data.getOffset() != null || data.getScale() != null) {
                Map<String, Object> dataAttributes = new HashMap<>();
                if (data.getOffset() != null) {
                    dataAttributes.put(PointSetConstants.ADD_OFFSET,
                            data.getOffset());
                }
                if (data.getScale() != null) {
                    dataAttributes.put(PointSetConstants.SCALE_FACTOR,
                            data.getScale());
                }

                dataRecord.setDataAttributes(dataAttributes);
            }
            dataStore.addDataRecord(dataRecord);
        } else if (obj != null) {
            throw new IllegalArgumentException("Cannot handle "
                    + obj.getClass().getSimpleName());
        }
        return dataStore;
    }

}
