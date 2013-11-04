/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.grib.ogc;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.util.GridStyleUtil;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.edex.ogc.common.IStyleLookupCallback;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.styling.ColormapStyleProvider;

/**
 * Style provider specific to grid colormapped imagery
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 2, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GridStyleProvider extends ColormapStyleProvider<GridRecord> {
    


    /**
     * @param styleLibraryFileName
     * @param defaultColormap
     */
    public GridStyleProvider(IStyleLookupCallback<GridRecord> callback,
            String defaultColormap) {
        super(callback, defaultColormap);
    }

    /**
     * @param styleLibraryFileName
     */
    public GridStyleProvider(IStyleLookupCallback<GridRecord> callback) {
        super(callback);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.styling.ColormapStyleProvider#getCriteria(com
     * .raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected ParamLevelMatchCriteria getCriteria(GridRecord record)
            throws WmsException {
        return GridStyleUtil
                .getMatchCriteria((GridRecord) record);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.styling.ColormapStyleProvider#getParamUnits(
     * com.raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected Unit<?> getParamUnits(GridRecord record) throws WmsException {
        Parameter parameter = record.getInfo().getParameter();
        return parameter.getUnit();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.styling.ColormapStyleProvider#getRawData(com
     * .raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected Object getRawData(GridRecord record) throws WmsException {
        Object data;
        try {
            GridDao dao = new GridDao();
            IDataRecord[] res = dao.getHDF5Data(record, 0);
            FloatDataRecord datarecord = (FloatDataRecord) res[0];
            data = datarecord.getFloatData();
        } catch (PluginException e) {
            log.error("Unable to retrieve grib data", e);
            throw new WmsException(Code.InternalServerError);
        }
        return data;
    }

}
