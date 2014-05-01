package gov.noaa.nws.ncep.edex.plugin.gempak.handler;

import gov.noaa.nws.ncep.common.dataplugin.gempak.request.GetGridDataRequest;

import java.util.List;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.dataquery.GridQueryAssembler;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

public class GetGridDataRequestHandler implements IRequestHandler<GetGridDataRequest> {

    @Override
    public IDataRecord handleRequest(GetGridDataRequest request) throws Exception {
        PluginDao gribDao = null;
        gribDao = PluginFactory.getInstance().getPluginDao("grid");
        
        GridQueryAssembler gqa = new GridQueryAssembler("GEMPAK");
        gqa.setDatasetId(request.getModelId());
        gqa.setMasterLevelName(request.getVcoord());
        gqa.setParameterAbbreviation(request.getParm());
        gqa.setLevelOneValue(Double.valueOf(request.getLevel1()));
        gqa.setLevelTwoValue(Double.valueOf(request.getLevel2()));
       
        List<QueryParam> qpl = gqa.getQueryParams();
                
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        for (QueryParam qp: qpl) {
        	System.out.println(qp);
        	if ( ! qp.getField().equals("pluginName") ) {
        		query.addQueryParam(qp);
        	}
        }
        query.addQueryParam("dataTime.refTime", request.getReftime(), "=");
        query.addQueryParam("dataTime.fcstTime", request.getFcstsec(), "=");

        IDataRecord gridData = null;

        List<?> dbList = gribDao.queryByCriteria(query);
        if (dbList != null && !dbList.isEmpty()) {
            for (Object pdo : dbList) {
                GridRecord record = (GridRecord) pdo;
          
                System.out.println(record.getDataURI());
                
                IDataStore dataStore = gribDao.getDataStore((IPersistable) record);
                gridData = dataStore.retrieve(record.getDataURI(), "Data", Request.ALL);
                
            }
        }
        
        return gridData;
    }
}
