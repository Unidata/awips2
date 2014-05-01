package gov.noaa.nws.ncep.edex.plugin.gempak.handler;

import gov.noaa.nws.ncep.common.dataplugin.gempak.request.GetGridInfoRequest;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapper;
import com.raytheon.uf.common.parameter.mapping.ParameterMapper;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

public class GetGridInfoRequestHandler implements IRequestHandler<GetGridInfoRequest> {

    @Override
    public List<Map<String,String>> handleRequest(GetGridInfoRequest request) throws Exception {
        CoreDao gribDao = null;
        gribDao = new CoreDao(DaoConfig.forClass(GridRecord.class));

        List<Map<String,String>> gridList = new ArrayList<Map<String,String>>();
        
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam("info.datasetId", request.getModelId(), "=");

        List<?> dbList = gribDao.queryByCriteria(query);
        if (dbList != null && !dbList.isEmpty()) {
            for (Object pdo : dbList) {
                GridRecord record = (GridRecord) pdo;
          
                Map<String,String> gridMap = new HashMap<String,String>();

                gridMap.put("model", record.getDatasetId());
                gridMap.put("second", record.getSecondaryId());
                gridMap.put("ensemble", record.getEnsembleId());
                gridMap.put("param", ParameterMapper.getInstance().lookupAlias(
                		record.getParameter().getAbbreviation(), "GEMPAK"));
                gridMap.put("vcoord", LevelMapper.getInstance().lookupAlias(
                		record.getLevel().getMasterLevel().getName(), "GEMPAK"));
                gridMap.put("level1", record.getLevel().getLevelOneValueAsString());
                gridMap.put("level2", record.getLevel().getLevelTwoValueAsString());
                gridMap.put("reftime", record.getDataTime().getRefTime().toString());
                gridMap.put("fcstsec", String.valueOf(record.getDataTime().getFcstTime()));

                gridList.add(gridMap);

            }
        }
        
        return gridList;
    }
}
