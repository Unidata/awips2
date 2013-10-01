package gov.noaa.nws.ncep.edex.plugin.gempak.handler;

import gov.noaa.nws.ncep.common.dataplugin.gempak.request.GetTimesRequest;
import gov.noaa.nws.ncep.common.dataplugin.gempak.request.GetTimesResponse;

import java.util.List;

import com.raytheon.uf.common.dataquery.db.ReturnedField;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

public class GetTimesHandler implements IRequestHandler<GetTimesRequest> {
    private PluginDao dao;

    @Override
    public Object handleRequest(GetTimesRequest request) throws Exception {
        dao = PluginFactory.getInstance().getPluginDao(request.getPluginName());
        String entity = PluginFactory.getInstance()
                .getPluginRecordClass(request.getPluginName()).getName();
        DatabaseQuery dbQuery = new DatabaseQuery(entity);
        dbQuery.setDistinct(true);
        dbQuery.addReturnedField(new ReturnedField(request.getTimeField()));

        List<?> results = dao.queryByCriteria(dbQuery);
        GetTimesResponse response = new GetTimesResponse();
        response.setTimes(results);

        return response;
    }

}
