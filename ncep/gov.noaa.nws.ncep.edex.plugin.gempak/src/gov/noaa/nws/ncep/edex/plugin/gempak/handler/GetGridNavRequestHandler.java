package gov.noaa.nws.ncep.edex.plugin.gempak.handler;

import gov.noaa.nws.ncep.common.dataplugin.gempak.request.GetGridNavRequest;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.projection.MapProjection;
import org.geotools.referencing.operation.projection.MapProjection.AbstractProvider;
import org.opengis.parameter.ParameterValueGroup;

import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

public class GetGridNavRequestHandler implements IRequestHandler<GetGridNavRequest> {

    @Override
    public List<Map<String,String>> handleRequest(GetGridNavRequest request) throws Exception {
        CoreDao gribDao = null;
        gribDao = new CoreDao(DaoConfig.forClass(GridInfoRecord.class));
        
        String label;
        Double value;
        
        List<Map<String,String>> navList = new ArrayList<Map<String,String>>();
        
        DatabaseQuery query = new DatabaseQuery(GridInfoRecord.class);
        query.addQueryParam("datasetId", request.getModelId(), "=");
        query.addDistinctParameter("location");
        
        List<?> dbList = gribDao.queryByCriteria(query);
        if (dbList != null && !dbList.isEmpty()) {
            for (Object pdo : dbList) {
            	GridCoverage record = (GridCoverage) pdo;
                MapProjection mp = CRS.getMapProjection(record.getCrs());

                ParameterValueGroup group = mp.getParameterValues();
                
                Map<String,String> gridNav = new HashMap<String,String>();
                gridNav.put("projtype", record.getProjectionType());
                gridNav.put("spatialkey", record.spatialKey());
                gridNav.put("spacingunit", record.getSpacingUnit());
                gridNav.put("lowerleftlat", record.getLowerLeftLat().toString());
                gridNav.put("lowerleftlon", record.getLowerLeftLon().toString());
            	
            	label = AbstractProvider.CENTRAL_MERIDIAN.getName().getCode();
            	try {
            		value = group.parameter(label).doubleValue();
            	} catch (RuntimeException e) {
            		value = 0.0;
            	}
                gridNav.put(label, Double.toString(value));
            	
            	label = AbstractProvider.LATITUDE_OF_ORIGIN.getName().getCode();
            	try {
            		value = group.parameter(label).doubleValue();
            	} catch (RuntimeException e) {
            		value = 0.0;
            	}
                gridNav.put(label, Double.toString(value));
            	
            	label = AbstractProvider.STANDARD_PARALLEL_1.getName().getCode();
            	try {
            		value = group.parameter(label).doubleValue();
            	} catch (RuntimeException e) {
            		value = 0.0;
            	}
                gridNav.put(label, Double.toString(value));
            	
            	label = AbstractProvider.STANDARD_PARALLEL_2.getName().getCode();
            	try {
            		value = group.parameter(label).doubleValue();
            	} catch (RuntimeException e) {
            		value = 0.0;
            	}
                gridNav.put(label, Double.toString(value));
                
            	label = AbstractProvider.SCALE_FACTOR.getName().getCode();
            	try {
            		value = group.parameter(label).doubleValue();
            	} catch (RuntimeException e) {
            		value = 0.0;
            	}
                gridNav.put(label, Double.toString(value));
 
                navList.add(gridNav);

            }
        }
        
        return navList;
    }
}
