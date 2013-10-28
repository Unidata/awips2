package gov.noaa.nws.ncep.edex.plugin.gempak.handler;

import gov.noaa.nws.ncep.common.dataplugin.gempak.request.GetStationsRequest;
import gov.noaa.nws.ncep.common.dataplugin.gempak.request.Station;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataquery.db.ReturnedField;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;
import com.vividsolutions.jts.geom.Point;

public class GetStationsHandler implements IRequestHandler<GetStationsRequest> {

    private static final String STATION_DAO = "ObStation";

    private static final String STATION_ID = "stationId";

    private static final String WMO_INDEX = "wmoIndex";

    private static final String ELEVATION = "elevation";

    private static final String COUNTRY = "country";

    private static final String STATE = "state";

    private static final String LOCATION = "location";

    @Override
    public List<Station> handleRequest(GetStationsRequest request)
            throws Exception {
        // dao =
        List<Station> stnList = new ArrayList<Station>();
        // PluginFactory.getInstance().getPluginDao(request.getPluginName());
        String entity = PluginFactory.getInstance()
                .getPluginRecordClass(request.getPluginName()).getName();
        ObStationDao obStationDao = new ObStationDao();

        DatabaseQuery dbQuery = new DatabaseQuery(STATION_DAO);
        dbQuery.addDistinctParameter(STATION_ID);
        // dbQuery.addReturnedField(new ReturnedField(STATION_ID));
        dbQuery.addReturnedField(new ReturnedField(WMO_INDEX));
        dbQuery.addReturnedField(new ReturnedField(ELEVATION));
        dbQuery.addReturnedField(new ReturnedField(COUNTRY));
        dbQuery.addReturnedField(new ReturnedField(STATE));
        dbQuery.addReturnedField(new ReturnedField(LOCATION));
        dbQuery.addJoinField(STATION_DAO, entity, STATION_ID,
                "location.stationId");

        List<?> results = obStationDao.queryByCriteria(dbQuery);
        for (Object obj : results) {
            // System.out.println(obj.getClass().getCanonicalName());
            Object[] row = (Object[]) obj;
            Station stn = new Station();
            stn.setStationId((String) row[0]);
            if (row[1] != null)
                stn.setWmoIndex((Integer) row[1]);
            if (row[2] != null)
                stn.setElevation((Integer) row[2]);
            if (row[3] != null)
                stn.setCountry((String) row[3]);
            if (row[4] != null)
                stn.setState((String) row[4]);
            if (row[5] != null) {
                Point loc = (Point) row[5];
                stn.setLongitude(loc.getX());
                stn.setLatitude(loc.getY());
            }

            if (stn != null)
                stnList.add(stn);
        }
        // System.out.println("Retrieved: " + results.size());

        return stnList;
    }
}
