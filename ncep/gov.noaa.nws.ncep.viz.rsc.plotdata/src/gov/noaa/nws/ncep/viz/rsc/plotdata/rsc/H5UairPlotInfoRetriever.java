package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.rsc.retrieve.AbstractDbPlotInfoRetriever;
import com.raytheon.viz.pointdata.rsc.retrieve.PointDataPlotInfoRetriever;

/**
 * 
 * A plotInfoRetriever for the h5uair plugin.
 *  
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/11/11       #441      G. Hull    Initial creation
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */

// The H5Uair decoder uses 'stId' instead of 'stationId' and 'slat' and 'slon' instead of 
//
public class H5UairPlotInfoRetriever extends PointDataPlotInfoRetriever {

	@Override
	protected void addColumns(DbQuery dq) {
        dq.addColumn("dataURI");
        dq.addColumn("slat");
        dq.addColumn("slon");
        dq.addColumn("stid");
        dq.addColumn("dataTime");
	}
	
	// override since h5uair is storing lat/lon as Floats instead of Double
    @Override
    protected PlotInfo getPlotInfo(Object[] data) {
        PlotInfo stationInfo = new PlotInfo();
        stationInfo.dataURI = (String) data[0];
        stationInfo.latitude = ((Float)data[1]).doubleValue();
        stationInfo.longitude = ((Float)data[2]).doubleValue();
        stationInfo.stationId = (String)data[3];
        if (stationInfo.stationId == null ) {
            stationInfo.stationId = "" + data[1] + "#" + data[2];
        }
        else if (stationInfo.stationId.isEmpty() ) {
            stationInfo.stationId = "" + data[1] + "#" + data[2];
        }
        stationInfo.dataTime = (DataTime) data[4];
        return stationInfo;
    }

}
