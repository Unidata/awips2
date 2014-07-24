
package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import java.sql.Timestamp;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataquery.db.OrderField.ResultOrder;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.rsc.retrieve.AbstractDbPlotInfoRetriever;

/**
 * 
 * A plotInfoRetriever for taf
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/03/2011              sgurung     Initial creation
 * 11/16/2011              sgurung     Order columns by issue_time and sequenceId desc for correct time matching
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class TafPlotInfoRetriever extends AbstractDbPlotInfoRetriever {
	protected boolean onlyRefTime = false;
    @Override
    protected void addColumns(DbQuery dq) {
        dq.addColumn("dataURI");
        dq.addColumn("location.latitude");
        dq.addColumn("location.longitude");
        dq.addColumn("location.stationId");
        if (onlyRefTime) {
            // refTime retrieval is much faster
            dq.addColumn("dataTime.refTime");
        } else {
            dq.addColumn("dataTime");
        }
        //dq.addColumn("issue_time");
        //dq.addColumn("sequenceId");
        try {
        	/*
        	 * TAF can have multiple reports valid at the same refTime, need to get the latest one based on 
        	 * issue_time (the latest report) and sequenceId (TEMPO/PROB record, if present, overrides the 
        	 * initial data, has sequenceId greater than initial record). 
        	 * Since the first record is taken, ordering by issue_time and sequenceId in descending order
        	 * will ensure that the latest one is displayed
        	 * 
        	 */
            dq.addOrderBy("issue_time", ResultOrder.DESC, RecordFactory.getInstance()
                    .getPluginClass("nctaf").getName());
            dq.addOrderBy("sequenceId", ResultOrder.DESC, RecordFactory.getInstance()
                    .getPluginClass("nctaf").getName());
            } catch (Exception e) {
            	
        }  
    }

    @Override
    protected PlotInfo getPlotInfo(Object[] data) {
        PlotInfo stationInfo = new PlotInfo();
        stationInfo.dataURI = (String) data[0];
        stationInfo.latitude = ((Number) data[1]).doubleValue();
        stationInfo.longitude = ((Number) data[2]).doubleValue();
        stationInfo.stationId = (String) data[3];
        if (stationInfo.stationId == null) {
            stationInfo.stationId = "" + data[1] + "#" + data[2];
        }
        if (onlyRefTime) {
            stationInfo.dataTime = new DataTime((Timestamp) data[4]);
        } else {
            stationInfo.dataTime = (DataTime) data[4];
        }
        
        return stationInfo;
    }

}
