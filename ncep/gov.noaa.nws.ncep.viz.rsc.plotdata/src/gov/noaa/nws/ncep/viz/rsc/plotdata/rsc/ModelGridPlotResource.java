package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.PlotResource2.FrameData;

import java.util.ArrayList;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.LoadProperties;


/**
 * Place holder : not sure if this is needed yet.........
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  06/08/2011     #441     ghull       Created 
 *  05/23/2012     785      Q. Zhou     Added getName for legend.
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public class ModelGridPlotResource extends FcstPlotResource {

    ArrayList<DataTime> dataTimes = new ArrayList<DataTime>();

	public ModelGridPlotResource(PlotResourceData data, LoadProperties props) {
		super(data, props);
		
		
        data.setPlotInfoRetriever( new ModelGridPlotInfoRetriever() );

	}
	
	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		
		if (fd == null || fd.getFrameTime() == null || fd.isStationMapEmpty()) {
			return legendString + "-No Data";
		}
		
		if (legendString == null || legendString.equalsIgnoreCase("")) {
			return "Model Grid";
		}
		else {
			return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
		}
	}
}
