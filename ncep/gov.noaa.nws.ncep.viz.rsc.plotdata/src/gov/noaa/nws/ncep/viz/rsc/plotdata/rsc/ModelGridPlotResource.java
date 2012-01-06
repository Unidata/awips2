package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

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
 * 
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
	
}
