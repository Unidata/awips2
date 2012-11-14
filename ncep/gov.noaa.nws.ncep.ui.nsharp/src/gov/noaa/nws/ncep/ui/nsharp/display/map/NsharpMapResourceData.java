/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResourceData
 * 
 * This java class performs the NSHARP ResourceData functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display.map;

import gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData.*;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

public class NsharpMapResourceData extends AbstractResourceData {

	private MarkerState 	markerState = MarkerState.MARKER_ONLY;
	private MarkerType  	markerType = MarkerType.DIAMOND;
	private Float       	markerSize = 1f;
	private Integer     	markerWidth = 2;
	private MarkerTextSize 	markerTextSize = MarkerTextSize.MEDIUM;
	private String 			mapName = "NSHARP";
	private MarkerType  	stnMarkerType = MarkerType.LARGE_X;
	
	public MarkerType getStnMarkerType() {
		return stnMarkerType;
	}

	public NsharpMapResourceData() {
		super();
	}
	
	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon.uf.viz.core.comm.LoadProperties, com.raytheon.uf.viz.core.drawables.IDescriptor)
	 */
	@Override
	public NsharpMapResource construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		// TODO Auto-generated method stub
		return new NsharpMapResource(this, loadProperties);
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object)
	 */
	@Override
	public void update(Object updateData) {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean equals(Object obj) {
		return obj == this;
	}
	
	public MarkerState getMarkerState() {
		return markerState;
	}

	public void setMarkerState(MarkerState markerState) {
		this.markerState = markerState;
	}

	public MarkerType getMarkerType() {
		return markerType;
	}

	public void setMarkerType(MarkerType markerType) {
		this.markerType = markerType;
	}

	public Float getMarkerSize() {
		return markerSize;
	}

	public void setMarkerSize(Float markerSize) {
		this.markerSize = markerSize;
	}

	public Integer getMarkerWidth() {
		return markerWidth;
	}

	public void setMarkerWidth(Integer markerWidth) {
		this.markerWidth = markerWidth;
	}

	public MarkerTextSize getMarkerTextSize() {
		return markerTextSize;
	}

	public void setMarkerTextSize(MarkerTextSize markerTextSize) {
		this.markerTextSize = markerTextSize;
	}

	public String getMapName() {
		return mapName;
	}

	public void setMapName(String mapName) {
		this.mapName = mapName;
	}
}
