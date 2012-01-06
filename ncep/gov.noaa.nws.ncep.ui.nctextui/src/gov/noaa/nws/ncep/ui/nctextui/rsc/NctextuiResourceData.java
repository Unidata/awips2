/*
 * gov.noaa.nws.ncep.ui.nctextui.rsc.NctextuiResourceData
 * 
 * 1/7/2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * @author Chin Chen
 * @version 1.0
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/28/2011    T402       X. Guo     Re-format NCTEXT view panel, check
 *                                     the click action on nctext legend
 */
package gov.noaa.nws.ncep.ui.nctextui.rsc;

import gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData.*;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;


public class NctextuiResourceData extends AbstractResourceData {

	private RGB 			color = new RGB(255, 0, 0);
	private MarkerState 	markerState = MarkerState.MARKER_PLUS_TEXT;
	private MarkerType  	markerType = MarkerType.DIAMOND;
	private RGB 			pkStncolor = new RGB(0, 255, 0);
	private MarkerState 	pkStnmarkerState = MarkerState.MARKER_PLUS_TEXT;
	private MarkerType  	pkStnmarkerType = MarkerType.PLUS_SIGN;
	private Float       	markerSize = 1.5f;
	private Integer     	markerWidth = 2;
	private MarkerTextSize 	markerTextSize = MarkerTextSize.MEDIUM;
	private String 			mapName = "NCTEXTUI";

	public NctextuiResourceData() {
		super();
	}
	
	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon.uf.viz.core.comm.LoadProperties, com.raytheon.uf.viz.core.drawables.IDescriptor)
	 */
	@Override
	public NctextuiResource construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		// TODO Auto-generated method stub
		return new NctextuiResource(this, loadProperties);
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
		// TODO Auto-generated method stub
		return false;
	}
	
	public RGB getColor() {
		return color;
	}

	public void setColor(RGB color) {
		this.color = color;
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

	public RGB getPkStncolor() {
		return pkStncolor;
	}

	public void setPkStncolor(RGB pkStncolor) {
		this.pkStncolor = pkStncolor;
	}

	public MarkerState getPkStnmarkerState() {
		return pkStnmarkerState;
	}

	public void setPkStnmarkerState(MarkerState pkStnmarkerState) {
		this.pkStnmarkerState = pkStnmarkerState;
	}

	public MarkerType getPkStnmarkerType() {
		return pkStnmarkerType;
	}

	public void setPkStnmarkerType(MarkerType plStnmarkerType) {
		this.pkStnmarkerType = plStnmarkerType;
	}

}
