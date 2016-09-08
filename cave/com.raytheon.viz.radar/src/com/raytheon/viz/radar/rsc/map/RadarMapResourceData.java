package com.raytheon.viz.radar.rsc.map;

import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerState;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerTextSize;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerType;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

public class RadarMapResourceData extends AbstractResourceData {

	private MarkerState markerState = MarkerState.MARKER_ONLY;

    private MarkerType markerType = MarkerType.DIAMOND;

    private Float markerSize = 1.5f;

    private Integer markerWidth = 2;

    private MarkerTextSize markerTextSize = MarkerTextSize.MEDIUM;

    private String mapName = "NEXRAD";

    private MarkerType stnMarkerType = MarkerType.LARGE_X;

    public MarkerType getStnMarkerType() {
        return stnMarkerType;
    }
    
	public RadarMapResourceData() {
		super();
	}
	
	@Override
	public RadarMapResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        // TODO Auto-generated method stub
        return new RadarMapResource(this, loadProperties);
	}

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof RadarMapResourceData))
            return false;
        RadarMapResourceData rdata = (RadarMapResourceData) obj;
        if (this.markerState.equals(rdata.getMarkerState())
                && this.markerType.equals(rdata.getMarkerType())
                && this.markerSize.equals(rdata.getMarkerSize())
                && this.markerWidth.equals(rdata.getMarkerWidth())
                && this.markerTextSize.equals(rdata.getMarkerTextSize())
                && this.stnMarkerType.equals(rdata.getStnMarkerType()))
            return true;

        return false;
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
