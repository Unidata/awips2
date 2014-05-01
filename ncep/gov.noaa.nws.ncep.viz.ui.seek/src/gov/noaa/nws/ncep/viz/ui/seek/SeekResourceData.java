/**
 * 
 */
package gov.noaa.nws.ncep.viz.ui.seek;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author sgilbert
 *
 */
public class SeekResourceData extends AbstractResourceData {
    protected Map<Coordinate, String> strings = new HashMap<Coordinate, String>();
    private  Coordinate firstPt = null;
    
    /**
	 * @return the firstPt
	 */
	public  Coordinate getFirstPt() {
		return firstPt;
	}

	/**
	 * @param firstPt the firstPt to set
	 */
	public void setFirstPt(Coordinate firstPt) {
		this.firstPt = firstPt;
	}

	/**
	 * @return the lastPt
	 */
	public Coordinate getLastPt() {
		return lastPt;
	}

	/**
	 * @param lastPt the lastPt to set
	 */
	public void setLastPt(Coordinate lastPt) {
		this.lastPt = lastPt;
	}

	/**
	 * @return the point1
	 */
	public Coordinate getPoint1() {
		return point1;
	}

	/**
	 * @param point1 the point1 to set
	 */
	public void setPoint1(Coordinate point1) {
		this.point1 = point1;
	}

	/**
	 * @return the point2
	 */
	public Coordinate getPoint2() {
		return point2;
	}

	/**
	 * @param point2 the point2 to set
	 */
	public void setPoint2(Coordinate point2) {
		this.point2 = point2;
	}

	private Coordinate lastPt = null;
    
    private Coordinate point1 = null;
    
    private Coordinate point2 = null;
	
    public void drawString(Coordinate c, String str) {
        this.strings.put(c, str);
    }

    public void clearStrings() {
        this.strings.clear();
    }

    public void clearLine() {
    	this.setPoint1(null);
    	this.setPoint2(null);
    }	
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    public void disposeInternal() {
//    	firstPt = null;
//    	lastPt = null;
    	setFirstPt(null);
    	setLastPt(null);
    	clearStrings();
    	clearLine();
    }
    
	/**
	 * 
	 */
	public SeekResourceData() {
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon.uf.viz.core.comm.LoadProperties, com.raytheon.uf.viz.core.drawables.IDescriptor)
	 */
	@Override
	public SeekDrawingLayer construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		// TODO Auto-generated method stub
		return new SeekDrawingLayer(this, loadProperties);
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
		
		// TODO Migrate this
		return false;
	}

}
