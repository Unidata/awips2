/**
 * 
 */
package gov.noaa.nws.ncep.viz.tools.contour;

import java.util.List;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LinearRing;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

/**
 * @author sgilbert
 *
 */
public class ContourGenerator {

	private float[] gridData;
	private int xDimension;
	private int yDimension;
	
	private float minValue;
	private float maxValue;
	private boolean contourValuesSet;
	
	private float[] contourValues;
	
	/**
	 * @param gridData
	 * @param dimension
	 * @param dimension2
	 */
	public ContourGenerator(float[] gridData, int xDimension, int yDimension) {
		this.gridData = gridData;
		this.xDimension = xDimension;
		this.yDimension = yDimension;
	
		contourValuesSet = false;
        maxValue = Float.MIN_VALUE;
        minValue = Float.MAX_VALUE;

        for (int i=0; i < xDimension*yDimension; i++ ) {
        	if ( gridData[i] != IDecoderConstantsN.GRID_MISSING ) {
        		maxValue = Math.max( maxValue, gridData[i]);
        		minValue = Math.min( minValue, gridData[i]);
        	}
        }
	
	}

	/**
	 * @param contourValues the contourValues to set
	 */
	public void setContourValues(List<Double> cntrValues) {
		
		if ( contourValuesSet ) {
			CNFNative.cleanUp();
		}
		this.contourValues = new float[cntrValues.size()];
		for ( int i=0; i<cntrValues.size(); i++ ) this.contourValues[i] = cntrValues.get(i).floatValue();
		
		CNFNative.setContourValues(contourValues);
		contourValuesSet = true;
	}

	/**
	 * @return the minValue
	 */
	public float getMinValue() {
		return minValue;
	}

	/**
	 * @return the maxValue
	 */
	public float getMaxValue() {
		return maxValue;
	}
	
	
	
	/**
	 * @return the contourValues
	 */
	public float[] getContourValues() {
		return contourValues;
	}

	public void generateContours() throws ContourException {
		if ( ! contourValuesSet ) {
			throw new ContourException("Must set contour values first");
		}
		CNFNative.generateContours(gridData, xDimension, yDimension);
	}
	
	public void dispose() {
		CNFNative.cleanUp();
	}

	public Geometry getContours(float cval) {
		return CNFNative.getContour(cval);
	}

	public LinearRing getEdges() {
		return (LinearRing)CNFNative.getEdges();
	}
	
}
