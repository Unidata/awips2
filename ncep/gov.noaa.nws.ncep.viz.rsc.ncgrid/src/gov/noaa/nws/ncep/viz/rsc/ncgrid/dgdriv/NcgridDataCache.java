package gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv;

import java.util.HashMap;
import java.util.Map.Entry;

/**
 * This is the grid data cache for diagnostic.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 25, 2012            xguo      Initial creation * 
 * 
 * </pre>
 * 
 * @author xguo
 *
 */
public class NcgridDataCache {

	/*
	 * Cache retrieved Data URI
	 */
	HashMap <String, String> dataURI; // key is the requested constraint
	
	/*
	 * grid data structure
	 */
	public class NcgridData {
		int nx;
		int ny;
		float[] data;
		
		public float [] getData () {
			return data;
		}
		
		public void setData (float [] d ) {
			data = new float [d.length];
			System.arraycopy(d, 0, data, 0, d.length);
		}
		
		public int getNx () {
			return nx;
		}
		
		public void setNx ( int x ) {
			nx = x;
		}
		
		public int getNy () {
			return ny;
		}
		
		public void setNy ( int y) {
			ny = y ;
		}
	}
	/*
	 * Cache retrieved grid data
	 */
	HashMap<String, NcgridData> gridData; // key is the data URI
	/*
	 * Initialize all HashMap
	 */
	public NcgridDataCache () {
		dataURI = new HashMap <String, String>();
		gridData = new HashMap<String, NcgridData>();
	}
	
	/*
	 * Get Data URI
	 */
	public String getDataURI ( String key) {
		return (dataURI.get(key));
	}
	
	/*
	 * Add Data URI
	 */
	
	public void addDataURI (String key, String dataUri ) {
		dataURI.put(key, dataUri);
	}
	
	/*
	 * get gempak parameter from dataURI
	 */
	public String getGempakParam ( String dataUri ) {
		String parm = null;
		
		for ( Entry<String, String> e : dataURI.entrySet() ) {
			if ( e.getValue().equals(dataURI)){
				String[] parmList = e.getKey().split("\\|");
				parm = parmList[2];
				break;
			}
		}
		return parm;
	}
	/*
	 * Get grid data
	 */
	
	public NcgridData getGridData ( String dataUri ) {
		return gridData.get(dataUri);
	}
	
	/*
	 * Add grid data
	 */
	
	public void addGridData (String dataUri, int nx, int ny, float [] data) {
		NcgridData nd = new NcgridData ();
		
		nd.setNx(nx);
		nd.setNy(ny);
		nd.setData(data);
		gridData.put(dataUri, nd);
	}
	
	public void clear () {
		dataURI.clear();
		gridData.clear();
	}
}
