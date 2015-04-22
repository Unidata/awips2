package gov.noaa.nws.ncep.viz.common.graphicUtil;

import java.util.ArrayList;
import java.util.List;



import com.vividsolutions.jts.geom.CoordinateList;

/**
 * Reduce points of a polygon by examining the angle at each point.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/12/2011     430      Q.Zhou      Initial creation.
 * 
 * </pre>
 * 
 * @author Q.Zhou
 * @version 1.0
 */
public class ReducePoints {
    CoordinateList xyList;
    int reduceNum;
    List<Integer> reduceFlg;
    
    // bellow might be needed in the near future
    List<Integer> rmFlg;
    List<Integer> orig = new ArrayList<Integer>();
    List<Double> areaDiff;    	
	int index;
	double incrPct;
	double incrDst;
	double incrPctOrig;	
	String prefix;
	
	public ReducePoints( String option) {
		
		if (option.equalsIgnoreCase("ALG_ANGLE") ) 
			reduceByAngle(xyList, reduceFlg, reduceNum); 
//		else if (option.equalsIgnoreCase(" ") ) 
//			divide(x, y);
//	    else if (option.equalsIgnoreCase("ALG_SIZE"))  
//	    	reduceBySize(xyList, reduceFlg, orig, reduceNum, incrPct, incrDst);
//		else if (option.equalsIgnoreCase("ALG_PCT_DIST") ) 
//			reduceByPctDist(xyList, reduceFlg, orig, incrPct, incrPctOrig, incrDst, prefix);
//		else  // ALG_KEEP_CONCAV
//			reduceKeepConcav(xyList, reduceFlg, orig, incrPct, incrDst, prefix);
	}
	
	
	/*
	 * This function examines the angle at each point. The one(s) closest to 180 degree
	 * is(are) removed, if the reduceFlg is either NULL or TRUE  for that point. 
	 */
	public static CoordinateList reduceByAngle(CoordinateList xyList, List<Integer> reduceFlg, int reduceNum) {
		int listSize = 0;
		double	ang1, ang2;
		double minDang;
		int minDangIdx = -1;

		if (xyList != null)
			listSize = xyList.size();
		
		if (reduceNum < 3)
			return xyList;
		
		if (reduceNum >= listSize)
			return xyList;
		
		while (reduceNum < listSize) {
			minDang = Math.PI;
			int i = 0;
			for ( i = 0; i< listSize; i++ )  {

			    if ( reduceFlg == null || reduceFlg.get(i).intValue() >0) {
			    	
			        if ( i == (listSize-1) )  {

			            ang1 = Math.atan2( (xyList.getCoordinate(0).y - xyList.getCoordinate(i).y), 
				                  (xyList.getCoordinate(0).x - xyList.getCoordinate(i).x) );
			        }
			        else  {
			            ang1 = Math.atan2( (xyList.getCoordinate(i+1).y - xyList.getCoordinate(i).y), 
				                  (xyList.getCoordinate(i+1).x - xyList.getCoordinate(i).x) );
			        }
			    
			        if ( i == 0 )  {
			            ang2 = Math.atan2( (xyList.getCoordinate(listSize-1).y - xyList.getCoordinate(i).y), 
				                  (xyList.getCoordinate(listSize-1).x - xyList.getCoordinate(i).x) );
			        }
			        else  {
			        	ang2 = Math.atan2( (xyList.getCoordinate(i-1).y - xyList.getCoordinate(i).y), 
				       	          (xyList.getCoordinate(i-1).x - xyList.getCoordinate(i).x) );
			        }

			        
			        double dang = Math.abs( Math.PI - Math.abs(ang1 - ang2) );
			        if (dang == Math.PI || dang == 0.0 || new Double(dang).isNaN())
			        	dang = 0.0;
			        
			        if ( dang < minDang )  {
			            minDang = dang;
			            minDangIdx = i;
			            
			        }
			    }
		    }
				
			/*
			 *  Remove the point.
			 */
			if (minDangIdx == -1) {//not reduceable anymore
				return xyList;
			}
			else {
				xyList.remove(minDangIdx);	
				reduceFlg.remove(minDangIdx);
				listSize -= 1;
				minDangIdx = -1;
			}
		}
		
		return xyList;
	}
	

	/*
	 *  get & set
	 */

	public void setXyList(CoordinateList xyList) {
		this.xyList = xyList;
	}

	public CoordinateList getXyList() {
		return xyList;
	}
 
	public void setReduceFlg(List<Integer> reduceFlg) {
		this.reduceFlg = reduceFlg;
	}

	public List<Integer> getReduceFlg() {
		return reduceFlg;
	}   

	public void setRmFlg(List<Integer> rmFlg) {
		this.rmFlg = rmFlg;
	}

	public List<Integer> getRmFlg() {
		return rmFlg;
	} 
	
	public void setOrig(List<Integer> orig) {
		this.orig = orig;
	}

	public List<Integer> getOrig() {
		return orig;
	} 
 
	public void setAreaDiff(List<Double> areaDiff) {
		this.areaDiff = areaDiff;
	}

	public List<Double> getAreaDiff() {
		return areaDiff;
	} 

	public void setReduceNum(int reduceNum) {
		this.reduceNum = reduceNum;
	}

	public int getReduceNum() {
		return reduceNum;
	}
	
	public void setIndex(int index) {
		this.index = index;
	}

	public int getIndex() {
		return index;
	}
	
	public void setIncrPct(double incrPct) {
		this.incrPct = incrPct;
	}

	public double getIncrPct() {
		return incrPct;
	}
	
	public void setIncrDst(double incrDst) {
		this.incrDst = incrDst;
	}

	public double getIncrDst() {
		return incrDst;
	}

	public void setIncrPctOrig(double incrPctOrig) {
		this.incrPctOrig = incrPctOrig;
	}

	public double getIncrPctOrig() {
		return incrPctOrig;
	}
	
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	public String getPrefix() {
		return prefix;
	}	
	
}