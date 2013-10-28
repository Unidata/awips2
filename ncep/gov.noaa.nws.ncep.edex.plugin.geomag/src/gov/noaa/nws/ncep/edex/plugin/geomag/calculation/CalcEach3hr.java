package gov.noaa.nws.ncep.edex.plugin.geomag.calculation;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.vividsolutions.jts.geom.Coordinate;
/*
 * The calculation of k, 3 hour related.
 * 
 * <pre>
 * SOFTWARE HISTORY
 *                   
 * ate          Ticket#     Engineer   Description
 * -----------  ----------  ---------- --------------------------
 * 05/14/2013   #989        qzhou      Initial Creation
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
public class CalcEach3hr {
	private static final float MISSING_VAL = 99999.99f;
	private static final int NIGHT_LENGTH = 90; //min
	private static final int DAWN_LENGTH = 60;
	private static final int DAY_LENGTH = 0;
	private static final int DUSK_LENGTH = 60;
	private static int DAYS = 30;
	private static int HOURS = 24;
	private static int MINUTES = 60; 
	
//	public class DBLevel implements Comparable<DBLevel>{
//		private float dB;
//		private int index;
//		
//		public DBLevel(int index, float db) {
//			super();
//			this.index = index;
//			this.dB = dB;
//			
//		}
//		public int getIndex() {
//			return index;
//		}
//		public void setIndex(int index) {
//			this.index = index;
//		}
//		public float getDB() {
//			return dB;
//		}
//		public void setDB(float dB) {
//			this.dB = dB;
//		}
//		
//		public class ChangeComparator implements Comparator<DBLevel> {	
////		public int compareTo(DBLevel compareDB) {
////			 
////			float compareQuantity = ((DBLevel) compareDB).getDB(); 
////	 
////			//ascending order
////			return this.DBLevel - compareQuantity;
////	 
////			//descending order
////			//return compareQuantity - this.quantity;
////	 
////		}
////	 
////		public static Comparator<DBLevel> FruitNameComparator 
////	                          = new Comparator<DBLevel>() {
//	 
//		    public int compare(DBLevel fruit1, DBLevel fruit2) {
//	 
//		      Float db1 = fruit1.getDB();
//		      Float db2 = fruit2.getDB();
//	 
//		      //ascending order
//		      return db1.compareTo(db2);
//	 
//		      //descending order
//		      //return fruitName2.compareTo(fruitName1);
//		    }
//	 
//		}
//		@Override
//		public int compareTo(DBLevel o) {
//			// TODO Auto-generated method stub
//			return 0;
//		};
//	}
	
	/*
	 * calculate hrAvgs for this hour
	 * @param bestList -- contains 1 hour data
	 */
	public static float[] getSimpleHourAvg(List bestList){ 
		float[] simpHrAvg = new float[2];
		float simpHrAvg1 = 0;	
		float simpHrAvg2 = 0;		
		double sum1 = 0;
		double sum2 = 0;
		int rec1 = 0;
		int rec2 = 0;
		//System.out.println("***bestList sz "+bestList.size());
		for (int i = 0; i < bestList.size(); i++) {	
			//System.out.println("***bestList avg "+bestList.size()+" "+bestList.get(i));
			//List<List> list = (List<List>) bestList.get(i);
			List<Float> list = (List<Float>) bestList.get(i);
			//float comp1 = (Float)list.get(0).get(2);
			float comp1 = (Float)list.get(1);
			float comp2 = (Float)list.get(2);
			//System.out.println("***comp12 " + comp1+" "+comp2);
			if ( comp1 != MISSING_VAL) {
				sum1 += comp1;	
				rec1++;
			}	
			if ( comp2 != MISSING_VAL) {
				sum2 += comp2;	
				rec2++;
			}
		}
		
		if (rec1 > 30) // less than half missing value		    			    	
			simpHrAvg1 = (float) sum1 / rec1; 
		else
			simpHrAvg1 = MISSING_VAL;
		
		if (rec2 > 30) // less than half missing value		    			    	
			simpHrAvg2 = (float) sum2 / rec2; 
		else
			simpHrAvg2 = MISSING_VAL;
		
		simpHrAvg[0] = simpHrAvg1;	
		simpHrAvg[1] = simpHrAvg2;
		//System.out.println("***simpHrAvg " + rec1+" "+rec2+" "+simpHrAvg1 +" "+simpHrAvg2+ " "+bestList.size());
		return simpHrAvg;
	}
	
	/*
	 * calculate hrAvgs for this day.
	 * @param data -- data of one day, 1440
	 */
	public static float[] getSimpleHourAvg(float[] data){ //data 1440
		//System.out.println("**datalength "+data.length); 
		float[] simpHrAvg = new float[HOURS];
		
		for (int ihr = 0; ihr < HOURS; ihr++) {
			double sum = 0;
			int missing = 0;
			
			for ( int i = ihr*MINUTES; i < ihr*MINUTES+MINUTES; i++) {
				
				if (data[i] != MISSING_VAL) 
					sum += data[i];
				else
					missing++;					
			}
	    
			if (missing < 30) // less than half missing value		    			    	
				simpHrAvg[ihr] = (float) sum / (MINUTES-missing); 
			else
				simpHrAvg[ihr] = MISSING_VAL;
		}
		
		return simpHrAvg;
	}

	/*
	 * calculate hrAvgs for this hour in data array
	 * @param data -- data of one day, 1440
	 */
	public static float getSimpleHourAvg(float[] data, int hour){ //one day 1440, avg for hour-1
		
		float simpHrAvg = 0;		
		double sum = 0;
		int rec = 0;
		
		if (data.length <= hour*MINUTES+MINUTES)
			for (int i = hour*MINUTES; i < data.length; i++) {						
				if ( data[i] != MISSING_VAL) {
					sum += data[i];	
					rec++;
				}			
		}
		else 
			for (int i = hour*MINUTES; i < hour*MINUTES+MINUTES; i++) {			
				if ( data[i] != MISSING_VAL) {
					sum += data[i];	
					rec++;
				}	
			}
		
		if (rec > 30) // less than half missing value		    			    	
			simpHrAvg = (float) sum / (rec); 
		else
			simpHrAvg = MISSING_VAL;
				
		return simpHrAvg;
	}
	
	/*
	 * @param simpHrAvgH -- data of 30 intervals(720 hours)
	 * @return disturbance levels for 30 intervals
	 */
	public static float[] getDisturbanceLevel(float[] simpHrAvgH, float[] simpHrAvgD){ 
		float[] dB = new float[30];
		
		for (int j = 0; j < DAYS; j++) {
			double sum = 0;
			int missing = 0;
			
			int endOfArray = simpHrAvgH.length;
			int endTime = (endOfArray > j*HOURS+HOURS) ? j*HOURS+HOURS :endOfArray;
			
			for ( int i = j*HOURS; i < endTime-1; i++) {
				 
				if (simpHrAvgH[i] != MISSING_VAL && simpHrAvgD[i] != MISSING_VAL &&
						simpHrAvgH[i+1] != MISSING_VAL && simpHrAvgD[i+1] != MISSING_VAL ) {
					sum += Math.sqrt( Math.pow((simpHrAvgH[i+1] - simpHrAvgH[i]), 2) 
							+ Math.pow((simpHrAvgD[i+1] - simpHrAvgD[i]), 2) );
				}
				else
					missing++;
			}
			 
			if (missing <= 12) // not 12 or more missing	    			    	
				dB[j] = (float) sum / (HOURS-1-missing); 
			else
				dB[j] = MISSING_VAL;
			System.out.print("***dB[j] "+dB[j] + " ");
		}
		
		return dB;
	}		
		
	/*
	 * @param dB -- float[30 ]
	 * @return --5 smallest disturbance levels
	 */
	public static Map getSmallDisturbanceLevel(float[] dB){
		//create a map that key=dBIndex and value=dBValue. 
		//create a duplicate array dBDup. Sort it.
		//take 5 smallest dBDup[i]. Then find its index and value from the dB. Put them to the map
		Map<Integer, Float> dBSmall = new HashMap<Integer, Float>();
//		Map<Float, Integer> temp = new HashMap<Float, Integer>();
//		Map<Float, Integer> tempDup = new HashMap<Float, Integer>();
//		for (int i = 0; i < dB.length; i++) {
//			temp.put(dB[i], i);
//			tempDup.put(dB[i], i);
//			System.out.println("***temp "+dB[i] + " "+i);
//		}
		
		float[] dBDup = new float[dB.length];
		for (int i = 0; i < dBDup.length; i++) {
			dBDup[i] = dB[i];
		}
		Arrays.sort(dBDup);
//		for (int i = 0; i < dBDup.length; i++) {
//			System.out.print("***dBsort "+dBDup[i] +" ");
//		}
		
		float dupIndex = (int)MISSING_VAL ;
		float wk = 0;
		//take 5 smallest dBDup
		for (int j = 0; j < 5; j++) {
			for (int i = 0; i < dB.length; i++) {
				if (dB[i] == dBDup[j] && i != dupIndex) { //for duplicated values				
					//System.out.println("***dBDup[j] "+dBDup[j] +" "+wk+" "+i +" "+j);
					dBSmall.put(i, dB[i]);
					dupIndex = i;
					break;
				}
			}
		}
		
//		for (int i = 0; i < 5; i++) {
//			System.out.println("***temp.get(dB[i]) "+temp.get(dB[i]) );
//			//previous = temp.get(dB[i]);
//			if (previous == temp.get(dB[i])) {
//				
//				System.out.println("***previous) "+previous +" "+i );
//				 tempDup.remove(dB[i]);
//				 System.out.println("***tempDup) "+tempDup.size() +" "+tempDup.get(dB[i]) );
//				 //put next dB[i]
//				 dBSmall.put(tempDup.get(dB[i]), dB[i]);
//			     
//			}
//			else {
//				dBSmall.put(temp.get(dB[i]), dB[i]);
//				previous = temp.get(dB[i]);
//			}
//			System.out.println("***dBSmall "+temp.get(dB[i]) + " "+dB[i]);
//		}
		
		return dBSmall;
	}
//	public static Map getSmallDisturbanceLevel(float[] dB){
//		Arrays.sort(dB);
//		Map<Integer, Float> dBSmall = new HashMap<Integer, Float>();
//		
//		List<Float> dBlist = new ArrayList<Float>();
//		for (int i = 0; i < dB.length; i++)
//			dBlist.add(dB[i]);
//		
//		//float[] smaller = new float[5];// index of hrAvg that has smallest dB
//		int index = 0;
//		float wk;
//		float previousId = MISSING_VAL;
//		
//		for (int j = 0; j < 5; j++) {
//			float minimum = MISSING_VAL;
//			for (int i = 0; i < dBlist.size(); i++) {
//				
//				if (dBlist.get(i) < minimum) {
//					minimum = dBlist.get(i);
//					index = i;
//				}
//			}
//			
//			if (minimum < 1)
//				wk = 1;
//			else
//				wk = 1 / (minimum *minimum);
//			
//			// since dBlist.remove(index); index needs to refer to original index
//			if (previousId > index) {
//				dBSmall.put(index, wk);
//				previousId = index;
//			}
//			else {
//				dBSmall.put(index+1, wk); // +j: original dB was reduced by j
//				previousId = index;
//			}
//			System.out.println("dBlist "+index+" "+wk);
//			dBlist.remove(index);
//		}
//		
//		return dBSmall;
//	}
	
	/*
	 * @param -- dBSmall, 5 set map
	 * @param -- simpHrAvg, -- float[720]
	 * @rturn -- quietLevelHourAvg, float[24]
	 */
	public static float[] getQuietLevelHourAvg(Map<Integer, Float> dBSmall, float[] simpHrAvg){
		if (dBSmall.entrySet().size() < 5)
			return simpHrAvg;
		
		float[] quietHrAvg = new float[24];
		Arrays.fill(quietHrAvg, MISSING_VAL);
		int[] index = new int [5];
		float[] dB = new float[5];
		
		int k = 0;
		Iterator<?> iter = dBSmall.entrySet().iterator();
		while (iter.hasNext()) {
			@SuppressWarnings("unchecked")
			Map.Entry<Integer, Float> mEntry = (Map.Entry<Integer, Float>) iter.next(); //sorted on key
			
			index[k] = mEntry.getKey();
			dB[k] = mEntry.getValue();
			System.out.println("***index[k] "+k+" "+index[k] + " "+ dB[k]+" "+simpHrAvg.length);
			k++;
		}				
		
		
		//construct smallHrAvg array (24*5) from simpHrAvg (24*30)
		float[] smallHrAvg = new float[24*5];
		
		for (int j = 0; j < 5; j++) {   //k=5
			int endOfArray = smallHrAvg.length;
			int endTime = (endOfArray > j*HOURS+HOURS) ? j*HOURS+HOURS :endOfArray;
			
			for (int i = j*HOURS; i < endTime; i++) {
				smallHrAvg[i] = simpHrAvg[ index[j]*HOURS + i%HOURS ]; //700
			}
		}
		
		
		for (int ihr = 0; ihr < HOURS; ihr++) {
			float sumAvg = 0;
			float sumWk = 0;
			float wk = 0;
			
			for (int jk = 0; jk < 5; jk++) {
				int ind = jk*HOURS+ihr;
				if (dB[jk] < 1)
					wk = 1;
				else
					wk = 1 / (dB[jk] *dB[jk]);	
				
				if (smallHrAvg[ind] != MISSING_VAL){
					sumAvg += wk * smallHrAvg[ind];
					sumWk += wk;
				}
			}
			
			if (sumWk >0)
				quietHrAvg[ihr] = sumAvg / sumWk;	
			
		}
		
		return quietHrAvg;
	}

	/*
	 * @param --  quietHrAvg, float[24]
	 * @return -- shifted quietLevelHourAvg, float[24]
	 */
	public static float[] getQHA(float[] quietHrAvg){
		float[] QHA = new float[24];
		
		if (quietHrAvg.length != 24)
			return quietHrAvg;
		
		for (int ihr = 0; ihr < 24; ihr++) {
			QHA[ihr] = quietHrAvg[ (ihr+3) %24 ];
		}
		
		return QHA;
	}
		

	/*
	 * @return -- 24 element floating point array. Default fitting lengths.
	 * (one for each hour of the 24 hour interval that ends at EPtime).
	 */
	public static float[] getDefLength(String station, int epHour) {
		float[] defLength = new float[24];
		float lon = CalcUtil.getLongitude(station);
		int UTdiff = Math.round(1440.0f * lon / 360.0f);
		int minute0 = epHour * MINUTES;
		//System.out.println("**epHour "+epHour);
		for (int ihr = 0; ihr < HOURS; ihr++) {
			float sum = 0;
			
			for (int imin = 0; imin < MINUTES; imin++) {
				int curMin = (minute0 + ihr*MINUTES + imin) % 1440;
				int localMin = (curMin + UTdiff) % 1440;
				
				if (localMin >= 0 && localMin < 180)
					sum += NIGHT_LENGTH;
				else if (localMin >= 180 && localMin < 360)
					sum += DAWN_LENGTH;
				else if (localMin >= 360 && localMin < 1080)
					sum += DAY_LENGTH;
				else if (localMin >= 1080 && localMin < 1260)
					sum += DUSK_LENGTH;
				else if (localMin >= 1260 && localMin < 1440)
					sum += NIGHT_LENGTH;
			}
			defLength[ihr] = sum / MINUTES;
			//System.out.println("**defLength "+defLength[ihr]);
		}
		
		return defLength;
	}
	
}


