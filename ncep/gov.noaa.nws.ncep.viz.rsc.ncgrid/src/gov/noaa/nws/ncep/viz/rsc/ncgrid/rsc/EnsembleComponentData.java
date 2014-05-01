package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.time.DataTime;

//
// 
public class EnsembleComponentData {

	// for the gridResource
	private String ensCompsStringForRefTime = null;
	private String ensCompsStringsForUnreferencedTimes = null;
	
	private Date   seldRefTime = null;
	
	public class EnsComp {
		private String ensCompName;
		private String modName; // same as ensCompName if not an ens member
		private String memberName = null; 
		
		private int weight = -1;
//		private String cycle = null;
		private Date   cycleTime = null;
		private Integer   cycleOffsetHrs = 0;
		boolean isFirst;
		
		// cyc is now expecting a relative cycle time such as CYC-6
		// 
		EnsComp(String name, int wt, Date cyc, boolean first) {
			ensCompName = name;
			int indx = ensCompName.indexOf(":"); 
			if( indx == -1 ) {
				modName = ensCompName;
				memberName = null;
			}
			else {
				modName = ensCompName.substring(0,indx);
				memberName = ensCompName.substring(indx+1, ensCompName.length() );
			}
			
			weight = wt;
//			cycle = cyc.toUpperCase();
			cycleTime = cyc;
			
			isFirst = first;
			
//			int cycIndx = cyc.indexOf("CYC");
//			
//			if( cycIndx == -1 ) {
//				System.out.println("Warning parsing Ens cycle time for " + cyc+", expecting 'CYC'" );				
//			}
//			cycIndx = cyc.substring( cycIndx ).indexOf("-");
//			
//			// 
//			if( cycIndx != -1 ) {
//				cycleOffsetHrs = Integer.parseInt( cyc.substring( cyc.indexOf("-")+1 ) );				
//			}
		}
		
//		ModelInfo(){}

		public String getEnsCompName() {
			return ensCompName;
		}

		public boolean isEnsembleMember() {
			return (memberName != null); 
		}
		
		// JUST THE MODEL w/o any member name if applicable
		public String getModelName() {
			return modName;
		}

		public String getEnsembleMemberName() {
			return memberName;
		}
		
		public int getWeight() {
			return weight;
		}

		public void setWeight(int weight) {
			this.weight = weight;
		}

		public Integer getCycleOffsetHrs() {
			return cycleOffsetHrs;
		}

		public Date getCycleTime() {
			return cycleTime;
		}

		public void setCycle(Date cycle) {
			this.cycleTime = cycle;
		}
//		public String getCycle() {
//			return cycle;
//		}
//
//		public void setCycle(String cycle) {
//			this.cycle = cycle;
//		}

		public boolean isFirst() {
			return isFirst;
		}

		public void setFirst(boolean isFirst) {
			this.isFirst = isFirst;
		};
		
	}
	
	protected List<EnsComp> ensCompsList;
	
	public EnsembleComponentData( Date cyclTime, String ensCompsStr) {
		
		seldRefTime = new Date( cyclTime.getTime() );
		
		this.ensCompsStringForRefTime = ensCompsStr;

		if (ensCompsStr == null)
			ensCompsList = null;
		
		int firstIdx = ensCompsStringForRefTime.indexOf('{');
		int lastIdx = ensCompsStringForRefTime.indexOf('}');
		//firstIdx = ( firstIdx == -1 ? 0 : firstIdx);
		lastIdx = ( lastIdx == -1 ? ensCompsStringForRefTime.length() : lastIdx);
		
		String listStr = ensCompsStringForRefTime.substring(firstIdx+1,  lastIdx);
		String list[] = listStr.split(",");
		
		ensCompsList = new ArrayList<EnsComp>();
		for(int i = 0; i < list.length; i++) {
			String modelName=null, cycleStr=null;
			int weight = -1;
			
			if (!list[i].contains("|") && !list[i].contains("%")) {
				modelName = list[i].toString().trim();
			}
			else if (!list[i].contains("|") && list[i].contains("%")) {
				String[] tmp = list[i].trim().split("%");
				weight = Integer.valueOf(tmp[0]);
				modelName = tmp[1].trim();
			}
			else if (list[i].contains("|") && !list[i].contains("%")) {
				int index = list[i].indexOf('|');
				modelName = list[i].substring(0, index).trim();
				cycleStr = list[i].substring(index+1).trim();
			}
			else if (list[i].contains("|") && list[i].contains("%")) {
				String[] tmp = list[i].trim().split("%");
				weight = Integer.valueOf(tmp[0]);
				
				int index = tmp[1].indexOf('|');
				modelName = tmp[1].substring(0, index).trim();
				cycleStr = tmp[1].substring(index+1).trim();
			}
			// If 'CYC' is not given then assume the selected cycletime (ie 0 offset)
			long cyclTimeMs = seldRefTime.getTime();
			
			int cycIndx = (cycleStr == null ? -1 : cycleStr.indexOf("CYC") );
			
			if( cycIndx == -1 || cycleStr.indexOf("-" ) == -1 ) {
//				System.out.println("Warning parsing Ens cycle time for " + cycleStr+", expecting 'CYC'" );				
			}
			else {
				cycIndx = cycleStr.substring( cycIndx ).indexOf("-");
				int cycleOffsetHrs = Integer.parseInt( cycleStr.substring( cycleStr.indexOf("-")+1 ) );
				cyclTimeMs = cyclTimeMs - (cycleOffsetHrs*60*60*1000);
			}

			ensCompsList.add( 
					new EnsComp( modelName, weight, 
								     new Date( cyclTimeMs ), (i == 0) ) );
		}
	}
	
	// 
	public ArrayList<String> getEnsembleMembersForModel( String modelName ) {
		ArrayList<String> ensMembers = new ArrayList<String>();

		for( EnsComp ensComp : ensCompsList ) {
			if( ensComp.getModelName().equals( modelName ) ) {
				if( ensComp.getEnsembleMemberName() != null ) {
					ensMembers.add( ensComp.getEnsembleMemberName() );
				}
				else {
					ensMembers.add( ensComp.getModelName() );
				}
			}
		}
		return ensMembers;
	}
	
	// just the model name and not the ensComp name (ie. members not included)
	
//	public String getPrimaryModel( ) {		
//		for( EnsComp ec : ensCompsList ) {
//			if( ec.isFirst ) {
//				return ec.getModelName();
//			}
//		}
//		return "";
//	}
	
	public void setModelAsPrimary( String model ) {
//		for( EnsComp ec : ensCompsList ) {
//			if( model.equals( ec.getModelName() ) ) {
//				ec.setFirst( true );
//			}
//			else if( ec.isFirst() ) {
//				ec.setFirst( false );
//			}
//		}		
	}
	
	public void reset() {
		ensCompsList = new ArrayList<EnsComp>();
		ensCompsStringForRefTime = null;
	}
	
	public List<EnsComp> getEnsembleComponentsList() {
		return ensCompsList;
	}
				
	// create both the ensCompsStringsForUnreferencedTimes, and ensCompsStringForRefTime
	// strings used 
	public void setSelectedModelStrings() {
		
		if (ensCompsList == null) {
			ensCompsStringForRefTime = null;
			ensCompsStringsForUnreferencedTimes = null;
			return;
		}
		
		/*
		 * Find the 'First' model
		 */
		String firstModel = null;
		String firstModelUnresolved = null;
		
		int firstIndex = -1;
		for (int i = 0; i < ensCompsList.size(); i++) {
			if (ensCompsList.get(i).isFirst()) {
			
				Date cycleTime = ensCompsList.get(i).getCycleTime();
				String resolvedCyclStr = getCycleTimeStrFromDataTime( cycleTime );
				String unresolvedCyclStr =  getRelativeCycleTimeString( seldRefTime, cycleTime );
					
				if (ensCompsList.get(i).weight > -1 && cycleTime != null) {
					firstModel = String.valueOf(ensCompsList.get(i).weight) + "%" + ensCompsList.get(i).getEnsCompName() +
						"|" + resolvedCyclStr;
					firstModelUnresolved = String.valueOf(ensCompsList.get(i).weight) + "%" + ensCompsList.get(i).getEnsCompName() +
						"|" + unresolvedCyclStr;
				}
				else if (ensCompsList.get(i).weight == -1 && cycleTime != null) {
					firstModel = ensCompsList.get(i).getEnsCompName() + "|" + resolvedCyclStr;
					firstModelUnresolved = ensCompsList.get(i).getEnsCompName() + "|" + unresolvedCyclStr;
				} 
				else {
					firstModel = ensCompsList.get(i).getEnsCompName();
					firstModelUnresolved = ensCompsList.get(i).getEnsCompName();
				}
				
				firstIndex = i;
				break;
			}
		}
		
		/*
		 * Other models
		 */
		ensCompsStringForRefTime = "{";
		ensCompsStringsForUnreferencedTimes = "{";
		
		if (firstModel != null) { 
			ensCompsStringForRefTime = ensCompsStringForRefTime + firstModel;
			ensCompsStringsForUnreferencedTimes = ensCompsStringsForUnreferencedTimes + firstModelUnresolved;
		}
		
		for (int i = 0; i < ensCompsList.size(); i++) {
			if (i == firstIndex)  // why not just check !isFirst()....
				continue;
			
			String model = null, unresolvedModel = null;
			Date cycleTime = ensCompsList.get(i).getCycleTime();
			String resolvedCyclStr = getCycleTimeStrFromDataTime( cycleTime );
			String unresolvedCyclStr =  getRelativeCycleTimeString( seldRefTime, cycleTime );

			// first set the 'resolved' cycle time string
			//String cyclStr = ( ensCompsList.get(i).getCycleTime() );
			
			if (ensCompsList.get(i).weight > -1 && cycleTime != null) {
				model = String.valueOf(ensCompsList.get(i).weight) + "%" + ensCompsList.get(i).getEnsCompName() +
				        "|" + resolvedCyclStr;
				unresolvedModel = String.valueOf(ensCompsList.get(i).weight) + "%" + ensCompsList.get(i).getEnsCompName() +
						"|" + unresolvedCyclStr;
			}
			else if (ensCompsList.get(i).weight == -1 && cycleTime != null) {
				model = ensCompsList.get(i).getEnsCompName() + "|" + resolvedCyclStr;
				unresolvedModel = ensCompsList.get(i).getEnsCompName() + "|" + unresolvedCyclStr;
			} 
			else {
				model = ensCompsList.get(i).getEnsCompName();
				unresolvedModel = ensCompsList.get(i).getEnsCompName();
			}
			
			if (ensCompsStringForRefTime.endsWith("{")) {
				ensCompsStringForRefTime = ensCompsStringForRefTime + model;
				ensCompsStringsForUnreferencedTimes = ensCompsStringsForUnreferencedTimes + unresolvedModel;
			}
			else {
				ensCompsStringForRefTime = ensCompsStringForRefTime + ","+model;
				ensCompsStringsForUnreferencedTimes = ensCompsStringsForUnreferencedTimes + ","+unresolvedModel;
			}			
		}
		ensCompsStringForRefTime = ensCompsStringForRefTime + "}";
		ensCompsStringsForUnreferencedTimes = ensCompsStringsForUnreferencedTimes + "}";
	}
	
	
	public String  getEnsCompsStringWithRelativeCycTimes() {
		return ensCompsStringsForUnreferencedTimes;
	}
	
	public String  getEnsCompsStringForRefTime() {
		return ensCompsStringForRefTime;
	}

	public void addModel(String name, int wt, Date cyc, boolean first) {
		EnsComp model = new EnsComp(name, wt, cyc, first);
		ensCompsList.add(model);
	}
	
	public static String getRelativeCycleTimeString( Date refTime, Date cycleTime ) {
		if( refTime == null || cycleTime == null ) {
			return null;
		}
		
		long hrsDiff = (refTime.getTime() - cycleTime.getTime())/60/60/1000;
		
		if( hrsDiff == 0 ) {
			return "CYC";
		}
		else {
			return "CYC-"+Long.toString( hrsDiff );
		}
	}
	
	public static  String getCycleTimeStrFromDataTime( Date dt ) {

		if( dt == null ) {
			return null;
		}
		
    	NumberFormat nf = NumberFormat.getInstance();
    	nf.setMinimumIntegerDigits(2);
    	nf.setMinimumFractionDigits(0);
    	nf.setMaximumFractionDigits(2);
    	nf.setMaximumIntegerDigits(2);

    	Calendar cal = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
    	cal.setTime(dt);//.getRefTime());
    	String dd = nf.format(cal.get(Calendar.DAY_OF_MONTH));
    	String hh = nf.format(cal.get(Calendar.HOUR_OF_DAY));

    	String cycleTimeStr = String.format("%s/%s", dd, hh);
    	return cycleTimeStr;
    }
}
