package gov.noaa.nws.ncep.viz.common.ui;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;

public class ModelListInfo {

	private String modelListStr = null;
	
	public class ModelInfo {
		private String modelName;
		private int weight = -1;
		private String cycle = null;
		boolean isFirst;
		
		ModelInfo(String name, int wt, String cyc, boolean first) {
			modelName = name;
			weight = wt;
			cycle = cyc;
			isFirst = first;
		}
		
		ModelInfo(){}

		public String getModelName() {
			return modelName;
		}

		public void setModelName(String modelName) {
			this.modelName = modelName;
		}

		public int getWeight() {
			return weight;
		}

		public void setWeight(int weight) {
			this.weight = weight;
		}

		public String getCycle() {
			return cycle;
		}

		public void setCycle(String cycle) {
			this.cycle = cycle;
		}

		public boolean isFirst() {
			return isFirst;
		}

		public void setFirst(boolean isFirst) {
			this.isFirst = isFirst;
		};
		
	}
	
	protected List<ModelInfo> modelInfoList;
	
	public ModelListInfo (String modelListString) {
		this.modelListStr = modelListString;
		SetModelInfoList(modelListString);
	}
	
	public ModelListInfo (boolean update) {
		if (update) modelInfoList = new ArrayList<ModelInfo>();
	}
	
	public ModelListInfo () {
	}
	
	public void setModelListString(String modelList) {
		this.modelListStr = modelList;
	}
	
	public List<ModelInfo> getModelList() {
		return modelInfoList;
	}
	
	public void SetModelInfoList(String str) {
		if (str == null) modelInfoList = null;
		int firstIdx = str.indexOf('{');
		int lastIdx = str.indexOf('}');
		String listStr = str.substring(firstIdx+1, lastIdx);
		String list[] = listStr.split(",");
		
		modelInfoList = new ArrayList<ModelInfo>();
		for(int i = 0; i < list.length; i++) {
			ModelInfo aModel = new ModelInfo();
			if (!list[i].contains("|") && !list[i].contains("%")) {
				aModel.modelName = list[i].toString().trim();
			}
			else if (!list[i].contains("|") && list[i].contains("%")) {
				String[] tmp = list[i].trim().split("%");
				aModel.weight = Integer.valueOf(tmp[0]);
				aModel.modelName = tmp[1].trim();
			}
			else if (list[i].contains("|") && !list[i].contains("%")) {
				int index = list[i].indexOf('|');
				aModel.modelName = list[i].substring(0, index).trim();
				aModel.cycle = list[i].substring(index+1);
			}
			else if (list[i].contains("|") && list[i].contains("%")) {
				String[] tmp = list[i].trim().split("%");
				aModel.weight = Integer.valueOf(tmp[0]);
				
				int index = tmp[1].indexOf('|');
				aModel.modelName = tmp[1].substring(0, index).trim();
				aModel.cycle = tmp[1].substring(index+1);
			}
			
			aModel.isFirst = (i == 0) ? true : false;
			
			modelInfoList.add(aModel);
		}
	}
	
	public void setSelectedModelString() {
		
		if (modelInfoList == null) {
			modelListStr = null;
			return;
		}
		
		/*
		 * Find the 'First' model
		 */
		String firstModel = null;
		int firstIndex = -1;
		for (int i = 0; i < modelInfoList.size(); i++) {
			if (modelInfoList.get(i).isFirst()) {
				if (modelInfoList.get(i).weight > -1 && modelInfoList.get(i).cycle != null) {
					firstModel = String.valueOf(modelInfoList.get(i).weight) + "%" + modelInfoList.get(i).getModelName() +
					"|" + modelInfoList.get(i).cycle;
				}
				else if (modelInfoList.get(i).weight == -1 && modelInfoList.get(i).cycle != null) {
					firstModel = modelInfoList.get(i).getModelName() + "|" + modelInfoList.get(i).cycle;
				} 
				else {
					firstModel = modelInfoList.get(i).getModelName();
				}
				firstIndex = i;
				break;
			}
		}
		
		/*
		 * Other models
		 */
		modelListStr = "{";
		if (firstModel != null) modelListStr = modelListStr + firstModel;
		
		for (int i = 0; i < modelInfoList.size(); i++) {
			if (i == firstIndex) continue;
			String model = null;
			
			if (modelInfoList.get(i).weight > -1 && modelInfoList.get(i).cycle != null) {
				model = String.valueOf(modelInfoList.get(i).weight) + "%" + modelInfoList.get(i).getModelName() +
				        "|" + modelInfoList.get(i).cycle;
			}
			else if (modelInfoList.get(i).weight == -1 && modelInfoList.get(i).cycle != null) {
				model = modelInfoList.get(i).getModelName() + "|" + modelInfoList.get(i).cycle;
			} 
			else {
				model = modelInfoList.get(i).getModelName();
			}
			
			if (modelListStr.endsWith("{"))
				modelListStr = modelListStr + model;
			else
				modelListStr = modelListStr + ","+model;
			
		}
		modelListStr = modelListStr + "}";
	}
	
	public String  getSelectedModelString() {
		return modelListStr;
	}
	
	public void addModel(String name, int wt, String cyc, boolean first) {
		ModelInfo model = new ModelInfo(name, wt, cyc, first);
		modelInfoList.add(model);
	}
	
	@SuppressWarnings("null")
	public static ArrayList<String> getCycles(String modelName) {
		ArrayList<String> cyclesList = new ArrayList<String>();
		
		HashMap<String,RequestConstraint> constraintMap = new HashMap<String,RequestConstraint>();
		constraintMap.put( "pluginName", new RequestConstraint( "grid", ConstraintType.EQUALS) );
		constraintMap.put( "info.datasetId", 
        		new RequestConstraint( modelName.toUpperCase(), ConstraintType.EQUALS ) );
		
		LayerProperty property = new LayerProperty();
        property.setDesiredProduct( ResourceType.PLAN_VIEW );
        DataTime[] availableTimes;
        
        try {
			property.setEntryQueryParameters( constraintMap );
	        availableTimes = property.getEntryTimes();

//	        System.out.println("availableTimes.lenght==="+availableTimes.length);
	        
	        for( DataTime dt : availableTimes ) {
	        	NumberFormat nf = NumberFormat.getInstance();
	        	nf.setMinimumIntegerDigits(2);
	        	nf.setMinimumFractionDigits(0);
	        	nf.setMaximumFractionDigits(2);
	        	nf.setMaximumIntegerDigits(2);

	        	Calendar cal = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
	        	cal.setTime(dt.getRefTime());
	        	String dd = nf.format(cal.get(Calendar.DAY_OF_MONTH));
	        	String hh = nf.format(cal.get(Calendar.HOUR_OF_DAY));

	        	String cycleTimeStr = String.format("%s/%s", dd, hh);
	        	if (cyclesList != null) {
	        		boolean dup = false;
	        		for(int i = 0; i < cyclesList.size(); i++) {
	        			if (cyclesList.get(i).equals(cycleTimeStr)) {
	        				dup = true;
	        				break;
	        			}
	        		}
	        		if (!dup) cyclesList.add(cycleTimeStr);
	        	}
	        	else {
	        		cyclesList.add(cycleTimeStr);
	        	}
	        }
	      
        } catch (VizException e) {
			System.out.println("Error querying cycle times.");
		}
        
		
		return cyclesList;
		
	}
	/*
	public static void main(String args[]) {
		ModelListInfo m1 = new ModelListInfo("{nam}");
		List<ModelInfo> out1 = m1.getModelList();
		for (int i = 0; i < out1.size(); i++) {
			System.out.println(i+" modelName="+out1.get(i).modelName+" weight="+out1.get(i).weight+" cycle="+out1.get(i).cycle+" isFirst="+out1.get(i).isFirst);
		}
		
		System.out.println("======================================");
		m1 = new ModelListInfo("{50%gfs|00,gfs|06,gfs|12,gfs|18}");
		out1 = m1.getModelList();
		for (int i = 0; i < out1.size(); i++) {
			System.out.println(i+" modelName="+out1.get(i).modelName+" weight="+out1.get(i).weight+" cycle="+out1.get(i).cycle+" isFirst="+out1.get(i).isFirst);
		}
	}
    */	
}
