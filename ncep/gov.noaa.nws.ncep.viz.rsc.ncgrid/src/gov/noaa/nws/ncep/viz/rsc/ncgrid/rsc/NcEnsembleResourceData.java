package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv.GridDBConstants;

import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Resource data for Ensemble grids
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/13/2011               G Hull     Created.
 * 04/02/2012   #606        G Hull     added primaryModel for Ensem
 *  09/11/2012   #743    Archana   Added CLRBAR            
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-EnsembleResourceData")
public class NcEnsembleResourceData extends NcgridResourceData {

//	@XmlElement
//	protected String primaryModel;

	@XmlElement
	protected String availableModels; // comma separated string

	// like GDFILE except with relative cycle times
	// 
	@XmlElement 
	protected String ensembleComponentWeights;
	
	public NcEnsembleResourceData() {
        super();
    }

	@Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
		
    	return new NcgridResource(this, loadProperties);
    }
	
	// the primary model is stored in the metadata map.
	//
	public String getPrimaryModel() {
		HashMap<String, RequestConstraint> reqConstraints  = getMetadataMap();
		if( reqConstraints.containsKey(GridDBConstants.MODEL_NAME_QUERY) ) {
			return reqConstraints.get(GridDBConstants.MODEL_NAME_QUERY).getConstraintValue(); 
    	}
		else {
			return null;
		}
	}

	public void setPrimaryModel(String primaryModel) {
		getMetadataMap().put(GridDBConstants.MODEL_NAME_QUERY, 
				new RequestConstraint( primaryModel, ConstraintType.EQUALS ) );
	}

    public String getAvailableModels() {
    	// TODO : if the primaryModel is not in the list, then add it.
    	//
		return availableModels;
	}

	public void setAvailableModels(String availableModels) {
		this.availableModels = availableModels;
	}

	public String getEnsembleComponentWeights() {
		return ensembleComponentWeights;
	}

	public void setEnsembleComponentWeights(String ensembleComponentWeights) {
		this.ensembleComponentWeights = ensembleComponentWeights;
	}

	
// Not sure if we need to override this or not?
//	
//	@Override
//	public DataTime[] getAvailableTimes() throws VizException {
//
//		if ( getPluginName().equalsIgnoreCase( GempakGrid.gempakPluginName )) {
//			try {
//				String currentCycle = getResourceName().getCycleTime().toString();
//				String dataLoc = null;
//				try {
//					dataLoc = GempakGrid.getGempakGridPath( getGdfile() );
//
//				} catch (VizException e) {
//					throw new VizException (e);
//				}
//				String []  gridAvailableTimes = GempakGrid
//				.getAvailableGridTimes(dataLoc, currentCycle);
//				DataTime[] availableTimes = new DataTime[gridAvailableTimes.length];
//
//				for ( int ii=0; ii<gridAvailableTimes.length; ii++) {
//					availableTimes[ii] = new DataTime (gridAvailableTimes[ii]);
//				}
//				return availableTimes;
//			} catch (Exception e) {
//				// TODO Auto-generated catch block
//				throw new VizException();
//			}
//		}
//		else {
//			DataTime[] availableTimes = super.getAvailableTimes();
//			return availableTimes;
//		}
//}

	public ArrayList<String> getAvailEnsembleComponents() {
		ArrayList<String> availEnsCompsList = new ArrayList<String>();
		
		if( availableModels != null ) {			
			String[] ensCompStrs = availableModels.split(";");
			if( ensCompStrs.length > 0 ) {
				for( int ec=0 ; ec<ensCompStrs.length ; ec++ ) {
					int indx = ensCompStrs[ec].indexOf(":");
					if( indx == -1 ) {
						availEnsCompsList.add( ensCompStrs[ec] );	
					}
					else { // if ens members
						String modelName = ensCompStrs[ec].substring(0,indx);
						String[] memStrs = ensCompStrs[ec].substring(indx+1,
																ensCompStrs[ec].length()).split(",");
						for( String memStr : memStrs ) {
							availEnsCompsList.add( modelName+":"+memStr );
						}
					}
				}
			}
		}
		
		return availEnsCompsList;
	}
	
    // set metadataMap with the primary modelName constraint and return it
	// 
/**************************** This is now set in the resource template with the primaryModel parameter
	@Override
	public HashMap<String, RequestConstraint> getMetadataMap() {
    	
		HashMap<String, RequestConstraint> queryList = super.getMetadataMap();

		DataTime cycleTime = getResourceName().getCycleTime();

		EnsembleComponentData ensCompData = new EnsembleComponentData( 
					cycleTime.getRefTime(), getEnsembleComponentWeights() );
		
		String primModel = ensCompData.getPrimaryModel();
		
		// TODO : we could add code to check for ensemble members and add a constraint
		// on the eventName but the times should be the same.
    	if( primModel != null ) {
    		queryList.put("modelInfo.modelName", 
    				new RequestConstraint( primModel, ConstraintType.EQUALS ) );
    	}

		return queryList;	
//  previously this code was ...    	
//		if( getGdfile().startsWith("{") && getGdfile().endsWith("}")) {
//			ModelListInfo modelListInfo = new ModelListInfo( getGdfile());
//        	String modelName = modelListInfo.getModelList().get(0).getModelName().toUpperCase();
//        	getMetadataMap().put("modelInfo.modelName", 
//                		new RequestConstraint( modelName, ConstraintType.EQUALS ) );
//		}
    }
**************************/
	public void createGdFile() {
		// use the selected cycle time to update the ensembleComponentWeights
		// NOTE: The primary used to be specified by putting it first in the list
		// but now it is stored in the resource defn. However, the legacy code is still 
		// expecting a GDFILE with the primary first. 
		//
		DataTime cycleTime = getResourceName().getCycleTime();
		
		EnsembleComponentData ensCompData = new EnsembleComponentData( 
				cycleTime.getRefTime(), getEnsembleComponentWeights() );
		ensCompData.setSelectedModelStrings();
		
		ensCompData.setModelAsPrimary( getPrimaryModel() );
		// testing 
		//getEnsembleMembersForModel( "GEFS" );
		super.setGdfile( ensCompData.getEnsCompsStringForRefTime() );
	}
	
	// not what EnsembleComponentData was originally created for, but seems to work ok.
	//
	public ArrayList<String> getEnsembleMembersForModel( String modelName ) {
		DataTime cycleTime = getResourceName().getCycleTime();		
		EnsembleComponentData ensCompData = new EnsembleComponentData( 
				cycleTime.getRefTime(), getEnsembleComponentWeights() );
		
		ArrayList<String> ensMembers = ensCompData.getEnsembleMembersForModel( modelName );

		// the modelname w/o a member indicates that all members are represented
		//
		if( ensMembers.contains( modelName ) ) {
			ensMembers.clear();
			ArrayList<String> availEnsComps = getAvailEnsembleComponents();
			for( String ensComp : availEnsComps ) {
				int indx = ensComp.indexOf(":");				
				if( indx != -1 ) {
					String model = ensComp.substring(0,indx);
					if( model.equals( modelName ) ) {
						ensMembers.add( ensComp.substring( indx+1, ensComp.length() ) );
					}
				}
			}
		}
		
		return ensMembers;
	}
    // Getter/Setters for the Attributes
    // These need to be here because the code that accesses the attributes using 
    // reflection was not written to check for getter/setters in super classes.
    //
	public String getGdfile() {
		createGdFile();
		return gdfile;
	}

	public void setGdfile(String gdfile) {
		super.setGdfile(gdfile);
	}

	public String getCint() {
		return super.getCint();
	}

	public void setCint(String cint) {
		super.setCint(cint);
	}

	public String getGvcord() {
		return super.getGvcord();
	}

	public void setGvcord(String gvcord) {
		super.setGvcord(gvcord);
	}

	public String getGlevel() {
		return super.getGlevel();
	}

	public void setGlevel(String glevel) {
		super.setGlevel(glevel);
	}


	public String getGdpfun() {
		return super.getGdpfun();
	}

	public void setGdpfun(String gdpfun) {
		super.setGdpfun(gdpfun);
	}

	public String getSkip() {
		return super.getSkip();
	}

	public void setSkip(String skip) {
		super.setSkip(skip);
	}

	public String getFilter() {
		return super.getFilter();
	}

	public void setFilter(String filter) {
		super.setFilter(filter);
	}

	
	public String getScale() {
		return super.getScale();
	}

	public void setScale(String scale) {
		super.setScale(scale);
	}

	public String getTitle() {
		return super.getTitle();
	}

	public void setTitle(String title) {
		super.setTitle(title);
	}
	
	public String getType() {
		return super.getType();
	}

	public void setType(String type) {
		super.setType(type);
	}

	public String getLineAttributes() {
		return super.getLineAttributes();
	}

	public void setLineAttributes(String lineAttributes) {
		super.setLineAttributes(lineAttributes);
	}

	public String getColors() {
		return super.getColors();
	}

	public void setColors(String colors) {
		super.setColors(colors);
	}

	public String getMarker() {
		return super.getMarker();
	}

	public void setMarker(String marker) {
		super.setMarker(marker);
	}

	public int getGrdlbl() {
		return super.getGrdlbl();
	}

	public void setGrdlbl(int grdlbl) {
		super.setGrdlbl(grdlbl);
	}

	public String getFint() {
		return super.getFint();
	}

	public void setFint(String fint) {
		super.setFint(fint);
	}

	public String getFline() {
		return super.getFline();
	}

	public void setFline(String fline) {
		super.setFline(fline);
	}

	public String getWind() {
		return super.getWind();
	}

	public void setWind(String wind) {
		super.setWind(wind);
	}
	
	public String getHilo() {
		return super.getHilo();
	}

	public void setHilo(String hilo) {
		super.setHilo(hilo);
	}
	
	public String getHlsym() {
		return super.getHlsym();
	}

	public void setHlsym(String hlsym) {
		super.setHlsym(hlsym);
	}    
	
	
	public String getClrbar() {
		return super.getClrbar();
	}

	public void setClrbar(String clrbar) {
		super.setClrbar(clrbar);
	} 
	
    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof NcEnsembleResourceData == false) {
            return false;
        }

        NcEnsembleResourceData other = (NcEnsembleResourceData) obj;
        
        if (this.availableModels != null && other.availableModels == null) {
            return false;
        } else if (this.availableModels == null && other.availableModels != null) {
            return false;
        } else if (this.availableModels != null
                && this.availableModels.equals(other.availableModels) == false) {
            return false;
        }
        
        if (this.ensembleComponentWeights != null && other.ensembleComponentWeights == null) {
            return false;
        } else if (this.ensembleComponentWeights == null && other.ensembleComponentWeights != null) {
            return false;
        } else if (this.ensembleComponentWeights != null
                && this.ensembleComponentWeights.equals(other.ensembleComponentWeights) == false) {
            return false;
        }

        return true;
    }

}
