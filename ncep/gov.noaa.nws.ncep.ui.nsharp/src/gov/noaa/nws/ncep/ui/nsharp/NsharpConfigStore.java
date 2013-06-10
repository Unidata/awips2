/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore
 * 
 * This java class performs the surface station locator functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/13/2012   			Chin Chen	Initial coding
 * 										
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "NsharpConfigStore")
@XmlAccessorType(XmlAccessType.NONE)
public class NsharpConfigStore implements ISerializableObject{

	
	
	public NsharpConfigStore() {
		super();
		//set linePropertyMap default values
		int i =0;
		for(String lnName: NsharpConstants.lineNameArray){
			NsharpLineProperty lp = NsharpConstants.defaultLineProperty[i];
			linePropertyMap.put(lnName, lp);
			i++;
		}
	}
	/*
	 * #1 When new development adding parameters, existing configuration xml may not have such parameters. Add defaults to it.
	 * OR, 
	 * #2 if an old parameter is not supported anymore, remove it from map.
	 */
	public void upToDateLinePropertyMap (){
		//#1 job
		int i =0;
		for(String lnName: NsharpConstants.lineNameArray){
			if(linePropertyMap.get(lnName) == null){
				NsharpLineProperty lp = NsharpConstants.defaultLineProperty[i];
			
				linePropertyMap.put(lnName, lp);
			}
			i++;
		}
		// #2 job
		List<String> deleteLsit = new ArrayList<String>();
		for(String key: linePropertyMap.keySet()){
			boolean found=false;
			for(String lnName: NsharpConstants.lineNameArray){
				if(key.equals(lnName)){
					found= true;
					break;
				}
			}
			if(found==false){
				deleteLsit.add(key);
			}
		}
		for(String key: deleteLsit){
			linePropertyMap.remove(key);
		}
	}
	@XmlElement
	private NsharpGraphProperty graphProperty = new NsharpGraphProperty();
	
	@XmlElement
	@XmlJavaTypeAdapter(value = NsharpConfigHashMapAdaptor.class)
	private HashMap<String, NsharpLineProperty> linePropertyMap = new HashMap<String, NsharpLineProperty>();
	
	@XmlElement
	NsharpDataPageProperty dataPageProperty = new NsharpDataPageProperty();
	
	public HashMap<String, NsharpLineProperty> getLinePropertyMap() {
		return linePropertyMap;
	}

	public void setLinePropertyMap(
			HashMap<String, NsharpLineProperty> linePropertyMap) {
		this.linePropertyMap = linePropertyMap;
				
	}

	public NsharpGraphProperty getGraphProperty() {
		return graphProperty;
	}

	public void setGraphProperty(NsharpGraphProperty graphProperty) {
		this.graphProperty = graphProperty;
	}

	public NsharpDataPageProperty getDataPageProperty() {
		return dataPageProperty;
	}

	public void setDataPageProperty(NsharpDataPageProperty dataPageProperty) {
		this.dataPageProperty = dataPageProperty;
	}
}
