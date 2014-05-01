package gov.noaa.nws.ncep.edex.common.sounding;


import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.common.metparameters.AbstractMetParameter;
import gov.noaa.nws.ncep.edex.common.metparameters.AirTemperature;
import gov.noaa.nws.ncep.edex.common.metparameters.DewPointTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.HeightAboveSeaLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.Omega;
import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.RelativeHumidity;
import gov.noaa.nws.ncep.edex.common.metparameters.SpecificHumidity;
import gov.noaa.nws.ncep.edex.common.metparameters.TempTndncyFromAllRad;
import gov.noaa.nws.ncep.edex.common.metparameters.TempTndncyFromConvPhaseChange;
import gov.noaa.nws.ncep.edex.common.metparameters.TempTndncyFromGrdSclPhaseChange;
import gov.noaa.nws.ncep.edex.common.metparameters.TempTndncyFromLongWaveRad;
import gov.noaa.nws.ncep.edex.common.metparameters.TempTndncyFromShortWaveRad;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirection;
import gov.noaa.nws.ncep.edex.common.metparameters.WindSpeed;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirectionUComp;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirectionVComp;
//import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer.DataType;

//import java.io.Serializable;
//import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
//import com.vividsolutions.jts.geom.Coordinate;


/**
 * 
 * gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer2
 * 
 * This java class provides sounding data data structure for used with NC sounding query.
 * Each NcSoundingLayer2 contains one layer of sounding information (pressure, height, temperature, dewpoint and so on) for
 * one point (lat/lon) at a particular time (timeLine) and particular height.  
 * Each piece of this sounding information is stored as a Met parameter that has a value and a unit associated with it.
 * These Met parameters are stored in a Map and they should be accessed directly from this map. 
 * The getter/setter methods provided in this class, to access each Met parameter  are intended as a convenience only.
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	           -------- 	-----------
 *08/19/11     465                 Archana       Initial creation  
 *</pre>
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcSoundingLayer2 implements  ISerializableObject, Cloneable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 3120604609064402378L;
	
	@DynamicSerializeElement
	public static final float LEGACY_MISSING = -999.f;
	
	@DynamicSerializeElement
	public static final float MISSING = IDecoderConstantsN.UAIR_FLOAT_MISSING;/*-9999.f;
    
    /**UAIR (standard) data type definitions*/
    

	public static enum DataType {
		TTAA, TTBB, TTCC, TTDD, PPAA, PPBB, PPCC, PPDD, 
		UUAA, UUBB, UUCC, UUDD, MAXWIND_A,MAXWIND_C, TROPOPAUSE_A, TROPOPAUSE_C, ALLDATA
	}
    
    /**mapping from UAIR (standard) data type to BUFRUA data type.*/
//	@DynamicSerializeElement
	public static HashMap<String,Integer > dataTypeMap = new  HashMap<String,Integer>();
    static {
    	dataTypeMap.put(DataType.TTAA.toString(), 2020); //Mandatory, Troppause, MaxWind
    	dataTypeMap.put(DataType.TTCC.toString(), 2030); //Mandatory, Troppause, MaxWind
    	dataTypeMap.put(DataType.TTBB.toString(), 2022); //SigT
    	dataTypeMap.put(DataType.TTDD.toString(), 2032); //SigT
    	dataTypeMap.put(DataType.PPBB.toString(), 2021); //SigW
    	dataTypeMap.put(DataType.PPDD.toString(), 2031); //SigW
    	dataTypeMap.put(DataType.PPAA.toString(), 0000); //TBD, BUFRUA does not support this now
    	dataTypeMap.put(DataType.PPCC.toString(), 0000); //TBD, BUFRUA does not support this now
    	dataTypeMap.put(DataType.MAXWIND_A.toString(), 2020);
    	dataTypeMap.put(DataType.TROPOPAUSE_A.toString(), 2020);
    	dataTypeMap.put(DataType.MAXWIND_C.toString(), 2030);
    	dataTypeMap.put(DataType.TROPOPAUSE_C.toString(), 2030);
    	
    }
    
    @DynamicSerializeElement
    /**
     * A Map of the available met parameters reported in the sounding profile. Can be used to 
     * accommodate met parameters that can be derived from the parameters available in the map
     */
    Map<String, AbstractMetParameter> metParamsMap;

    private void initializeMetParamsMap()
       {
    	      
    	       try {
    	         metParamsMap.put( AirTemperature.class.getSimpleName(), new AirTemperature() );
    	         metParamsMap.put( PressureLevel.class.getSimpleName(), new PressureLevel() );
    	         metParamsMap.put( DewPointTemp.class.getSimpleName(), new DewPointTemp() );
    	         metParamsMap.put( HeightAboveSeaLevel.class.getSimpleName(), new HeightAboveSeaLevel() );
    	         metParamsMap.put( WindSpeed.class.getSimpleName(), new WindSpeed() );
    	         metParamsMap.put( WindDirection.class.getSimpleName(), new WindDirection() );
    	         metParamsMap.put( WindDirectionUComp.class.getSimpleName(), new WindDirectionUComp() );
    	         metParamsMap.put( WindDirectionVComp.class.getSimpleName(), new WindDirectionVComp() );
    	         metParamsMap.put( Omega.class.getSimpleName(), new Omega() );
    	         metParamsMap.put( RelativeHumidity.class.getSimpleName(), new RelativeHumidity() );	        
    	         metParamsMap.put( SpecificHumidity.class.getSimpleName(), new SpecificHumidity() );
      	         metParamsMap.put( TempTndncyFromAllRad.class.getSimpleName(), new TempTndncyFromAllRad() );
      	         metParamsMap.put( TempTndncyFromConvPhaseChange.class.getSimpleName(), new TempTndncyFromConvPhaseChange() );
      	         metParamsMap.put( TempTndncyFromGrdSclPhaseChange.class.getSimpleName(), new TempTndncyFromGrdSclPhaseChange() );
      	         metParamsMap.put( TempTndncyFromLongWaveRad.class.getSimpleName(), new TempTndncyFromLongWaveRad() );
      	         metParamsMap.put( TempTndncyFromShortWaveRad.class.getSimpleName(), new TempTndncyFromShortWaveRad() );
    	       }catch(Exception e ){
    	    	   System.out.println(e.getMessage());
    	       }
    }

    public NcSoundingLayer2() throws Exception{
  	    metParamsMap = new HashMap<String, AbstractMetParameter>(0);
    	initializeMetParamsMap();
    }

	/**
	 * @return the pressure
	 */
	public PressureLevel getPressure() {
//		return pressure;
		return (PressureLevel) metParamsMap.get("PressureLevel");
	}

	/**
	 * @param pressure the pressure to set
	 */
	public void setPressure(PressureLevel pressure) {
//		this.pressure = pressure;
		  this.metParamsMap.put("PressureLevel", pressure);
	}

	/**
	 * @return the temperature
	 */
	public AirTemperature getTemperature() {
//		return temperature;
		return ( AirTemperature ) metParamsMap.get("AirTemperature");
	}

	/**
	 * @param temperature the temperature to set
	 */
	public void setTemperature(AirTemperature temperature) {
//		this.temperature = temperature;
		this.metParamsMap.put("AirTemperature", temperature);
	}

	/**
	 * @return the dewpoint
	 */
	public DewPointTemp getDewpoint() {
//		return dewpoint;
		return ( DewPointTemp ) metParamsMap.get("DewPointTemp");		
	}

	/**
	 * @param dewpoint the dewpoint to set
	 */
	public void setDewpoint(DewPointTemp dewpoint) {
//		this.dewpoint = dewpoint;
		this.metParamsMap.put("DewPointTemp", dewpoint);
	}

	/**
	 * @return the windSpeed
	 */
	public WindSpeed getWindSpeed() {
		return ( WindSpeed ) metParamsMap.get("WindSpeed");	
//		return windSpeed;
	}

	/**
	 * @param windSpeed the windSpeed to set
	 */
	public void setWindSpeed(WindSpeed windSpeed) {
//		this.windSpeed = windSpeed;
		this.metParamsMap.put("WindSpeed", windSpeed);
	}

	/**
	 * @return the windDirection
	 */
	public WindDirection getWindDirection() {
//		return windDirection;
		return ( WindDirection ) metParamsMap.get("WindDirection");	
	}

	/**
	 * @param windDirection the windDirection to set
	 */
	public void setWindDirection(WindDirection windDirection) {
//		this.windDirection = windDirection;
		this.metParamsMap.put("WindDirection", windDirection);
	}

	/**
	 * @return the windU
	 */
	public WindDirectionUComp getWindU() {
//		return windU;
		return ( WindDirectionUComp ) metParamsMap.get("WindDirectionUComp");	
	}

	/**
	 * @param windU the windU to set
	 */
	public void setWindU(WindDirectionUComp windU) {
//		this.windU = windU;
		this.metParamsMap.put("WindDirectionUComp", windU);
	}

	/**
	 * @return the windV
	 */
	public WindDirectionVComp getWindV() {
//		return windV;
		return ( WindDirectionVComp ) metParamsMap.get("WindDirectionVComp");	
	}

	/**
	 * @param windV the windV to set
	 */
	public void setWindV(WindDirectionVComp windV) {
//		this.windV = windV;
		this.metParamsMap.put("WindDirectionVComp", windV);
	}

	/**
	 * @return the geoHeight
	 */
	
	public HeightAboveSeaLevel getGeoHeight() {
//		return geoHeight;
		return ( HeightAboveSeaLevel ) metParamsMap.get("HeightAboveSeaLevel");
	}

	/**
	 * @param geoHeight the geoHeight to set
	 */
	public void setGeoHeight(HeightAboveSeaLevel geoHeight) {
//		this.geoHeight = geoHeight;
		this.metParamsMap.put("HeightAboveSeaLevel", geoHeight);
	}

	/**
	 * @return the omega
	 */
	public Omega getOmega() {
//		return omega;
		return ( Omega ) metParamsMap.get("Omega");
	}

	/**
	 * @param omega the omega to set
	 */
	public void setOmega(Omega omega) {
//		this.omega = omega;
		this.metParamsMap.put("Omega", omega);
	}

	/**
	 * @return the realtiveHumidity
	 */
	public RelativeHumidity getRelativeHumidity() {
//		return relativeHumidity;
		return ( RelativeHumidity ) metParamsMap.get("RelativeHumidity");
	}

	/**
	 * @param realtiveHumidity the realtiveHumidity to set
	 */
	public void setRelativeHumidity(RelativeHumidity relativeHumidity) {
//		this.relativeHumidity = realtiveHumidity;
		this.metParamsMap.put("RelativeHumidity", relativeHumidity);
	}

	/**
	 * @return the specificHumidity
	 */
	public SpecificHumidity getSpecificHumidity() {
//		return specificHumidity;
		return ( SpecificHumidity ) metParamsMap.get("SpecificHumidity");
	}

	/**
	 * @param specificHumidity the specificHumidity to set
	 */
	public void setSpecificHumidity(SpecificHumidity specificHumidity) {
//		this.specificHumidity = specificHumidity;
		this.metParamsMap.put("SpecificHumidity", specificHumidity);
	}

	/**
	 * @return the temperatureTendencyFromAllRadiation
	 */
	public TempTndncyFromAllRad getTemperatureTendencyFromAllRadiation() {
//		return temperatureTendencyFromAllRadiation;
		return ( TempTndncyFromAllRad ) metParamsMap.get("TempTndncyFromAllRad");
	}

	/**
	 * @param temperatureTendencyFromAllRadiation the temperatureTendencyFromAllRadiation to set
	 */
	public void setTemperatureTendencyFromAllRadiation(
			TempTndncyFromAllRad temperatureTendencyFromAllRadiation) {
//		this.temperatureTendencyFromAllRadiation = temperatureTendencyFromAllRadiation;
		this.metParamsMap.put("TempTndncyFromAllRad", temperatureTendencyFromAllRadiation);
	}

	/**
	 * @return the temperatureTendencyFromConvPhaseChange
	 */
	public final TempTndncyFromConvPhaseChange getTemperatureTendencyFromConvPhaseChange() {
//		return temperatureTendencyFromConvPhaseChange;
		return ( TempTndncyFromConvPhaseChange ) metParamsMap.get("TempTndncyFromConvPhaseChange");
	}
	/**
	 * @param temperatureTendencyFromConvPhaseChange the temperatureTendencyFromConvPhaseChange to set
	 */
	public final void setTemperatureTendencyFromConvPhaseChange(
			TempTndncyFromConvPhaseChange temperatureTendencyFromConvPhaseChange) {
//		this.temperatureTendencyFromConvPhaseChange = temperatureTendencyFromConvPhaseChange;
		this.metParamsMap.put("TempTndncyFromConvPhaseChange", temperatureTendencyFromConvPhaseChange);
	}
	/**
	 * @return the temperatureTendencyFromGrdSclPhaseChange
	 */
	public final TempTndncyFromGrdSclPhaseChange getTemperatureTendencyFromGrdSclPhaseChange() {
//		return temperatureTendencyFromGrdSclPhaseChange;
		return ( TempTndncyFromGrdSclPhaseChange ) metParamsMap.get("TempTndncyFromGrdSclPhaseChange");
	}
	/**
	 * @param temperatureTendencyFromGrdSclPhaseChange the temperatureTendencyFromGrdSclPhaseChange to set
	 */
	public final void setTemperatureTendencyFromGrdSclPhaseChange(
			TempTndncyFromGrdSclPhaseChange temperatureTendencyFromGrdSclPhaseChange) {
//		this.temperatureTendencyFromGrdSclPhaseChange = temperatureTendencyFromGrdSclPhaseChange;
		this.metParamsMap.put("TempTndncyFromGrdSclPhaseChange", temperatureTendencyFromGrdSclPhaseChange);
	}
	/**
	 * @return the temperatureTendencyFromLongWaveRad
	 */
	public final TempTndncyFromLongWaveRad getTemperatureTendencyFromLongWaveRad() {
//		return temperatureTendencyFromLongWaveRad;
		return ( TempTndncyFromLongWaveRad ) metParamsMap.get("TempTndncyFromLongWaveRad");
	}
	/**
	 * @param temperatureTendencyFromLongWaveRad the temperatureTendencyFromLongWaveRad to set
	 */
	public final void setTemperatureTendencyFromLongWaveRad(
			TempTndncyFromLongWaveRad temperatureTendencyFromLongWaveRad) {
//		this.temperatureTendencyFromLongWaveRad = temperatureTendencyFromLongWaveRad;
		this.metParamsMap.put("TempTndncyFromLongWaveRad", temperatureTendencyFromLongWaveRad);	
	}
	/**
	 * @return the temperatureTendencyFromShortWaveRad
	 */
	public final TempTndncyFromShortWaveRad getTemperatureTendencyFromShortWaveRad() {
//		return temperatureTendencyFromShortWaveRad;
		return ( TempTndncyFromShortWaveRad ) metParamsMap.get("TempTndncyFromShortWaveRad");
	}
	/**
	 * @param temperatureTendencyFromShortWaveRad the temperatureTendencyFromShortWaveRad to set
	 */
	public final void setTemperatureTendencyFromShortWaveRad(
			TempTndncyFromShortWaveRad temperatureTendencyFromShortWaveRad) {
//		this.temperatureTendencyFromShortWaveRad = temperatureTendencyFromShortWaveRad;
		this.metParamsMap.put("TempTndncyFromShortWaveRad", temperatureTendencyFromShortWaveRad);	
	}
	/**
	 * @return the metParamsMap
	 */
	public final Map<String, AbstractMetParameter> getMetParamsMap() {
		return metParamsMap;
	}
	/**
	 * @param metParamsMap the metParamsMap to set
	 */
	public final void setMetParamsMap(
			Map<String, AbstractMetParameter> metParamsMap) {
		this.metParamsMap = metParamsMap;
	}
    
}

















