package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.SurfaceEquivPotentialTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.SurfacePotentialTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

/**
 * Maps to the GEMPAK parameter LTMP
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


 public class LiftedSurfaceAirTempAt500mb extends AbstractMetParameter
 implements javax.measure.quantity.Temperature, ISerializableObject {

		/**
	 * 
	 */
	private static final long serialVersionUID = -6642535132031119396L;

		public LiftedSurfaceAirTempAt500mb() {
			super( UNIT );
		}

		@DeriveMethod
		public LiftedSurfaceAirTempAt500mb derive(SurfacePotentialTemp pt, SurfaceEquivPotentialTemp eqt ) throws InvalidValueException, NullPointerException  {
			if( pt.hasValidValue() && eqt.hasValidValue() ) {
                 /*The pressure is hard-coded as 500 mb, since legacy hard-codes the pressure
                  * value as 500 mb as well*/
				Amount val = PRLibrary.prLtmp(pt, eqt, 
						new Amount( new Double(500.0), NcUnits.MILLIBAR ) ); // the pressureLevel
				this.setValue(val);
			}
			else {
				setValueToMissing();
			}
			return this;
		} 

		/****  The methods below may apply for a generalized LiftedSurfaceAirTemp parameter
		@DeriveMethod		
		public LiftedSurfaceAirTempAt500mb derive(SurfacePotentialTemperature pt, SurfaceEquivPotentialTemp eqt, PressureLevel p) throws InvalidValueException, NullPointerException  {
			Amount val = PRLibrary.prLtmp(pt, eqt, p );
			this.setValue(val);
			return this;
		}
		
		@DeriveMethod		
		public LiftedSurfaceAirTempAt500mb derive(SurfacePotentialTemperature pt, SurfaceEquivPotentialTemp eqt, SurfacePressure p) throws InvalidValueException, NullPointerException  {
			Amount val = PRLibrary.prLtmp(pt, eqt, p );
			this.setValue(val);
			return this;
		}		
		
		@DeriveMethod		
		public LiftedSurfaceAirTempAt500mb derive(SurfacePotentialTemperature pt, SurfaceEquivPotentialTemp eqt, SeaLevelPressure p) throws InvalidValueException, NullPointerException  {
			Amount val = PRLibrary.prLtmp(pt, eqt, p );
			this.setValue(val);
			return this;
		}		
		
		@DeriveMethod		
		public LiftedSurfaceAirTempAt500mb derive(SurfacePotentialTemperature pt, SurfaceEquivPotentialTemp eqt, MeanSeaLevelPres p) throws InvalidValueException, NullPointerException  {
			Amount val = PRLibrary.prLtmp(pt, eqt, p );
			this.setValue(val);
			return this;
		}		

		@DeriveMethod
		//TODO: is this the right kind of Pressure being used?
		public LiftedSurfaceAirTempAt500mb derive( PotentialTemperature pt, EquivPotentialTemp eqt, PressureLevel p) throws InvalidValueException, NullPointerException  {
			Amount val = PRLibrary.prLtmp(pt, eqt, p );
			this.setValue(val);
			return this;
		}		
		******/
 }

 
 
 
 
 