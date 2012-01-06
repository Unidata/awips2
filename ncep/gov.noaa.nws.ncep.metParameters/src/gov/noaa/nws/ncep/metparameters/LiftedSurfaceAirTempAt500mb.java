package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.NcUnits;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

 public class LiftedSurfaceAirTempAt500mb extends AbstractMetParameter implements javax.measure.quantity.Temperature {

		public LiftedSurfaceAirTempAt500mb() {
			super( UNIT );
		}

		@DeriveMethod
		public LiftedSurfaceAirTempAt500mb derive(SurfacePotentialTemp pt, SurfaceEquivPotentialTemp eqt ) throws InvalidValueException, NullPointerException, InvalidRangeException {
			if( pt.hasValidValue() && eqt.hasValidValue() ) {
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
		public LiftedSurfaceAirTempAt500mb derive(SurfacePotentialTemperature pt, SurfaceEquivPotentialTemp eqt, PressureLevel p) throws InvalidValueException, NullPointerException, InvalidRangeException {
			Amount val = PRLibrary.prLtmp(pt, eqt, p );
			this.setValue(val);
			return this;
		}
		
		@DeriveMethod		
		public LiftedSurfaceAirTempAt500mb derive(SurfacePotentialTemperature pt, SurfaceEquivPotentialTemp eqt, SurfacePressure p) throws InvalidValueException, NullPointerException, InvalidRangeException {
			Amount val = PRLibrary.prLtmp(pt, eqt, p );
			this.setValue(val);
			return this;
		}		
		
		@DeriveMethod		
		public LiftedSurfaceAirTempAt500mb derive(SurfacePotentialTemperature pt, SurfaceEquivPotentialTemp eqt, SeaLevelPressure p) throws InvalidValueException, NullPointerException, InvalidRangeException {
			Amount val = PRLibrary.prLtmp(pt, eqt, p );
			this.setValue(val);
			return this;
		}		
		
		@DeriveMethod		
		public LiftedSurfaceAirTempAt500mb derive(SurfacePotentialTemperature pt, SurfaceEquivPotentialTemp eqt, MeanSeaLevelPres p) throws InvalidValueException, NullPointerException, InvalidRangeException {
			Amount val = PRLibrary.prLtmp(pt, eqt, p );
			this.setValue(val);
			return this;
		}		

		@DeriveMethod
		//TODO: is this the right kind of Pressure being used?
		public LiftedSurfaceAirTempAt500mb derive( PotentialTemperature pt, EquivPotentialTemp eqt, PressureLevel p) throws InvalidValueException, NullPointerException, InvalidRangeException {
			Amount val = PRLibrary.prLtmp(pt, eqt, p );
			this.setValue(val);
			return this;
		}		
		******/
 }

 
 
 
 
 