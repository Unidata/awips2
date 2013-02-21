package gov.noaa.nws.ncep.edex.common.metparameters;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.HashMap;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

//import com.raytheon.uf.viz.core.exception.VizException;

// TODO : this class can be enhanced to read AbstractNcParameter objects either from an extension point or to read
// jaxb files from a directory. Till then the AbstractNcParameter's are just created here.
//
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MetParameterFactory implements ISerializableObject{

	/**
	 * 
	 */
	@DynamicSerializeElement
	private static final long serialVersionUID = 5589025391819288854L;

	// map from the alias to the ncep parameter name.
	@DynamicSerializeElement
	private HashMap<String, String> ncParamsAliasMap = new HashMap<String, String>();
	
	// 
	@DynamicSerializeElement
	private HashMap<String, AbstractMetParameter> ncParamsMap = 
					new HashMap<String, AbstractMetParameter>();
	
	@DynamicSerializeElement
	private static MetParameterFactory instance=null;
	
	public static MetParameterFactory getInstance() {
		if( instance == null ) {
			instance = new MetParameterFactory();
		}
		return instance;
	}

	private MetParameterFactory() {
		try{
		ncParamsMap.put( AircraftType.class.getSimpleName(),  new AircraftType() );
		ncParamsMap.put( AircraftReportType.class.getSimpleName(),  new AircraftReportType() );
		ncParamsMap.put( Avg3HrShipSpeed.class.getSimpleName(),  new Avg3HrShipSpeed() );
		ncParamsMap.put( Avg1HrHeatFlux.class.getSimpleName(),  new Avg1HrHeatFlux() );
		ncParamsMap.put( Avg1HrSnowPhaseChangeHeatFlux.class.getSimpleName(),  new Avg1HrSnowPhaseChangeHeatFlux() );
		ncParamsMap.put( Avg1HrSubSurfaceHeatFlux.class.getSimpleName(),  new Avg1HrSubSurfaceHeatFlux() );
		ncParamsMap.put( BaseOfIcing.class.getSimpleName(),  new BaseOfIcing() );
		ncParamsMap.put( BaseOfTurbulence.class.getSimpleName(),  new BaseOfTurbulence() );
		ncParamsMap.put( BaseOfWeather.class.getSimpleName(),  new BaseOfWeather() );
		ncParamsMap.put( BruntVaisalaFreq.class.getSimpleName(),  new BruntVaisalaFreq() );
		ncParamsMap.put( BruntVaisalaPeriod.class.getSimpleName(),  new BruntVaisalaPeriod() );
//		ncParamsMap.put( BruntVaisalaFrequencySquared.class.getSimpleName(),  new BruntVaisalaFrequencySquared() );
		ncParamsMap.put( CatFcstCeilingHeightCond.class.getSimpleName(),  new CatFcstCeilingHeightCond() );
		ncParamsMap.put( CatFcstObstructionsVision.class.getSimpleName(),  new CatFcstObstructionsVision() );
		ncParamsMap.put( CatFcstPrecipitation.class.getSimpleName(),  new CatFcstPrecipitation() );
		ncParamsMap.put( CatFcstSnowAmountFalling24hr.class.getSimpleName(),  new CatFcstSnowAmountFalling24hr() );
		ncParamsMap.put( CatFcstVisibilityCond.class.getSimpleName(),  new CatFcstVisibilityCond() );
		ncParamsMap.put( CeilingFromSurface.class.getSimpleName(),  new CeilingFromSurface() );
		ncParamsMap.put( CeilingFromSeaLevel.class.getSimpleName(),  new CeilingFromSeaLevel() );
		ncParamsMap.put( CloudCover.class.getSimpleName(),  new CloudCover() );
		ncParamsMap.put( ClimDayTemp.class.getSimpleName(),  new ClimDayTemp() );
		ncParamsMap.put( ClimNightTemp.class.getSimpleName(),  new ClimNightTemp() );
		ncParamsMap.put( Clim12HrPOP.class.getSimpleName(),  new Clim12HrPOP() );
		ncParamsMap.put( Clim24HrPOP.class.getSimpleName(),  new Clim24HrPOP() );
		ncParamsMap.put( CloudBase1.class.getSimpleName(),  new CloudBase1() );
		ncParamsMap.put( CloudBase2.class.getSimpleName(),  new CloudBase2() );
		ncParamsMap.put( CloudFractionInLayer.class.getSimpleName(),  new CloudFractionInLayer() );
		ncParamsMap.put( CloudWater.class.getSimpleName(),  new CloudWater() );
		ncParamsMap.put( CloudTop1.class.getSimpleName(),  new CloudTop1() );
		ncParamsMap.put( CloudTop2.class.getSimpleName(),  new CloudTop2() );
		ncParamsMap.put( CondProbOf6HrSevereWeather.class.getSimpleName(),  new CondProbOf6HrSevereWeather() );
		ncParamsMap.put( CondProbOf12HrSevereWeather.class.getSimpleName(),  new CondProbOf12HrSevereWeather() );
		ncParamsMap.put( CondProbOf12HrFreezingPrecip.class.getSimpleName(),  new CondProbOf12HrFreezingPrecip() );
		ncParamsMap.put( CondProbOf12HrRain.class.getSimpleName(),  new CondProbOf12HrRain() );
		ncParamsMap.put( CondFcstPrecip12HrType.class.getSimpleName(),  new CondFcstPrecip12HrType() );
		ncParamsMap.put( CondProbOf12HrSnow.class.getSimpleName(),  new CondProbOf12HrSnow() );
		ncParamsMap.put( CondProbOf24HrSevereWeather.class.getSimpleName(),  new CondProbOf24HrSevereWeather() );
		ncParamsMap.put( CondProbOfFreezingPrecip.class.getSimpleName(),  new CondProbOfFreezingPrecip() );
		ncParamsMap.put( CondProbOfSnow.class.getSimpleName(),  new CondProbOfSnow() );
		ncParamsMap.put( ConvectivePrecip.class.getSimpleName(),  new ConvectivePrecip() );
		ncParamsMap.put( ShipCourse.class.getSimpleName(), new ShipCourse() );
		ncParamsMap.put( DayTempAnomaly.class.getSimpleName(),  new DayTempAnomaly() );
		ncParamsMap.put( DayTempFcst.class.getSimpleName(),  new DayTempFcst() );
		ncParamsMap.put( DewPointDepression.class.getSimpleName(),  new DewPointDepression() );
		ncParamsMap.put( DewPointTemp.class.getSimpleName(), new DewPointTemp() );
		ncParamsMap.put( DryBulbTemp.class.getSimpleName(), new DryBulbTemp() );
		ncParamsMap.put( FiveSecPeakWindDir.class.getSimpleName(), new FiveSecPeakWindDir() );
		ncParamsMap.put( PredomSwellWaveDir.class.getSimpleName(), new PredomSwellWaveDir() );
		ncParamsMap.put( SecondarySwellWaveDir.class.getSimpleName(), new SecondarySwellWaveDir() );
		ncParamsMap.put( DPRN.class.getSimpleName(), new DPRN() );	
		ncParamsMap.put( DryAirDensity.class.getSimpleName(), new DryAirDensity() );
		ncParamsMap.put( DryHydrostaticHeight.class.getSimpleName(), new DryHydrostaticHeight() );		
		ncParamsMap.put( TimeOf5SecPeakWindInHrs.class.getSimpleName(), new TimeOf5SecPeakWindInHrs() );
		ncParamsMap.put( TimeOf5SecPeakWindInMins.class.getSimpleName(), new TimeOf5SecPeakWindInMins() );
		ncParamsMap.put( EquivPotentialTemp.class.getSimpleName(), new EquivPotentialTemp() );
		ncParamsMap.put( EquivWindSpeed10min.class.getSimpleName(), new EquivWindSpeed10min() );
		ncParamsMap.put( EquivWindSpeed20min.class.getSimpleName(), new EquivWindSpeed20min() );
		ncParamsMap.put( FcstFZRainAccumulationIn12Hours.class.getSimpleName(), new FcstFZRainAccumulationIn12Hours() );
		ncParamsMap.put( FcstFZRainAccumulationToWatchThresh.class.getSimpleName(), new FcstFZRainAccumulationToWatchThresh() );
		ncParamsMap.put( FcstSnowIcePelletAccumulation12Hrs.class.getSimpleName(), new FcstSnowIcePelletAccumulation12Hrs() );
		ncParamsMap.put( FcstSnowIcePelletAccumToWatchThresh.class.getSimpleName(), new FcstSnowIcePelletAccumToWatchThresh() );
		ncParamsMap.put( FlashFloodGuid01Hr.class.getSimpleName(), new FlashFloodGuid01Hr() );
		ncParamsMap.put( FlashFloodGuid03Hr.class.getSimpleName(), new FlashFloodGuid03Hr() );
		ncParamsMap.put( FlashFloodGuid06Hr.class.getSimpleName(), new FlashFloodGuid06Hr() );
		ncParamsMap.put( FlashFloodGuid12Hr.class.getSimpleName(), new FlashFloodGuid12Hr() );
		ncParamsMap.put( FlashFloodGuid24Hr.class.getSimpleName(), new FlashFloodGuid24Hr() );
		ncParamsMap.put( FlightRulesID.class.getSimpleName(), new FlightRulesID() );
		ncParamsMap.put( FlightLevel.class.getSimpleName(), new FlightLevel() );
		ncParamsMap.put( FosbergFireWxIndex.class.getSimpleName(), new FosbergFireWxIndex() );
		ncParamsMap.put( FZRainWatchThresh.class.getSimpleName(), new FZRainWatchThresh() );
//		ncParamsMap.put( GenericDimensionlessParameter.class.getSimpleName(), new GenericDimensionlessParameter() );
//		ncParamsMap.put( GustBarb.class.getSimpleName(), new GustBarb() );
		ncParamsMap.put( HailSize.class.getSimpleName(), new HailSize() );
		ncParamsMap.put( HeatIndex.class.getSimpleName(), new HeatIndex() );
		ncParamsMap.put( HeightAboveSeaLevel.class.getSimpleName(), new HeightAboveSeaLevel() );
		ncParamsMap.put( InstrumentWaveHeight.class.getSimpleName(), new InstrumentWaveHeight() );
		ncParamsMap.put( PredomSwellWaveHeight.class.getSimpleName(), new PredomSwellWaveHeight() );
		ncParamsMap.put( SecondarySwellWaveHeight.class.getSimpleName(), new SecondarySwellWaveHeight() );
		ncParamsMap.put( WaveHeight.class.getSimpleName(), new WaveHeight() );
		ncParamsMap.put( WindWaveHeight.class.getSimpleName(), new WindWaveHeight() );
		ncParamsMap.put( Highest1MinMeanWindSpeedInPastHour.class.getSimpleName(), new Highest1MinMeanWindSpeedInPastHour() );
		ncParamsMap.put( HighResWaveHeight.class.getSimpleName(), new HighResWaveHeight() );
		ncParamsMap.put( HumitureIndex.class.getSimpleName(), new HumitureIndex() );
		ncParamsMap.put( IceCode.class.getSimpleName(), new IceCode() );
		ncParamsMap.put( IceType.class.getSimpleName(), new IceType() );
		ncParamsMap.put( IcingIntensitySymbol.class.getSimpleName(), new IcingIntensitySymbol() );
		ncParamsMap.put( IcingTypeSymbol.class.getSimpleName(), new IcingTypeSymbol() );
		ncParamsMap.put( InterWindDir.class.getSimpleName(), new InterWindDir() );
		ncParamsMap.put( InterWindSpeed.class.getSimpleName(), new InterWindSpeed() );
		ncParamsMap.put( InterWindTime.class.getSimpleName(), new InterWindTime() );
		ncParamsMap.put( LandSea.class.getSimpleName(), new LandSea() );
		ncParamsMap.put( LatentHeatOfVapor.class.getSimpleName(), new LatentHeatOfVapor() );
		ncParamsMap.put( LCLParcelPressure.class.getSimpleName(), new LCLParcelPressure() );
		ncParamsMap.put( LCLParcelTemperature.class.getSimpleName(), new LCLParcelTemperature() );
		ncParamsMap.put( LiftedIndex.class.getSimpleName(), new LiftedIndex() );
		ncParamsMap.put( LiftedSurfaceAirTempAt500mb.class.getSimpleName(), new LiftedSurfaceAirTempAt500mb() );
		ncParamsMap.put( Lowest01MinAvgPressInPastHour.class.getSimpleName(), new Lowest01MinAvgPressInPastHour() );	
		ncParamsMap.put( Max24HrTemp.class.getSimpleName(), new Max24HrTemp() );	
		ncParamsMap.put( Max6HrTemp.class.getSimpleName(), new Max6HrTemp() );	
		ncParamsMap.put( MaxDailyWeatherMapTemp.class.getSimpleName(), new MaxDailyWeatherMapTemp() );
		ncParamsMap.put( MaxDayTemp.class.getSimpleName(), new MaxDayTemp() );
		ncParamsMap.put( MaxEditedTemp.class.getSimpleName(), new MaxEditedTemp() );
		ncParamsMap.put( MaxCloudCover.class.getSimpleName(), new MaxCloudCover() );
		ncParamsMap.put( MaxPrecipPR6X.class.getSimpleName(), new MaxPrecipPR6X() ); //remove??		
		ncParamsMap.put( MaxMidnightTemp.class.getSimpleName(), new MaxMidnightTemp() );
		ncParamsMap.put( MaxOrMinTemp.class.getSimpleName(), new MaxOrMinTemp() );
		ncParamsMap.put( Max12HrPrecipFcst.class.getSimpleName(), new Max12HrPrecipFcst() );
		ncParamsMap.put( MaxWindSpeed.class.getSimpleName(), new MaxWindSpeed() );
		ncParamsMap.put( MeanSeaLevelPres.class.getSimpleName(), new MeanSeaLevelPres() );	
		ncParamsMap.put( Min24HrTemp.class.getSimpleName(), new Min24HrTemp() );	
		ncParamsMap.put( Min6HrTemp.class.getSimpleName(), new Min6HrTemp() );
		ncParamsMap.put( MinDailyWeatherMapTemp.class.getSimpleName(), new MinDailyWeatherMapTemp() );
		ncParamsMap.put( MinNightTemp.class.getSimpleName(), new MinNightTemp() );
		ncParamsMap.put( MixingRatio.class.getSimpleName(), new MixingRatio() );
		ncParamsMap.put( MoistHydrostaticHeight.class.getSimpleName(), new MoistHydrostaticHeight() );
		ncParamsMap.put( MontgomeryStreamFnct.class.getSimpleName(), new MontgomeryStreamFnct() );
		ncParamsMap.put( MountainObscThreshMetIndicator.class.getSimpleName(), new MountainObscThreshMetIndicator() );
		ncParamsMap.put( MountainObscThresh.class.getSimpleName(), new MountainObscThresh() );
		ncParamsMap.put( NightTempAnomaly.class.getSimpleName(), new NightTempAnomaly() );
		ncParamsMap.put( NewSnowAmount.class.getSimpleName(), new NewSnowAmount() );
		ncParamsMap.put( NightTempFcst.class.getSimpleName(), new NightTempFcst() );
		ncParamsMap.put( NumInterWinds.class.getSimpleName(), new NumInterWinds() );
		ncParamsMap.put( AirParcelTemp.class.getSimpleName(), new AirParcelTemp() );
		ncParamsMap.put( Omega.class.getSimpleName(), new Omega() );
		ncParamsMap.put( PeakWindDir.class.getSimpleName(), new PeakWindDir() );
		ncParamsMap.put( PeakWindSpeed.class.getSimpleName(), new PeakWindSpeed() );
		ncParamsMap.put( PeakWindSpeedTime.class.getSimpleName(), new PeakWindSpeedTime() );
		ncParamsMap.put( InstrumentWavePeriod.class.getSimpleName(), new InstrumentWavePeriod() );
		ncParamsMap.put( PredomSwellWavePeriod.class.getSimpleName(), new PredomSwellWavePeriod() );
		ncParamsMap.put( SecondarySwellWavePeriod.class.getSimpleName(), new SecondarySwellWavePeriod() );
		ncParamsMap.put( WavePeriod.class.getSimpleName(), new WavePeriod() );
		ncParamsMap.put( WindWavePeriod.class.getSimpleName(), new WindWavePeriod() );
		ncParamsMap.put( PerpendicularWindComp.class.getSimpleName(), new PerpendicularWindComp() );
		ncParamsMap.put( PotentialTemp.class.getSimpleName(), new PotentialTemp() );
		ncParamsMap.put( PotentialTempAt10Meters.class.getSimpleName(), new PotentialTempAt10Meters() );
		ncParamsMap.put( PlatformTrueDirection.class.getSimpleName(), new PlatformTrueDirection() );
		ncParamsMap.put( PlatformTrueSpeed.class.getSimpleName(), new PlatformTrueSpeed() );
		ncParamsMap.put( PotentialTempLapseRate.class.getSimpleName(), new PotentialTempLapseRate() );
		ncParamsMap.put( PrecipitableWaterForEntireSounding.class.getSimpleName(), new PrecipitableWaterForEntireSounding() );
		ncParamsMap.put( PrecipitableWaterUptoSpecifiedLevel.class.getSimpleName(), new PrecipitableWaterUptoSpecifiedLevel() );
		ncParamsMap.put( Precipitation.class.getSimpleName(), new Precipitation() );
		ncParamsMap.put( Precip01Hr.class.getSimpleName(), new Precip01Hr() );
		ncParamsMap.put( Precip03Hr.class.getSimpleName(), new Precip03Hr() );
		ncParamsMap.put( Precip06Hr.class.getSimpleName(), new Precip06Hr() );
//		ncParamsMap.put( PrecipitationIn09Hours.class.getSimpleName(), new PrecipitationIn09Hours() );
		ncParamsMap.put( Precip12Hr.class.getSimpleName(), new Precip12Hr() );
		ncParamsMap.put( Precip18Hr.class.getSimpleName(), new Precip18Hr() );
		ncParamsMap.put( Precip24Hr.class.getSimpleName(), new Precip24Hr() );		
		ncParamsMap.put( PresentWeather.class.getSimpleName(), new PresentWeather() );
		ncParamsMap.put( PressureLevel.class.getSimpleName(), new PressureLevel() );
		ncParamsMap.put( PressChange3Hr.class.getSimpleName(), new PressChange3Hr() );
		ncParamsMap.put( PressChange24Hr.class.getSimpleName(), new PressChange24Hr() );
		ncParamsMap.put( PressureTendencySymbol.class.getSimpleName(), new PressureTendencySymbol() );
		ncParamsMap.put( POPFcst06Hrs.class.getSimpleName(), new POPFcst06Hrs() );
		ncParamsMap.put( POPFcst12Hrs.class.getSimpleName(), new POPFcst12Hrs() );
		ncParamsMap.put( POPFcst24Hrs.class.getSimpleName(), new POPFcst24Hrs() );
		ncParamsMap.put( POP12Hrs.class.getSimpleName(), new POPAnomalyIn12hrs() );
		ncParamsMap.put( POPAnomalyIn12hrs.class.getSimpleName(), new POPAnomalyIn12hrs() );
		ncParamsMap.put( POP24Hrs.class.getSimpleName(), new POP24Hrs() );
		ncParamsMap.put( POPAnomalyIn24hrs.class.getSimpleName(), new POPAnomalyIn24hrs() );	
		ncParamsMap.put( RateOfIceAccretionOnVesselInSaltWater.class.getSimpleName(), new RateOfIceAccretionOnVesselInSaltWater() );
		ncParamsMap.put( RelativeHumidity.class.getSimpleName(), new RelativeHumidity() );
		ncParamsMap.put( RichardsonNumber.class.getSimpleName(), new RichardsonNumber() );
		ncParamsMap.put( SatEquivPotentialTemp.class.getSimpleName(), new SatEquivPotentialTemp() );
		ncParamsMap.put( SatMixingRatio.class.getSimpleName(), new SatMixingRatio() );
		ncParamsMap.put( SatVaporPressure.class.getSimpleName(), new SatVaporPressure() );
		ncParamsMap.put( SeaIceDriftDist.class.getSimpleName(), new SeaIceDriftDist() );
		ncParamsMap.put( SeaLevelPressure.class.getSimpleName(), new SeaLevelPressure() );
		ncParamsMap.put( SeaSurfaceTemp.class.getSimpleName(), new SeaSurfaceTemp() );
		ncParamsMap.put( ShowalterIndex.class.getSimpleName(), new ShowalterIndex() );
		ncParamsMap.put( SkinTemperature.class.getSimpleName(), new SkinTemperature() );
		ncParamsMap.put( SkyCoverage.class.getSimpleName(), new SkyCoverage() );
		ncParamsMap.put( SnowDepth.class.getSimpleName(), new SnowDepth() );
		ncParamsMap.put( SnowIcePelletWatchThresh.class.getSimpleName(), new SnowIcePelletWatchThresh() );
		ncParamsMap.put( SpeedOf05SecPeakWind.class.getSimpleName(), new SpeedOf05SecPeakWind() );
		ncParamsMap.put( SpecificHumidity.class.getSimpleName(), new SpecificHumidity() );
		ncParamsMap.put( SpecificHumidityAt02Meters.class.getSimpleName(), new SpecificHumidityAt02Meters());
		ncParamsMap.put( SpecificHumidityAt10Meters.class.getSimpleName(), new SpecificHumidityAt10Meters() );
		ncParamsMap.put( StabilityWithRespectToPressure.class.getSimpleName(), new StabilityWithRespectToPressure() );
		ncParamsMap.put( StationElevation.class.getSimpleName(), new StationElevation() );
		ncParamsMap.put( StationID.class.getSimpleName(), new StationID() );
		ncParamsMap.put( StationLatitude.class.getSimpleName(), new StationLatitude() );
		ncParamsMap.put( StationLongitude.class.getSimpleName(), new StationLongitude() );
		ncParamsMap.put( StationName.class.getSimpleName(), new StationName() );
		ncParamsMap.put( SurfacePressure.class.getSimpleName(), new SurfacePressure() );
		ncParamsMap.put( StormMotionSpeed.class.getSimpleName(), new StormMotionSpeed() );	
		ncParamsMap.put( StormMotionDirection.class.getSimpleName(), new StormMotionDirection() );
		//		ncParamsMap.put( SumOfFour6HrPrecipitation.class.getSimpleName(), new SumOfFour6HrPrecipitation() );
		ncParamsMap.put( SunshineDuration.class.getSimpleName(), new SunshineDuration() );
		ncParamsMap.put( SurfaceEquivPotentialTemp.class.getSimpleName(), new SurfaceEquivPotentialTemp() );
		ncParamsMap.put( SurfaceMixingRatio.class.getSimpleName(), new SurfaceMixingRatio() );
		ncParamsMap.put( SurfacePotentialTemp.class.getSimpleName(), new SurfacePotentialTemp() );		
		ncParamsMap.put( SurfacePressure.class.getSimpleName(), new SurfacePressure() );
		ncParamsMap.put( SurfaceSatEquivPotentialTemp.class.getSimpleName(), new SurfaceSatEquivPotentialTemp() );
		ncParamsMap.put( SurfaceSatMixingRatio.class.getSimpleName(), new SurfaceSatMixingRatio() );
        ncParamsMap.put( AirTemperature.class.getSimpleName(), new AirTemperature() );
        ncParamsMap.put( TempLapseRate.class.getSimpleName(), new TempLapseRate() );
        ncParamsMap.put( ProbableCeiling.class.getSimpleName(), new ProbableCeiling() );
		ncParamsMap.put( ProbableCeilingAsMeanSeaLevel.class.getSimpleName(),  new ProbableCeilingAsMeanSeaLevel() );
		ncParamsMap.put( ProbableFlightRuleIdentifier.class.getSimpleName(), new ProbableFlightRuleIdentifier() );
		ncParamsMap.put( ProbableMountainObscThreshMetIndicator.class.getSimpleName(), new ProbableMountainObscThreshMetIndicator() );
		ncParamsMap.put( ProbableVisibility.class.getSimpleName(), new ProbableVisibility() );
		ncParamsMap.put( ProbableWindDirection.class.getSimpleName(), new ProbableWindDirection() );
		ncParamsMap.put( ProbableWindGust.class.getSimpleName(), new ProbableWindGust() );
		ncParamsMap.put( ProbableWindSpeed.class.getSimpleName(), new ProbableWindSpeed() );
//		ncParamsMap.put( ThunderstormOccurring2hr.class.getSimpleName(), new ThunderstormOccurring2hr() );
//		ncParamsMap.put( ThunderstormOccurring6hr.class.getSimpleName(), new ThunderstormOccurring6hr() );
//		ncParamsMap.put( ThunderstormOccurring12hr.class.getSimpleName(), new ThunderstormOccurring12hr() );
//		ncParamsMap.put( ThunderstormOccurring24hr.class.getSimpleName(), new ThunderstormOccurring24hr() );
		ncParamsMap.put( ShipIceThickness.class.getSimpleName(), new ShipIceThickness() );
		ncParamsMap.put( StationNumber.class.getSimpleName(), new StationNumber() );
		ncParamsMap.put( TopOfIcing.class.getSimpleName(), new TopOfIcing() );
		ncParamsMap.put( TopOfTurbulence.class.getSimpleName(), new TopOfTurbulence() );
		ncParamsMap.put( TopOfWeather.class.getSimpleName(), new TopOfWeather() );
		ncParamsMap.put( TotalSkyCoverFcst12hr.class.getSimpleName(), new TotalSkyCoverFcst12hr() );
		ncParamsMap.put( TotalPrecip.class.getSimpleName(), new TotalPrecip() );
		ncParamsMap.put( TotalSkyCoverFcst12hr.class.getSimpleName(), new TotalSkyCoverFcst12hr() );
		ncParamsMap.put( TurbulenceFrequencySymbol.class.getSimpleName(), new TurbulenceFrequencySymbol() );
		ncParamsMap.put( TurbulenceIntensitySymbol.class.getSimpleName(), new TurbulenceIntensitySymbol() );
		ncParamsMap.put( TurbulenceTypeSymbol.class.getSimpleName(), new TurbulenceTypeSymbol() );
		ncParamsMap.put( TurbulentKineticEnergy.class.getSimpleName(), new TurbulentKineticEnergy() );
		ncParamsMap.put( EstStormDirectionUComp.class.getSimpleName(), new EstStormDirectionUComp() );
		ncParamsMap.put( UCompAt10Meters.class.getSimpleName(), new UCompAt10Meters() );
		ncParamsMap.put( UncondProbOfTstorms2hr.class.getSimpleName(), new UncondProbOfTstorms2hr() );
		ncParamsMap.put( UncondProbOfTstorms6hr.class.getSimpleName(), new UncondProbOfTstorms6hr() );	
		ncParamsMap.put( UncondProbOfTstorms12hr.class.getSimpleName(), new UncondProbOfTstorms12hr() );	
		ncParamsMap.put( UncondProbOfTstorms24hr.class.getSimpleName(), new UncondProbOfTstorms24hr() );	
		ncParamsMap.put( VCompAt10Meters.class.getSimpleName(), new VCompAt10Meters() );
		ncParamsMap.put( WindDirectionUComp.class.getSimpleName(), new WindDirectionUComp() );	
		ncParamsMap.put( VaporPressure.class.getSimpleName(), new VaporPressure() );
		ncParamsMap.put( EstStormDirectionVComp.class.getSimpleName(), new EstStormDirectionVComp() );	
		ncParamsMap.put( IsentropesVerticalSeparation.class.getSimpleName(), new IsentropesVerticalSeparation() );
		ncParamsMap.put( VerticalVelocity.class.getSimpleName(), new VerticalVelocity() );	
		ncParamsMap.put( VirtualPotentialTemp.class.getSimpleName(), new VirtualPotentialTemp() );
		ncParamsMap.put( VirtualTemp.class.getSimpleName(), new VirtualTemp() );
		ncParamsMap.put( Visibility.class.getSimpleName(), new Visibility() );	
		ncParamsMap.put( WindDirectionVComp.class.getSimpleName(), new WindDirectionVComp() );
		ncParamsMap.put( WaterEquivOfNewSnow.class.getSimpleName(), new
				WaterEquivOfNewSnow() );
		ncParamsMap.put( WaveSteepness.class.getSimpleName(), new WaveSteepness() );
		ncParamsMap.put( WetBulbPotentialTemp.class.getSimpleName(), new WetBulbPotentialTemp() );	
		ncParamsMap.put( WetBulbTemp.class.getSimpleName(), new WetBulbTemp() );
//		ncParamsMap.put( WindBarb.class.getSimpleName(), new WindBarb() );	
		ncParamsMap.put( WindChillEquivalentTemp.class.getSimpleName(), new WindChillEquivalentTemp() );
		ncParamsMap.put( WindChillTemperature.class.getSimpleName(), new WindChillTemperature() );
		ncParamsMap.put( WindSpeedComp.class.getSimpleName(), new WindSpeedComp() );
		ncParamsMap.put( WindCompDirection.class.getSimpleName(), new WindCompDirection() );
        ncParamsMap.put( WindDirection.class.getSimpleName(), new WindDirection() );
		ncParamsMap.put( WindGust.class.getSimpleName(), new WindGust() );
		ncParamsMap.put( WindSpeed.class.getSimpleName(), new WindSpeed() );
		ncParamsMap.put( CeilingFromSeaLevelWorstCase.class.getSimpleName(), new CeilingFromSeaLevelWorstCase() );
		ncParamsMap.put( FlightRulesIdWorstCase.class.getSimpleName(), new FlightRulesIdWorstCase() );
		ncParamsMap.put( MountainObscThreshMetIndicatorWorstCase.class.getSimpleName(), new MountainObscThreshMetIndicatorWorstCase() );
		ncParamsMap.put( Probability.class.getSimpleName(), new Probability() );	
		ncParamsMap.put( ProbableSkyCoverage.class.getSimpleName(), new ProbableSkyCoverage() );	
		ncParamsMap.put( ReportTimeInHourMins.class.getSimpleName(), new ReportTimeInHourMins() );		
		ncParamsMap.put( ProbablePresentWeather.class.getSimpleName(), new ProbablePresentWeather() );	
		ncParamsMap.put( LowLevelWindShear.class.getSimpleName(), new LowLevelWindShear() );
		ncParamsMap.put( WxPresent.class.getSimpleName(), new WxPresent() );
		}
		catch(Exception e ){
			e.printStackTrace();
		}
	}
		
	public boolean isValidMetParameterName( String metPrmName ) {
		return ncParamsMap.containsKey( metPrmName );
	}
	
	// get the default units
	public AbstractMetParameter createParameter( String prmName ) {
		String ncParamName;
		if( ncParamsAliasMap.containsKey( prmName ) ) {
			ncParamName = ncParamsAliasMap.get( prmName );
		}
		else {
			ncParamName = prmName;
		}
		
		if( ncParamName == null ||
			!ncParamsMap.containsKey( ncParamName )) {
			System.out.println("can't find ncParam for : "+prmName );
			return null;
		}
		
		AbstractMetParameter ncParam = ncParamsMap.get( ncParamName );

		try {
			AbstractMetParameter newParam = (AbstractMetParameter)
								ncParam.getClass().getConstructor().newInstance();
			return newParam;
		} catch (Exception e) {
			System.out.println("error getting newInstance for metParam " +
									ncParam.getClass().getSimpleName() );
		}
		
		return null;
	}
	
	// u is the expected units and must be compatible with the units for the implemented
	// Quantity.
	public AbstractMetParameter createParameter( String prmName, String unitName ) {
		Unit<?> units;
		
		if( unitName == null ) {
			units = Unit.ONE;
		}
		else {
			try {
				units = UnitFormat.getUCUMInstance().parseProductUnit(
								unitName, new ParsePosition(0) );
			} catch (ParseException e) {
				System.out.println("unable to determine units for "+unitName);
				return null;
			}
		}

		return createParameter(prmName, units);
	}

	// create a parameter value with MISSING value.
	// name may be an alias, or the name of the parameter
	// 
	public AbstractMetParameter createParameter( String prmName, Unit<?> u ) {
		AbstractMetParameter newParam = createParameter( prmName );
		newParam.setUnitPair( u );
		return newParam;
	}
	
	public AbstractMetParameter createParameter( String prmName, DataTime dt ) {
		AbstractMetParameter newParam = createParameter( prmName );
		newParam.setValidTime( dt );
		return newParam;
	}
	
	public AbstractMetParameter createParameter( String prmName, Unit<?> u, DataTime dt ) {
		AbstractMetParameter newParam = createParameter( prmName );
		newParam.setValidTime( dt );
		newParam.setUnitPair( u );
		return newParam;
	}

	public boolean alias( String prmName, String alias ) {
		
		if( ncParamsAliasMap.containsKey( alias ) &&
			!ncParamsAliasMap.get( alias ).equals( prmName ) ) {
			System.out.println("Error aliasing ncParam "+prmName+" : "+alias +
					" is already aliased to "+ncParamsAliasMap.get( alias ) );
	// 		return false;
		}
		
		ncParamsAliasMap.put( alias, prmName );
		
		return true;
	}

	@DynamicSerialize
	public static class NotDerivableException extends Exception {
		
		@DynamicSerializeElement
		private static final long serialVersionUID = -5750539413381916413L;

		public NotDerivableException( String msg) {
			super( msg );
		}
	}

	public @Retention(RetentionPolicy.RUNTIME) @interface DeriveMethod {
	}

}