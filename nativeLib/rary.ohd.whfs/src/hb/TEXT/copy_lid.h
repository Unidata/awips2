/*
 	Procedure:	copy_lid.h
	Date:		May 1997
	Author:		Paul Taylor

	Purpose:	Procedure to copy all lid-related information
			from one lid to another.
			
	WARNING:  This is a "perl-generated" function.
	          DO NOT modify this "h" file directly !!
*/

#ifndef copy_lid_h
#define copy_lid_h


void	CopyLid_All (char *fromlid, char *tolid);
void	CopyLid_Reference (char *fromlid, char *tolid);

void	Copy_Location(char *fromlid, char *tolid);
void	Copy_Riverstat(char *fromlid, char *tolid);
void	Copy_RiverMonLocation(char *fromlid, char *tolid);
void	Copy_FcstPtService(char *fromlid, char *tolid);
void	Copy_FcstPtWatSup(char *fromlid, char *tolid);
void	Copy_FcstPtDeterm(char *fromlid, char *tolid);
void	Copy_FcstPtESP(char *fromlid, char *tolid);
void	Copy_AdjustFactor(char *fromlid, char *tolid);
void	Copy_Agricultural(char *fromlid, char *tolid);
void	Copy_AlertAlarmVal(char *fromlid, char *tolid);
void	Copy_ArealFcst(char *fromlid, char *tolid);
void	Copy_ArealObs(char *fromlid, char *tolid);
void	Copy_Benchmark(char *fromlid, char *tolid);
void	Copy_CommentValue(char *fromlid, char *tolid);
void	Copy_Contacts(char *fromlid, char *tolid);
void	Copy_ContingencyValue(char *fromlid, char *tolid);
void	Copy_Countynum(char *fromlid, char *tolid);
void	Copy_Crest(char *fromlid, char *tolid);
void	Copy_CurPC(char *fromlid, char *tolid);
void	Copy_CurPP(char *fromlid, char *tolid);
void	Copy_DailyPP(char *fromlid, char *tolid);
void	Copy_Datum(char *fromlid, char *tolid);
void	Copy_Dcp(char *fromlid, char *tolid);
void	Copy_Descrip(char *fromlid, char *tolid);
void	Copy_Discharge(char *fromlid, char *tolid);
void	Copy_Evaporation(char *fromlid, char *tolid);
void	Copy_FcstDischarge(char *fromlid, char *tolid);
void	Copy_FcstHeight(char *fromlid, char *tolid);
void	Copy_FcstPrecip(char *fromlid, char *tolid);
void	Copy_FcstTemperature(char *fromlid, char *tolid);
void	Copy_FcstOther(char *fromlid, char *tolid);
void	Copy_FishCount(char *fromlid, char *tolid);
void	Copy_Flood(char *fromlid, char *tolid);
void	Copy_Floodcat(char *fromlid, char *tolid);
void	Copy_Floodstmt(char *fromlid, char *tolid);
void	Copy_FloodTs(char *fromlid, char *tolid);
void	Copy_FpPrevProd(char *fromlid, char *tolid);
void	Copy_Gage(char *fromlid, char *tolid);
void	Copy_GateDam(char *fromlid, char *tolid);
void	Copy_Ground(char *fromlid, char *tolid);
void	Copy_Height(char *fromlid, char *tolid);
void	Copy_HgStation(char *fromlid, char *tolid);
void	Copy_HourlyPC(char *fromlid, char *tolid);
void	Copy_HourlyPP(char *fromlid, char *tolid);
void	Copy_Ice(char *fromlid, char *tolid);
void	Copy_IngestFilter(char *fromlid, char *tolid);
void	Copy_Lake(char *fromlid, char *tolid);
void	Copy_LatestObsValue(char *fromlid, char *tolid);
void	Copy_LocArea(char *fromlid, char *tolid);
void	Copy_LocExtAgency(char *fromlid, char *tolid);
void	Copy_LocImage(char *fromlid, char *tolid);
void	Copy_LocDataLimits(char *fromlid, char *tolid);
void	Copy_Lowwater(char *fromlid, char *tolid);
void	Copy_Moisture(char *fromlid, char *tolid);
void	Copy_MonthlyValues(char *fromlid, char *tolid);
void	Copy_OFSStnTrans(char *fromlid, char *tolid);
void	Copy_Observer(char *fromlid, char *tolid);
void	Copy_PairedValue(char *fromlid, char *tolid);
void	Copy_Power(char *fromlid, char *tolid);
void	Copy_Pressure(char *fromlid, char *tolid);
void	Copy_ProcValue(char *fromlid, char *tolid);
void	Copy_ProductLink(char *fromlid, char *tolid);
void	Copy_Pub(char *fromlid, char *tolid);
void	Copy_Radiation(char *fromlid, char *tolid);
void	Copy_Rating(char *fromlid, char *tolid);
void	Copy_RatingShift(char *fromlid, char *tolid);
void	Copy_RawPC(char *fromlid, char *tolid);
void	Copy_RawPP(char *fromlid, char *tolid);
void	Copy_RawPother(char *fromlid, char *tolid);
void	Copy_Refer(char *fromlid, char *tolid);
void	Copy_RejectedData(char *fromlid, char *tolid);
void	Copy_Reservoir(char *fromlid, char *tolid);
void	Copy_Rescap(char *fromlid, char *tolid);
void	Copy_RpfFcstPoint(char *fromlid, char *tolid);
void	Copy_Snow(char *fromlid, char *tolid);
void	Copy_SshpConfig(char *fromlid, char *tolid);
void	Copy_StnClass(char *fromlid, char *tolid);
void	Copy_Telem(char *fromlid, char *tolid);
void	Copy_Temperature(char *fromlid, char *tolid);
void	Copy_UnitGraph(char *fromlid, char *tolid);
void	Copy_UnkStn(char *fromlid, char *tolid);
void	Copy_UnkStnValue(char *fromlid, char *tolid);
void	Copy_WaterQuality(char *fromlid, char *tolid);
void	Copy_Weather(char *fromlid, char *tolid);
void	Copy_Wind(char *fromlid, char *tolid);
void	Copy_YUnique(char *fromlid, char *tolid);
void	Copy_Zonenum(char *fromlid, char *tolid);


#endif



