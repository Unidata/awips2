package gov.noaa.nws.ncep.ui.nsharp.display.map;

import gov.noaa.nws.ncep.common.dataplugin.gpd.GenericPointDataConstants;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataLevel;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataParameter;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataStationProduct;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

// import gov.noaa.nws.ncep.viz.rsc.gpd.query.GpdQuery;
/*
 * Chin Note; User only be able to pick one station from Nsharp map at a time, however, the picked station may have several time lines
 * associated with it. stnPtDataLineLst contains  a list of same stations but with different time lines (if provided)
 */
public class NsharpGpdSoundingQuery {
    public static void getGpdPfcSndData(
            List<NsharpStationInfo> stnPtDataLineLst,
            Map<String, List<NcSoundingLayer>> soundingLysLstMap,
            String prodName) {
        for (NsharpStationInfo StnPt : stnPtDataLineLst) {
            // one StnPt represent one reference time line
            // with one reference time, there may be many forecast time / range
            // start time
            List<Date> rangeTimeList = new ArrayList<Date>();
            for (NsharpStationInfo.timeLineSpecific tmlinSpc : StnPt
                    .getTimeLineSpList()) {
                rangeTimeList.add(tmlinSpc.getTiemLine());
            }
            List<GenericPointDataStationProduct> stnPList = null;
            // TBDGPD List<GenericPointDataStationProduct> stnPList =
            // GpdQuery.getGpdStationModelSoundingProductList(prodName,GenericPointDataQueryKey.BY_SLAT_SLON,
            // null,(float)stnPtDataLineLst.get(0).getLatitude(),(float)stnPtDataLineLst.get(0).getLongitude(),
            // rangeTimeList,false,0,StnPt.getReftime());
            if (stnPList != null && stnPList.size() > 0) {
                for (GenericPointDataStationProduct stnProd : stnPList) {
                    List<GenericPointDataLevel> levelList = stnProd
                            .getLevelLst();
                    List<NcSoundingLayer> sndLayerList = new ArrayList<NcSoundingLayer>();
                    for (GenericPointDataLevel gpdl : levelList) {
                        List<GenericPointDataParameter> gpdParmList = gpdl
                                .getGpdParameters();
                        NcSoundingLayer sndLyer = new NcSoundingLayer();
                        for (GenericPointDataParameter gpdParm : gpdParmList) {
                            if (gpdParm.getName().equals(
                                    GenericPointDataConstants.GEMPAK_PRESSURE)) {
                                sndLyer.setPressure(gpdParm.getValue());
                            } else if (gpdParm.getName().equals(
                                    GenericPointDataConstants.GEMPAK_HEIGHT)) {
                                sndLyer.setGeoHeight(gpdParm.getValue());
                            } else if (gpdParm.getName().equals(
                                    GenericPointDataConstants.GEMPAK_TEMP)) {
                                sndLyer.setTemperature(gpdParm.getValue());
                            } else if (gpdParm.getName().equals(
                                    GenericPointDataConstants.GEMPAK_DEWPT)) {
                                sndLyer.setDewpoint(gpdParm.getValue());
                            } else if (gpdParm.getName().equals(
                                    GenericPointDataConstants.GEMPAK_WIND_DIR)) {
                                sndLyer.setWindDirection(gpdParm.getValue());
                            } else if (gpdParm
                                    .getName()
                                    .equals(GenericPointDataConstants.GEMPAK_WIND_SPEED)) {
                                sndLyer.setWindSpeed(gpdParm.getValue());
                            }
                        }
                        sndLayerList.add(sndLyer);
                    }
                    String dispInfo = "";
                    for (NsharpStationInfo.timeLineSpecific tmlinSpc : StnPt
                            .getTimeLineSpList()) {
                        if (tmlinSpc.getTiemLine().getTime() == (stnProd
                                .getForecastTime() * 1000 + stnProd
                                .getRefTime().getTime())) {
                            dispInfo = tmlinSpc.getDisplayInfo();
                            soundingLysLstMap.put(dispInfo, sndLayerList);
                            break;
                        }
                    }
                }
            }

        }
    }

    public static void getGpdObsSndData(
            List<NsharpStationInfo> stnPtDataLineLst,
            Map<String, List<NcSoundingLayer>> soundingLysLstMap,
            String prodName) {
        List<Date> refTimeList = new ArrayList<Date>();
        for (NsharpStationInfo StnPt : stnPtDataLineLst) {
            // one StnPt represent one data time line
            Date refTime = new Date();
            refTime.setTime(StnPt.getReftime().getTime());
            refTimeList.add(refTime);
        }
        // System.out.println("stn lat =" +
        // stnPtDataLineLst.get(0).getLatitude()
        // + " lon=" + stnPtDataLineLst.get(0).getLongitude());
        List<GenericPointDataStationProduct> stnPList = null;
        // TBDGPD List<GenericPointDataStationProduct> stnPList =
        // GpdQuery.getGpdStationProductList(prodName,GenericPointDataQueryKey.BY_SLAT_SLON,
        // null,(float)stnPtDataLineLst.get(0).getLatitude(),(float)stnPtDataLineLst.get(0).getLongitude(),
        // refTimeList,false,0);
        if (stnPList != null && stnPList.size() > 0) {
            for (GenericPointDataStationProduct stnProd : stnPList) {
                List<GenericPointDataLevel> levelList = stnProd.getLevelLst();
                List<NcSoundingLayer> sndLayerList = new ArrayList<NcSoundingLayer>();
                for (GenericPointDataLevel gpdl : levelList) {
                    List<GenericPointDataParameter> gpdParmList = gpdl
                            .getGpdParameters();
                    NcSoundingLayer sndLyer = new NcSoundingLayer();
                    for (GenericPointDataParameter gpdParm : gpdParmList) {
                        if (gpdParm.getName().equals(
                                GenericPointDataConstants.GEMPAK_PRESSURE)) {
                            sndLyer.setPressure(gpdParm.getValue());
                        } else if (gpdParm.getName().equals(
                                GenericPointDataConstants.GEMPAK_HEIGHT)) { // NcSoundingProfile
                                                                            // sndPf=
                                                                            // PfcSoundingQuery.getPfcSndData(StnPt.getDatauri(),(float)StnPt.getLatitude(),
                                                                            // (float)StnPt.getLongitude(),
                                                                            // StnPt.getReftime(),
                            // StnPt.getRangestarttime(),
                            // PfcSoundingQuery.PfcSndType.NAMSND);

                            sndLyer.setGeoHeight(gpdParm.getValue());
                        } else if (gpdParm.getName().equals(
                                GenericPointDataConstants.GEMPAK_TEMP)) {
                            sndLyer.setTemperature(gpdParm.getValue());
                        } else if (gpdParm.getName().equals(
                                GenericPointDataConstants.GEMPAK_DEWPT)) {
                            sndLyer.setDewpoint(gpdParm.getValue());
                        } else if (gpdParm.getName().equals(
                                GenericPointDataConstants.GEMPAK_WIND_DIR)) {
                            sndLyer.setWindDirection(gpdParm.getValue());
                        } else if (gpdParm.getName().equals(
                                GenericPointDataConstants.GEMPAK_WIND_SPEED)) {
                            sndLyer.setWindSpeed(gpdParm.getValue());
                        }
                    }
                    sndLayerList.add(sndLyer);
                }
                String dispInfo = "";
                for (NsharpStationInfo StnPt : stnPtDataLineLst) {
                    if (StnPt.getReftime().getTime() == stnProd.getRefTime()
                            .getTime()) {
                        dispInfo = StnPt.getStnDisplayInfo();
                        soundingLysLstMap.put(dispInfo, sndLayerList);
                        break;
                    }
                }
            }
        }
    }
}
