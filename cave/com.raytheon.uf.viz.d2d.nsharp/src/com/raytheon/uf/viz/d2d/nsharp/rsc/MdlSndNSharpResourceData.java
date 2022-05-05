/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.d2d.nsharp.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.viz.soundingrequest.NcSoundingQuery;

/**
 * 
 * Provides sounding data to nsharp from model sounding data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bsteffen     Initial creation
 * Jul 23, 2014 3410       bclement     preparePointInfo() calls unpackResultLocation()
 * Mar 28, 2018 6800       bsteffen     Extend PointDataNSharpResourceData.
 * Aug 27, 2018 7409       bsteffen     Include the name of the point in pointName.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MdlSndNSharpResourceData extends PointDataNSharpResourceData {

    public MdlSndNSharpResourceData() {
        super();
    }

    public MdlSndNSharpResourceData(String soundingType) {
        super(soundingType);
    }

    @Override
    protected void preparePointInfo() throws VizException {
        String oldPointName = this.pointName;
        super.preparePointInfo();
        if (oldPointName != null && !oldPointName.equals(pointName)
                && oldPointName.startsWith("Point")) {
            /*
             * Super changes point name to the station name but the requirement
             * is to show station name and the point name.
             */
            this.pointName += ":" + oldPointName;
        }
    }

    @Override
    protected NcSoundingCube getSoundingCube(NsharpStationInfo stnInfo) {
        double[][] latLon = {
                { stnInfo.getLatitude(), stnInfo.getLongitude() } };
        return NcSoundingQuery.pfcSoundingQueryByLatLon(
                stnInfo.getReftime().getTime(),
                stnInfo.getRangestarttime().getTime(), latLon,
                stnInfo.getSndType(), NcSoundingLayer.DataType.ALLDATA, false,
                "-1");
    }

    @Override
    protected String getPointNameKey() {
        return STATION_ID;
    }

}
