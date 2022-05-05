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

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile.ObsSndType;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.viz.soundingrequest.NcSoundingQuery;

/**
 * 
 * Provides sounding data to nsharp from RAOB data.
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
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class BufruaNSharpResourceData extends PointDataNSharpResourceData {

    private static final String STATION_NAME = "stationName";

    public BufruaNSharpResourceData() {
        super(ObsSndType.BUFRUA.toString());
    }

    @Override
    protected NcSoundingCube getSoundingCube(NsharpStationInfo stnInfo) {
        String[] latLon = { pointName };
        return NcSoundingQuery.soundingQueryByStnId(
                stnInfo.getReftime().getTime(), latLon, stnInfo.getSndType(),
                NcSoundingLayer.DataType.ALLDATA, true, "-1");
    }

    @Override
    protected String getPointNameKey() {
        return STATION_NAME;
    }

}
