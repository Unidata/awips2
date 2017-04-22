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
package com.raytheon.uf.edex.plugin.mpe.rocchecker;

import javax.xml.bind.annotation.XmlEnum;

import com.raytheon.uf.common.dataplugin.shef.tables.Agricultural;
import com.raytheon.uf.common.dataplugin.shef.tables.Discharge;
import com.raytheon.uf.common.dataplugin.shef.tables.Evaporation;
import com.raytheon.uf.common.dataplugin.shef.tables.Gatedam;
import com.raytheon.uf.common.dataplugin.shef.tables.Ground;
import com.raytheon.uf.common.dataplugin.shef.tables.Height;
import com.raytheon.uf.common.dataplugin.shef.tables.Ice;
import com.raytheon.uf.common.dataplugin.shef.tables.Lake;
import com.raytheon.uf.common.dataplugin.shef.tables.Moisture;
import com.raytheon.uf.common.dataplugin.shef.tables.Pressure;
import com.raytheon.uf.common.dataplugin.shef.tables.Procvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.Radiation;
import com.raytheon.uf.common.dataplugin.shef.tables.Snow;
import com.raytheon.uf.common.dataplugin.shef.tables.Temperature;
import com.raytheon.uf.common.dataplugin.shef.tables.Weather;
import com.raytheon.uf.common.dataplugin.shef.tables.Wind;
import com.raytheon.uf.common.dataplugin.shef.tables.Yunique;
import com.raytheon.uf.common.dataplugin.shef.data.Observation;

/**
 * Enum identifying which ihfs tables can be mapped to the {@link Observation}.
 * This enum is accurate as of May 2016.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 20, 2016 5590       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlEnum
public enum ObservationTable {

    agricultural(Agricultural.class), discharge(Discharge.class), evaporation(
            Evaporation.class), gatedam(Gatedam.class), ground(Ground.class), height(
            Height.class), ice(Ice.class), lake(Lake.class), moisture(
            Moisture.class), pressure(Pressure.class), procvalue(
            Procvalue.class), radiation(Radiation.class), snow(Snow.class), temperature(
            Temperature.class), weather(Weather.class), wind(Wind.class), yunique(
            Yunique.class);

    private final Class<?> entityClass;

    private ObservationTable(Class<?> entityClass) {
        this.entityClass = entityClass;
    }

    public Class<?> getEntityClass() {
        return entityClass;
    }
}