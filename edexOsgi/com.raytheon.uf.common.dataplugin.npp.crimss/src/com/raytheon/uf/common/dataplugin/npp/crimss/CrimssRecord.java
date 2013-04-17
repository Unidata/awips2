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
package com.raytheon.uf.common.dataplugin.npp.crimss;

import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.npp.sounding.NPPSoundingRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * PluginDataObject record for NPP CrIMSS data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2011            bsteffen     Initial creation
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "crimssseq")
@Table(name = "crimss", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@DynamicSerialize
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class CrimssRecord extends NPPSoundingRecord {

    private static final long serialVersionUID = 1L;

    public static final String PDV_SURFACE_PRESSURE = "SurfacePressure";

    public static final String PDV_ALTITUDE = "AltitudeLevels_Pressure";

    public static final String PDV_P_ALTITUDE = "Pressure";

    public static final String PDV_TEMPERATURE = "Temperature";

    public static final String PDV_P_TEMPERATURE = "PressureLevels_Temperature";

    public static final String PDV_H2O = "H2O";

    public static final String PDV_P_H2O = "PressureLevels_H2O";

}
