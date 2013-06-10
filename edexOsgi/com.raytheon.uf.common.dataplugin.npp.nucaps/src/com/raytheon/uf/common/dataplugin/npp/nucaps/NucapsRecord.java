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
package com.raytheon.uf.common.dataplugin.npp.nucaps;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
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
 * PDO for NPP NUCAPS data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2013            mschenke     Initial creation
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "nucapsseq")
@Table(name = "nucaps", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@DynamicSerialize
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class NucapsRecord extends NPPSoundingRecord {

    private static final long serialVersionUID = 1L;

    public static final String PDV_SURFACE_PRESSURE = "Surface_Pressure";

    public static final String PDV_PRESSURE = "Pressure";

    public static final String PDV_SKIN_TEMPERATURE = "Skin_Temperature";

    public static final String PDV_TEMPERATURE = "Temperature";

    public static final String PDV_TOPOGRAPHY = "Topography";

    public static final String PDV_EFFECTIVE_PRESSURE = "Effective_Pressure";

    public static final String PDV_WATER_VAPOR_MIXING_RATIO = "H2O_MR";

    public static final String PDV_LIQUID_WATER_MIXING_RATIO = "Liquid_H2O_MR";

    public static final String PDV_OZONE_MIXING_RATIO = "O3_MR";

    public static final String PDV_SULFER_DIOXIDE_MIXING_RATIO = "SO2_MR";

    public static final String PDV_QUALITY_FLAG = "Quality_Flag";

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

}
