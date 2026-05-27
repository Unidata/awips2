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

package com.raytheon.uf.common.grib.tables;

import java.util.ArrayList;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElements;
import jakarta.xml.bind.annotation.XmlRootElement;

/**
 * The class used for unmarshalling the grib1 parameter conversion XML file.
 * grib1ParameterConvTable.xml
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 09, 2010  4758     bphillip    Initial Creation
 * Oct 15, 2013  2473     bsteffen    Remove deprecated ISerializableObject.
 * May 06, 2016  5572     bsteffen    Move to common
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "grib1ParameterSet")
@XmlAccessorType(XmlAccessType.NONE)
public class Grib1ParameterSet {

    /** The unmarshalled grib 1 parameters */
    @XmlElements( { @XmlElement(name = "grib1Parameter", type = Grib1Parameter.class) })
    private ArrayList<Grib1Parameter> parameters;

    /**
     * @return the parameters
     */
    public ArrayList<Grib1Parameter> getParameters() {
        return parameters;
    }

    /**
     * @param parameters
     *            the parameters to set
     */
    public void setParameters(ArrayList<Grib1Parameter> parameters) {
        this.parameters = parameters;
    }
}
