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

package gov.noaa.nws.ncep.common.dataplugin.ncgrib.util;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.Ncgrib1Parameter;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * The class used for unmarshalling the grib1 parameter conversion XML file.
 * ncgrib1ParameterConvTable.xml
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/9/10       4758        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "ncgrib1ParameterSet")
@XmlAccessorType(XmlAccessType.NONE)
public class Ncgrib1ParameterSet implements ISerializableObject {

    /** The unmarshalled grib 1 parameters */
    @XmlElements( { @XmlElement(name = "ncgrib1Parameter", type = Ncgrib1Parameter.class) })
    private ArrayList<Ncgrib1Parameter> parameters;

    /**
     * @return the parameters
     */
    public ArrayList<Ncgrib1Parameter> getParameters() {
        return parameters;
    }

    /**
     * @param parameters
     *            the parameters to set
     */
    public void setParameters(ArrayList<Ncgrib1Parameter> parameters) {
        this.parameters = parameters;
    }
}
