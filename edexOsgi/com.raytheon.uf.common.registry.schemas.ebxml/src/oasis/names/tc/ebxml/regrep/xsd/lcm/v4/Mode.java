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

package oasis.names.tc.ebxml.regrep.xsd.lcm.v4;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * <p>
 * Java class for mode.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * <p>
 * 
 * <pre>
 * &lt;simpleType name="mode">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}NCName">
 *     &lt;enumeration value="CreateOrReplace"/>
 *     &lt;enumeration value="CreateOrVersion"/>
 *     &lt;enumeration value="CreateOnly"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "mode")
@XmlEnum
@DynamicSerialize
public enum Mode {

    @XmlEnumValue("CreateOrReplace")
    CREATE_OR_REPLACE("CreateOrReplace"), @XmlEnumValue("CreateOrVersion")
    CREATE_OR_VERSION("CreateOrVersion"), @XmlEnumValue("CreateOnly")
    CREATE_ONLY("CreateOnly");
    private final String value;

    Mode(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static Mode fromValue(String v) {
        for (Mode c : Mode.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
