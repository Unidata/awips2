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
package com.raytheon.uf.edex.netcdf.description.field;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Base class for field descriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 26, 2015  4699     nabowle   Initial creation
 * Sep 09, 2015  4696     nabowle   Add retrieval at different indices and
 *                                  getLength().
 * Dec 08, 2015  5059     nabowle   Add isNumeric() and isPresent().
 * Jan 25, 2016  5208     bsteffen  Add validation.
 * Mar 21, 2016  5450     nabowle   Add more subclasses to XmlSeeAlso
 * May 19, 2016  5584     nabowle   Moved abstract methods to IFieldDescription.
 * 
 * 
 * </pre>
 * 
 * @author nabowle
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractFieldDescription implements IFieldDescription {

    @XmlAttribute
    protected String name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
