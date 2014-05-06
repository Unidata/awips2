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
package com.raytheon.uf.common.parameter;

import com.raytheon.uf.common.dataplugin.annotations.DataURIFieldConverter;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;

/**
 * A DataURIFieldConverter that ensures that a String parameter abbreviation
 * will be looked up to potentially contain all the fields.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 6, 2014  2060       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ParameterConverter implements DataURIFieldConverter<Parameter> {

    @Override
    public String toString(Parameter field) {
        return field.getAbbreviation();
    }

    @Override
    public Parameter fromString(String string) {
        Parameter p = ParameterLookup.getInstance().getParameter(string);
        if (p == null) {
            p = new Parameter(string);
        }
        return p;
    }

}
