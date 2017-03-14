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
/**
 * 
 */
package com.raytheon.viz.gfe.core.script;

import java.util.regex.Pattern;

import org.eclipse.jface.dialogs.IInputValidator;

/**
 * A class to validate the name of a script
 * 
 * @author wldougher
 * 
 */
public class NewInputValidator implements IInputValidator {

    private static final Pattern VALID_NAME = Pattern
            .compile("^[\\p{Alpha}_]+\\w*");

    public NewInputValidator() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.IInputValidator#isValid(java.lang.String)
     */
    @Override
    public String isValid(String newText) {
        String newTextTrim = newText.trim();
        if (newTextTrim.isEmpty()) {
            return "Name cannot be blank.";
        }
        if (!VALID_NAME.matcher(newTextTrim).matches()) {
            return "Only letters, digits, and underscores are allowed.";
        }
        return null;
    }

}
