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
 * A validator for destination script names.
 * 
 * @author wldougher
 * 
 */
public class CopyInputValidator implements IInputValidator {

    protected String script;

    protected String scriptf;

    protected IScriptUtil util;

    private static final Pattern VALID_NAME = Pattern
            .compile("^[\\p{Alpha}_]+\\w*");

    /**
     * Constructor.
     * 
     * @param script
     *            The name of the script to copy
     * @param util
     *            The script utility object that will do the work.
     */
    public CopyInputValidator(String script, IScriptUtil util) {
        this.script = script;
        this.util = util;
        this.scriptf = util.scripted(script);
    }

    /**
     * Check the name. This validator only allows simple names without blanks or
     * punctuation except underline, and doesn't allow the user to enter the
     * original script name.
     * 
     * @see org.eclipse.jface.dialogs.IInputValidator#isValid(java.lang.String)
     */
    @Override
    public String isValid(String newText) {
        if (newText.trim().length() == 0) {
            return ("Name cannot be blank.");
        }
        if (!VALID_NAME.matcher(newText.trim()).matches()) {
            return "Only letters, digits, and underscores are allowed.";
        }
        String newfname = util.scripted(newText);
        if (scriptf.equals(newfname)) {
            return script + " cannot be copied to itself.";
        }
        return null;
    }

}
