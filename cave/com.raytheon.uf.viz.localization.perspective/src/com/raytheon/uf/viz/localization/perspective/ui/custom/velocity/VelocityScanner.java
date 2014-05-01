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
package com.raytheon.uf.viz.localization.perspective.ui.custom.velocity;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.IWhitespaceDetector;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.swt.SWT;

import com.raytheon.uf.viz.localization.perspective.ui.custom.velocity.VelocityTemplateEditor.IVelocityColorConstants;
import com.raytheon.uf.viz.localization.perspective.ui.custom.velocity.VelocityTemplateEditor.VelocityColorManager;

/**
 * Code scanner for finding velocity tokens (functions and object references
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VelocityScanner extends RuleBasedScanner {

    private IToken funcNameToken;

    private IToken funcStartToken;

    private IToken objectToken;

    private IToken objectStartToken;

    private IToken stringToken;

    private IToken defaultToken;

    public VelocityScanner(VelocityColorManager colorManager) {
        funcNameToken = new Token(new TextAttribute(
                colorManager.getColor(IVelocityColorConstants.FUNCTION), null,
                SWT.BOLD));
        objectToken = new Token(new TextAttribute(
                colorManager.getColor(IVelocityColorConstants.OBJECT)));
        stringToken = new Token(new TextAttribute(
                colorManager.getColor(IVelocityColorConstants.STRING), null,
                SWT.ITALIC));
        objectStartToken = new Token(new TextAttribute(
                colorManager.getColor(IVelocityColorConstants.OBJECT_START)));
        funcStartToken = new Token(new TextAttribute(
                colorManager.getColor(IVelocityColorConstants.FUNCTION_START)));
        defaultToken = new Token(new TextAttribute(
                colorManager.getColor(IVelocityColorConstants.DEFAULT)));

        setDefaultReturnToken(defaultToken);

        List<IRule> rules = new ArrayList<IRule>();

        rules.add(new WhitespaceRule(new IWhitespaceDetector() {
            @Override
            public boolean isWhitespace(char c) {
                return Character.isWhitespace(c);
            }
        }));
        rules.add(new SingleLineRule("\"", "\"", stringToken, '\\'));
        rules.add(new VelocityWordRule(funcNameToken, objectToken,
                funcStartToken, objectStartToken));

        setRules(rules.toArray(new IRule[rules.size()]));
    }

}
