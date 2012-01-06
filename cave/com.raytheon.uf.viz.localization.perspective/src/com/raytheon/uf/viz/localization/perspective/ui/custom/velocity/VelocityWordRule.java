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

import java.util.Stack;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

/**
 * Class that finds velocity word rules
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VelocityWordRule implements IRule {

    private IToken functionToken;

    private IToken objectToken;

    private IToken objectStartToken;

    private IToken functionStartToken;

    private IToken lastToken = null;

    private boolean escaped = false;

    private Stack<IToken> objectStack = new Stack<IToken>();

    public VelocityWordRule(IToken functionToken, IToken objectToken,
            IToken functionStartToken, IToken objectStartToken) {
        this.functionToken = functionToken;
        this.objectToken = objectToken;
        this.functionStartToken = functionStartToken;
        this.objectStartToken = objectStartToken;

        objectStack.push(null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.rules.IRule#evaluate(org.eclipse.jface.text.rules
     * .ICharacterScanner)
     */
    @Override
    public IToken evaluate(ICharacterScanner scanner) {
        if (lastToken == null) {
            int first = scanner.read();
            // look for start of default token
            if ((first == '$' || first == '#') && !escaped) {
                int second = scanner.read();
                if (Character.isLetter(second)
                        || (first == '$' && second == '{')) {
                    // we have a valid start of word after identifier, set
                    // lastToken so we know to look for a word next
                    if (second != '{' || Character.isLetter(scanner.read())) {
                        lastToken = first == '$' ? objectStartToken
                                : functionStartToken;
                        if (second == '{') {
                            objectStack.push(objectStartToken);
                        }
                        scanner.unread();
                        return lastToken;
                    }
                }
                scanner.unread();
            }
            scanner.unread();
        } else if (lastToken == functionStartToken
                || lastToken == objectStartToken) {
            // lastToken was an identifier of word token and we know a word is
            // coming, find the word

            StringBuilder sb = new StringBuilder();
            // we know the first one is good, read it
            int next = scanner.read();
            while (Character.isLetter(next) || Character.isDigit(next)
                    || next == '_' || next == '-') {
                sb.append((char) next);
                next = scanner.read();
            }

            scanner.unread();
            IToken rval = lastToken == objectStartToken ? objectToken
                    : functionToken;
            lastToken = null;
            return rval;
        }

        IToken returnVal = Token.UNDEFINED;

        int check = scanner.read();
        if (check == '\\') {
            escaped = !escaped;
            scanner.read();
        } else {
            escaped = false;
            if (check == '}' && objectStack.peek() != null) {
                returnVal = objectStack.pop();
                scanner.read();
            }
        }
        scanner.unread();

        return returnVal;
    }
}
