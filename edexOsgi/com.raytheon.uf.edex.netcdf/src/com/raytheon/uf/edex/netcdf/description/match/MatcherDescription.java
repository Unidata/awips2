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
package com.raytheon.uf.edex.netcdf.description.match;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlSeeAlso;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * A matcher that contains other matchers and applies boolean logic to their
 * results.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2016 5584       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlSeeAlso(FieldMatcherDescription.class)
@XmlAccessorType(XmlAccessType.NONE)
public class MatcherDescription implements IMatcherDescription {

    @XmlAttribute
    private Type type = Type.AND;

    @XmlElements({
            @XmlElement(name = "match", type = FieldMatcherDescription.class),
            @XmlElement(name = "matches", type = MatcherDescription.class) })
    private List<IMatcherDescription> matches;

    /**
     * Constructor.
     */
    public MatcherDescription() {
        super();
    }

    @Override
    public boolean matches(NetcdfFile file) throws InvalidDescriptionException {
        if (this.matches == null || this.matches.isEmpty()) {
            // configuration is invalid. don't match anything.
            return false;
        }

        Type matchType = getType();
        boolean matches;
        for (IMatcherDescription matcher : this.matches) {
            matches = matcher.matches(file);
            if (matchType == Type.AND && !matches) {
                return false;
            } else if (matchType == Type.OR && matches) {
                return true;
            }
        }
        /*
         * If we've made it here, either matchType == AND and we've matched
         * everything so we should return True, or matchType == OR and we've
         * matched nothing and should return False.
         */
        return matchType == Type.AND;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        if (matches == null || matches.isEmpty()) {
            throw new InvalidDescriptionException("matches are not configured.");
        }
        for (IMatcherDescription matcher : matches) {
            matcher.validate();
        }
    }

    /**
     * @return the type. If not configured, AND is assumed.
     */
    public Type getType() {
        if (type == null) {
            return Type.AND;
        }
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(Type type) {
        this.type = type;
    }

    /**
     * @return the matches
     */
    public List<IMatcherDescription> getMatches() {
        return matches;
    }

    /**
     * @param matches
     *            the matches to set
     */
    public void setMatches(List<IMatcherDescription> matches) {
        this.matches = matches;
    }

    public static enum Type {
        AND,

        OR;
    }
}
