package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.datadelivery.registry.ParameterLevel;
import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;
import com.raytheon.uf.common.registry.ebxml.DoubleAttribute;
import com.raytheon.uf.common.registry.ebxml.IntegerAttribute;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Extension of the AdhocQuery registry query. This implementation searches the
 * registry for ParameterLevel Objects that satisfy the values added with the
 * various set methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 24, 2012 455        jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * Aug 20, 2012 0743       djohnson    Use {@link IntegerAttribute#fromIntegers(List)}.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ParameterLevelQuery extends AdhocRegistryQuery<ParameterLevel> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<ParameterLevel> getObjectType() {
        return ParameterLevel.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<ParameterLevel> getResultType() {
        return ParameterLevel.class;
    }

    /**
     * A setter for the queryable attribute LevelHeight equals a single String
     * value. Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param parameterName
     *            The value of the parameterName attribute to search for.
     */
    public void setLevelHeight(double parameterName) {
        setAttribute("levelHeight", new DoubleAttribute(parameterName));
    }

    /**
     * A setter for the queryable attribute LevelHeight equals a List of Double
     * values. Using this setter will equate to an HQL "in list" query against
     * the specified column name.
     * 
     * @param parameterNames
     *            The values of the parameterName attribute to search for.
     */
    public void setLevelHeights(List<Double> parameterNames) {
        setAttribute("levelHeight", new DoubleAttribute(parameterNames));
    }

    /**
     * A setter for the queryable attribute levelId equals a single String
     * value. Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param parameterName
     *            The value of the levelId attribute to search for.
     */
    public void setLevelId(int levelId) {
        setAttribute("levelId", new IntegerAttribute(levelId));
    }

    /**
     * A setter for the queryable attribute levelId equals a List of String
     * values. Using this setter will equate to an HQL "in list" query against
     * the specified column name.
     * 
     * @param levelIds
     *            The values of the levelId attribute to search for.
     */
    public void setLevelIds(List<Integer> levelIds) {
        setAttribute("levelId", IntegerAttribute.fromIntegers(levelIds));
    }

    /**
     * A setter for the queryable attribute sequenceNumber equals a single int
     * value. Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param parameterName
     *            The value of the levelId attribute to search for.
     */
    public void setSequenceNumber(int levelId) {
        setAttribute("sequenceNumber", new IntegerAttribute(levelId));
    }

    /**
     * A setter for the queryable attribute sequenceNumber equals a List of
     * Integer values. Using this setter will equate to an HQL "in list" query
     * against the specified column name.
     * 
     * @param levelIds
     *            The values of the levelId attribute to search for.
     */
    public void setSequenceNumbers(List<Integer> levelIds) {
        setAttribute("sequenceNumber", IntegerAttribute.fromIntegers(levelIds));
    }
}
