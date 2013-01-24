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
package com.raytheon.uf.common.serialization;


/**
 * Provides reusable functionality among {@link ISerializationContext}
 * implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2012 1169       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public abstract class BaseSerializationContext implements
        ISerializationContext, IDeserializationContext {
    protected final DynamicSerializationManager serializationManager;

    /**
     * Constructor.
     * 
     * @param serializationManager
     *            the serialization manager
     */
    public BaseSerializationContext(
            DynamicSerializationManager serializationManager) {
        this.serializationManager = serializationManager;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object readObject() throws SerializationException {
        return this.serializationManager.deserialize(this);
    }

    /**
     * {@inheritDoc}
     * 
     * @throws SerializationException
     */
    @Override
    public void writeObject(Object obj) throws SerializationException {
        this.serializationManager.serialize(this, obj);
    }
}
