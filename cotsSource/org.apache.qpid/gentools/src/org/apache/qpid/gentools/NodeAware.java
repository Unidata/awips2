/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.gentools;

import org.w3c.dom.Node;

/**
 * @author kpvdr
 *         Interface allowing the addition of elements from a node in the
 *         DOM of the AMQP specification. It is used by each of the model
 *         elements in a recursive fashion to build the model.
 */
public interface NodeAware
{
    /**
     * Add a model element from the current DOM node. All model elements must implement
     * this interface. If the node contains children that are also a part of the model,
     * then this method is called on new instances of those model elements.
     *
     * @param n Node from which the current model element is to be added.
     * @param o Ordinal value of the current model elemet.
     * @param v Verion of the DOM from which the node comes.
     * @throws AmqpParseException
     * @throws AmqpTypeMappingException
     * @returns true if a node was added, false if not
     */
    public boolean addFromNode(Node n, int o, AmqpVersion v)
            throws AmqpParseException, AmqpTypeMappingException;
}
