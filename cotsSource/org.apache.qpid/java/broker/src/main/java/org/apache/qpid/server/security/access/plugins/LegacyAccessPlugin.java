/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */
package org.apache.qpid.server.security.access.plugins;

import java.util.Collection;
import java.util.HashSet;

import org.apache.commons.configuration.Configuration;
import org.apache.qpid.server.security.access.ACLPlugin;
import org.apache.qpid.server.security.access.ACLPluginFactory;

/**
 * 
 * Used to suppress warnings in legacy config files that have things in <security> which aren't handled by a plugin directly. 
 *
 */
public class LegacyAccessPlugin extends BasicACLPlugin
{
    public static final ACLPluginFactory FACTORY = new ACLPluginFactory()
    {
        private Collection maskedTags = new HashSet<String>();
        {
            maskedTags.add("principal-databases");
            maskedTags.add("access");
            maskedTags.add("msg-auth");
            maskedTags.add("false");
            maskedTags.add("jmx");
        }

        public boolean supportsTag(String name)
        {
            return maskedTags .contains(name);
        }

        public ACLPlugin newInstance(Configuration config)
        {
            return new LegacyAccessPlugin();
        }
    };
    
    public String getPluginName()
    {
        return getClass().getSimpleName();
    }

    @Override 
    protected AuthzResult getResult()
    {
        // Always abstain
        return AuthzResult.ABSTAIN;
    }

}
