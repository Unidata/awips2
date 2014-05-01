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
package org.apache.qpid.agent.binding;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.qpid.agent.annotations.QMFEvent;
import org.apache.qpid.agent.annotations.QMFObject;
import org.apache.qpid.agent.annotations.QMFSeeAlso;
import org.apache.qpid.agent.annotations.QMFType;

/**
 * Contains the mappings from java classes to QMF schema and back. There is one
 * context per agent, and it contains all the metadata.
 */
public class BindingContext
{
    private static Log log = LogFactory.getLog(BindingContext.class);
    private Map<Key, ClassBinding> classes = new Hashtable<Key, ClassBinding>();
    private ArrayList<String> packages = new ArrayList<String>();

    static class Key
    {
        String packageName = "";
        String className = "";
        boolean object = false;

        @Override
        public int hashCode()
        {
            return (packageName + "." + className).hashCode();
        }

        @Override
        public boolean equals(Object obj)
        {
            return ((obj.getClass() == Key.class)
                    && (((Key) obj).packageName.equals(packageName)) && (((Key) obj).className
                    .equals(className)));
        }
    }

    public BindingContext()
    {
        Key key = new Key();
        key.className = "Object";
        key.packageName = "org.apache.qmf";
        key.object = false;
        ClassBinding cb = new ClassBinding("org.apache.qmf", "Object",
                Object.class, false, this);
        classes.put(key, cb);
        packages.add("org.apache.qmf");
    }

    public ClassBinding getClassBinding(Class clazz)
    {
        return classes.get(getClassKey(clazz));
    }

    public ClassBinding getClassBinding(String packageName, String className)
    {
        Key key = new Key();
        key.packageName = packageName;
        key.className = className;
        return classes.get(key);
    }

    public ClassBinding register(Class cls)
    {
        String name = cls.getName();
        ClassBinding cb = getClassBinding(cls);
        if (cb == null)
        {
            Key key = getClassKey(cls);
            // Create and store the internal representations
            if (cls.isEnum())
            {
                cb = new EnumBinding(key.packageName, key.className, cls,
                        key.object, this);
            } else
            {
                cb = new ClassBinding(key.packageName, key.className, cls,
                        key.object, this);
            }
            log.debug(String.format(
                    "Added class binding '%s' in package %s for class %s'",
                    key.className, key.packageName, cls.getCanonicalName()));
            classes.put(key, cb);
            if (!packages.contains(key.packageName))
            {
                packages.add(key.packageName);
            }
            // Parse the methods after adding the class to avoid recursion
            cb.parse();
            // See if there are other classes which should be looked at
            QMFSeeAlso seeAlso = (QMFSeeAlso) cls
                    .getAnnotation(QMFSeeAlso.class);
            if (seeAlso != null)
            {
                for (Class seeAlsoCls : seeAlso.value())
                {
                    this.register(seeAlsoCls);
                }
            }
        }
        return cb;
    }

    public TypeBinding getTypeBinding(Class cls)
    {
        // Look for a built in type
        TypeBinding type = QMFTypeBinding.forClass(cls);
        // Have we seen it before?
        if (type == null)
        {
            type = this.getClassBinding(cls);
        }
        if ((type == null) && List.class.isAssignableFrom(cls))
        {
            type = new ListBinding(this, cls);
        }
        if ((type == null) && Map.class.isAssignableFrom(cls))
        {
            type = new MapBinding(this, cls);
        }
        // Add it, but since we have not seen it before do not expose methods
        if (type == null)
        {
            type = this.register(cls);
        }
        return type;
    }

    // FIXME: Need to store these keys off so we dont create alot of objects
    protected Key getClassKey(Class cls)
    {
        Key key = new Key();
        QMFObject objAnnotation = (QMFObject) cls
                .getAnnotation(QMFObject.class);
        if (objAnnotation != null)
        {
            key.className = objAnnotation.className();
            key.packageName = objAnnotation.packageName();
            key.object = true;
        } else
        {
            QMFType typeAnnotation = (QMFType) cls.getAnnotation(QMFType.class);
            if (typeAnnotation != null)
            {
                key.className = typeAnnotation.className();
                key.packageName = typeAnnotation.packageName();
            } else
            {
                QMFEvent eventAnnotation = (QMFEvent) cls
                        .getAnnotation(QMFEvent.class);
                if (eventAnnotation != null)
                {
                    key.className = eventAnnotation.eventName();
                    key.packageName = eventAnnotation.packageName();
                } else
                {
                    // If this is Object, we return the fake
                    // object value
                    if (cls == Object.class)
                    {
                        key.className = "Object";
                        key.packageName = "org.apache.qmf";
                    } else
                    {
                        String name = cls.getName();
                        int lastDot = name.lastIndexOf('.');
                        key.className = name.substring(lastDot + 1);
                        key.packageName = name.substring(0, lastDot);
                    }
                }
            }
        }
        return key;
    }

    public ArrayList<String> getPackages()
    {
        return packages;
    }

    public Collection<ClassBinding> getAllBindings()
    {
        return classes.values();
    }
}
