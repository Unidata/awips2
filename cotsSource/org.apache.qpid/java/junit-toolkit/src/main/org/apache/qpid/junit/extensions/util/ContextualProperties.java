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
package org.apache.qpid.junit.extensions.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

/**
 * ContextualProperties is an extension of {@link java.util.Properties} that automatically selects properties based on an
 * environment parameter (defined by the system property {@link #ENV_SYS_PROPERTY}), the name of a class, plus a modifier
 * (which can be used to name a method of a class) and a property key. It also supports the definition of arrays of
 * property values using indexes. The properties are searched in the following order until a match is found:
 *
 * <ol>
 * <li>environment + class name with package name + modifier + key
 * <li>environment + class name with package name + key
 * <li>environment + key
 * <li>class name with package name + modifier + key
 * <li>class name with package name + key
 * <li>key
 * </ol>
 *
 * <p>To create arrays of property values add index numbers onto the end of the property keys. An array of string values
 * will be created with the elements of the array set to the value of the property at the matching index. Ideally the
 * index values will be contiguous, starting at 0. This does not need to be the case however. If an array definition
 * begins at index n, and ends at index m, Then an array big enough to hold m + 1 elements will be created and populated
 * with values from n to m. Values before n and any missing values between n and m will be null in the array.
 *
 * <p>To give an example, suppose you have two different environments 'DEVELOPMENT' and 'PRODUCTION' and they each need
 * the same properties but set to different values for each environment and some properties the same in both, you could
 * create a properties file like:
 *
 * <p><code>
 * # Project configuration properties file.<br/>
 * <br/>
 * # These properties are environment specific.<br/>
 * DEVELOPMENT.debug=true<br/>
 * PRODUCTION.debug=false<br/>
 * <br/>
 * # Always debug MyClass in all environments but not the myMethod method.<br/>
 * MyClass.debug=true<br/>
 * MyClass.myMethod.debug=false<br/>
 * <br/>
 * # Set up an array of my ten favourite animals. Leave elements 3 to 8 as null as I haven't decided on them yet.<br/>
 * animals.0=cat<br/>
 * animals.1=dog<br/>
 * animals.2=elephant<br/>
 * animals.9=lion<br/>
 * <br/>
 * # This is a default value that will be used when the environment is not known.<br/>
 * debug=false<br/>
 * </code>
 *
 * <p>The most specific definition of a property is searched for first moving out to the most general. This allows
 * general property defaults to be set and then overiden for specific uses by some classes and modifiers.
 *
 * <p>A ContextualProperties object can be loaded in the same way as a java.utils.Properties. A recommended way to do
 * this that does not assume that the properties file is a file (it could be in a jar) is to load the properties from the
 * url for the resource lookup up on the classpath:
 *
 * <p><code>
 * Properties configProperties = new ContextualProperties();<br/>
 * configProperties.load(this.getClass().getClassLoader().getResourceAsStream("config.properties"));<br/>
 * </code>
 *
 * <p>EnvironmentProperties will load the 'DEVELOPMENT.debug' property or 'PROUCTION.debug' property based on the setting
 * of the system environment property. If a matching property for the environment cannot be found then the simple property
 * name without the environment pre-pended onto it will be used instead. This 'use of default environments' behaviour is
 * turned on initially but it can be disabled by calling the {@link #useDefaultEnvironments} method.
 *
 * <p>When a property matching a key cannot be found then the property accessor methods will always return null. If a
 * default value for a property exists but the 'use of default environments' behavious prevents it being used then the
 * accessor methods will return null.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Automatically select properties dependant on environment, class name and modifier as well as property key.
 * <tr><td> Convert indexed properties into arrays.
 * </table>
 *
 * @author Rupert Smith
 */
public class ContextualProperties extends ParsedProperties
{
    /** The name of the system property that is used to define the environment. */
    public static final String ENV_SYS_PROPERTY = "environment";

    /**
     * <p>Holds the iteration count down order.
     *
     * <p>If e = 4, b = 2, m = 1 then the iteration order or i is 7,6,4 and then if using environment defaults 3,2,0
     * where the accessor key is:
     * (i & e != 0 ? environment : "") + (i & b != 0 ? base : "") + (1 + m != 0 ? modifier : "") + key
     *
     * <p>In other words the presence or otherwise of the three least significant bits when counting down from 7
     * specifies which of the environment, base and modifier are to be included in the key where the environment, base
     * and modifier stand for the bits in positions 2, 1 and 0. The numbers 5 and 1 are missed out of the count because
     * they stand for the case where the modifier is used without the base which is not done.
     */
    private static final int[] ORDER = new int[] { 7, 6, 4, 3, 2, 0 };

    /**
     * Defines the point in the iteration count order below which the 'use environment defaults' feature is being used.
     */
    private static final int ENVIRONMENT_DEFAULTS_CUTOFF = 4;

    /** Defines the bit representation for the environment in the key ordering. See {@link #ORDER}. */
    private static final int E = 4;

    /** Defines the bit representation for the base in the key ordering. See {@link #ORDER}. */
    private static final int B = 2;

    /** Defines the bit representation for the modifier in the key ordering. See {@link #ORDER}. */
    private static final int M = 1;

    /** Used to hold the value of the environment system property. */
    private String environment;

    /** Used to indicate that the 'use of defaults' behaviour should be used. */
    private boolean useDefaults = true;

    /** Used to hold all the array properties. This is a mapping from property names to ArrayLists of Strings. */
    protected Map arrayProperties = new HashMap();

    /**
     * Default constructor that builds a ContextualProperties that uses environment defaults.
     */
    public ContextualProperties()
    {
        super();

        // Keep the value of the system environment property.
        environment = System.getProperty(ENV_SYS_PROPERTY);
    }

    /**
     * Creates a ContextualProperties that uses environment defaults and is initialized with the specified properties.
     *
     * @param props The properties to initialize this with.
     */
    public ContextualProperties(Properties props)
    {
        super(props);

        // Keep the value of the system environment property.
        environment = System.getProperty(ENV_SYS_PROPERTY);

        // Extract any array properties as arrays.
        createArrayProperties();
    }

    /**
     * Parses an input stream as properties.
     *
     * @param inStream The input stream to read the properties from.
     *
     * @exception IOException If there is an IO error during reading from the input stream.
     */
    public void load(InputStream inStream) throws IOException
    {
        super.load(inStream);

        // Extract any array properties as arrays.
        createArrayProperties();
    }

    /**
     * Tells this environment aware properties object whether it should use default environment properties without a
     * pre-pended environment when a property for the current environment cannot be found.
     *
     * @param flag True to use defaults, false to not use defaults.
     */
    public void useDefaultEnvironments(boolean flag)
    {
        useDefaults = flag;
    }

    /**
     * Looks up a property value relative to the environment, callers class and method. The default environment will be
     * checked for a matching property if defaults are being used. In order to work out the callers class and method this
     * method throws an exception and then searches one level up its stack frames.
     *
     * @param key The property key.
     *
     * @return The value of this property searching from the most specific definition (environment, class, method, key)
     * to the most general (key only), unless use of default environments is turned off in which case the most general
     * proeprty searched is (environment, key).
     */
    public String getProperty(String key)
    {
        // Try to get the callers class name and method name by examing the stack.
        String className = null;
        String methodName = null;

        // Java 1.4 onwards only.
        /*try
        {
            throw new Exception();
        }
        catch (Exception e)
        {
            StackTraceElement[] stack = e.getStackTrace();

            // Check that the stack trace contains at least two elements, one for this method and one for the caller.
            if (stack.length >= 2)
            {
                className = stack[1].getClassName();
                methodName = stack[1].getMethodName();
            }
        }*/

        // Java 1.5 onwards only.
        StackTraceElement[] stack = Thread.currentThread().getStackTrace();

        // Check that the stack trace contains at least two elements, one for this method and one for the caller.
        if (stack.length >= 2)
        {
            className = stack[1].getClassName();
            methodName = stack[1].getMethodName();
        }

        // Java 1.3 and before? Not sure, some horrible thing that parses the text spat out by printStackTrace?

        return getProperty(className, methodName, key);
    }

    /**
     * Looks up a property value relative to the environment, base class and modifier. The default environment will be
     * checked for a matching property if defaults are being used.
     *
     * @param base An object of the class to retrieve properties relative to.
     * @param modifier The modifier (which may stand for a method of the class).
     * @param key The property key.
     *
     * @return The value of this property searching from the most specific definition (environment, class, modifier, key)
     * to the most general (key only), unless use of default environments is turned off in which case the most general
     * property searched is (environment, key).
     */
    public String getProperty(Object base, String modifier, String key)
    {
        return getProperty(base.getClass().getName(), modifier, key);
    }

    /**
     * Looks up a property value relative to the environment, base class and modifier. The default environment will be
     * checked for a matching property if defaults are being used.
     *
     * @param base The name of the class to retrieve properties relative to.
     * @param modifier The modifier (which may stand for a method of the class).
     * @param key The property key.
     *
     * @return The value of this property searching from the most specific definition (environment, class, modifier, key)
     * to the most general (key only), unless use of default environments is turned off in which case the most general
     * property searched is (environment, key).
     */
    public String getProperty(String base, String modifier, String key)
    {
        String result = null;

        // Loop over the key orderings, from the most specific to the most general, until a matching value is found.
        for (Iterator i = getKeyIterator(base, modifier, key); i.hasNext();)
        {
            String nextKey = (String) i.next();

            result = super.getProperty(nextKey);

            if (result != null)
            {
                break;
            }
        }

        return result;
    }

    /**
     * Looks up an array property value relative to the environment, callers class and method. The default environment
     * will be checked for a matching array property if defaults are being used. In order to work out the callers class
     * and method this method throws an exception and then searches one level up its stack frames.
     *
     * @param key The property key.
     *
     * @return The array value of this indexed property searching from the most specific definition (environment, class,
     *         method, key) to the most general (key only), unless use of default environments is turned off in which
     *         case the most general proeprty searched is (environment, key).
     */
    public String[] getProperties(String key)
    {
        // Try to get the callers class name and method name by throwing an exception an searching the stack frames.
        String className = null;
        String methodName = null;

        /* Java 1.4 onwards only.
           try {
             throw new Exception();
           } catch (Exception e) {
             StackTraceElement[] stack = e.getStackTrace();
             // Check that the stack trace contains at least two elements, one for this method and one for the caller.
             if (stack.length >= 2) {
               className = stack[1].getClassName();
               methodName = stack[1].getMethodName();
             }
           }*/
        return getProperties(className, methodName, key);
    }

    /**
     * Looks up an array property value relative to the environment, base class and modifier. The default environment will
     * be checked for a matching array property if defaults are being used.
     *
     * @param base An object of the class to retrieve properties relative to.
     * @param modifier The modifier (which may stand for a method of the class).
     * @param key The property key.
     *
     * @return The array value of this indexed property searching from the most specific definition (environment, class,
     *         modifier, key) to the most general (key only), unless use of default environments is turned off in which
     *         case the most general proeprty searched is (environment, key).
     */
    public String[] getProperties(Object base, String modifier, String key)
    {
        return getProperties(base.getClass().getName(), modifier, key);
    }

    /**
     * Looks up an array property value relative to the environment, base class and modifier. The default environment will
     * be checked for a matching array property if defaults are being used.
     *
     * @param base The name of the class to retrieve properties relative to.
     * @param modifier The modifier (which may stand for a method of the class).
     * @param key The property key.
     *
     * @return The array value of this indexed property searching from the most specific definition (environment, class,
     *         modifier, key) to the most general (key only), unless use of default environments is turned off in which
     *         case the most general property searched is (environment, key).
     */
    public String[] getProperties(String base, String modifier, String key)
    {
        String[] result = null;

        // Loop over the key orderings, from the most specific to the most general, until a matching value is found.
        for (Iterator i = getKeyIterator(base, modifier, key); i.hasNext();)
        {
            String nextKey = (String) i.next();
            ArrayList arrayList = (ArrayList) arrayProperties.get(nextKey);

            if (arrayList != null)
            {
                result = (String[]) arrayList.toArray(new String[] {});

                break;
            }
        }

        return result;
    }

    /**
     * For a given environment, base, modifier and key and setting of the use of default environments feature this
     * generates an iterator that walks over the order in which to try and access properties.
     *
     * <p>See the {@link #ORDER} constant for an explanation of how the key ordering is generated.
     *
     * @param base The name of the class to retrieve properties relative to.
     * @param modifier The modifier (which may stand for a method of the class).
     * @param key The property key.
     *
     * @return An Iterator over String keys defining the order in which properties should be accessed.
     */
    protected Iterator getKeyIterator(final String base, final String modifier, final String key)
    {
        return new Iterator()
            {
                // The key ordering count always begins at the start of the ORDER array.
                private int i = 0;

                public boolean hasNext()
                {
                    return (useDefaults ? ((i < ORDER.length) && (ORDER[i] > ENVIRONMENT_DEFAULTS_CUTOFF))
                                        : (i < ORDER.length));
                }

                public Object next()
                {
                    // Check that there is a next element and return null if not.
                    if (!hasNext())
                    {
                        return null;
                    }

                    // Get the next ordering count.
                    int o = ORDER[i];

                    // Do bit matching on the count to choose which elements to include in the key.
                    String result =
                        (((o & E) != 0) ? (environment + ".") : "") + (((o & B) != 0) ? (base + ".") : "")
                        + (((o & M) != 0) ? (modifier + ".") : "") + key;

                    // Increment the iterator to get the next key on the next call.
                    i++;

                    return result;
                }

                public void remove()
                {
                    // This method is not supported.
                    throw new UnsupportedOperationException("remove() is not supported on this key order iterator as "
                        + "the ordering cannot be changed");
                }
            };
    }

    /**
     * Scans all the properties in the parent Properties object and creates arrays for any array property definitions.
     *
     * <p>Array properties are defined with indexes. For example:
     *
     * <p><code>
     * property.1=one<br/>
     * property.2=two<br/>
     * property.3=three<br/>
     * </code>
     *
     * <p>Note that these properties will be stored as the 'empty string' or "" property array.
     *
     * <p><code>
     * .1=one<br/>
     * 2=two<br/>
     * </code>
     */
    protected void createArrayProperties()
    {
        // Scan through all defined properties.
        for (Object o : keySet())
        {
            String key = (String) o;
            String value = super.getProperty(key);

            // Split the property key into everything before the last '.' and after it.
            int lastDotIndex = key.lastIndexOf('.');
            String keyEnding = key.substring(lastDotIndex + 1, key.length());
            String keyStart = key.substring(0, (lastDotIndex == -1) ? 0 : lastDotIndex);

            // Check if the property key ends in an integer, in which case it is an array property.
            int index = 0;

            try
            {
                index = Integer.parseInt(keyEnding);
            }
            // The ending is not an integer so its not an array.
            catch (NumberFormatException e)
            {
                // Scan the next property.
                continue;
            }

            // Check if an array property already exists for this base name and create one if not.
            ArrayList propArray = (ArrayList) arrayProperties.get(keyStart);

            if (propArray == null)
            {
                propArray = new ArrayList();
                arrayProperties.put(keyStart, propArray);
            }

            // Add the new property value to the array property for the index.
            propArray.set(index, value);
        }
    }
}
