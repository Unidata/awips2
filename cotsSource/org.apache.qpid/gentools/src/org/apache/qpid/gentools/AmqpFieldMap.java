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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

@SuppressWarnings("serial")
public class AmqpFieldMap implements VersionConsistencyCheck
{

    private final TreeMap<String, AmqpField> _map = new TreeMap<String, AmqpField>();

    private final AmqpVersionSet _versionSet = new AmqpVersionSet();

    public void removeVersion(AmqpVersion version)
    {
        String[] fieldNameArray = new String[size()];
        _map.keySet().toArray(fieldNameArray);
        Iterator<Entry<String, AmqpField>> iter = _map.entrySet().iterator();

        while (iter.hasNext())
        {
            Entry<String, AmqpField> entry = iter.next();
            entry.getValue().removeVersion(version);
            iter.remove();
        }
    }

    public int size()
    {
        return _map.size();

    }

    public AmqpFieldMap getFieldMapForOrdinal(int ordinal)
    {
        AmqpFieldMap newMap = new AmqpFieldMap();
        for (AmqpField field : _map.values())
        {

            TreeMap<Integer, AmqpVersionSet> ordinalMap = field.getOrdinalMap();
            AmqpVersionSet ordinalVersions = ordinalMap.get(ordinal);
            if (ordinalVersions != null)
            {
                newMap.add(field.getName(), field);
            }
        }
        return newMap;
    }

    public void add(String name, AmqpField field)
    {
        _versionSet.addAll(field.getVersionSet());
        _map.put(name, field);
    }

    public AmqpOrdinalFieldMap getMapForVersion(AmqpVersion version, boolean codeTypeFlag,
                                                LanguageConverter converter)
    {
        // TODO: REVIEW THIS! There may be a bug here that affects C++ generation (only with >1 version)...
        // If version == null (a common scenario) then the version map is built up on the
        // basis of first found item, and ignores other version variations.
        // This should probably be disallowed by throwing an NPE, as AmqpOrdinalFieldMap cannot
        // represent these possibilities.
        // *OR*
        // Change the structure of AmqpOrdianlFieldMap to allow for the various combinations that
        // will result from version variation - but that is what AmqpFieldMap is... :-$
        AmqpOrdinalFieldMap ordinalFieldMap = new AmqpOrdinalFieldMap();
        for (AmqpField field : _map.values())
        {

            if (version == null || field.getVersionSet().contains(version))
            {
                // 1. Search for domain name in field domain map with version that matches
                String domain = "";
                boolean dFound = false;
                for (String thisDomainName : field.getDomainMap().keySet())
                {
                    domain = thisDomainName;
                    AmqpVersionSet versionSet = field.getDomainMap().get(domain);
                    if (version == null || versionSet.contains(version))
                    {
                        if (codeTypeFlag)
                        {
                            domain = converter.getGeneratedType(domain, version);
                        }
                        dFound = true;
                    }
                }

                // 2. Search for ordinal in field ordianl map with version that matches
                int ordinal = -1;
                boolean oFound = false;
                for (Integer thisOrdinal : field.getOrdinalMap().keySet())
                {
                    ordinal = thisOrdinal;
                    AmqpVersionSet versionSet = field.getOrdinalMap().get(ordinal);
                    if (version == null || versionSet.contains(version))
                    {
                        oFound = true;
                    }
                }

                if (dFound && oFound)
                {
                    String[] fieldDomainPair = {field.getName(), domain};
                    ordinalFieldMap.put(ordinal, fieldDomainPair);
                }
            }
        }
        return ordinalFieldMap;
    }

    public boolean isDomainConsistent(Generator generator, AmqpVersionSet versionSet)
            throws AmqpTypeMappingException
    {
        if (size() != 1) // Only one field for this ordinal
        {
            return false;
        }
        return _map.get(_map.firstKey()).isConsistent(generator);
    }

    public int getNumFields(AmqpVersion version)
    {
        int fCntr = 0;
        for (AmqpField field : _map.values())
        {

            if (field.getVersionSet().contains(version))
            {
                fCntr++;
            }
        }
        return fCntr;
    }

    public String parseFieldMap(CommandGenerateMethod commonGenerateMethod, MangledGenerateMethod mangledGenerateMethod,
                                int indentSize, int tabSize, LanguageConverter converter)
    {
        String indent = Utils.createSpaces(indentSize);
        String cr = Utils.LINE_SEPARATOR;
        StringBuffer sb = new StringBuffer();

        if (commonGenerateMethod == null)
        {
            // Generate warnings in code if required methods are null.
            sb.append(indent + "/*********************************************************" + cr);
            sb.append(indent + " * WARNING: Generated code could be missing." + cr);
            sb.append(indent + " * In call to parseFieldMap(), generation method was null." + cr);
            sb.append(indent + " * Check for NoSuchMethodException on startup." + cr);
            sb.append(indent + " *********************************************************/" + cr);
        }

        Iterator<Entry<String, AmqpField>> itr = _map.entrySet().iterator();
        while (itr.hasNext())
        {
            Entry<String, AmqpField> entry = itr.next();
            String fieldName = entry.getKey();
            AmqpField field = entry.getValue();
            if (field.isCodeTypeConsistent(converter))
            {
                // All versions identical - Common declaration
                String domainName = field.getDomainMap().firstKey();
                AmqpVersionSet versionSet = field.getDomainMap().get(domainName);
                String codeType = converter.getGeneratedType(domainName, versionSet.first());
                if (commonGenerateMethod != null)
                {
                    sb.append(commonGenerateMethod.generate(codeType, field, versionSet,
                                                            indentSize, tabSize, itr.hasNext()));
                }
            }
            else if (mangledGenerateMethod != null) // Version-mangled
            {
                sb.append(mangledGenerateMethod.generate(field, indentSize, tabSize,
                                                         itr.hasNext()));
            }
        }
        return sb.toString();
    }

    public String parseFieldMapOrdinally(GenerateMethod generateMethod, BitFieldGenerateMethod bitGenerateMethod,
                                         int indentSize, int tabSize, Generator codeGenerator)
    {
        String indent = Utils.createSpaces(indentSize);
        String cr = Utils.LINE_SEPARATOR;
        StringBuffer sb = new StringBuffer();

        // Generate warnings in code if required methods are null.
        if (generateMethod == null || bitGenerateMethod == null)
        {
            sb.append(indent + "/***********************************************" + cr);
            sb.append(indent + " * WARNING: In call to parseFieldMapOrdinally():" + cr);
            if (generateMethod == null)
            {
                sb.append(indent + " *  => generateMethod is null." + cr);
            }
            if (bitGenerateMethod == null)
            {
                sb.append(indent + " *  => bitGenerateMethod is null." + cr);
            }
            sb.append(indent + " * Generated code could be missing." + cr);
            sb.append(indent + " * Check for NoSuchMethodException on startup." + cr);
            sb.append(indent + " ***********************************************/" + cr);
        }

        /* We must process elements in ordinal order because adjacent booleans (bits)
           * must be combined into a single byte (in groups of up to 8). Start with shared
           * declarations until an ordinal divergence is found. (For most methods where
           * there is no difference between versions, this will simplify the generated
           * code. */

        ArrayList<String> bitFieldList = new ArrayList<String>();
        boolean ordinalDivergenceFlag = false;
        int ordinal = 0;
        while (ordinal < size() && !ordinalDivergenceFlag)
        {
            /* Since the getFieldMapOrdinal() function may map more than one Field to
                * an ordinal, the number of ordinals may be less than the total number of
                * fields in the fieldMap. Check for empty fieldmaps... */
            AmqpFieldMap ordinalFieldMap = getFieldMapForOrdinal(ordinal);
            if (ordinalFieldMap.size() > 0)
            {
                if (ordinalFieldMap.isDomainConsistent(codeGenerator, getVersionSet()))
                {
                    String fieldName = ordinalFieldMap.getFirstFieldName();
                    String domain = ordinalFieldMap._map.get(fieldName).getDomainMap().firstKey();

                    String domainType = codeGenerator.getDomainType(domain,
                                                                    codeGenerator.getVersionSet().first());

                    if (domainType.compareTo("bit") == 0)
                    {
                        bitFieldList.add(fieldName);
                    }
                    else if (bitFieldList.size() > 0)
                    {
                        // End of bit types - handle deferred bit type generation
                        if (bitGenerateMethod != null)
                        {
                            sb.append(bitGenerateMethod.generate(bitFieldList, ordinal,
                                                                 indentSize, tabSize));
                        }
                        bitFieldList.clear();
                    }
                    if (!ordinalDivergenceFlag)
                    {
                        // Defer generation of bit types until all adjacent bits have been
                        // accounted for.
                        if (bitFieldList.size() == 0 && generateMethod != null)
                        {
                            sb.append(generateMethod.generate(domainType, fieldName, ordinal,
                                                              indentSize, tabSize));
                        }
                    }
                    ordinal++;
                }
                else
                {
                    ordinalDivergenceFlag = true;
                }
            }
        }

        // Check if there is still more to do under a version-specific breakout
        if (ordinalDivergenceFlag && ordinal < size())
        {
            // 1. Cycle through all versions in order, create outer if(version) structure
            AmqpVersion[] versionArray = new AmqpVersion[getVersionSet().size()];
            getVersionSet().toArray(versionArray);
            for (int v = 0; v < versionArray.length; v++)
            {
                sb.append(indent);
                if (v > 0)
                {
                    sb.append("else ");
                }
                sb.append("if (major == " + versionArray[v].getMajor() + " && minor == " +
                          versionArray[v].getMinor() + ")" + cr);
                sb.append(indent + "{" + cr);

                // 2. Cycle though each ordinal from where we left off in the loop above.
                ArrayList<String> bitFieldList2 = new ArrayList<String>(bitFieldList);
                for (int o = ordinal; o < size(); o++)
                {
                    AmqpFieldMap ordinalFieldMap = getFieldMapForOrdinal(o);
                    if (ordinalFieldMap.size() > 0)
                    {
                        // 3. Cycle through each of the fields that have this ordinal.
                        Iterator<Map.Entry<String, AmqpField>> i = ordinalFieldMap._map.entrySet().iterator();
                        while (i.hasNext())
                        {

                            Map.Entry<String, AmqpField> entry = i.next();
                            AmqpField field = entry.getValue();
                            String fieldName = entry.getKey();

                            // 4. Some fields may have more than one ordinal - match by both
                            //    ordinal and version.
                            Iterator<Integer> j = field.getOrdinalMap().keySet().iterator();
                            while (j.hasNext())
                            {
                                int thisOrdinal = j.next();
                                AmqpVersionSet v1 = field.getOrdinalMap().get(thisOrdinal);
                                if (thisOrdinal == o && v1.contains(versionArray[v]))
                                {
                                    // 5. Now get the domain for this version
                                    int domainCntr = 0;
                                    Iterator<String> k = field.getDomainMap().keySet().iterator();
                                    while (k.hasNext())
                                    {
                                        // Mangle domain-divergent field names
                                        String mangledFieldName = fieldName;
                                        if (field.getDomainMap().size() > 1)
                                        {
                                            mangledFieldName += "_" + (domainCntr++);
                                        }
                                        String domainName = k.next();
                                        AmqpVersionSet v2 = field.getDomainMap().get(domainName);
                                        if (v2.contains(versionArray[v]))
                                        {
                                            // 6. (Finally!!) write the declaration
                                            String domainType = codeGenerator.getDomainType(domainName,
                                                                                            versionArray[v]);
                                            if (domainType.compareTo("bit") == 0)
                                            {
                                                bitFieldList2.add(mangledFieldName);
                                            }
                                            else if (bitFieldList2.size() > 0)
                                            {
                                                // End of bit types - handle deferred bit type generation
                                                if (bitGenerateMethod != null)
                                                {
                                                    sb.append(bitGenerateMethod.generate(
                                                            bitFieldList2, o, indentSize + tabSize,
                                                            tabSize));
                                                }
                                                bitFieldList2.clear();
                                            }
                                            // Defer generation of bit types until all adjacent bits have
                                            // been accounted for.
                                            if (bitFieldList2.size() == 0 && generateMethod != null)
                                            {
                                                sb.append(generateMethod.generate(domainType,
                                                                                  mangledFieldName, o, indentSize + tabSize, tabSize));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                // Check for remaining deferred bits
                if (bitFieldList2.size() > 0 && bitGenerateMethod != null)
                {
                    sb.append(bitGenerateMethod.generate(bitFieldList2, size(),
                                                         indentSize + tabSize, tabSize));
                }
                sb.append(indent + "}" + cr);
            }
        }
        // Check for remaining deferred bits
        else if (bitFieldList.size() > 0 && bitGenerateMethod != null)
        {
            sb.append(bitGenerateMethod.generate(bitFieldList, size(),
                                                 indentSize, tabSize));
        }
        return sb.toString();
    }

    private String getFirstFieldName()
    {
        return _map.firstKey();
    }

    public boolean isVersionConsistent(AmqpVersionSet globalVersionSet)
    {
        for (String thisFieldName : _map.keySet())
        {
            AmqpField field = _map.get(thisFieldName);
            if (!field.isVersionConsistent(globalVersionSet))
            {
                return false;
            }
        }
        return true;
    }

    public boolean isVersionInterfaceConsistent(AmqpVersionSet globalVersionSet)
    {
        for (String thisFieldName : _map.keySet())
        {
            AmqpField field = _map.get(thisFieldName);
            if (!field.isVersionInterfaceConsistent(globalVersionSet))
            {
                return false;
            }
        }
        return true;
    }


    public AmqpVersionSet getVersionSet()
    {
        return _versionSet;
    }

    public Collection<AmqpField> values()
    {
        return _map.values();
    }

    public AmqpField get(String fieldName)
    {
        return _map.get(fieldName);
    }

    public void remove(String fieldName)
    {
        _map.remove(fieldName);
    }

    public Set<String> keySet()
    {
        return _map.keySet();
    }
}
