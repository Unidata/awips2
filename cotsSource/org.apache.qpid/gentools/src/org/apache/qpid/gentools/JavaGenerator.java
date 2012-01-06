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

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

public class JavaGenerator extends Generator
{
    // TODO: Move this to parent class
    protected static final int FIELD_NAME = 0;
    protected static final int FIELD_CODE_TYPE = 1;

    private class DomainInfo
    {
        final public String type;
        final public String size;
        final public String encodingType;
        final public String encodeExpression;
        final public String decodeExpression;

        public DomainInfo(String domain, String size, String encodingType, String encodeExpression, String decodeExpression)
        {
            this.type = domain;
            this.size = size;
            this.encodeExpression = encodeExpression;
            this.decodeExpression = decodeExpression;
            this.encodingType = encodingType;
        }
    }

    private static TreeMap<String, DomainInfo> typeMap = new TreeMap<String, DomainInfo>();

    // Methods used for generation of code snippets called from the field map parsers

    // Common methods
    private final CommandGenerateMethod declarationGenerateMethod = new CommandGenerateMethod()
    {
        public String generate(String codeType, AmqpField field, AmqpVersionSet versionSet, int indentSize, int tabSize, boolean notLast)
        {
            return generateFieldDeclaration(codeType, field, versionSet, indentSize, tabSize, notLast);
        }
    };

    private MangledGenerateMethod mangledDeclarationGenerateMethod = new MangledGenerateMethod()
    {
        public String generate(AmqpField field, int indentSize, int tabSize, boolean notLast)
        {
            return generateMangledFieldDeclaration(field, indentSize, tabSize, notLast);
        }
    };

    // Methods for MessageBody classes
    private CommandGenerateMethod mbGetGenerateMethod = new CommandGenerateMethod()
    {
        public String generate(String codeType, AmqpField field, AmqpVersionSet versionSet, int indentSize, int tabSize, boolean notLast)
        {
            return generateMbGetMethod(codeType, field, versionSet, indentSize, tabSize, notLast);  //To change body of implemented methods use File | Settings | File Templates.
        }
    };

    private MangledGenerateMethod mbMangledGetGenerateMethod = new MangledGenerateMethod()
    {
        public String generate(AmqpField field, int indentSize, int tabSize, boolean notLast)
        {
            return generateMbMangledGetMethod(field, indentSize, tabSize, notLast);
        }
    };
    private CommandGenerateMethod mbParamListGenerateMethod = new CommandGenerateMethod()
    {
        public String generate(String codeType, AmqpField field, AmqpVersionSet versionSet, int indentSize, int tabSize, boolean notLast)
        {
            return generateMbParamList(codeType, field, versionSet, indentSize, tabSize, notLast);
        }
    };
    private CommandGenerateMethod mbPassedParamListGenerateMethod = new CommandGenerateMethod()
    {
        public String generate(String codeType, AmqpField field, AmqpVersionSet versionSet, int indentSize, int tabSize, boolean notLast)
        {
            return generateMbPassedParamList(codeType, field, versionSet, indentSize, tabSize, notLast);
        }
    };
    private MangledGenerateMethod mbMangledParamListGenerateMethod = new MangledGenerateMethod()
    {
        public String generate(AmqpField field, int indentSize, int tabSize, boolean notLast)
        {
            return generateMbMangledParamList(field, indentSize, tabSize, notLast);
        }
    };
    private MangledGenerateMethod mbMangledPassedParamListGenerateMethod = new MangledGenerateMethod()
    {
        public String generate(AmqpField field, int indentSize, int tabSize, boolean notLast)
        {
            return generateMbMangledPassedParamList(field, indentSize, tabSize, notLast);
        }
    };
    private CommandGenerateMethod mbBodyInitGenerateMethod = new CommandGenerateMethod()
    {
        public String generate(String codeType, AmqpField field, AmqpVersionSet versionSet, int indentSize, int tabSize, boolean notLast)
        {
            return generateMbBodyInit(codeType, field, versionSet, indentSize, tabSize, notLast);
        }
    };
    private MangledGenerateMethod mbMangledBodyInitGenerateMethod = new MangledGenerateMethod()
    {
        public String generate(AmqpField field, int indentSize, int tabSize, boolean notLast)
        {
            return generateMbMangledBodyInit(field, indentSize, tabSize, notLast);
        }
    };
    private GenerateMethod mbSizeGenerateMethod = new GenerateMethod()
    {
        public String generate(String domainType, String fieldName, int ordinal, int indentSize, int tabSize)
        {
            return generateMbFieldSize(domainType, fieldName, ordinal, indentSize, tabSize);
        }
    };
    private BitFieldGenerateMethod mbBitSizeGenerateMethod = new BitFieldGenerateMethod()
    {
        public String generate(List<String> bitFieldList, int ordinal, int indentSize, int tabSize)
        {
            return generateMbBitArrayFieldSize(bitFieldList, ordinal, indentSize, tabSize);
        }
    };
    private GenerateMethod mbEncodeGenerateMethod = new GenerateMethod()
    {
        public String generate(String domainType, String fieldName, int ordinal, int indentSize, int tabSize)
        {
            return generateMbFieldEncode(domainType, fieldName, ordinal, indentSize, tabSize);
        }
    };
    private BitFieldGenerateMethod mbBitEncodeGenerateMethod = new BitFieldGenerateMethod()
    {
        public String generate(List<String> bitFieldList, int ordinal, int indentSize, int tabSize)
        {
            return generateMbBitFieldEncode(bitFieldList, ordinal, indentSize, tabSize);
        }
    };
    private GenerateMethod mbDecodeGenerateMethod = new GenerateMethod()
    {
        public String generate(String domainType, String fieldName, int ordinal, int indentSize, int tabSize)
        {
            return generateMbFieldDecode(domainType, fieldName, ordinal, indentSize, tabSize);
        }
    };
    private BitFieldGenerateMethod mbBitDecodeGenerateMethod = new BitFieldGenerateMethod()
    {
        public String generate(List<String> bitFieldList, int ordinal, int indentSize, int tabSize)
        {
            return generateMbBitFieldDecode(bitFieldList, ordinal, indentSize, tabSize);
        }
    };
    private GenerateMethod mbToStringGenerateMethod = new GenerateMethod()
    {
        public String generate(String domainType, String fieldName, int ordinal, int indentSize, int tabSize)
        {
            return generateMbFieldToString(domainType, fieldName, ordinal, indentSize, tabSize);
        }
    };
    private BitFieldGenerateMethod mbBitToStringGenerateMethod = new BitFieldGenerateMethod()
    {
        public String generate(List<String> bitFieldList, int ordinal, int indentSize, int tabSize)
        {
            return generateMbBitFieldToString(bitFieldList, ordinal, indentSize, tabSize);
        }
    };

    // Methods for PropertyContentHeader classes
    private CommandGenerateMethod pchClearGenerateMethod = new CommandGenerateMethod()
    {
        public String generate(String codeType, AmqpField field, AmqpVersionSet versionSet, int indentSize, int tabSize, boolean notLast)
        {
            return generatePchClearMethod(codeType, field, versionSet, indentSize, tabSize, notLast);
        }
    };
    private MangledGenerateMethod pchMangledClearGenerateMethod = new MangledGenerateMethod()
    {
        public String generate(AmqpField field, int indentSize, int tabSize, boolean notLast)
        {
            return generatePchMangledClearMethod(field, indentSize, tabSize, notLast);
        }
    };
    private CommandGenerateMethod pchGetGenerateMethod = new CommandGenerateMethod()
    {
        public String generate(String codeType, AmqpField field, AmqpVersionSet versionSet, int indentSize, int tabSize, boolean notLast)
        {
            return generatePchGetMethod(codeType, field, versionSet, indentSize, tabSize, notLast);
        }
    };
    private MangledGenerateMethod pchMangledGetGenerateMethod = new MangledGenerateMethod()
    {
        public String generate(AmqpField field, int indentSize, int tabSize, boolean notLast)
        {
            return generatePchMangledGetMethod(field, indentSize, tabSize, notLast);
        }
    };
    private CommandGenerateMethod pchSetGenerateMethod = new CommandGenerateMethod()
    {
        public String generate(String codeType, AmqpField field, AmqpVersionSet versionSet, int indentSize, int tabSize, boolean notLast)
        {
            return generatePchSetMethod(codeType, field, versionSet, indentSize, tabSize, notLast);
        }
    };
    private MangledGenerateMethod pchMangledSetGenerateMethod = new MangledGenerateMethod()
    {
        public String generate(AmqpField field, int indentSize, int tabSize, boolean notLast)
        {
            return generatePchMangledSetMethod(field, indentSize, tabSize, notLast);
        }
    };
    private GenerateMethod pchSizeGenerateMethod = new GenerateMethod()
    {
        public String generate(String domainType, String fieldName, int ordinal, int indentSize, int tabSize)
        {
            return generatePchFieldSize(domainType, fieldName, ordinal, indentSize, tabSize);
        }
    };
    private BitFieldGenerateMethod pchBitSizeGenerateMethod = new BitFieldGenerateMethod()
    {
        public String generate(List<String> bitFieldList, int ordinal, int indentSize, int tabSize)
        {
            return generatePchBitArrayFieldSize(bitFieldList, ordinal, indentSize, tabSize);
        }
    };
    private GenerateMethod pchEncodeGenerateMethod = new GenerateMethod()
    {
        public String generate(String domainType, String fieldName, int ordinal, int indentSize, int tabSize)
        {
            return generatePchFieldEncode(domainType, fieldName, ordinal, indentSize, tabSize);
        }
    };
    private BitFieldGenerateMethod pchBitEncodeGenerateMethod = new BitFieldGenerateMethod()
    {
        public String generate(List<String> bitFieldList, int ordinal, int indentSize, int tabSize)
        {
            return generatePchBitFieldEncode(bitFieldList, ordinal, indentSize, tabSize);
        }
    };
    private GenerateMethod pchDecodeGenerateMethod = new GenerateMethod()
    {
        public String generate(String domainType, String fieldName, int ordinal, int indentSize, int tabSize)
        {
            return generatePchFieldDecode(domainType, fieldName, ordinal, indentSize, tabSize);
        }
    };
    private BitFieldGenerateMethod pchBitDecodeGenerateMethod = new BitFieldGenerateMethod()
    {
        public String generate(List<String> bitFieldList, int ordinal, int indentSize, int tabSize)
        {
            return generatePchBitFieldDecode(bitFieldList, ordinal, indentSize, tabSize);
        }
    };
    private GenerateMethod pchGetPropertyFlagsGenerateMethod = new GenerateMethod()
    {
        public String generate(String domainType, String fieldName, int ordinal, int indentSize, int tabSize)
        {
            return generatePchGetPropertyFlags(domainType, fieldName, ordinal, indentSize, tabSize);
        }
    };
    private BitFieldGenerateMethod pchBitGetPropertyFlagsGenerateMethod = new BitFieldGenerateMethod()
    {
        public String generate(List<String> bitFieldList, int ordinal, int indentSize, int tabSize)
        {
            return generatePchBitGetPropertyFlags(bitFieldList, ordinal, indentSize, tabSize);
        }
    };
    private GenerateMethod pchSetPropertyFlagsGenerateMethod = new GenerateMethod()
    {
        public String generate(String domainType, String fieldName, int ordinal, int indentSize, int tabSize)
        {
            return generatePchSetPropertyFlags(domainType, fieldName, ordinal, indentSize, tabSize);
        }
    };
    private BitFieldGenerateMethod pchBitSetPropertyFlagsGenerateMethod = new BitFieldGenerateMethod()
    {
        public String generate(List<String> bitFieldList, int ordinal, int indentSize, int tabSize)
        {
            return generatePchBitSetPropertyFlags(bitFieldList, ordinal, indentSize, tabSize);
        }
    };


    public String getNativeType(String type)
    {
        return typeMap.get(type).type;
    }

    public String getEncodingType(String type)
    {
        return typeMap.get(type).encodingType;
    }


    public JavaGenerator()
    {
        super();
        // Load Java type and size maps.
        // Adjust or add to these lists as new types are added/defined.
        // The char '#' will be replaced by the field variable name (any type).
        // The char '~' will be replaced by the compacted bit array size (type bit only).
        typeMap.put("bit", new DomainInfo(
                "boolean",                                        // Java code type
                "~",                                            // size
                "Boolean",                                        // Java code type
                "EncodingUtils.writeBooleans(buffer, #)",        // encode expression
                "# = EncodingUtils.readBooleans(buffer)"));        // decode expression
        typeMap.put("bitfield", new DomainInfo(
                        "byte",                                        // Java code type
                        "~",                                            // size
                        "Bitfield",
                        "EncodingUtils.writeBooleans(buffer, #)",        // encode expression
                        "# = EncodingUtils.readBooleans(buffer)"));        // decode expression

        typeMap.put("content", new DomainInfo(
                "Content",                                        // Java code type
                "EncodingUtils.encodedContentLength(#)",     // size
                "Content",                                        // Java code type
                "EncodingUtils.writeContentBytes(buffer, #)", // encode expression
                "# = EncodingUtils.readContent(buffer)"));    // decode expression
        typeMap.put("long", new DomainInfo(
                "long",                                            // Java code type
                "4",                                            // size
                "UnsignedInteger",                              // Java code type
                "EncodingUtils.writeUnsignedInteger(buffer, #)", // encode expression
                "# = buffer.getUnsignedInt()"));                 // decode expression
        typeMap.put("longlong", new DomainInfo(
                "long",                                            // Java code type
                "8",                                            // size
                "Long",
                "buffer.putLong(#)",                             // encode expression
                "# = buffer.getLong()"));                         // decode expression
        typeMap.put("longstr", new DomainInfo(
                "byte[]",                                        // Java code type
                "EncodingUtils.encodedLongstrLength(#)",         // size
                "Bytes",
                "EncodingUtils.writeLongStringBytes(buffer, #)", // encode expression
                "# = EncodingUtils.readLongstr(buffer)"));        // decode expression
        typeMap.put("octet", new DomainInfo(
                "short",                                        // Java code type
                "1",                                            // size
                "UnsignedByte",
                "EncodingUtils.writeUnsignedByte(buffer, #)",    // encode expression
                "# = buffer.getUnsigned()"));                     // decode expression
        typeMap.put("short", new DomainInfo(
                "int",                                            // Java code type
                "2",                                            // size
                "UnsignedShort",
                "EncodingUtils.writeUnsignedShort(buffer, #)",    // encode expression
                "# = buffer.getUnsignedShort()"));                 // decode expression
        typeMap.put("shortstr", new DomainInfo(
                "AMQShortString",                                        // Java code type
                "EncodingUtils.encodedShortStringLength(#)",    // size
                "AMQShortString",                                        // Java code type
                "EncodingUtils.writeShortStringBytes(buffer, #)", // encode expression
                "# = EncodingUtils.readAMQShortString(buffer)"));    // decode expression
        typeMap.put("table", new DomainInfo(
                "FieldTable",                                    // Java code type
                "EncodingUtils.encodedFieldTableLength(#)",     // size
                "FieldTable",                                    // Java code type
                "EncodingUtils.writeFieldTableBytes(buffer, #)", // encode expression
                "# = EncodingUtils.readFieldTable(buffer)"));    // decode expression
        typeMap.put("timestamp", new DomainInfo(
                "long",                                            // Java code type
                "8",                                            // size
                "Timestamp",
                "EncodingUtils.writeTimestamp(buffer, #)",        // encode expression
                "# = EncodingUtils.readTimestamp(buffer)"));    // decode expression
    }

    // === Start of methods for Interface LanguageConverter ===

    public String prepareClassName(String className)
    {
        return camelCaseName(className, true);
    }

    public String prepareMethodName(String methodName)
    {
        return camelCaseName(methodName, false);
    }

    public String prepareDomainName(String domainName)
    {
        return camelCaseName(domainName, false);
    }


    public String getGeneratedType(String domainName, AmqpVersion version)
    {
        String domainType = getDomainType(domainName, version);
        if (domainType == null)
        {
            throw new AmqpTypeMappingException("Domain type \"" + domainName +
                                               "\" not found in Java typemap.");
        }
        DomainInfo info = typeMap.get(domainType);
        if (info == null)
        {
            throw new AmqpTypeMappingException("Unknown domain: \"" + domainType + "\"");
        }
        return info.type;
    }

    // === Abstract methods from class Generator - Java-specific implementations ===

    @Override
    protected String prepareFilename(String filenameTemplate, AmqpClass thisClass, AmqpMethod method,
                                     AmqpField field, AmqpVersion version)
    {
        StringBuffer sb = new StringBuffer(filenameTemplate);
        if (thisClass != null)
        {
            replaceToken(sb, "${CLASS}", thisClass.getName());
        }
        if (method != null)
        {
            replaceToken(sb, "${METHOD}", method.getName());
        }
        if (field != null)
        {
            replaceToken(sb, "${FIELD}", field.getName());
        }
        if (version != null)
        {
            replaceToken(sb, "${MAJOR}", String.valueOf(version.getMajor()));
            replaceToken(sb, "${MINOR}", String.valueOf(version.getMinor()));
        }
        return sb.toString();
    }

    @Override
    protected void processModelTemplate(NamedTemplate template)
    {
        processTemplate(template, null, null, null, null);
    }

    @Override
    protected void processClassTemplate(NamedTemplate template, AmqpClass thisClass)
    {
        processTemplate(template, thisClass, null, null,
                        thisClass.getVersionSet().size() == 1 ? thisClass.getVersionSet().first() : null);
    }

    @Override
    protected void processMethodTemplate(NamedTemplate template, AmqpClass thisClass,
                                         AmqpMethod method)
    {
        processTemplate(template, thisClass, method, null,
                        thisClass.getVersionSet().size() == 1 ? thisClass.getVersionSet().first() : null);
    }

    protected void processFieldTemplate(NamedTemplate template, AmqpClass thisClass,
                                        AmqpMethod method, AmqpField field)
    {
        processTemplate(template, thisClass, method, field,
                        thisClass.getVersionSet().size() == 1 ? thisClass.getVersionSet().first() : null);
    }

    @Override
    protected void processTemplate(NamedTemplate template, AmqpClass thisClass,
                                   AmqpMethod method, AmqpField field, AmqpVersion version)
    {
        StringBuffer sb = new StringBuffer(template.getTemplate());
        String filename = prepareFilename(getTemplateFileName(sb), thisClass, method, field, version);
        processTemplate(sb, thisClass, method, field, template.getName(), version);
        writeTargetFile(sb, new File(getOutputDirectory() + Utils.FILE_SEPARATOR + filename));
        generatedFileCounter++;
    }

    protected void processTemplate(StringBuffer sb, AmqpClass thisClass, AmqpMethod method,
                                   AmqpField field, String templateFileName, AmqpVersion version)
    {
        try
        {
            processAllLists(sb, thisClass, method, version);
        }
        catch (AmqpTemplateException e)
        {
            System.out.println("WARNING: " + templateFileName + ": " + e.getMessage());
        }
        try
        {
            processAllTokens(sb, thisClass, method, field, version);
        }
        catch (AmqpTemplateException e)
        {
            System.out.println("WARNING: " + templateFileName + ": " + e.getMessage());
        }
    }

    @Override
    protected String processToken(String token, AmqpClass thisClass, AmqpMethod method, AmqpField field,
                                  AmqpVersion version)
    {
        if (token.compareTo("${GENERATOR}") == 0)
        {
            return GENERATOR_INFO;
        }
        if (token.compareTo("${CLASS}") == 0 && thisClass != null)
        {
            return thisClass.getName();
        }
        if (token.compareTo("${CLASS_ID_INIT}") == 0 && thisClass != null)
        {
            return generateIndexInitializer("registerClassId", thisClass.getIndexMap(), 8);
        }
        if (token.compareTo("${METHOD}") == 0 && method != null)
        {
            return method.getName();
        }
        if (token.compareTo("${METHOD_ID_INIT}") == 0 && method != null)
        {
            return generateIndexInitializer("registerMethodId", method.getIndexMap(), 8);
        }
        if (token.compareTo("${FIELD}") == 0 && field != null)
        {
            return field.getName();
        }

        // This token is used only with class or method-level templates
        if (token.compareTo("${pch_property_flags_declare}") == 0)
        {
            return generatePchPropertyFlagsDeclare();
        }
        else if (token.compareTo("${pch_property_flags_initializer}") == 0)
        {
            int mapSize = method == null ? thisClass.getFieldMap().size() : method.getFieldMap().size();
            return generatePchPropertyFlagsInitializer(mapSize);
        }
        else if (token.compareTo("${pch_compact_property_flags_initializer}") == 0)
        {
            return generatePchCompactPropertyFlagsInitializer(thisClass, 8, 4);
        }
        else if (token.compareTo("${pch_compact_property_flags_check}") == 0)
        {
            return generatePchCompactPropertyFlagsCheck(thisClass, 8, 4);
        }

        // Oops!
        throw new AmqpTemplateException("Template token " + token + " unknown.");
    }

    @Override
    protected void processClassList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                    AmqpModel model, AmqpVersion version)
    {
        String codeSnippet;
        int lend = sb.indexOf(CR, listMarkerStartIndex) + 1; // Include cr at end of line
        String tline = sb.substring(listMarkerEndIndex, lend); // Line excluding line marker, including cr
        int tokStart = tline.indexOf('$');
        String token = tline.substring(tokStart).trim();
        sb.delete(listMarkerStartIndex, lend);

        if (token.compareTo("${reg_map_put_method}") == 0)
        {
            codeSnippet = generateRegistry(model, 8, 4);
        }

        else // Oops!
        {
            throw new AmqpTemplateException("Template token " + token + " unknown.");
        }

        sb.insert(listMarkerStartIndex, codeSnippet);
    }

    @Override
    protected void processMethodList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                     AmqpClass thisClass)
    {
        String codeSnippet;
        int lend = sb.indexOf(CR, listMarkerStartIndex) + 1; // Include cr at end of line
        String tline = sb.substring(listMarkerEndIndex, lend); // Line excluding line marker, including cr
        int tokStart = tline.indexOf('$');
        String token = tline.substring(tokStart).trim();
        sb.delete(listMarkerStartIndex, lend);

        //TODO - we don't have any cases of this (yet).
        if (token.compareTo("${???}") == 0)
        {
            codeSnippet = token;
        }
        else // Oops!
        {
            throw new AmqpTemplateException("Template token " + token + " unknown.");
        }

        sb.insert(listMarkerStartIndex, codeSnippet);
    }

    @Override
    protected void processFieldList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                    AmqpFieldMap fieldMap, AmqpVersion version)
    {
        String codeSnippet;
        int lend = sb.indexOf(CR, listMarkerStartIndex) + 1; // Include cr at end of line
        String tline = sb.substring(listMarkerEndIndex, lend); // Line excluding line marker, including cr
        int tokStart = tline.indexOf('$');
        String token = tline.substring(tokStart).trim();
        sb.delete(listMarkerStartIndex, lend);

        // Field declarations - common to MethodBody and PropertyContentHeader classes
        if (token.compareTo("${field_declaration}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMap(declarationGenerateMethod,
                                                 mangledDeclarationGenerateMethod, 4, 4, this);
        }

        // MethodBody classes
        else if (token.compareTo("${mb_field_get_method}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMap(mbGetGenerateMethod,
                                                 mbMangledGetGenerateMethod, 4, 4, this);
        }
        else if (token.compareTo("${mb_field_parameter_list}") == 0)
        {
            // <cringe> The code generated by this is ugly... It puts a comma on a line by itself!
            // TODO: Find a more elegant solution here sometime...
            codeSnippet = fieldMap.size() > 0 ? Utils.createSpaces(42) + "," + CR : "";
            // </cringe>
            codeSnippet += fieldMap.parseFieldMap(mbParamListGenerateMethod,
                                                  mbMangledParamListGenerateMethod, 42, 4, this);
        }

        else if (token.compareTo("${mb_field_passed_parameter_list}") == 0)
        {
            // <cringe> The code generated by this is ugly... It puts a comma on a line by itself!
            // TODO: Find a more elegant solution here sometime...
            codeSnippet = fieldMap.size() > 0 ? Utils.createSpaces(42) + "," + CR : "";
            // </cringe>
            codeSnippet += fieldMap.parseFieldMap(mbPassedParamListGenerateMethod,
                                                  mbMangledPassedParamListGenerateMethod, 42, 4, this);
        }
        else if (token.compareTo("${mb_field_body_initialize}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMap(mbBodyInitGenerateMethod,
                                                 mbMangledBodyInitGenerateMethod, 8, 4, this);
        }
        else if (token.compareTo("${mb_field_size}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMapOrdinally(mbSizeGenerateMethod,
                                                          mbBitSizeGenerateMethod, 8, 4, this);
        }
        else if (token.compareTo("${mb_field_encode}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMapOrdinally(mbEncodeGenerateMethod,
                                                          mbBitEncodeGenerateMethod, 8, 4, this);
        }
        else if (token.compareTo("${mb_field_decode}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMapOrdinally(mbDecodeGenerateMethod,
                                                          mbBitDecodeGenerateMethod, 8, 4, this);
        }
        else if (token.compareTo("${mb_field_to_string}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMapOrdinally(mbToStringGenerateMethod,
                                                          mbBitToStringGenerateMethod, 8, 4, this);
        }

        // PropertyContentHeader classes
        else if (token.compareTo("${pch_field_list_size}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMapOrdinally(pchSizeGenerateMethod,
                                                          pchBitSizeGenerateMethod, 12, 4, this);
        }
        else if (token.compareTo("${pch_field_list_payload}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMapOrdinally(pchEncodeGenerateMethod,
                                                          pchBitEncodeGenerateMethod, 12, 4, this);
        }
        else if (token.compareTo("${pch_field_list_decode}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMapOrdinally(pchDecodeGenerateMethod,
                                                          pchBitDecodeGenerateMethod, 12, 4, this);
        }
        else if (token.compareTo("${pch_get_compact_property_flags}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMapOrdinally(pchGetPropertyFlagsGenerateMethod,
                                                          pchBitGetPropertyFlagsGenerateMethod, 8, 4, this);
        }
        else if (token.compareTo("${pch_set_compact_property_flags}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMapOrdinally(pchSetPropertyFlagsGenerateMethod,
                                                          pchBitSetPropertyFlagsGenerateMethod, 8, 4, this);
        }
        else if (token.compareTo("${pch_field_clear_methods}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMap(pchClearGenerateMethod,
                                                 pchMangledClearGenerateMethod, 4, 4, this);
        }
        else if (token.compareTo("${pch_field_get_methods}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMap(pchGetGenerateMethod,
                                                 pchMangledGetGenerateMethod, 4, 4, this);
        }
        else if (token.compareTo("${pch_field_set_methods}") == 0)
        {
            codeSnippet = fieldMap.parseFieldMap(pchSetGenerateMethod,
                                                 pchMangledSetGenerateMethod, 4, 4, this);
        }

        else // Oops!
        {
            throw new AmqpTemplateException("Template token " + token + " unknown.");
        }
        sb.insert(listMarkerStartIndex, codeSnippet);
    }

    @Override
    protected void processConstantList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                       AmqpConstantSet constantSet)
    {
        String codeSnippet;
        int lend = sb.indexOf(CR, listMarkerStartIndex) + 1; // Include cr at end of line
        String tline = sb.substring(listMarkerEndIndex, lend); // Line excluding line marker, including cr
        int tokStart = tline.indexOf('$');
        String token = tline.substring(tokStart).trim();
        sb.delete(listMarkerStartIndex, lend);

        if (token.compareTo("${const_get_method}") == 0)
        {
            codeSnippet = generateConstantGetMethods(constantSet, 4, 4);
        }

        else // Oops!
        {
            throw new AmqpTemplateException("Template token " + token + " unknown.");
        }

        sb.insert(listMarkerStartIndex, codeSnippet);
    }

    // === Protected and private helper functions unique to Java implementation ===

    // Methods used for generation of code snippets called from the field map parsers

    // Common methods

    protected String generateFieldDeclaration(String codeType, AmqpField field,
                                              AmqpVersionSet versionSet, int indentSize, int tabSize, boolean nextFlag)
    {
        return Utils.createSpaces(indentSize) + "public " + codeType + " " + field.getName() +
               "; // AMQP version(s): " + versionSet + CR;
    }

    protected String generateMangledFieldDeclaration(AmqpField field, int indentSize,
                                                     int tabSize, boolean nextFlag)
    {
        StringBuffer sb = new StringBuffer();
        Iterator<String> dItr = field.getDomainMap().keySet().iterator();
        int domainCntr = 0;
        while (dItr.hasNext())
        {
            String domainName = dItr.next();
            AmqpVersionSet versionSet = field.getDomainMap().get(domainName);
            String codeType = getGeneratedType(domainName, versionSet.first());
            sb.append(Utils.createSpaces(indentSize) + "public " + codeType + " " +
                      field.getName() + "_" + (domainCntr++) + "; // AMQP Version(s): " + versionSet +
                      CR);
        }
        return sb.toString();
    }

    protected String generateIndexInitializer(String mapName, AmqpOrdinalVersionMap indexMap, int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();

        Iterator<Integer> iItr = indexMap.keySet().iterator();
        while (iItr.hasNext())
        {
            int index = iItr.next();
            AmqpVersionSet versionSet = indexMap.get(index);
            Iterator<AmqpVersion> vItr = versionSet.iterator();
            while (vItr.hasNext())
            {
                AmqpVersion version = vItr.next();
                sb.append(indent + mapName + "( (byte) " + version.getMajor() + ", (byte) " + version.getMinor() + ", " + index + ");" + CR);
            }
        }
        return sb.toString();
    }

    protected String generateRegistry(AmqpModel model, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();

        for (String className : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(className);
            for (String methodName : thisClass.getMethodMap().keySet())
            {
                AmqpMethod method = thisClass.getMethodMap().get(methodName);
                for (AmqpVersion version : model.getVersionSet())
                {
                    // Find class and method index for this version (if it exists)
                    try
                    {
                        int classIndex = findIndex(thisClass.getIndexMap(), version);
                        int methodIndex = findIndex(method.getIndexMap(), version);
                        sb.append(indent + "registerMethod(" + CR);
                        sb.append(indent + tab + "(short)" + classIndex +
                                  ", (short)" + methodIndex + ", (byte)" + version.getMajor() +
                                  ", (byte)" + version.getMinor() + ", " + CR);
                        sb.append(indent + tab + Utils.firstUpper(thisClass.getName()) +
                                  Utils.firstUpper(method.getName()) + "Body.getFactory());" + CR);
                    }
                    catch (Exception e)
                    {
                    } // Ignore
                }
            }
        }
        return sb.toString();
    }

    protected int findIndex(TreeMap<Integer, AmqpVersionSet> map, AmqpVersion version)
    {
        Iterator<Integer> iItr = map.keySet().iterator();
        while (iItr.hasNext())
        {
            int index = iItr.next();
            AmqpVersionSet versionSet = map.get(index);
            if (versionSet.contains(version))
            {
                return index;
            }
        }
        throw new IllegalArgumentException("Index not found");
    }

    // Methods for AmqpConstants class


    public String prepareConstantName(String constantName)
    {
        return upperCaseName(constantName);
    }


    protected String generateConstantGetMethods(AmqpConstantSet constantSet,
                                                int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();

        for (AmqpConstant constant : constantSet.getContstants())
        {

            if (constant.isVersionConsistent(constantSet.getVersionSet()))
            {
                // return a constant
                String value = constant.firstKey();
                if (Utils.containsOnlyDigits(value))
                {
                    sb.append(indent + "public static final int " + constant.getName() + " = " +
                              constant.firstKey() + ";" + CR);
                }
                else if (Utils.containsOnlyDigitsAndDecimal(value))
                {
                    sb.append(indent + "public static double " + constant.getName() + " = " +
                              constant.firstKey() + "; " + CR);
                }
                else
                {
                    sb.append(indent + "public static String " + constant.getName() + " = " +
                              constant.firstKey() + "\"; " + CR);

                }
                sb.append(CR);
            }
            else
            {
                // Return version-specific constant
                sb.append(generateVersionDependentGet(constant, "String", "", "\"", "\"", indentSize, tabSize));
                sb.append(generateVersionDependentGet(constant, "int", "AsInt", "", "", indentSize, tabSize));
                sb.append(generateVersionDependentGet(constant, "double", "AsDouble", "(double)", "", indentSize, tabSize));
                sb.append(CR);
            }
        }
        return sb.toString();
    }

    protected String generateVersionDependentGet(AmqpConstant constant,
                                                 String methodReturnType, String methodNameSuffix, String returnPrefix, String returnPostfix,
                                                 int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        sb.append(indent + "public static " + methodReturnType + " " + constant.getName() +
                  methodNameSuffix + "(byte major, byte minor) throws AMQProtocolVersionException" + CR);
        sb.append(indent + "{" + CR);
        boolean first = true;
        Iterator<String> sItr = constant.keySet().iterator();
        while (sItr.hasNext())
        {
            String value = sItr.next();
            AmqpVersionSet versionSet = constant.get(value);
            sb.append(indent + tab + (first ? "" : "else ") + "if (" + generateVersionCheck(versionSet) +
                      ")" + CR);
            sb.append(indent + tab + "{" + CR);
            if (methodReturnType.compareTo("int") == 0 && !Utils.containsOnlyDigits(value))
            {
                sb.append(generateConstantDeclarationException(constant.getName(), methodReturnType,
                                                               indentSize + (2 * tabSize), tabSize));
            }
            else if (methodReturnType.compareTo("double") == 0 && !Utils.containsOnlyDigitsAndDecimal(value))
            {
                sb.append(generateConstantDeclarationException(constant.getName(), methodReturnType,
                                                               indentSize + (2 * tabSize), tabSize));
            }
            else
            {
                sb.append(indent + tab + tab + "return " + returnPrefix + value + returnPostfix + ";" + CR);
            }
            sb.append(indent + tab + "}" + CR);
            first = false;
        }
        sb.append(indent + tab + "else" + CR);
        sb.append(indent + tab + "{" + CR);
        sb.append(indent + tab + tab + "throw new AMQProtocolVersionException(\"Constant \\\"" +
                  constant.getName() + "\\\" \" +" + CR);
        sb.append(indent + tab + tab + tab +
                  "\"is undefined for AMQP version \" + major + \"-\" + minor + \".\");" + CR);
        sb.append(indent + tab + "}" + CR);
        sb.append(indent + "}" + CR);
        return sb.toString();
    }

    protected String generateConstantDeclarationException(String name, String methodReturnType,
                                                          int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        sb.append(indent + "throw new AMQProtocolVersionException(\"Constant \\\"" +
                  name + "\\\" \" +" + CR);
        sb.append(indent + tab + "\"cannot be converted to type " + methodReturnType +
                  " for AMQP version \" + major + \"-\" + minor + \".\");" + CR);
        return sb.toString();
    }

    // Methods for MessageBody classes
    protected String generateMbGetMethod(String codeType, AmqpField field,
                                         AmqpVersionSet versionSet, int indentSize, int tabSize, boolean nextFlag)
    {
        return Utils.createSpaces(indentSize) + "public " + codeType + " get" +
               Utils.firstUpper(field.getName()) + "() { return " + field.getName() + "; }" +
               CR;
    }

    protected String generateMbMangledGetMethod(AmqpField field, int indentSize,
                                                int tabSize, boolean nextFlag)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer(CR);
        sb.append(indent + "public <T> T get" + Utils.firstUpper(field.getName()) +
                  "(Class<T> classObj) throws AMQProtocolVersionException" + CR);
        sb.append(indent + "{" + CR);
        Iterator<String> dItr = field.getDomainMap().keySet().iterator();
        int domainCntr = 0;
        while (dItr.hasNext())
        {
            String domainName = dItr.next();
            AmqpVersionSet versionSet = field.getDomainMap().get(domainName);
            String codeType = getGeneratedType(domainName, versionSet.first());
            sb.append(indent + tab + "if (classObj.equals(" + codeType +
                      ".class)) // AMQP Version(s): " + versionSet + CR);
            sb.append(indent + tab + tab + "return (T)(Object)" + field.getName() + "_" +
                      (domainCntr++) + ";" + CR);
        }
        sb.append(indent + tab +
                  "throw new AMQProtocolVersionException(\"None of the AMQP versions defines \" +" +
                  CR + "            \"field \\\"" + field.getName() +
                  "\\\" as domain \\\"\" + classObj.getName() + \"\\\".\");" + CR);
        sb.append(indent + "}" + CR);
        sb.append(CR);
        return sb.toString();
    }

    protected String generateMbParamList(String codeType, AmqpField field,
                                         AmqpVersionSet versionSet, int indentSize, int tabSize, boolean nextFlag)
    {
        return Utils.createSpaces(indentSize) + codeType + " " + field.getName() +
               (nextFlag ? "," : "") + " // AMQP version(s): " + versionSet + CR;
    }


    protected String generateMbPassedParamList(String codeType, AmqpField field,
                                               AmqpVersionSet versionSet, int indentSize, int tabSize, boolean nextFlag)
    {
        return Utils.createSpaces(indentSize) + field.getName() +
               (nextFlag ? "," : "") + " // AMQP version(s): " + versionSet + CR;
    }


    protected String generateMbMangledParamList(AmqpField field, int indentSize,
                                                int tabSize, boolean nextFlag)
    {
        StringBuffer sb = new StringBuffer();
        Iterator<String> dItr = field.getDomainMap().keySet().iterator();
        int domainCntr = 0;
        while (dItr.hasNext())
        {
            String domainName = dItr.next();
            AmqpVersionSet versionSet = field.getDomainMap().get(domainName);
            String codeType = getGeneratedType(domainName, versionSet.first());
            sb.append(Utils.createSpaces(indentSize) + codeType + " " + field.getName() + "_" +
                      (domainCntr++) + (nextFlag ? "," : "") + " // AMQP version(s): " +
                      versionSet + CR);
        }
        return sb.toString();
    }

    protected String generateMbMangledPassedParamList(AmqpField field, int indentSize,
                                                      int tabSize, boolean nextFlag)
    {
        StringBuffer sb = new StringBuffer();
        Iterator<String> dItr = field.getDomainMap().keySet().iterator();
        int domainCntr = 0;
        while (dItr.hasNext())
        {
            String domainName = dItr.next();
            AmqpVersionSet versionSet = field.getDomainMap().get(domainName);
            sb.append(Utils.createSpaces(indentSize) + field.getName() + "_" +
                      (domainCntr++) + (nextFlag ? "," : "") + " // AMQP version(s): " +
                      versionSet + CR);
        }
        return sb.toString();
    }


    protected String generateMbBodyInit(String codeType, AmqpField field,
                                        AmqpVersionSet versionSet, int indentSize, int tabSize, boolean nextFlag)
    {
        return Utils.createSpaces(indentSize) + "this." + field.getName() + " = " + field.getName() +
               ";" + CR;
    }

    protected String generateMbMangledBodyInit(AmqpField field, int indentSize,
                                               int tabSize, boolean nextFlag)
    {
        StringBuffer sb = new StringBuffer();
        Iterator<String> dItr = field.getDomainMap().keySet().iterator();
        int domainCntr = 0;
        while (dItr.hasNext())
        {
            dItr.next();
            sb.append(Utils.createSpaces(indentSize) + "this." + field.getName() + "_" + domainCntr +
                      " = " + field.getName() + "_" + (domainCntr++) + ";" + CR);
        }
        return sb.toString();
    }

    protected String generateMbFieldSize(String domainType, String fieldName,
                                         int ordinal, int indentSize, int tabSize)
    {
        StringBuffer sb = new StringBuffer();
        sb.append(Utils.createSpaces(indentSize) + "size += " +
                  typeMap.get(domainType).size.replaceAll("#", fieldName) +
                  "; // " + fieldName + ": " + domainType + CR);
        return sb.toString();
    }

    protected String generateMbBitArrayFieldSize(List<String> bitFieldList,
                                                 int ordinal, int indentSize, int tabSize)
    {
        StringBuffer sb = new StringBuffer();
        int numBytes = ((bitFieldList.size() - 1) / 8) + 1;
        String comment = bitFieldList.size() == 1 ?
                         bitFieldList.get(0) + ": bit" :
                         "Combinded bits: " + bitFieldList;
        sb.append(Utils.createSpaces(indentSize) + "size += " +
                  typeMap.get("bit").size.replaceAll("~", String.valueOf(numBytes)) +
                  "; // " + comment + CR);
        return sb.toString();
    }

    protected String generateMbFieldEncode(String domain, String fieldName,
                                           int ordinal, int indentSize, int tabSize)
    {
        StringBuffer sb = new StringBuffer();
        sb.append(Utils.createSpaces(indentSize) +
                  typeMap.get(domain).encodeExpression.replaceAll("#", fieldName) +
                  "; // " + fieldName + ": " + domain + CR);
        return sb.toString();
    }

    protected String generateMbBitFieldEncode(List<String> bitFieldList,
                                              int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);

        StringBuilder sb = new StringBuilder();
        int i = 0;
        while (i < bitFieldList.size())
        {

            StringBuilder line = new StringBuilder();

            for (int j = 0; i < bitFieldList.size() && j < 8; i++, j++)
            {
                if (j != 0)
                {
                    line.append(", ");
                }
                line.append(bitFieldList.get(i));
            }

            sb.append(indent +
                      typeMap.get("bit").encodeExpression.replaceAll("#", line.toString()) + ";" + CR);
        }
        return sb.toString();
    }

    protected String generateMbFieldDecode(String domain, String fieldName,
                                           int ordinal, int indentSize, int tabSize)
    {
        StringBuffer sb = new StringBuffer();
        sb.append(Utils.createSpaces(indentSize) +
                  typeMap.get(domain).decodeExpression.replaceAll("#", fieldName) +
                  "; // " + fieldName + ": " + domain + CR);
        return sb.toString();
    }

    protected String generateMbBitFieldDecode(List<String> bitFieldList,
                                              int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);

        StringBuilder sb = new StringBuilder(indent);
        sb.append("byte packedValue;");
        sb.append(CR);

        // RG HERE!

        int i = 0;
        while (i < bitFieldList.size())
        {
            sb.append(indent + "packedValue = EncodingUtils.readByte(buffer);" + CR);

            for (int j = 0; i < bitFieldList.size() && j < 8; i++, j++)
            {
                sb.append(indent + bitFieldList.get(i) + " = ( packedValue & (byte) (1 << " + j + ") ) != 0;" + CR);
            }
        }
        return sb.toString();
    }

    protected String generateMbFieldToString(String domain, String fieldName,
                                             int ordinal, int indentSize, int tabSize)
    {
        StringBuffer sb = new StringBuffer();
        sb.append(Utils.createSpaces(indentSize) +
                  "buf.append(\"  " + fieldName + ": \" + " + fieldName + ");" + CR);
        return sb.toString();
    }

    protected String generateMbBitFieldToString(List<String> bitFieldList,
                                                int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < bitFieldList.size(); i++)
        {
            String bitFieldName = bitFieldList.get(i);
            sb.append(indent + "buf.append(\"  " + bitFieldName + ": \" + " + bitFieldName +
                      ");" + CR);
        }
        return sb.toString();
    }

    // Methods for PropertyContentHeader classes

    protected String generatePchClearMethod(String codeType, AmqpField field,
                                            AmqpVersionSet versionSet, int indentSize, int tabSize, boolean nextFlag)
    {
        // This is one case where the ordinal info is the only significant factor,
        // the domain info plays no part. Defer to the mangled version; the code would be
        // identical anyway...
        return generatePchMangledClearMethod(field, indentSize, tabSize, nextFlag);
    }

    protected String generatePchMangledClearMethod(AmqpField field, int indentSize,
                                                   int tabSize, boolean nextFlag)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        sb.append(indent + "public void clear" + Utils.firstUpper(field.getName()) +
                  "()" + CR);
        sb.append(indent + "{" + CR);

        // If there is more than one ordinal for this field or the ordinal does not
        // apply to all known versions, then we need to generate version checks so
        // we know which fieldProperty to clear.
        if (field.getOrdinalMap().size() == 1 &&
            field.getOrdinalMap().get(field.getOrdinalMap().firstKey()).size() == field.getVersionSet().size())
        {
            int ordinal = field.getOrdinalMap().firstKey();
            sb.append(indent + tab + "clearEncodedForm();" + CR);
            sb.append(indent + tab + "propertyFlags[" + ordinal + "] = false;" + CR);
        }
        else
        {
            Iterator<Integer> oItr = field.getOrdinalMap().keySet().iterator();
            while (oItr.hasNext())
            {
                int ordinal = oItr.next();
                AmqpVersionSet versionSet = field.getOrdinalMap().get(ordinal);
                sb.append(indent + tab);
                if (ordinal != field.getOrdinalMap().firstKey())
                {
                    sb.append("else ");
                }
                sb.append("if (");
                sb.append(generateVersionCheck(versionSet));
                sb.append(")" + CR);
                sb.append(indent + tab + "{" + CR);
                sb.append(indent + tab + tab + "clearEncodedForm();" + CR);
                sb.append(indent + tab + tab + "propertyFlags[" + ordinal + "] = false;" + CR);
                sb.append(indent + tab + "}" + CR);
            }
        }
        sb.append(indent + "}" + CR);
        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchGetMethod(String codeType, AmqpField field,
                                          AmqpVersionSet versionSet, int indentSize, int tabSize, boolean nextFlag)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer(indent + "public " + codeType + " get" +
                                           Utils.firstUpper(field.getName()) + "()" + CR);
        sb.append(indent + "{" + CR);
        sb.append(indent + tab + "decodeIfNecessary();" + CR);
        sb.append(indent + tab + "return " + field.getName() + ";" + CR);
        sb.append(indent + "}" + CR);
        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchMangledGetMethod(AmqpField field, int indentSize,
                                                 int tabSize, boolean nextFlag)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer(indent + "public <T> T get" +
                                           Utils.firstUpper(field.getName()) +
                                           "(Class<T> classObj) throws AMQProtocolVersionException" + CR);
        sb.append(indent + "{" + CR);
        Iterator<String> dItr = field.getDomainMap().keySet().iterator();
        int domainCntr = 0;
        while (dItr.hasNext())
        {
            String domainName = dItr.next();
            AmqpVersionSet versionSet = field.getDomainMap().get(domainName);
            String codeType = getGeneratedType(domainName, versionSet.first());
            sb.append(indent + tab + "if (classObj.equals(" + codeType +
                      ".class)) // AMQP Version(s): " + versionSet + CR);
            sb.append(indent + tab + "{" + CR);
            sb.append(indent + tab + tab + "decodeIfNecessary();" + CR);
            sb.append(indent + tab + tab + "return (T)(Object)" + field.getName() + "_" +
                      (domainCntr++) + ";" + CR);
            sb.append(indent + tab + "}" + CR);
        }
        sb.append(indent + tab +
                  "throw new AMQProtocolVersionException(\"None of the AMQP versions defines \" +" +
                  CR + "            \"field \\\"" + field.getName() +
                  "\\\" as domain \\\"\" + classObj.getName() + \"\\\".\");" + CR);
        sb.append(indent + "}" + CR);
        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchSetMethod(String codeType, AmqpField field,
                                          AmqpVersionSet versionSet, int indentSize, int tabSize, boolean nextFlag)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        sb.append(indent + "public void set" + Utils.firstUpper(field.getName()) +
                  "(" + codeType + " " + field.getName() + ")" + CR);
        sb.append(indent + "{" + CR);

        // If there is more than one ordinal for this field or the ordinal does not
        // apply to all known versions, then we need to generate version checks so
        // we know which fieldProperty to clear.
        if (field.getOrdinalMap().size() == 1 &&
            field.getOrdinalMap().get(field.getOrdinalMap().firstKey()).size() == field.getVersionSet().size())
        {
            int ordinal = field.getOrdinalMap().firstKey();
            sb.append(indent + tab + "clearEncodedForm();" + CR);
            sb.append(indent + tab + "propertyFlags[" + ordinal + "] = true;" + CR);
            sb.append(indent + tab + "this." + field.getName() + " = " + field.getName() + ";" + CR);
        }
        else
        {
            Iterator<Integer> oItr = field.getOrdinalMap().keySet().iterator();
            while (oItr.hasNext())
            {
                int ordinal = oItr.next();
                AmqpVersionSet oVersionSet = field.getOrdinalMap().get(ordinal);
                sb.append(indent + tab);
                if (ordinal != field.getOrdinalMap().firstKey())
                {
                    sb.append("else ");
                }
                sb.append("if (");
                sb.append(generateVersionCheck(oVersionSet));
                sb.append(")" + CR);
                sb.append(indent + tab + "{" + CR);
                sb.append(indent + tab + tab + "clearEncodedForm();" + CR);
                sb.append(indent + tab + tab + "propertyFlags[" + ordinal + "] = true;" + CR);
                sb.append(indent + tab + tab + "this." + field.getName() + " = " + field.getName() + ";" + CR);
                sb.append(indent + tab + "}" + CR);
            }
        }
        sb.append(indent + "}" + CR);
        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchMangledSetMethod(AmqpField field, int indentSize,
                                                 int tabSize, boolean nextFlag)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();

        Iterator<String> dItr = field.getDomainMap().keySet().iterator();
        int domainCntr = 0;
        while (dItr.hasNext())
        {
            String domainName = dItr.next();
            AmqpVersionSet versionSet = field.getDomainMap().get(domainName);
            String codeType = getGeneratedType(domainName, versionSet.first());

            // Find ordinal with matching version
            AmqpVersionSet commonVersionSet = new AmqpVersionSet();
            Iterator<Integer> oItr = field.getOrdinalMap().keySet().iterator();
            while (oItr.hasNext())
            {
                int ordinal = oItr.next();
                AmqpVersionSet oVersionSet = field.getOrdinalMap().get(ordinal);
                Iterator<AmqpVersion> vItr = oVersionSet.iterator();
                boolean first = true;
                while (vItr.hasNext())
                {
                    AmqpVersion thisVersion = vItr.next();
                    if (versionSet.contains(thisVersion))
                    {
                        commonVersionSet.add(thisVersion);
                    }
                }
                if (!commonVersionSet.isEmpty())
                {
                    sb.append(indent + "public void set" + Utils.firstUpper(field.getName()) +
                              "(" + codeType + " " + field.getName() + ")" + CR);
                    sb.append(indent + "{" + CR);
                    sb.append(indent + tab);
                    if (!first)
                    {
                        sb.append("else ");
                    }
                    sb.append("if (");
                    sb.append(generateVersionCheck(commonVersionSet));
                    sb.append(")" + CR);
                    sb.append(indent + tab + "{" + CR);
                    sb.append(indent + tab + tab + "clearEncodedForm();" + CR);
                    sb.append(indent + tab + tab + "propertyFlags[" + ordinal + "] = true;" + CR);
                    sb.append(indent + tab + tab + "this." + field.getName() + "_" + (domainCntr++) +
                              " = " + field.getName() + ";" + CR);
                    sb.append(indent + tab + "}" + CR);
                    sb.append(indent + "}" + CR);
                    sb.append(CR);
                    first = false;
                }
            }
        }
        return sb.toString();
    }

    protected String generatePchFieldSize(String domainType, String fieldName,
                                          int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer(indent + "if (propertyFlags[" + ordinal + "]) // " +
                                           fieldName + ": " + domainType + CR);
        sb.append(indent + Utils.createSpaces(tabSize) + "size += " +
                  typeMap.get(domainType).size.replaceAll("#", fieldName) + ";" + CR);
        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchBitArrayFieldSize(List<String> bitFieldList,
                                                  int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        String comment = bitFieldList.size() == 1 ?
                         bitFieldList.get(0) + ": bit" :
                         "Combinded bits: " + bitFieldList;
        StringBuffer sb = new StringBuffer();

        if (bitFieldList.size() == 1) // single bit
        {
            sb.append(indent + "if (propertyFlags[" + (ordinal - 1) + "]) // " + comment + CR);
            sb.append(indent + tab + "size += " +
                      typeMap.get("bit").size.replaceAll("~", "1") + ";" + CR);
        }
        else // multiple bits - up to 8 are combined into one byte
        {
            String bitCntrName = "bitCntr_" + ordinal;
            int startOrdinal = ordinal - bitFieldList.size();
            sb.append(indent + "// " + comment + CR);
            sb.append(indent + "int " + bitCntrName + " = 0;" + CR);
            sb.append(indent + "for (int i=" + startOrdinal + "; i<" + ordinal + "; i++)" + CR);
            sb.append(indent + "{" + CR);
            sb.append(indent + tab + "if (propertyFlags[i])" + CR);
            sb.append(indent + tab + tab + bitCntrName + "++;" + CR);
            sb.append(indent + "}" + CR);
            sb.append(indent + "size += " +
                      typeMap.get("bit").size.replaceAll("~", bitCntrName +
                                                              " > 0 ? ((" + bitCntrName + " - 1) / 8) + 1 : 0") + ";" + CR);
        }
        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchFieldEncode(String domainType, String fieldName,
                                            int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        sb.append(indent + "if (propertyFlags[" + ordinal + "]) // " + fieldName + ": " +
                  domainType + CR);
        sb.append(indent + Utils.createSpaces(tabSize) +
                  typeMap.get(domainType).encodeExpression.replaceAll("#", fieldName) + ";" + CR);
        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchBitFieldEncode(List<String> bitFieldList,
                                               int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        String comment = bitFieldList.size() == 1 ?
                         bitFieldList.get(0) + ": bit" :
                         "Combinded bits: " + bitFieldList;
        StringBuffer sb = new StringBuffer();

        if (bitFieldList.size() == 1) // single bit
        {
            sb.append(indent + "if (propertyFlags[" + (ordinal - 1) + "]) // " +
                      bitFieldList.get(0) + ": bit" + CR);
            sb.append(indent + tab + typeMap.get("bit").encodeExpression.replaceAll("#",
                                                                                    "new boolean[] {" + bitFieldList.get(0) + "}") + ";" + CR);
        }
        else // multiple bits - up to 8 are combined into one byte
        {
            int startOrdinal = ordinal - bitFieldList.size();
            String bitCntrName = "bitCntr" + startOrdinal;
            sb.append(indent + "// " + comment + CR);
            sb.append(indent + "int " + bitCntrName + " = 0;" + CR);
            sb.append(indent + "for (int i=" + startOrdinal + "; i<=" + (ordinal - 1) + "; i++)" + CR);
            sb.append(indent + "{" + CR);
            sb.append(indent + tab + "if (propertyFlags[i])" + CR);
            sb.append(indent + tab + tab + bitCntrName + "++;" + CR);
            sb.append(indent + "}" + CR);
            sb.append(indent + "if (" + bitCntrName + " > 0) // Are any of the property bits set?" + CR);
            sb.append(indent + "{" + CR);
            sb.append(indent + tab + "boolean[] fullBitArray = new boolean[] { ");
            for (int i = 0; i < bitFieldList.size(); i++)
            {
                if (i != 0)
                {
                    sb.append(", ");
                }
                sb.append(bitFieldList.get(i));
            }
            sb.append(" };" + CR);
            sb.append(indent + tab + "boolean[] flaggedBitArray = new boolean[" + bitCntrName +
                      "];" + CR);
            sb.append(indent + tab + bitCntrName + " = 0;" + CR);
            sb.append(indent + tab + "for (int i=" + startOrdinal + "; i<=" + (ordinal - 1) +
                      "; i++)" + CR);
            sb.append(indent + tab + "{" + CR);
            sb.append(indent + tab + tab + "if (propertyFlags[i])" + CR);
            sb.append(indent + tab + tab + tab + "flaggedBitArray[" + bitCntrName +
                      "++] = fullBitArray[i];" + CR);
            sb.append(indent + tab + "}" + CR);
            sb.append(indent + tab + typeMap.get("bit").encodeExpression.replaceAll("#",
                                                                                    "flaggedBitArray") + ";" + CR);
            sb.append(indent + "}" + CR);
        }
        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchFieldDecode(String domainType, String fieldName,
                                            int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        sb.append(indent + "if (propertyFlags[" + ordinal + "]) // " + fieldName + ": " +
                  domainType + CR);
        sb.append(indent + Utils.createSpaces(tabSize) +
                  typeMap.get(domainType).decodeExpression.replaceAll("#", fieldName) + ";" + CR);
        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchBitFieldDecode(List<String> bitFieldList,
                                               int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        String comment = bitFieldList.size() == 1 ?
                         bitFieldList.get(0) + ": bit" :
                         "Combinded bits: " + bitFieldList;
        StringBuffer sb = new StringBuffer();

        if (bitFieldList.size() == 1) // single bit
        {
            sb.append(indent + "if (propertyFlags[" + (ordinal - 1) + "]) // " +
                      bitFieldList.get(0) + ": bit" + CR);
            sb.append(indent + "{" + CR);
            sb.append(indent + tab + typeMap.get("bit").decodeExpression.replaceAll("#",
                                                                                    "boolean[] flaggedBitArray") + ";" + CR);
            sb.append(indent + tab + bitFieldList.get(0) + " = flaggedBitArray[0];" + CR);
            sb.append(indent + "}" + CR);
        }
        else // multiple bits - up to 8 are combined into one byte
        {
            int startOrdinal = ordinal - bitFieldList.size();
            String bitCntr = "bitCntr" + startOrdinal;
            sb.append(indent + "// " + comment + CR);
            sb.append(indent + "int " + bitCntr + " = 0;" + CR);
            sb.append(indent + "for (int i=" + startOrdinal + "; i<=" + (ordinal - 1) + "; i++)" + CR);
            sb.append(indent + "{" + CR);
            sb.append(indent + tab + "if (propertyFlags[i])" + CR);
            sb.append(indent + tab + tab + bitCntr + "++;" + CR);
            sb.append(indent + "}" + CR);
            sb.append(indent + "if (" + bitCntr + " > 0) // Are any of the property bits set?" + CR);
            sb.append(indent + "{" + CR);
            sb.append(indent + tab + typeMap.get("bit").decodeExpression.replaceAll("#",
                                                                                    "boolean[] flaggedBitArray") + ";" + CR);
            sb.append(indent + tab + bitCntr + " = 0;" + CR);
            for (int i = 0; i < bitFieldList.size(); i++)
            {
                sb.append(indent + tab + "if (propertyFlags[" + (startOrdinal + i) + "])" + CR);
                sb.append(indent + tab + tab + bitFieldList.get(i) + " = flaggedBitArray[" +
                          bitCntr + "++];" + CR);
            }
            sb.append(indent + "}" + CR);
        }

        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchGetPropertyFlags(String domainType, String fieldName,
                                                 int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        int word = ordinal / 15;
        int bit = 15 - (ordinal % 15);
        sb.append(indent + "if (propertyFlags[" + ordinal + "]) // " + fieldName + ": " +
                  domainType + CR);
        sb.append(indent + tab + "compactPropertyFlags[" + word + "] |= (1 << " +
                  bit + ");" + CR);
        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchBitGetPropertyFlags(List<String> bitFieldList,
                                                    int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        int startOrdinal = ordinal - bitFieldList.size();

        for (int i = 0; i < bitFieldList.size(); i++)
        {
            int thisOrdinal = startOrdinal + i;
            int word = thisOrdinal / 15;
            int bit = 15 - (thisOrdinal % 15);
            sb.append(indent + "if (propertyFlags[" + thisOrdinal + "])" + CR);
            sb.append(indent + tab + "compactPropertyFlags[" + word +
                      "] |= (1 << " + bit + ");" + CR);
        }

        sb.append(CR);
        return sb.toString();
    }

    protected String generatePchSetPropertyFlags(String domainType, String fieldName,
                                                 int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        int word = ordinal / 15;
        int bit = 15 - (ordinal % 15);
        sb.append(indent + "propertyFlags[" + ordinal + "] = (compactPropertyFlags[" +
                  word + "] & (1 << " + bit + ")) > 0;" + CR);
        return sb.toString();
    }

    protected String generatePchBitSetPropertyFlags(List<String> bitFieldList,
                                                    int ordinal, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        int startOrdinal = ordinal - bitFieldList.size();

        for (int i = 0; i < bitFieldList.size(); i++)
        {
            int thisOrdinal = startOrdinal + i;
            int word = thisOrdinal / 15;
            int bit = 15 - (thisOrdinal % 15);
            sb.append(indent + "propertyFlags[" + thisOrdinal + "] = (compactPropertyFlags[" +
                      word + "] & (1 << " + bit + ")) > 0;" + CR);
        }
        return sb.toString();
    }

    private String generatePchPropertyFlagsDeclare()
    {
        return "private boolean[] propertyFlags;";
    }

    private String generatePchPropertyFlagsInitializer(int totNumFields)
    {
        return "propertyFlags = new boolean[" + totNumFields + "];";
    }

    private String generatePchCompactPropertyFlagsInitializer(AmqpClass thisClass, int indentSize,
                                                              int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        Iterator<AmqpVersion> vItr = thisClass.getVersionSet().iterator();
        while (vItr.hasNext())
        {
            AmqpVersion version = vItr.next();
            int numBytes = ((thisClass.getFieldMap().getNumFields(version) - 1) / 15) + 1;

            sb.append(indent);
            if (!version.equals(thisClass.getVersionSet().first()))
            {
                sb.append("else ");
            }
            sb.append("if ( major == " + version.getMajor() + " && minor == " +
                      version.getMinor() + " )" + CR);
            sb.append(indent + tab + "compactPropertyFlags = new int[] { ");
            for (int i = 0; i < numBytes; i++)
            {
                if (i != 0)
                {
                    sb.append(", ");
                }
                sb.append(i < numBytes - 1 ? "1" : "0"); // Set the "continue" flag where required
            }
            sb.append(" };" + CR);
        }
        return sb.toString();
    }

    private String generatePchCompactPropertyFlagsCheck(AmqpClass thisClass, int indentSize,
                                                        int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        Iterator<AmqpVersion> vItr = thisClass.getVersionSet().iterator();
        while (vItr.hasNext())
        {
            AmqpVersion version = vItr.next();
            int numFields = thisClass.getFieldMap().getNumFields(version);
            int numBytes = ((numFields - 1) / 15) + 1;

            sb.append(indent);
            if (!version.equals(thisClass.getVersionSet().first()))
            {
                sb.append("else ");
            }
            sb.append("if ( major == " + version.getMajor() + " && minor == " +
                      version.getMinor() + " && compactPropertyFlags.length != " + numBytes + " )" + CR);
            sb.append(indent + tab +
                      "throw new AMQProtocolVersionException(\"Property flag array size mismatch:\" +" + CR);
            sb.append(indent + tab + tab + "\"(Size found: \" + compactPropertyFlags.length +" + CR);
            sb.append(indent + tab + tab + "\") Version " + version + " has " + numFields +
                      " fields which requires an int array of size " + numBytes + ".\");" + CR);
        }
        return sb.toString();
    }

    private String generateVersionCheck(AmqpVersionSet v)
    {
        StringBuffer sb = new StringBuffer();
        AmqpVersion[] versionArray = new AmqpVersion[v.size()];
        v.toArray(versionArray);
        for (int i = 0; i < versionArray.length; i++)
        {
            if (i != 0)
            {
                sb.append(" || ");
            }
            if (versionArray.length > 1)
            {
                sb.append("(");
            }
            sb.append("major == (byte)" + versionArray[i].getMajor() + " && minor == (byte)" +
                      versionArray[i].getMinor());
            if (versionArray.length > 1)
            {
                sb.append(")");
            }
        }
        return sb.toString();
    }

    private String camelCaseName(String name, boolean upperFirstFlag)
    {
        StringBuffer ccn = new StringBuffer();
        String[] toks = name.split("[-_.\\ ]");
        for (int i = 0; i < toks.length; i++)
        {
            StringBuffer b = new StringBuffer(toks[i]);
            if (upperFirstFlag || i > 0)
            {
                b.setCharAt(0, Character.toUpperCase(toks[i].charAt(0)));
            }
            ccn.append(b);
        }
        return ccn.toString();
    }


    private String upperCaseName(String name)
    {
        StringBuffer ccn = new StringBuffer();
        String[] toks = name.split("[-_.\\ ]");
        for (int i = 0; i < toks.length; i++)
        {
            if (i != 0)
            {
                ccn.append('_');
            }
            ccn.append(toks[i].toUpperCase());


        }
        return ccn.toString();
    }


    public static Factory<JavaGenerator> _factoryInstance = new Factory<JavaGenerator>()
    {

        public JavaGenerator newInstance()
        {
            return new JavaGenerator();
        }
    };

    public static Factory<JavaGenerator> getFactory()
    {
        return _factoryInstance;
    }


    void processModelTemplate(NamedTemplate template, AmqpVersion version)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    void processClassTemplate(NamedTemplate template, AmqpClass amqpClass, AmqpVersion version)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    void processMethodTemplate(NamedTemplate template, AmqpClass amqpClass, AmqpMethod amqpMethod, AmqpVersion version)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    void processFieldTemplate(NamedTemplate template, AmqpClass amqpClass, AmqpMethod amqpMethod, AmqpField amqpField, AmqpVersion version)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }


}
